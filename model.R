#!/usr/bin/env Rscript
# ============================================================================
# model.R -- Age-structured deterministic SEIR-V transmission model (deSolve)
# ----------------------------------------------------------------------------
# Compartments PER AGE GROUP a (n_age groups):
#   S   susceptible, unvaccinated
#   E   exposed (latent), unvaccinated
#   I   infectious, unvaccinated
#   R   recovered, unvaccinated
#   V   susceptible, vaccinated (reduced susceptibility)
#   Ev  exposed, vaccinated
#   Iv  infectious, vaccinated (reduced onward severity)
#   Rv  recovered, vaccinated
# Plus per-age accumulators (monotone, for incidence bookkeeping):
#   cInf  cumulative infections   cHosp cumulative hospitalisations
#   cICU  cumulative ICU          cDeath cumulative deaths
#   cDose cumulative persons vaccinated
#
# Time-varying drivers (passed as approxfun over model day t):
#   doses_f(t)  daily persons to vaccinate (from OBSERVED rollout or scenario)
#   vtrans_f(t) variant transmissibility multiplier
#   vsev_f(t)   variant severity multiplier
#   vve_f(t)    variant VE-against-infection multiplier
#   npi_f(t)    calibrated mobility/NPI transmission multiplier (<=1 dampens)
# ============================================================================

# ---- state-vector index helpers -------------------------------------------
.mk_index <- function(n_age) {
  comps <- c("S","E","I","R","V","Ev","Iv","Rv",
             "cInf","cHosp","cICU","cDeath","cDose")
  idx <- list()
  k <- 0
  for (cm in comps) { idx[[cm]] <- (k + 1):(k + n_age); k <- k + n_age }
  idx$comps <- comps
  idx$len <- k
  idx
}

# ---- derivative function ---------------------------------------------------
seir_deriv <- function(t, state, parms) {
  idx <- parms$idx; A <- parms$n_age; eps <- parms$eps

  S  <- state[idx$S];  E  <- state[idx$E];  I  <- state[idx$I];  R  <- state[idx$R]
  V  <- state[idx$V];  Ev <- state[idx$Ev]; Iv <- state[idx$Iv]; Rv <- state[idx$Rv]

  # time-varying multipliers
  vtrans <- parms$vtrans_f(t); vsev <- parms$vsev_f(t)
  vve    <- parms$vve_f(t);    npi  <- parms$npi_f(t)
  doses  <- max(0, parms$doses_f(t))

  # effective VE against infection this day (variant escape scales it)
  ve_inf_eff <- pmin(0.999, parms$ve_infection * vve)

  # seasonal forcing: dry-season (harmattan) transmission peak
  seas <- 1 + parms$seas_amp * cos(2 * pi * (t - parms$seas_phase) / 365)

  # force of infection by age: beta * variant * npi * seas * sum_b M[a,b]*Iinf_b/N_b
  Iinf <- I + Iv
  prev <- Iinf / parms$N_age                       # infectious prevalence by contactee age
  lambda <- parms$beta0 * vtrans * npi * seas *
              as.vector(parms$contact_matrix %*% prev)

  # vaccination allocation: distribute the day's doses across eligible S,
  # proportional to S_a * weight_a; never remove more than is available.
  elig_pool <- S * parms$vax_weight
  denom <- sum(elig_pool) + eps
  eff_doses <- min(doses, sum(elig_pool))          # cap: cannot vaccinate >S
  vax_rate <- eff_doses * elig_pool / denom        # persons/day, by age

  sig <- parms$sigma; gam <- parms$gamma
  ifr <- parms$ifr * vsev                          # variant-scaled severity
  ihr <- parms$ihr * vsev

  new_S  <- lambda * S
  new_V  <- (1 - ve_inf_eff) * lambda * V

  dS  <- -new_S - vax_rate
  dV  <-  vax_rate - new_V
  dE  <-  new_S - sig * E
  dEv <-  new_V - sig * Ev
  dI  <-  sig * E  - gam * I
  dIv <-  sig * Ev - gam * Iv

  # deaths drawn from infectious exit; vaccinated deaths reduced by ve_death
  death_u <- gam * ifr * I
  death_v <- gam * ifr * (1 - parms$ve_death) * Iv
  dR  <- gam * I  - death_u
  dRv <- gam * Iv - death_v

  # incidence accumulators (measured at symptom onset E->I)
  onset_u <- sig * E; onset_v <- sig * Ev
  hosp_inc <- ihr * onset_u + ihr * (1 - parms$ve_hosp) * onset_v
  dcInf   <- new_S + new_V
  dcHosp  <- hosp_inc
  dcICU   <- parms$icu_fraction * hosp_inc
  dcDeath <- death_u + death_v
  dcDose  <- vax_rate

  list(c(dS, dE, dI, dR, dV, dEv, dIv, dRv,
         dcInf, dcHosp, dcICU, dcDeath, dcDose))
}

# ---- build initial state ---------------------------------------------------
init_state <- function(CFG, idx) {
  A <- CFG$n_age
  z <- numeric(idx$len)
  S0 <- CFG$N_age
  seed <- numeric(A); seed[CFG$seed_age_index] <- CFG$seed_infections
  S0 <- S0 - seed
  z[idx$S] <- S0
  z[idx$I] <- seed
  z   # all V/E/R and accumulators start at 0
}

# ---- assemble time-varying driver functions --------------------------------
# `times` is the integer model-day grid; day 0 = CFG$date_start.
build_drivers <- function(CFG, times, doses_daily, npi_daily) {
  dates <- CFG$date_start + times
  # variant multipliers as step functions over calendar time
  vt <- rep(CFG$variants$transmissibility[1], length(times))
  vs <- rep(CFG$variants$severity[1],         length(times))
  vv <- rep(CFG$variants$ve_mult[1],          length(times))
  for (i in seq_len(nrow(CFG$variants))) {
    on <- dates >= CFG$variants$from[i]
    vt[on] <- CFG$variants$transmissibility[i]
    vs[on] <- CFG$variants$severity[i]
    vv[on] <- CFG$variants$ve_mult[i]
  }
  f <- function(y) approxfun(times, y, rule = 2)
  list(
    doses_f  = f(doses_daily),
    npi_f    = f(npi_daily),
    vtrans_f = f(vt),
    vsev_f   = f(vs),
    vve_f    = f(vv)
  )
}

# ---- run the model, return a tidy daily data frame -------------------------
run_model <- function(CFG, beta0, doses_daily, npi_daily,
                      vax_weight = CFG$vax_weight,
                      seas_amp = CFG$seas_amp, seas_phase = CFG$seas_phase) {
  idx <- .mk_index(CFG$n_age)
  n_days <- as.integer(CFG$date_end - CFG$date_start)
  times <- 0:n_days
  drivers <- build_drivers(CFG, times, doses_daily, npi_daily)

  parms <- c(list(
    idx = idx, n_age = CFG$n_age, eps = CFG$eps,
    beta0 = beta0, sigma = CFG$sigma, gamma = CFG$gamma,
    contact_matrix = CFG$contact_matrix, N_age = CFG$N_age,
    ifr = CFG$ifr, ihr = CFG$ihr, icu_fraction = CFG$icu_fraction,
    ve_infection = CFG$ve_infection, ve_death = CFG$ve_death,
    ve_hosp = CFG$ve_hosp, vax_weight = vax_weight,
    seas_amp = seas_amp, seas_phase = seas_phase
  ), drivers)

  y0 <- init_state(CFG, idx)
  # adams + loose tolerances: >2x faster, deaths stable to 4 sig figs at this
  # population scale (atol = 1 person, rtol = 1e-4).
  out <- deSolve::ode(y = y0, times = times, func = seir_deriv,
                      parms = parms, method = "adams",
                      rtol = 1e-4, atol = 1)
  out <- as.data.frame(out)

  # collapse per-age accumulators to national daily incidence
  agg <- function(comp) rowSums(out[, 1 + idx[[comp]], drop = FALSE])
  cum <- data.frame(
    day        = out$time,
    date       = CFG$date_start + out$time,
    cum_inf    = agg("cInf"),
    cum_hosp   = agg("cHosp"),
    cum_icu    = agg("cICU"),
    cum_death  = agg("cDeath"),
    cum_dose   = agg("cDose")
  )
  # daily increments
  d <- cum
  for (v in c("inf","hosp","icu","death","dose")) {
    cv <- paste0("cum_", v)
    d[[paste0("inc_", v)]] <- c(cum[[cv]][1], diff(cum[[cv]]))
  }
  # per-age cumulative deaths (needed for age-specific DALYs / YLL)
  age_death <- out[nrow(out), 1 + idx$cDeath]
  attr(d, "age_cum_death") <- as.numeric(age_death)
  attr(d, "age_cum_inf")   <- as.numeric(out[nrow(out), 1 + idx$cInf])
  attr(d, "age_cum_hosp")  <- as.numeric(out[nrow(out), 1 + idx$cHosp])
  d
}

invisible(TRUE)
