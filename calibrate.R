#!/usr/bin/env Rscript
# ============================================================================
# calibrate.R -- Fit the model to Nigeria's OBSERVED reported death series
# ----------------------------------------------------------------------------
# Free parameters (all identifiable from the death curve + rollout):
#   beta0      baseline transmission rate
#   rho_death  death ascertainment fraction (reported = rho_death x true)
#   npi[k]     quarterly transmission multipliers (mobility/behaviour/NPI),
#              linearly interpolated to daily -- a latent trajectory, NOT data.
#
# Objective: weighted SSE on log(weekly reported deaths + 1), model vs observed,
# plus a roughness penalty that keeps the NPI trajectory smooth (avoids the
# curve absorbing noise or masking the vaccination signal). The counterfactual
# reuses these SAME fitted values, so the averted estimate is self-consistent.
# ============================================================================

# aggregate a daily vector to non-overlapping weekly sums (length ceil(n/7))
.weekly <- function(x) {
  n <- length(x); g <- ((seq_len(n) - 1) %/% 7) + 1
  as.numeric(tapply(x, g, sum, na.rm = TRUE))
}

# aggregate a daily vector to calendar-month sums (robust to sparse counts)
.monthly <- function(x, dates) {
  as.numeric(tapply(x, format(dates, "%Y-%m"), sum, na.rm = TRUE))
}

# build a daily NPI vector from quarterly knots (linear interpolation)
.npi_from_knots <- function(knots, times) {
  kt <- seq(0, max(times), length.out = length(knots))
  approx(kt, knots, xout = times, rule = 2)$y
}

calibrate_model <- function(CFG, obs_death_daily, doses_daily,
                            n_quarters = NULL, n_starts = 3, maxit = 3000) {
  n_days <- as.integer(CFG$date_end - CFG$date_start)
  times  <- 0:n_days
  if (is.null(n_quarters))
    n_quarters <- max(6, round(n_days / 75))    # ~14 knots (~monthly-bimonthly)

  dates  <- CFG$date_start + times
  # -------------------------------------------------------------------------
  # METHODS DECLARATION (report this in the manuscript):
  # The model is calibrated to MONTHLY reported deaths, not daily or weekly.
  # Rationale: Nigeria's daily/weekly COVID-19 death counts are very small and
  # highly irregular (frequent zero days, reporting-batch spikes, weekend
  # effects), so daily/weekly targets fit reporting noise rather than epidemic
  # signal and destabilise the optimiser. Monthly aggregation preserves the
  # wave structure used to identify transmission and NPI parameters while
  # averaging out administrative noise. The cumulative death total is anchored
  # separately so temporal aggregation does not bias the overall level.
  # -------------------------------------------------------------------------
  obs_m  <- .monthly(obs_death_daily, dates)          # monthly reported deaths
  obs_total <- sum(obs_death_daily, na.rm = TRUE)

  # ---- pre-build everything that is CONSTANT across calibration evals -------
  # Only beta0 and the NPI trajectory vary; the dose series and variant drivers
  # are fixed, so we build them once and swap npi_f each evaluation. This avoids
  # rebuilding 5 approxfuns and a full data frame on every objective call.
  idx <- .mk_index(CFG$n_age)
  y0  <- init_state(CFG, idx)
  drv <- build_drivers(CFG, times, doses_daily, rep(1, length(times)))
  parms_base <- c(list(
    idx = idx, n_age = CFG$n_age, eps = CFG$eps,
    beta0 = NA, sigma = CFG$sigma, gamma = CFG$gamma,
    contact_matrix = CFG$contact_matrix, N_age = CFG$N_age,
    ifr = CFG$ifr, ihr = CFG$ihr, icu_fraction = CFG$icu_fraction,
    ve_infection = CFG$ve_infection, ve_death = CFG$ve_death,
    ve_hosp = CFG$ve_hosp, vax_weight = CFG$vax_weight,
    seas_amp = NA, seas_phase = NA), drv)

  # reference-date row for the attack-rate (seroprevalence) anchor
  ar_idx <- as.integer(CFG$attack_rate_ref_date - CFG$date_start) + 1
  ar_idx <- max(2, min(length(times), ar_idx))

  # fast path: run the ODE and return DAILY true deaths + attack rate at ref date
  run_deaths <- function(beta0, npi_daily, seas_amp, seas_phase) {
    parms <- parms_base
    parms$beta0 <- beta0
    parms$seas_amp <- seas_amp
    parms$seas_phase <- seas_phase
    parms$npi_f <- approxfun(times, npi_daily, rule = 2)
    out <- deSolve::ode(y = y0, times = times, func = seir_deriv,
                        parms = parms, method = "adams", rtol = 1e-4, atol = 1)
    cumd <- rowSums(out[, 1 + idx$cDeath, drop = FALSE])
    attack <- sum(out[ar_idx, 1 + idx$cInf]) / CFG$population_total
    list(daily = c(cumd[1], diff(cumd)), attack = attack)
  }

  # transforms keep the optimiser unconstrained but parameters in range.
  # theta = [log beta0, logit rho, logit amp, logit phase, npi knots...]
  unpack <- function(theta) {
    beta0 <- exp(theta[1])                          # >0
    rho   <- plogis(theta[2])                       # (0,1)
    amp   <- 0.6 * plogis(theta[3])                 # seasonal amplitude (0,0.6)
    phase <- 365 * plogis(theta[4])                 # day-of-peak (0,365)
    npi   <- 0.15 + 1.05 * plogis(theta[-(1:4)])    # (0.15,1.20)
    list(beta0 = beta0, rho = rho, amp = amp, phase = phase, npi = npi)
  }

  lambda_rough <- 3    # roughness penalty (relaxed so waves are not smoothed out)

  # objective: shape on MONTHLY reported deaths (robust to sparse weekly counts)
  # + a strong anchor on the cumulative reported-death total.
  objective <- function(theta) {
    p <- unpack(theta)
    npi_daily <- .npi_from_knots(p$npi, times)
    r <- tryCatch(run_deaths(p$beta0, npi_daily, p$amp, p$phase),
                  error = function(e) NULL)
    if (is.null(r) || any(!is.finite(r$daily))) return(1e12)
    mod_rep_daily <- r$daily * p$rho
    mod_m <- .monthly(mod_rep_daily, dates)
    m <- min(length(mod_m), length(obs_m))
    shape <- sum((log1p(mod_m[1:m]) - log1p(obs_m[1:m]))^2)
    rough <- lambda_rough * sum(diff(p$npi)^2)
    lvl <- (log1p(sum(mod_rep_daily)) - log1p(obs_total))^2
    # attack-rate (seroprevalence) anchor identifies transmission amplitude
    anchor <- CFG$attack_rate_weight *
                (r$attack - CFG$attack_rate_target)^2
    shape + rough + 25 * lvl + anchor
  }

  # DATA-INFORMED INITIALISATION (report in manuscript): a flat NPI start traps
  # Nelder-Mead in a wrong-shape local optimum. We instead seed the NPI knots
  # from the observed death curve itself: smooth reported deaths (30-day mean),
  # shift 30 days earlier (deaths lag transmission), sample at the knot times
  # and rescale to a plausible transmission-multiplier range. This puts the
  # optimiser in the correct basin so it recovers the observed wave timing.
  npi0 <- function(v) qlogis(pmin(0.999, pmax(0.001, (v - 0.15) / 1.05)))
  sm  <- stats::filter(obs_death_daily, rep(1 / 30, 30), sides = 2)
  sm[is.na(sm)] <- 0
  sm_lead <- c(sm[31:length(sm)], rep(tail(sm, 1), 30))   # lead deaths by 30d
  kt <- seq(0, max(times), length.out = n_quarters)
  ki <- approx(times, sm_lead, xout = kt, rule = 2)$y
  npi_seed <- 0.40 + 0.75 * (ki - min(ki)) / (max(ki) - min(ki) + 1e-9)  # [0.40,1.15]
  amp0 <- qlogis(0.20 / 0.6)

  # primary data-informed start, plus two perturbations for robustness
  starts <- list(
    c(log(0.06), qlogis(0.04), amp0, qlogis(310 / 365), npi0(npi_seed)),
    c(log(0.05), qlogis(0.03), amp0, qlogis(310 / 365), npi0(npi_seed * 0.9)),
    c(log(0.08), qlogis(0.02), amp0, qlogis(310 / 365), npi0(npi_seed * 1.1))
  )
  starts <- starts[seq_len(min(n_starts, length(starts)))]
  best <- NULL
  for (s in starts) {
    f <- optim(s, objective, method = "Nelder-Mead",
               control = list(maxit = maxit, reltol = 1e-8))
    if (is.null(best) || f$value < best$value) best <- f
  }
  fit <- best
  p <- unpack(fit$par)
  npi_daily <- .npi_from_knots(p$npi, times)

  # fit diagnostics on the reported-death scale (monthly = calibration target)
  out <- run_model(CFG, p$beta0, doses_daily, npi_daily,
                   seas_amp = p$amp, seas_phase = p$phase)
  mod_m <- .monthly(out$inc_death * p$rho, dates)
  m <- min(length(mod_m), length(obs_m))
  ss_res <- sum((mod_m[1:m] - obs_m[1:m])^2)
  ss_tot <- sum((obs_m[1:m] - mean(obs_m[1:m]))^2)
  r2 <- 1 - ss_res / ss_tot
  # supplementary honest metrics: Pearson correlation and R2 on log1p scale
  pearson <- suppressWarnings(cor(mod_m[1:m], obs_m[1:m]))
  r2_log  <- 1 - sum((log1p(mod_m[1:m]) - log1p(obs_m[1:m]))^2) /
                 sum((log1p(obs_m[1:m]) - mean(log1p(obs_m[1:m])))^2)
  total_reported_modelled <- sum(out$inc_death) * p$rho
  ar_idx <- as.integer(CFG$attack_rate_ref_date - CFG$date_start) + 1
  attack_ref <- out$cum_inf[min(nrow(out), max(1, ar_idx))] / CFG$population_total

  list(beta0 = p$beta0, rho_death = p$rho, npi_knots = p$npi,
       npi_daily = npi_daily, times = times, r2 = r2,
       pearson = pearson, r2_log = r2_log,
       seas_amp = p$amp, seas_phase = p$phase,
       attack_rate_ref = attack_ref,
       attack_rate_target = CFG$attack_rate_target,
       total_reported_modelled = total_reported_modelled,
       total_reported_observed = obs_total,
       convergence = fit$convergence, value = fit$value,
       fit_obj = fit)
}

invisible(TRUE)
