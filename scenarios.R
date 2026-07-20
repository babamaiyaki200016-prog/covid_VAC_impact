#!/usr/bin/env Rscript
# ============================================================================
# scenarios.R -- Observed rollout vs counterfactual vaccination strategies
# ----------------------------------------------------------------------------
# Every scenario reuses the SAME calibrated beta0 and NPI trajectory; only the
# vaccination programme (dose timing, total, or age allocation) changes. The
# difference between a scenario and the no-vaccination counterfactual is the
# burden that programme averted.
#
# Scenarios:
#   observed          actual reported rollout (reference)
#   no_vaccination    doses = 0 (primary counterfactual)
#   earlier_3months   observed doses shifted 90 days earlier
#   coverage_70       observed dose SHAPE scaled to 70% of eligible population
#   elderly_first     observed doses, allocation strongly favouring 65+ / 50-64
#   adults_first      observed doses, allocation favouring working-age adults
# ============================================================================

# shift a daily series earlier by `k` days (pad the tail with the last value's 0)
.shift_earlier <- function(x, k) {
  n <- length(x)
  c(x[(k + 1):n], rep(0, k))
}

build_scenarios <- function(CFG, cal, doses_observed) {
  times <- cal$times
  eligible_pop <- sum(CFG$N_age[CFG$vax_weight > 0])
  target_70 <- 0.70 * eligible_pop

  # scale observed dose shape up/down to hit a cumulative coverage target
  scale_to_total <- function(x, target) {
    s <- sum(x); if (s <= 0) return(x); x * (target / s)
  }

  list(
    observed = list(
      doses = doses_observed, weight = CFG$vax_weight),
    no_vaccination = list(
      doses = rep(0, length(doses_observed)), weight = CFG$vax_weight),
    earlier_3months = list(
      doses = .shift_earlier(doses_observed, 90), weight = CFG$vax_weight),
    coverage_70 = list(
      doses = scale_to_total(doses_observed, target_70), weight = CFG$vax_weight),
    elderly_first = list(
      doses = doses_observed, weight = c(0, 0.2, 0.4, 2.0, 4.0)),
    adults_first = list(
      doses = doses_observed, weight = c(0, 2.0, 2.0, 1.0, 1.0))
  )
}

run_scenarios <- function(CFG, cal, scenarios) {
  totals <- function(out) {
    tot <- function(v) tail(out[[v]], 1)
    data.frame(
      true_infections = tot("cum_inf"),
      true_hosp       = tot("cum_hosp"),
      true_icu        = tot("cum_icu"),
      true_deaths     = tot("cum_death"),
      reported_deaths = tot("cum_death") * cal$rho_death,
      doses_delivered = tot("cum_dose")
    )
  }
  runs <- list(); rows <- list()
  for (nm in names(scenarios)) {
    sc <- scenarios[[nm]]
    out <- run_model(CFG, cal$beta0, sc$doses, cal$npi_daily,
                     vax_weight = sc$weight)
    runs[[nm]] <- out
    rows[[nm]] <- cbind(scenario = nm, totals(out))
  }
  tab <- do.call(rbind, rows)
  rownames(tab) <- NULL

  # burden averted relative to the no-vaccination counterfactual
  base <- tab[tab$scenario == "no_vaccination", ]
  tab$deaths_averted_vs_novax <- base$true_deaths - tab$true_deaths
  tab$reported_deaths_averted  <- base$reported_deaths - tab$reported_deaths
  tab$infections_averted_vs_novax <- base$true_infections - tab$true_infections
  tab$hosp_averted_vs_novax    <- base$true_hosp - tab$true_hosp
  tab$icu_averted_vs_novax     <- base$true_icu - tab$true_icu

  # people are whole: round all person-count columns for reporting
  num_cols <- setdiff(names(tab), "scenario")
  tab[num_cols] <- lapply(tab[num_cols], round)

  list(table = tab, runs = runs)
}

invisible(TRUE)
