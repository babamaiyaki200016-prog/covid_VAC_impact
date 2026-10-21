#!/usr/bin/env Rscript
# ============================================================================
# economics.R -- DALYs and cost-effectiveness of the vaccination programme
# ----------------------------------------------------------------------------
# Health loss is expressed in DALYs = YLL + YLD.
#   YLL  years of life lost  = deaths_a x remaining life expectancy_a
#   YLD  years lived w/ disability = sum over morbidity states of
#        cases x disability weight x duration
# Cost-effectiveness compares the vaccination programme's incremental cost
# (dose delivery net of averted treatment costs) with the health gained.
# All monetary values 2021 USD; discounting is applied to the DALY stream and
# is a simplification (undiscounted deaths -> discounted YLL via annuity).
# ============================================================================

# discount a stream of `n` life-years at rate r, received continuously
.discount_annuity <- function(n_years, r) {
  if (r <= 0) return(n_years)
  (1 - exp(-r * n_years)) / r
}

# DALYs attributable to a single scenario's burden.
# `age_deaths` : numeric[n_age] cumulative deaths by age
# `total_inf`  : scalar cumulative infections (for symptomatic YLD)
# `total_hosp` : scalar cumulative hospitalisations (for severe YLD)
compute_dalys <- function(CFG, age_deaths, total_inf, total_hosp) {
  # YLL: discounted remaining life expectancy per death, by age
  yll_per_death <- vapply(CFG$life_expectancy_remaining,
                          .discount_annuity, numeric(1), r = CFG$discount_rate)
  yll <- sum(age_deaths * yll_per_death)

  symptomatic <- total_inf * CFG$prop_symptomatic
  yld_acute_mild   <- symptomatic * CFG$dw_symptomatic * CFG$dur_symptomatic
  yld_acute_severe <- total_hosp  * CFG$dw_severe      * CFG$dur_severe
  yld_postacute    <- symptomatic * CFG$prop_postacute *
                        CFG$dw_postacute * CFG$dur_postacute
  yld <- yld_acute_mild + yld_acute_severe + yld_postacute

  list(yll = yll, yld = yld, daly = yll + yld)
}

# Treatment cost of a scenario's clinical burden (hospital + ICU).
treatment_cost <- function(CFG, total_hosp, total_icu) {
  non_icu <- max(0, total_hosp - total_icu)
  non_icu * CFG$cost_hospitalisation + total_icu * CFG$cost_icu
}

# Full economic evaluation: observed (with-vaccination) vs a counterfactual.
# `obs` and `cf` are model-run outputs (data frames from run_model()).
economic_evaluation <- function(CFG, obs, cf, scale = 1, actual_doses = NULL) {
  # scale rescales modelled TRUE burden to a reporting basis if desired
  # (default 1 = evaluate on modelled true burden).
  a_obs <- attributes(obs); a_cf <- attributes(cf)

  d_obs <- compute_dalys(CFG, a_obs$age_cum_death * scale,
                         sum(a_obs$age_cum_inf) * scale,
                         sum(a_obs$age_cum_hosp) * scale)
  d_cf  <- compute_dalys(CFG, a_cf$age_cum_death * scale,
                         sum(a_cf$age_cum_inf) * scale,
                         sum(a_cf$age_cum_hosp) * scale)

  tot <- function(x, v) tail(x[[v]], 1)
  deaths_averted <- (tot(cf,"cum_death") - tot(obs,"cum_death")) * scale
  inf_averted    <- (tot(cf,"cum_inf")   - tot(obs,"cum_inf"))   * scale
  hosp_averted   <- (tot(cf,"cum_hosp")  - tot(obs,"cum_hosp"))  * scale
  icu_averted    <- (tot(cf,"cum_icu")   - tot(obs,"cum_icu"))   * scale
  daly_averted   <- d_cf$daly - d_obs$daly

  # persons protected (model S->V conversions) drive benefits; programme COST is
  # charged on ACTUAL doses delivered (real data) when supplied, since doses given
  # to already-immune people still incur cost.
  doses <- tot(obs, "cum_dose")
  cost_doses <- if (is.null(actual_doses)) doses else actual_doses
  # vaccine price + delivery cost per dose (each varied separately in sensitivity.R)
  programme_cost <- cost_doses * (CFG$vaccine_price_per_dose + CFG$delivery_cost_per_dose)
  treat_saved <- treatment_cost(CFG, tot(cf,"cum_hosp")*scale, tot(cf,"cum_icu")*scale) -
                 treatment_cost(CFG, tot(obs,"cum_hosp")*scale, tot(obs,"cum_icu")*scale)
  net_cost <- programme_cost - treat_saved   # payer perspective, health-system

  data.frame(
    doses_delivered      = cost_doses,
    persons_protected    = doses,
    deaths_averted       = deaths_averted,
    infections_averted   = inf_averted,
    hosp_averted         = hosp_averted,
    icu_averted          = icu_averted,
    yll_averted          = d_cf$yll - d_obs$yll,
    yld_averted          = d_cf$yld - d_obs$yld,
    dalys_averted        = daly_averted,
    programme_cost_usd    = programme_cost,
    treatment_cost_saved  = treat_saved,
    net_cost_usd          = net_cost,
    cost_per_death_averted = ifelse(deaths_averted > 0, net_cost/deaths_averted, NA),
    cost_per_hosp_averted  = ifelse(hosp_averted  > 0, net_cost/hosp_averted,  NA),
    cost_per_daly_averted  = ifelse(daly_averted  > 0, net_cost/daly_averted,  NA),
    icer_vs_threshold      = CFG$cet_threshold,
    cost_effective         = ifelse(daly_averted > 0,
                                    (net_cost/daly_averted) < CFG$cet_threshold, NA),
    stringsAsFactors = FALSE
  )
}

invisible(TRUE)
