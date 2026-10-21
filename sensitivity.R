#!/usr/bin/env Rscript
# ============================================================================
# sensitivity.R -- Sensitivity analyses for the Nigeria COVID-19 vaccination
#                  impact model
# ----------------------------------------------------------------------------
# (1) PRIMARY / structural: the seroprevalence attack-rate anchor. Reported
#     deaths alone do not identify the infection attack rate, so we re-calibrate
#     the whole model at a range of anchored cumulative attack rates and report
#     how deaths averted, DALYs averted and cost-effectiveness respond.
# (2) One-way parameter sensitivity (tornado): holding the base-case (50%)
#     calibration fixed, vary economic and vaccine-efficacy parameters across
#     plausible ranges and recompute the cost per DALY averted.
#
# Usage:  Rscript analysis/covid_vax_impact/sensitivity.R
# Outputs: sens_attack_rate.csv, sens_oneway.csv, figS3_*, figS4_* in outputs/.
# ============================================================================

suppressMessages({ library(deSolve); library(ggplot2); library(readr) })
HERE <- "analysis/covid_vax_impact"
source(file.path(HERE, "config.R"))
source(file.path(HERE, "model.R"))
source(file.path(HERE, "data.R"))
source(file.path(HERE, "calibrate.R"))
source(file.path(HERE, "scenarios.R"))
source(file.path(HERE, "economics.R"))
OUT <- file.path("outputs", "covid_vax_impact")

dat <- load_nigeria_data(CFG)
actual_doses <- sum(dat$doses_total_daily)   # all doses administered
zero_doses   <- rep(0, length(dat$doses_daily))

run_pair <- function(cfg, cal) {
  obs <- run_model(cfg, cal$beta0, dat$doses_daily, cal$npi_daily,
                   seas_amp = cal$seas_amp, seas_phase = cal$seas_phase)
  cf  <- run_model(cfg, cal$beta0, zero_doses, cal$npi_daily,
                   seas_amp = cal$seas_amp, seas_phase = cal$seas_phase)
  list(obs = obs, cf = cf)
}

# ---- (1) attack-rate anchor sensitivity (re-calibrate each target) ---------
targets <- c(0.30, 0.40, 0.50, 0.60, 0.70)
ar_rows <- list(); rank_rows <- list()
for (tg in targets) {
  cfg <- CFG; cfg$attack_rate_target <- tg
  cal <- calibrate_model(cfg, dat$death_daily, dat$doses_daily,
                         n_starts = 2, maxit = 2500)
  cfg$seas_amp <- cal$seas_amp; cfg$seas_phase <- cal$seas_phase
  pr  <- run_pair(cfg, cal)
  e   <- economic_evaluation(cfg, pr$obs, pr$cf, scale = 1,
                             actual_doses = actual_doses)
  ar_rows[[as.character(tg)]] <- data.frame(
    attack_target      = tg,
    attack_modelled    = round(cal$attack_rate_ref, 3),
    ascertainment_rho  = round(cal$rho_death, 4),
    fit_r2_log         = round(cal$r2_log, 3),
    true_deaths_obs    = round(tail(pr$obs$cum_death, 1)),
    deaths_averted     = round(e$deaths_averted),
    dalys_averted      = round(e$dalys_averted),
    cost_per_daly_usd  = round(e$cost_per_daly_averted),
    cost_effective_halfGDP = e$cost_effective)
  # does the strategy ranking hold under this calibration?
  sc <- run_scenarios(cfg, cal, build_scenarios(cfg, cal, dat$doses_daily))$table
  rank_rows[[as.character(tg)]] <- data.frame(
    attack_target = tg, scenario = sc$scenario,
    deaths_averted = round(sc$deaths_averted_vs_novax),
    rank = rank(-sc$deaths_averted_vs_novax, ties.method = "min"))
}
ar_tab <- do.call(rbind, ar_rows); rownames(ar_tab) <- NULL
write_csv(ar_tab, file.path(OUT, "sens_attack_rate.csv"))
rank_tab <- do.call(rbind, rank_rows); rownames(rank_tab) <- NULL
write_csv(rank_tab, file.path(OUT, "sens_strategy_ranking.csv"))

# ---- (2) one-way tornado on the base-case (50%) calibration ----------------
base_cal <- readRDS(file.path(OUT, "calibration.rds"))
base_cfg <- CFG
base_cfg$seas_amp <- base_cal$seas_amp; base_cfg$seas_phase <- base_cal$seas_phase
base_pair <- run_pair(base_cfg, base_cal)
base_e <- economic_evaluation(base_cfg, base_pair$obs, base_pair$cf,
                              scale = 1, actual_doses = actual_doses)
base_cpd <- base_e$cost_per_daly_averted

# each entry: label, field, low, high, dynamics? (does it change transmission/deaths)
params <- list(
  # vaccine price: AZ-like (base) to J&J-like US$10 (Ruiz 2023), same freight/wastage, 2021 US$
  list("Vaccine price per dose (US$)",   "vaccine_price_per_dose", CFG$vaccine_price_per_dose,
       CFG$vaccine_price_per_dose * 10 / 3, FALSE),
  # delivery: Nigeria financial (base, Noh 2024) to highest LMIC economic cost US$9.50 (Moi 2026)
  list("Delivery cost per dose (US$)",   "delivery_cost_per_dose", CFG$delivery_cost_per_dose,
       9.50 * 270.970 / 292.655, FALSE),
  list("ICU fraction of admissions",     "icu_fraction",         0.26,  0.38,  TRUE),
  list("Hospitalisation cost (x)",       "cost_hospitalisation", CFG$cost_hospitalisation * 0.5,
       CFG$cost_hospitalisation * 1.5, FALSE),
  list("ICU cost (x)",                   "cost_icu",             CFG$cost_icu * 0.5,
       CFG$cost_icu * 1.5, FALSE),
  list("Discount rate",                  "discount_rate",        0.00,  0.05,  FALSE),
  list("Symptomatic fraction",           "prop_symptomatic",     0.40,  0.70,  FALSE),
  list("Post-acute fraction",            "prop_postacute",       0.05,  0.20,  FALSE),
  list("VE vs death | infection",        "ve_death",             0.75,  0.95,  TRUE),
  list("VE vs infection",                "ve_infection",         0.45,  0.65,  TRUE)
)
one_rows <- list()
for (p in params) {
  lab <- p[[1]]; field <- p[[2]]; lo <- p[[3]]; hi <- p[[4]]; dyn <- p[[5]]
  vals <- c(lo, hi); out_cpd <- numeric(2); out_dav <- numeric(2)
  for (j in 1:2) {
    cfg <- base_cfg; cfg[[field]] <- vals[j]
    pr <- if (dyn) run_pair(cfg, base_cal) else base_pair
    e <- economic_evaluation(cfg, pr$obs, pr$cf, scale = 1,
                             actual_doses = actual_doses)
    out_cpd[j] <- e$cost_per_daly_averted; out_dav[j] <- e$deaths_averted
  }
  one_rows[[lab]] <- data.frame(
    parameter = lab, low_value = lo, high_value = hi,
    cost_per_daly_low = round(out_cpd[1]), cost_per_daly_high = round(out_cpd[2]),
    deaths_averted_low = round(out_dav[1]), deaths_averted_high = round(out_dav[2]),
    swing = round(abs(out_cpd[2] - out_cpd[1])))
}
one_tab <- do.call(rbind, one_rows); rownames(one_tab) <- NULL
one_tab <- one_tab[order(-one_tab$swing), ]
write_csv(one_tab, file.path(OUT, "sens_oneway.csv"))

# ---- supplementary figures -------------------------------------------------
theme_set(theme_minimal(base_size = 11))

ggsave(file.path(OUT, "figS3_attack_rate_sensitivity.png"),
  ggplot(ar_tab, aes(attack_target * 100)) +
    geom_col(aes(y = deaths_averted), fill = "#2166ac", width = 6) +
    geom_text(aes(y = deaths_averted,
              label = format(deaths_averted, big.mark = ",")), vjust = -0.4, size = 3) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(title = "Deaths averted vs anchored infection attack rate",
         subtitle = "Nigeria; each bar re-calibrates the whole model to that seroprevalence target",
         x = "Anchored cumulative attack rate by end-2021 (%)",
         y = "Deaths averted (modelled true)"),
  width = 8, height = 4.5, dpi = 120)

tb <- one_tab
tb$parameter <- factor(tb$parameter, levels = rev(tb$parameter))
tb$lo <- pmin(tb$cost_per_daly_low, tb$cost_per_daly_high)
tb$hi <- pmax(tb$cost_per_daly_low, tb$cost_per_daly_high)
ggsave(file.path(OUT, "figS4_tornado_cost_per_daly.png"),
  ggplot(tb) +
    geom_segment(aes(y = parameter, yend = parameter, x = lo, xend = hi),
                 linewidth = 6, colour = "#4393c3") +
    geom_vline(xintercept = base_cpd, linetype = 2, colour = "grey30") +
    labs(title = "One-way sensitivity of cost per DALY averted (tornado)",
         subtitle = sprintf("Base case (dashed) = US$%s per DALY; base-case calibration held fixed",
                            format(round(base_cpd), big.mark = ",")),
         x = "Cost per DALY averted (US$)", y = NULL),
  width = 8.5, height = 4.5, dpi = 120)

saveRDS(list(attack = ar_tab, oneway = one_tab, base_cpd = base_cpd),
        file.path(OUT, "sensitivity_results.rds"))
