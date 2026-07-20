#!/usr/bin/env Rscript
# ============================================================================
# run.R -- Orchestrator: Nigeria SARS-CoV-2 vaccination impact analysis
# ----------------------------------------------------------------------------
# Pipeline:
#   1. config.R   parameters (cited literature + clearly flagged placeholders)
#   2. data.R     REAL Nigeria cases/deaths/vaccination (OWID -> WHO/NCDC/NPHCDA)
#   3. calibrate  fit beta0, death ascertainment, NPI trajectory to deaths
#   4. scenarios  observed rollout vs no-vax + alternative strategies
#   5. economics  DALYs averted, costs, cost-effectiveness (ICER)
#   6. outputs    CSV tables + diagnostic/results plots + summary report
#
# Usage:   Rscript analysis/covid_vax_impact/run.R
# ============================================================================

suppressMessages({
  library(deSolve); library(ggplot2); library(dplyr); library(tidyr); library(readr)
})

HERE <- "analysis/covid_vax_impact"
source(file.path(HERE, "config.R"))
source(file.path(HERE, "model.R"))
source(file.path(HERE, "data.R"))
source(file.path(HERE, "calibrate.R"))
source(file.path(HERE, "scenarios.R"))
source(file.path(HERE, "economics.R"))

OUT <- file.path("outputs", "covid_vax_impact")
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

# ---- 1-2. data -------------------------------------------------------------
dat <- load_nigeria_data(CFG)

# ---- 3. calibration (cached; delete the .rds to force a refit) --------------
cache <- file.path(OUT, "calibration.rds")
srcs  <- file.path(HERE, c("config.R", "model.R", "calibrate.R"))
fresh <- file.exists(cache) &&
         file.info(cache)$mtime > max(file.info(srcs)$mtime)
if (fresh) {
  cal <- readRDS(cache)
} else {
  cal <- calibrate_model(CFG, dat$death_daily, dat$doses_daily)
  saveRDS(cal, cache)
}
# propagate fitted seasonality so every scenario run uses the same forcing
CFG$seas_amp   <- cal$seas_amp
CFG$seas_phase <- cal$seas_phase

# ---- 4. scenarios ----------------------------------------------------------
scen <- build_scenarios(CFG, cal, dat$doses_daily)
res  <- run_scenarios(CFG, cal, scen)
write_csv(res$table, file.path(OUT, "scenario_comparison.csv"))

# ---- 5. economics (observed rollout vs no-vaccination) ---------------------
obs_run <- res$runs$observed
cf_run  <- res$runs$no_vaccination
# report on TRUE modelled burden and on the REPORTED scale (x rho_death)
# programme cost is charged on ACTUAL doses delivered (real OWID/NPHCDA series)
actual_doses <- sum(dat$doses_daily)
econ_true     <- economic_evaluation(CFG, obs_run, cf_run, scale = 1,
                                     actual_doses = actual_doses)
econ_reported <- economic_evaluation(CFG, obs_run, cf_run, scale = cal$rho_death,
                                     actual_doses = actual_doses)
econ_true$basis     <- "modelled_true_burden"
econ_reported$basis <- "reported_scale"
econ <- rbind(econ_true, econ_reported)
# whole people / whole dollars in the reported table (keep boolean & basis)
round_cols <- setdiff(names(econ), c("cost_effective", "basis"))
econ[round_cols] <- lapply(econ[round_cols], round)
write_csv(econ, file.path(OUT, "economic_evaluation.csv"))

# headline averted table (observed programme vs no vaccination)
headline <- data.frame(
  metric = c("Infections averted", "Hospitalisations averted",
             "ICU admissions averted", "Deaths averted (modelled true)",
             "Deaths averted (reported scale)", "DALYs averted",
             "Persons vaccinated", "Programme cost (USD)",
             "Net cost (USD)", "Cost per death averted (USD)",
             "Cost per DALY averted (USD)",
             "Cost-effective at 0.5x GDPpc?"),
  value = c(
    round(econ_true$infections_averted),
    round(econ_true$hosp_averted),
    round(econ_true$icu_averted),
    round(econ_true$deaths_averted),
    round(econ_reported$deaths_averted),
    round(econ_true$dalys_averted),
    round(econ_true$doses_delivered),
    round(econ_true$programme_cost_usd),
    round(econ_true$net_cost_usd),
    round(econ_true$cost_per_death_averted),
    round(econ_true$cost_per_daly_averted),
    econ_true$cost_effective))
write_csv(headline, file.path(OUT, "headline_results.csv"))

# ---- 6. plots --------------------------------------------------------------
theme_set(theme_minimal(base_size = 11))

# (a) calibration fit: observed vs modelled reported deaths (monthly)
mth <- function(x) as.numeric(tapply(x, format(obs_run$date, "%Y-%m"), sum))
obs_m <- mth(dat$death_daily)
mod_m <- mth(obs_run$inc_death * cal$rho_death)
month_start <- as.Date(paste0(sort(unique(format(obs_run$date, "%Y-%m"))), "-01"))
fitdf <- data.frame(
  month = month_start,
  Observed = obs_m, Modelled = mod_m) |>
  pivot_longer(-month, names_to = "series", values_to = "deaths")
ggsave(file.path(OUT, "fig_calibration_fit.png"),
  ggplot(fitdf, aes(month, deaths, colour = series)) +
    geom_line(linewidth = 0.7) +
    labs(title = "Calibration: monthly reported COVID-19 deaths, Nigeria",
         subtitle = sprintf("Model vs observed reported deaths (monthly R2 = %.2f)", cal$r2),
         x = NULL, y = "Monthly reported deaths", colour = NULL),
  width = 9, height = 4.5, dpi = 120)

# (b) daily deaths: observed rollout vs no-vaccination counterfactual (true)
cmp <- data.frame(
  date = obs_run$date,
  `Observed rollout` = obs_run$inc_death,
  `No vaccination`   = cf_run$inc_death, check.names = FALSE) |>
  pivot_longer(-date, names_to = "scenario", values_to = "deaths")
ggsave(file.path(OUT, "fig_deaths_counterfactual.png"),
  ggplot(cmp, aes(date, deaths, colour = scenario)) +
    geom_line(linewidth = 0.6) +
    labs(title = "Modelled daily COVID-19 deaths: with vs without vaccination",
         subtitle = "Nigeria; shaded gap = deaths averted by the programme",
         x = NULL, y = "Modelled daily deaths (true)", colour = NULL),
  width = 9, height = 4.5, dpi = 120)

# (c) deaths averted vs no-vaccination, by scenario
sb <- res$table[res$table$scenario != "no_vaccination", ]
sb$scenario <- factor(sb$scenario, levels = sb$scenario[order(sb$deaths_averted_vs_novax)])
ggsave(file.path(OUT, "fig_scenarios_deaths_averted.png"),
  ggplot(sb, aes(deaths_averted_vs_novax, scenario)) +
    geom_col(fill = "#2c7fb8") +
    geom_text(aes(label = format(round(deaths_averted_vs_novax), big.mark=",")),
              hjust = -0.1, size = 3) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.18))) +
    labs(title = "Deaths averted vs no vaccination, by strategy (modelled true)",
         subtitle = "Nigeria SARS-CoV-2, 2020-2022", x = "Deaths averted", y = NULL),
  width = 9, height = 4.5, dpi = 120)

# (d) fitted NPI/mobility trajectory (latent, interpretable)
npidf <- data.frame(date = dat$grid$date, npi = cal$npi_daily)
ggsave(file.path(OUT, "fig_fitted_npi.png"),
  ggplot(npidf, aes(date, npi)) + geom_line(colour = "#d95f02", linewidth = 0.7) +
    geom_hline(yintercept = 1, linetype = 2, colour = "grey60") +
    labs(title = "Fitted transmission multiplier (mobility / NPI / behaviour)",
         subtitle = "Latent trajectory recovered during calibration; 1 = baseline",
         x = NULL, y = "Transmission multiplier"),
  width = 9, height = 4, dpi = 120)

# ---- supplementary figures ------------------------------------------------
# (S1) modelled cumulative infection attack rate with the seroprevalence anchor
attackdf <- data.frame(date = obs_run$date,
                       attack = 100 * obs_run$cum_inf / CFG$population_total)
anchor_pt <- data.frame(date = CFG$attack_rate_ref_date,
                        attack = 100 * CFG$attack_rate_target)
ggsave(file.path(OUT, "figS1_attack_rate.png"),
  ggplot(attackdf, aes(date, attack)) +
    geom_line(colour = "#1b7837", linewidth = 0.8) +
    geom_point(data = anchor_pt, aes(date, attack), colour = "#d73027", size = 3) +
    geom_text(data = anchor_pt, aes(date, attack,
              label = sprintf("seroprevalence anchor: %.0f%%", attack)),
              vjust = -1, hjust = 1, size = 3, colour = "#d73027") +
    labs(title = "Modelled cumulative SARS-CoV-2 infection attack rate, Nigeria",
         subtitle = "Green = model; red = external seroprevalence anchor at reference date",
         x = NULL, y = "Ever-infected (% of population)"),
  width = 9, height = 4.5, dpi = 120)

# (S2) age-specific deaths averted (no-vaccination minus observed, modelled true)
age_obs <- attr(obs_run, "age_cum_death")
age_cf  <- attr(cf_run,  "age_cum_death")
ageavdf <- data.frame(age = factor(CFG$age_labels, levels = CFG$age_labels),
                      averted = age_cf - age_obs)
ggsave(file.path(OUT, "figS2_age_deaths_averted.png"),
  ggplot(ageavdf, aes(age, averted)) +
    geom_col(fill = "#762a83") +
    geom_text(aes(label = format(round(averted), big.mark = ",")),
              vjust = -0.4, size = 3) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(title = "Deaths averted by the vaccination programme, by age group",
         subtitle = "Nigeria SARS-CoV-2, 2020-2022 (modelled true deaths)",
         x = "Age group (years)", y = "Deaths averted"),
  width = 8, height = 4.5, dpi = 120)

# ---- 7. markdown summary ---------------------------------------------------
e <- econ_true; er <- econ_reported
fmtn <- function(x) format(round(x), big.mark = ",")
report <- c(
  "# Nigeria SARS-CoV-2 vaccination impact — results summary",
  "",
  sprintf("_Generated %s. Real data: %s._", Sys.Date(), dat$provenance$cases_source),
  "",
  "## Model & calibration",
  sprintf("- Age-structured SEIR-V model, %d age groups, deterministic (deSolve).", CFG$n_age),
  sprintf("- Calibrated to **observed reported deaths** (n = %s, OWID/WHO/NCDC).",
          fmtn(dat$provenance$observed_total_reported_deaths)),
  sprintf("- Fitted baseline transmission beta0 = %.3f; death ascertainment rho = %.3f.",
          cal$beta0, cal$rho_death),
  sprintf("- Monthly-death fit: R2 = %.2f, R2(log) = %.2f, Pearson r = %.2f (limited by sparse, spiky reported counts).",
          cal$r2, cal$r2_log, cal$pearson),
  sprintf("- Modelled reported deaths = %s vs observed %s (level check).",
          fmtn(cal$total_reported_modelled), fmtn(cal$total_reported_observed)),
  sprintf("- Seasonal forcing: amplitude %.2f, transmission peak ~day-of-year %.0f (dry season).",
          cal$seas_amp, cal$seas_phase %% 365),
  sprintf("- Attack-rate anchor: modelled %.0f%% ever-infected by %s vs target %.0f%% (seroprevalence, placeholder).",
          100 * cal$attack_rate_ref, CFG$attack_rate_ref_date, 100 * cal$attack_rate_target),
  sprintf("- Implied modelled TRUE deaths = %s (reported deaths / ascertainment).",
          fmtn(tail(obs_run$cum_death, 1))),
  "",
  "## Impact of the observed rollout (vs no vaccination)",
  sprintf("- Infections averted: **%s**", fmtn(e$infections_averted)),
  sprintf("- Hospitalisations averted: **%s**", fmtn(e$hosp_averted)),
  sprintf("- ICU admissions averted: **%s**", fmtn(e$icu_averted)),
  sprintf("- Deaths averted (modelled true): **%s**", fmtn(e$deaths_averted)),
  sprintf("- Deaths averted (reported scale): **%s**", fmtn(er$deaths_averted)),
  sprintf("- DALYs averted: **%s** (YLL %s + YLD %s)",
          fmtn(e$dalys_averted), fmtn(e$yll_averted), fmtn(e$yld_averted)),
  "",
  "## Economic evaluation (health-system perspective, 2021 USD)",
  sprintf("- Persons vaccinated: %s; programme cost: $%s", fmtn(e$doses_delivered), fmtn(e$programme_cost_usd)),
  sprintf("- Treatment cost averted: $%s; net cost: $%s", fmtn(e$treatment_cost_saved), fmtn(e$net_cost_usd)),
  sprintf("- Cost per death averted: $%s", fmtn(e$cost_per_death_averted)),
  sprintf("- Cost per DALY averted: $%s (threshold 0.5x GDPpc = $%s)",
          fmtn(e$cost_per_daly_averted), fmtn(CFG$cet_threshold)),
  sprintf("- **Cost-effective at threshold: %s**", e$cost_effective),
  "",
  "## Counterfactual strategies (deaths averted vs no vaccination, modelled true)",
  "| scenario | true deaths | deaths averted | infections averted | doses |",
  "|---|---|---|---|---|",
  apply(res$table, 1, function(r) sprintf("| %s | %s | %s | %s | %s |",
        r["scenario"], fmtn(as.numeric(r["true_deaths"])),
        fmtn(as.numeric(r["deaths_averted_vs_novax"])),
        fmtn(as.numeric(r["infections_averted_vs_novax"])),
        fmtn(as.numeric(r["doses_delivered"])))),
  "",
  "## Outputs",
  "- `headline_results.csv`, `scenario_comparison.csv`, `economic_evaluation.csv`",
  "- `nigeria_observed_daily.csv`, `data_provenance.json`",
  "- `fig_calibration_fit.png`, `fig_deaths_counterfactual.png`,",
  "  `fig_scenarios_deaths_averted.png`, `fig_fitted_npi.png`",
  "",
  "> Epidemiological DATA are real (OWID/WHO/NCDC/NPHCDA). Severity, vaccine-",
  "> effectiveness, contact and cost PARAMETERS are literature values / flagged",
  "> placeholders in `config.R` — replace with Nigeria-specific estimates and",
  "> run a probabilistic sensitivity analysis before drawing policy conclusions."
)
writeLines(report, file.path(OUT, "RESULTS_SUMMARY.md"))
