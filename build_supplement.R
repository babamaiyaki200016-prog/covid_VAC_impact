#!/usr/bin/env Rscript
# ============================================================================
# build_supplement.R -- regenerate the Supplementary appendix from model outputs
# ----------------------------------------------------------------------------
# Every number in Tables S1-S4 is read from config.R or outputs/covid_vax_impact
# so the appendix can never drift from the analysis. Writes Markdown and, if
# pandoc is available, a .docx alongside it.
#
# Usage:   Rscript analysis/covid_vax_impact/build_supplement.R [out_dir]
# ============================================================================

HERE <- "analysis/covid_vax_impact"
RES  <- file.path("outputs", "covid_vax_impact")
args <- commandArgs(trailingOnly = TRUE)
OUTD <- if (length(args)) args[1] else "reports"
source(file.path(HERE, "config.R"))

econ <- read.csv(file.path(RES, "economic_evaluation.csv"))
ar   <- read.csv(file.path(RES, "sens_attack_rate.csv"))
ow   <- read.csv(file.path(RES, "sens_oneway.csv"))
tr <- econ[econ$basis == "modelled_true_burden", ]; rp <- econ[econ$basis == "reported_scale", ]
persons <- read.csv(file.path(RES, "headline_results.csv"))
persons <- persons$value[persons$metric == "Persons vaccinated (>=1 dose)"]

n0  <- function(x) format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)
pc  <- function(x, d = 1) paste0(formatC(100 * x, format = "f", digits = d), "%")
f2  <- function(x, d = 2) formatC(x, format = "f", digits = d)
yn  <- function(x) ifelse(as.logical(x), "Yes", "No")
row <- function(...) paste0("| ", paste(gsub("|", "/", c(...), fixed = TRUE), collapse = " | "), " |")
thr <- CFG$cet_threshold

L <- c(
"# Supplementary appendix", "",
"**Health impact, timing and targeting of COVID-19 vaccination in Nigeria: a counterfactual transmission-modelling and cost-effectiveness analysis**", "",
"*This appendix provides the supplementary methods, tables (S1–S5) and figures (S1–S4) referenced in the main text. Machine-readable versions of the supplementary tables are provided as CSV files in the source_data folder. Superscript numbers refer to the reference list in the main manuscript.*", "",
"## Supplementary methods", "",
"### Compartmental structure", "",
"For each of five age groups *a* (0–14, 15–29, 30–49, 50–64, ≥65 years) the model tracks unvaccinated compartments—susceptible (S), exposed (E), infectious (I) and recovered (R)—and parallel vaccinated compartments (V, Eᵥ, Iᵥ, Rᵥ), with monotone accumulators for cumulative infections, hospitalisations, intensive-care admissions, deaths and doses.", "",
"### Force of infection", "",
"The age-specific force of infection is", "",
"  λₐ(t) = β₀ · τ(t) · m(t) · s(t) · Σ_b C_ab · (I_b + Iᵥ,b) / N_b", "",
"where β₀ is the baseline transmission rate; τ(t) the variant transmissibility multiplier; m(t) the fitted latent mobility/non-pharmaceutical-intervention multiplier; s(t) = 1 + A·cos(2π(t − φ)/365) the seasonal term with amplitude A and peak day-of-year φ; C_ab the mean daily contacts between age groups *a* and *b*; and N_b the population of group *b*. Vaccinated susceptibles acquire infection at rate (1 − VE_infection·ε(t))·λₐ, where ε(t) is the variant immune-escape multiplier.", "",
"### Progression, severity and vaccination", "",
sprintf("Exposed individuals become infectious at rate σ = 1/%g per day and recover or die at rate γ = 1/%g per day. Deaths accrue as γ·IFRₐ·κ(t)·Iₐ (unvaccinated) and γ·IFRₐ·κ(t)·(1 − VE_death)·Iᵥ,ₐ (vaccinated), where κ(t) is the variant severity multiplier; hospitalisation and intensive-care incidence are derived analogously from IHRₐ and the intensive-care fraction (%s of admissions). Each day’s first doses are allocated across eligible age groups in proportion to the remaining susceptible pool and the prioritisation weight, and cannot exceed available susceptibles. Programme cost is charged on all doses administered (including second and booster doses).",
        CFG$latent_period, CFG$infectious_period, pc(CFG$icu_fraction, 0)), "",
"### Contact matrix", "",
"Mean daily contacts, all settings (contactor in rows, contactee in columns): the synthetic contact matrix for Nigeria of Prem et al.,<sup>26</sup> collapsed from 16 five-year age groups to the model’s five bands by weighting contactor groups by their 2020 population (UN World Population Prospects).<sup>54</sup> Reproduce with derive_contact_matrix.R.", "",
row("Contactor \\ Contactee", CFG$age_labels), paste0("|---", strrep("|---:", 5), "|"))
cm <- CFG$contact_matrix
for (i in 1:5) L <- c(L, row(CFG$age_labels[i], f2(cm[i, ], 1)))

L <- c(L, "", "## Supplementary tables", "",
"### Table S1. Age-specific demographic and severity parameters", "",
row("Age group (years)", "Population fraction", "Infection-fatality ratio", "Infection-hospitalisation ratio", "Remaining life expectancy (years)"),
"|---|---:|---:|---:|---:|")
for (i in 1:5) L <- c(L, row(CFG$age_labels[i], pc(CFG$age_frac[i]), pc(CFG$ifr[i], 3), pc(CFG$ihr[i], 2),
                             CFG$life_expectancy_remaining[i]))
L <- c(L, "",
sprintf("Infection-fatality ratios from model-based and meta-analytic estimates;<sup>28,29,30,31</sup> infection-hospitalisation ratios from severity estimates;<sup>32</sup> population structure from UN World Population Prospects;<sup>54</sup> remaining life expectancy at the age-band midpoint interpolated from the WHO 2019 life table for Nigeria.<sup>57</sup> The intensive-care fraction of hospitalised patients (%s; meta-analysis<sup>61</sup>) and vaccine effectiveness (%s infection, %s severe disease, %s death) were applied across all age groups.",
        f2(CFG$icu_fraction), pc(CFG$ve_infection, 0), pc(CFG$ve_hosp, 0), pc(CFG$ve_death, 0)), "",
"### Table S2. Full economic evaluation (health-system perspective, 2021 US$)", "",
row("Metric", "Modelled-true burden", "Reported-death scale"), "|---|---:|---:|",
row("Persons vaccinated (≥1 dose)", n0(persons), n0(persons)),
row("Doses administered (costed)", n0(tr$doses_delivered), n0(rp$doses_delivered)),
row("Persons protected in model (S→V)", n0(tr$persons_protected), n0(rp$persons_protected)),
row("Deaths averted", n0(tr$deaths_averted), n0(rp$deaths_averted)),
row("Infections averted", n0(tr$infections_averted), n0(rp$infections_averted)),
row("Hospitalisations averted", n0(tr$hosp_averted), n0(rp$hosp_averted)),
row("ICU admissions averted", n0(tr$icu_averted), n0(rp$icu_averted)),
row("DALYs averted (YLL + YLD)", n0(tr$dalys_averted), n0(rp$dalys_averted)),
row(sprintf("Programme cost (US$; vaccine US$%.2f + delivery US$%.2f per dose)", CFG$vaccine_price_per_dose, CFG$delivery_cost_per_dose), n0(tr$programme_cost_usd), n0(rp$programme_cost_usd)),
row("Treatment cost averted (US$)", n0(tr$treatment_cost_saved), n0(rp$treatment_cost_saved)),
row("Net cost (US$)", n0(tr$net_cost_usd), n0(rp$net_cost_usd)),
row("Cost per death averted (US$)", n0(tr$cost_per_death_averted), n0(rp$cost_per_death_averted)),
row("Cost per DALY averted (US$)", n0(tr$cost_per_daly_averted), n0(rp$cost_per_daly_averted)),
row(sprintf("Cost-effective at 0.5×GDP per capita (US$%s)?", n0(thr)), yn(tr$cost_effective), yn(rp$cost_effective)), "",
"The reported-death scale multiplies modelled true outcomes by the fitted death-ascertainment fraction.", "",
"### Table S3. Attack-rate (seroprevalence anchor) sensitivity analysis", "",
"Each row re-calibrates the entire model to the anchored cumulative attack rate.", "",
row("Anchored attack rate (target)", "Modelled attack rate", "Ascertainment (ρ)", "Fit R²(log)", "True deaths (observed rollout)", "Deaths averted", "DALYs averted", "Cost per DALY (US$)", "Cost-effective at 0.5×GDP?"),
"|---:|---:|---:|---:|---:|---:|---:|---:|:--|")
for (i in seq_len(nrow(ar))) {
  lab <- paste0(pc(ar$attack_target[i], 0), ifelse(abs(ar$attack_target[i] - 0.5) < 1e-9, " (base case)", ""))
  L <- c(L, row(lab, pc(ar$attack_modelled[i]), f2(ar$ascertainment_rho[i], 3), f2(ar$fit_r2_log[i]),
                n0(ar$true_deaths_obs[i]), n0(ar$deaths_averted[i]), n0(ar$dalys_averted[i]),
                n0(ar$cost_per_daly_usd[i]), yn(ar$cost_effective_halfGDP[i])))
}
L <- c(L, "",
ifelse(all(diff(ar$deaths_averted) > 0),
  "Deaths averted rose with the anchored attack rate: a larger infection burden leaves more deaths for vaccination to prevent. Reported deaths alone cannot distinguish between these calibrations. (The base-case main-text figures come from the primary 3-start calibration; small differences from the 50% row here reflect the 2-start settings used for the sensitivity sweep.)",
  "Deaths averted are not monotonic in the attack rate because the counterfactual depends on the timing of infection relative to the rollout, which reported deaths do not constrain. (The base-case main-text figures come from the primary 3-start calibration; small differences from the 50% row here reflect the 2-start settings used for the sensitivity sweep.)"), "",
"### Table S4. One-way sensitivity analysis of cost per DALY averted (tornado)", "",
sprintf("Base-case calibration held fixed; base-case cost per DALY averted = US$%s. Rows ordered by influence.", n0(tr$cost_per_daly_averted)), "",
row("Parameter", "Low value", "High value", "Cost/DALY at low (US$)", "Cost/DALY at high (US$)", "Deaths averted (low–high)", "Swing (US$)"),
"|---|---:|---:|---:|---:|---:|---:|")
ow <- ow[order(-ow$swing), ]
fmtv <- function(v) ifelse(v >= 100, n0(v), formatC(v, format = "f", digits = 2))
for (i in seq_len(nrow(ow))) {
  dlo <- ow$deaths_averted_low[i]; dhi <- ow$deaths_averted_high[i]
  L <- c(L, row(ow$parameter[i], fmtv(ow$low_value[i]), fmtv(ow$high_value[i]),
                n0(ow$cost_per_daly_low[i]), n0(ow$cost_per_daly_high[i]),
                ifelse(dlo == dhi, n0(dlo), paste0(n0(dlo), "–", n0(dhi))), n0(ow$swing[i])))
}
allabove <- all(c(ow$cost_per_daly_low, ow$cost_per_daly_high) > thr)
L <- c(L, "", sprintf("The cost per DALY averted %s the half-GDP threshold (US$%s) across every one-way variation.",
                      ifelse(allabove, "exceeded", "did not exceed"), n0(thr)), "",
"### Table S5. Deaths averted by strategy under each attack-rate calibration", "",
"Each column re-calibrates the whole model to the anchored attack rate and re-runs every strategy; rank in brackets (1 = most deaths averted).", "")
rk <- read.csv(file.path(RES, "sens_strategy_ranking.csv"))
lab <- c(earlier_3months = "Started 3 months earlier", elderly_first = "Adults \u226550 years first",
         coverage_70 = "70% coverage", observed = "Observed rollout", adults_first = "Working-age adults first")
tg  <- sort(unique(rk$attack_target))
L <- c(L, row("Strategy", paste0(pc(tg, 0), " anchor")), paste0("|---", strrep("|---:", length(tg)), "|"))
for (sname in names(lab)) {
  cells <- vapply(tg, function(t) { r <- rk[rk$scenario == sname & rk$attack_target == t, ]
                                    paste0(n0(r$deaths_averted), " (", r$rank, ")") }, "")
  L <- c(L, row(lab[[sname]], cells))
}
L <- c(L, "",
"## Supplementary figures", "",
"![](figures/fig_fitted_npi.png)", "",
"**Figure S1.** Fitted latent transmission (mobility / non-pharmaceutical-intervention) multiplier recovered during calibration; a value of 1 denotes baseline transmission.", "",
"![](figures/figS1_attack_rate.png)", "",
"**Figure S2.** Modelled cumulative SARS-CoV-2 infection attack rate over time, with the external seroprevalence anchor marked at the reference date.", "",
"![](figures/figS2_age_deaths_averted.png)", "",
"**Figure S3.** Deaths averted by the vaccination programme, by age group (modelled true deaths), showing concentration of benefit in older adults.", "",
"![](figures/figS4_tornado_cost_per_daly.png)", "",
"**Figure S4.** Tornado diagram of the one-way sensitivity of cost per DALY averted; the dashed line marks the base-case value, with the base-case calibration held fixed.")

# renumber <sup> citations to match the manuscript (renumber_references.R writes the map)
map_file <- file.path(RES, "reference_map.csv")
if (file.exists(map_file)) {
  mp <- read.csv(map_file); remap <- setNames(mp$new, mp$old)
  L <- vapply(L, function(line) {
    m <- gregexpr("<sup>[0-9,]+</sup>", line)[[1]]
    if (m[1] == -1) return(line)
    hits <- regmatches(line, list(m))[[1]]
    new <- vapply(hits, function(h) {
      n <- as.integer(strsplit(gsub("</?sup>", "", h), ",")[[1]])
      paste0("<sup>", paste(sort(unique(remap[as.character(n)])), collapse = ","), "</sup>")
    }, "")
    regmatches(line, list(m)) <- list(new); line
  }, "", USE.NAMES = FALSE)
}

dir.create(file.path(OUTD, "figures"), showWarnings = FALSE, recursive = TRUE)
figs <- c("fig_fitted_npi.png", "figS1_attack_rate.png", "figS2_age_deaths_averted.png", "figS4_tornado_cost_per_daly.png")
invisible(file.copy(file.path(RES, figs), file.path(OUTD, "figures", figs), overwrite = TRUE))
md <- file.path(OUTD, "supplementary_appendix_v5.md")
writeLines(L, md, useBytes = TRUE)
if (nzchar(Sys.which("pandoc"))) {
  old <- setwd(OUTD); on.exit(setwd(old))
  system2("pandoc", c("supplementary_appendix_v5.md", "-o", "supplementary_appendix_v5.docx", "--resource-path=."))
}
