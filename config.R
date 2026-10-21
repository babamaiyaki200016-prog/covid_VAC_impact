#!/usr/bin/env Rscript
# ============================================================================
# config.R -- Parameters for the Nigeria age-structured SARS-CoV-2 SEIR-V model
# ----------------------------------------------------------------------------
# Every number here is a MODELLING ASSUMPTION with a stated source or a clearly
# flagged placeholder. Swap placeholders for Nigeria-specific values as you
# obtain them (NCDC line lists, NPHCDA rollout files, GBD, NBS costing studies).
# Nothing downstream reads a hard-coded constant -- it all comes from CFG.
# ============================================================================

CFG <- list()

# ---- Time horizon ----------------------------------------------------------
# First confirmed Nigeria case: 2020-02-27 (NCDC). Model to end of 2022, which
# spans ancestral/Alpha, Delta and Omicron waves and the whole rollout.
CFG$date_start <- as.Date("2020-02-27")
CFG$date_end   <- as.Date("2022-12-31")

# ---- Demography (Nigeria, ~2021) ------------------------------------------
# Nigeria 2020 population and age structure, UN World Population Prospects 2019
# (both sexes, five-year groups, as distributed with Prem et al. 2021; total
# 206.14 million). Bands aggregated in derive_contact_matrix.R.
CFG$population_total <- 206.14e6
CFG$age_labels <- c("0-14", "15-29", "30-49", "50-64", "65+")
CFG$age_frac   <- c(0.4349,  0.2672,  0.2009,  0.0696, 0.0274)  # UN WPP 2019, 2020
CFG$age_frac   <- CFG$age_frac / sum(CFG$age_frac)                 # exact sum to 1
stopifnot(abs(sum(CFG$age_frac) - 1) < 1e-9)
CFG$n_age <- length(CFG$age_labels)

# ---- Contact matrix --------------------------------------------------------
# Per-capita daily contacts between age groups (rows = contactor, cols =
# contactee), all settings. Prem K, et al. PLoS Comput Biol 2021;17:e1009098
# (synthetic matrix for Nigeria, 16 five-year groups) collapsed to these 5
# bands with population-weighted contactor groups -- reproduce with
# derive_contact_matrix.R. Units: mean contacts per person per day.
CFG$contact_matrix <- matrix(c(
  #        0-14   15-29  30-49  50-64  65+
           9.733, 2.466, 2.893, 0.893, 0.477,   # contacts made by 0-14
           3.789, 7.958, 2.698, 0.942, 0.472,   # 15-29
           4.070, 3.217, 4.847, 1.479, 0.583,   # 30-49
           2.851, 3.068, 3.827, 1.748, 0.632,   # 50-64
           2.013, 1.606, 2.172, 1.128, 0.643    # 65+
), nrow = 5, byrow = TRUE)

# ---- Natural history -------------------------------------------------------
CFG$latent_period      <- 3.0    # days, E -> I  (sigma = 1/latent)
CFG$infectious_period  <- 6.0    # days, I -> R/D (gamma = 1/infectious)

# ---- Age-specific severity (per infection, ancestral variant baseline) -----
# IFR: infection fatality ratio. Log-linear age gradient, anchored on
# O'Driscoll et al. (2021, Nature) / Verity et al. (2020), NOT case fatality.
CFG$ifr <- c(0.00001, 0.00006, 0.00040, 0.00700, 0.05000)
# IHR: infection hospitalisation ratio (Salje et al. 2020, scaled by age).
CFG$ihr <- c(0.00030, 0.00120, 0.00600, 0.03500, 0.11000)
# Fraction of hospitalised patients admitted to ICU: pooled rate 32% (95% CI
# 26-38%), Abate SM, et al. PLoS One 2020;15:e0235653 (PMID 32649661).
CFG$icu_fraction <- 0.32

# ---- Vaccine effectiveness (leaky, "all-or-nothing" not assumed) -----------
# Interpreted as reduction versus an unvaccinated person. Death/hosp values are
# CONDITIONAL on breakthrough infection, so overall protection compounds:
#   overall VE against death = 1 - (1 - ve_infection)(1 - ve_death).
CFG$ve_infection <- 0.55   # against acquiring infection (2-dose, pre-Omicron)
CFG$ve_death     <- 0.85   # against death | infected
CFG$ve_hosp      <- 0.75   # against hospitalisation | infected

# ---- Variant timeline ------------------------------------------------------
# Each row: variant becomes dominant from `from` until the next row's `from`.
# transmissibility & severity are multipliers on baseline; ve_mult scales
# ve_infection to capture Delta/Omicron immune escape. Dates approximate
# Nigeria genomic surveillance (NCDC/GISAID). REPLACE with dated GISAID shares.
CFG$variants <- data.frame(
  name             = c("Ancestral/Alpha", "Delta",        "Omicron"),
  from             = as.Date(c("2020-02-27", "2021-07-01", "2021-12-01")),
  transmissibility = c(1.00,               1.50,           2.10),
  severity         = c(1.00,               1.30,           0.60),
  ve_mult          = c(1.00,               0.90,           0.55),
  stringsAsFactors = FALSE
)

# ---- Vaccination programme (observed rollout shape) ------------------------
# Nigeria's programme began 2021-03-05 (NPHCDA). We model the OBSERVED rollout
# as a logistic ramp to a cumulative dose total; data.R will use a real dose
# file if you provide one. Eligibility/priority weights are used to allocate
# each day's doses across age groups (proportional to S_a * weight_a).
CFG$vax_start        <- as.Date("2021-03-05")
CFG$vax_total_doses  <- 60e6     # cumulative primary-series persons by 2022-12-31
CFG$vax_ramp_days    <- 300      # logistic time-scale of the rollout
# Observed strategy: adults 18+; here 15+ groups eligible, mild elderly priority.
CFG$vax_weight <- c(0.0, 1.0, 1.0, 1.5, 2.0)   # by age group; 0 = ineligible

# ---- Economic parameters (2021 USD) ----------------------------------------
# All costs converted to 2021 US$ with the US CPI-U annual average (BLS series
# CUUR0000SA0: 2019 255.657, 2020 258.811, 2021 270.970, 2022 292.655).
.cpi <- c("2019" = 255.657, "2020" = 258.811, "2021" = 270.970, "2022" = 292.655)
.to2021 <- function(x, yr) x * .cpi[["2021"]] / .cpi[[as.character(yr)]]
# Vaccine: Nigeria-specific purchase price US$3 per AstraZeneca-like dose
# (2020 US$) + 10% freight, with 15% wastage -- Ruiz FJ, et al. PLOS Glob
# Public Health 2023;3:e0001693 (PMID 36963054).
CFG$vaccine_price_per_dose  <- .to2021(3 * 1.10 / (1 - 0.15), 2020)   # 4.06
# Delivery: Nigeria, nine states, US$0.84 per dose (2022 US$) -- Noh DH, et al.
# BMC Health Serv Res 2024;24:1232 (PMID 39402600). Financial cost only.
CFG$delivery_cost_per_dose  <- .to2021(0.84, 2022)                    # 0.78
CFG$cost_per_dose <- CFG$vaccine_price_per_dose + CFG$delivery_cost_per_dose
# Case management, Nigeria unit costs (2019 US$): US$38.37 per severe-case
# bed-day x 8 days; US$272.59 per critical-case (ICU) day x 10 days --
# Torres-Rueda S, et al. BMJ Glob Health 2021;6:e005759, Table SR1 (PMID 34857521).
CFG$cost_hospitalisation <- .to2021(38.37 * 8, 2019)    # 325 per non-ICU admission
CFG$cost_icu             <- .to2021(272.59 * 10, 2019)  # 2,889 per ICU admission
CFG$discount_rate        <- 0.03     # annual, health outcomes (iDSI reference case)
# GDP per capita, Nigeria 2021, current US$: World Bank WDI NY.GDP.PCAP.CD
# (accessed 2026-10-21; post-revision series).
CFG$gdp_per_capita       <- 2787.5
# Cost-effectiveness thresholds (cost per DALY averted).
CFG$cet_threshold        <- 0.5 * CFG$gdp_per_capita   # 0.5x GDP per capita

# ---- DALY parameters -------------------------------------------------------
# Remaining life expectancy at the midpoint of each age band (7.5, 22.5, 40,
# 57.5, 72.5 years), linearly interpolated from the WHO Global Health
# Observatory 2019 life table for Nigeria (LIFE_0000000035, both sexes):
# e5 65.8, e10 61.7, e20 52.5, e25 47.9, e40 34.4, e55 21.8, e60 17.9,
# e70 11.3, e75 8.5. Used for years of life lost.
CFG$life_expectancy_remaining <- c(63.75, 50.20, 34.40, 19.85, 9.90)
# Non-fatal morbidity: disability weight x duration (years) for a symptomatic
# case, plus a post-acute ("long COVID") add-on. GBD 2020 COVID DWs.
CFG$dw_symptomatic   <- 0.051   # moderate acute infection
CFG$dur_symptomatic  <- 14 / 365
CFG$dw_severe        <- 0.133   # severe (hospitalised) acute
CFG$dur_severe       <- 21 / 365
CFG$dw_postacute     <- 0.219
CFG$dur_postacute    <- 90 / 365
CFG$prop_postacute   <- 0.10    # fraction of symptomatic with post-acute sequelae
CFG$prop_symptomatic <- 0.55    # clinical fraction of all infections

# ---- Attack-rate anchor (IDENTIFIABILITY — important) -----------------------
# Reported deaths ALONE cannot identify the infection attack rate: transmission
# amplitude (how many were ever infected) trades off against death
# under-ascertainment (what fraction of deaths were reported). Both reproduce
# the same reported-death curve. We therefore anchor the cumulative infection
# attack rate at a reference date to an external SEROPREVALENCE estimate, which
# fixes that ridge and makes true-burden and vaccine-impact estimates meaningful.
# PLACEHOLDER — replace target & date with a Nigerian SARS-CoV-2 serosurvey
# (e.g. NCDC national household seroprevalence). Set weight 0 to disable.
CFG$attack_rate_target   <- 0.50            # ever-infected fraction by ref date
CFG$attack_rate_ref_date <- as.Date("2021-12-31")
CFG$attack_rate_weight   <- 2500            # objective weight (binds attack rate; 0 disables)

# ---- Seasonal forcing ------------------------------------------------------
# Transmission is scaled by (1 + seas_amp * cos(2*pi*(t - seas_phase)/365)),
# capturing Nigeria's dry-season / harmattan (Dec-Feb) transmission peaks that
# drove the Jan-2021 and Jan-2022 waves. Amplitude and phase (day-of-year of
# the peak) are estimated in calibration; defaults 0 = no seasonality.
CFG$seas_amp   <- 0
CFG$seas_phase <- 0

# ---- Numerical / seeding ---------------------------------------------------
CFG$seed_infections <- 20       # initial infectious individuals (working age)
CFG$seed_age_index  <- 3        # 30-49 group
CFG$eps <- 1e-9

# Derived scalars used across modules.
CFG$sigma <- 1 / CFG$latent_period
CFG$gamma <- 1 / CFG$infectious_period
CFG$N_age <- CFG$population_total * CFG$age_frac   # population per age group

invisible(CFG)
