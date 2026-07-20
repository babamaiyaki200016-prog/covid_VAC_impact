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
# Total population ~206 million (UN WPP 2021). Age structure is a young pyramid;
# proportions below are rounded UN WPP single-year aggregates. REPLACE with
# National Population Commission / WorldPop age-sex bands when available.
CFG$population_total <- 206e6
CFG$age_labels <- c("0-14", "15-29", "30-49", "50-64", "65+")
CFG$age_frac   <- c(0.430,   0.270,   0.200,   0.070,  0.030)   # sums to 1
stopifnot(abs(sum(CFG$age_frac) - 1) < 1e-9)
CFG$n_age <- length(CFG$age_labels)

# ---- Contact matrix --------------------------------------------------------
# Per-capita daily contacts between age groups (rows = contactor, cols =
# contactee). Synthetic, POLYMOD-shaped and adapted for a high-contact,
# young-skewed setting: strong within-group and child-adult mixing. REPLACE
# with Prem et al. (2021) synthetic contact matrix for Nigeria, collapsed to
# these 5 bands. Units: mean contacts per person per day.
CFG$contact_matrix <- matrix(c(
  #        0-14  15-29 30-49 50-64  65+
           8.0,  2.5,  3.0,  1.0,  0.4,   # contacts made by 0-14
           2.5,  7.0,  3.5,  1.2,  0.5,   # 15-29
           2.2,  3.0,  5.5,  1.5,  0.7,   # 30-49
           1.0,  1.2,  1.8,  3.0,  0.8,   # 50-64
           0.4,  0.5,  0.7,  0.8,  1.5    # 65+
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
# Fraction of hospitalised patients needing ICU (age-invariant placeholder).
CFG$icu_fraction <- 0.15

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
# Placeholders -- replace with Nigerian costing (e.g. NHA, published COVID
# treatment-cost studies) and local GDP figures.
CFG$cost_per_dose        <- 8.0      # delivered cost incl. wastage & logistics
CFG$cost_hospitalisation <- 450      # per non-ICU admission
CFG$cost_icu             <- 2500     # per ICU admission
CFG$discount_rate        <- 0.03     # annual, for DALYs and costs
CFG$gdp_per_capita       <- 2100     # Nigeria, ~2021 (World Bank)
# Cost-effectiveness thresholds (cost per DALY averted).
CFG$cet_threshold        <- 0.5 * CFG$gdp_per_capita   # 0.5x GDP per capita

# ---- DALY parameters -------------------------------------------------------
# Remaining life expectancy at the midpoint of each age band (Nigeria abridged
# life table, ~2019; used for Years of Life Lost). REPLACE with GBD std table.
CFG$life_expectancy_remaining <- c(60, 47, 30, 17, 8)
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
