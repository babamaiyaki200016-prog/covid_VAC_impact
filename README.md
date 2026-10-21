# Nigeria SARS-CoV-2 vaccination impact model

An age-structured, deterministic **SEIR-V** transmission model for Nigeria,
calibrated to observed reported COVID-19 deaths, used to estimate the health
and economic impact of the national vaccination programme via
observed-vs-counterfactual simulation.

## What it answers
- Infections, hospitalisations, ICU admissions and **deaths averted** by the
  actual rollout, versus a no-vaccination counterfactual.
- **DALYs averted** and a health-system **cost-effectiveness** analysis
  (cost per death / hospitalisation / DALY averted; ICER vs a GDP-based threshold).
- Alternative rollout strategies: starting 3 months earlier, reaching 70%
  coverage, and elderly-first vs adults-first prioritisation.

## Data — real and verifiable only
The epidemic and rollout are **never** synthesised. Inputs are public, citable,
refresh-able series for Nigeria:

| Input | Source file (Our World in Data) | Underlying authority |
|---|---|---|
| Daily cases & deaths | `data/covid/owid_cases_deaths_all.csv` | WHO / NCDC |
| Daily persons vaccinated (first dose; drives protection) | `data/covid/owid_vaccinations_all.csv` | NPHCDA / WHO |
| Daily doses administered (all doses; drives programme cost) | `data/covid/owid_vaccinations_all.csv` | NPHCDA / WHO |

If a file is missing or has no Nigeria rows, the pipeline **stops** with a
download command — it will not substitute a placeholder series. Observed totals
over 2020-02-27 → 2022-12-31: **3,155 reported deaths, 266,415 cases, 74.9M
persons vaccinated (≥1 dose) and 105.8M doses administered**.

Refresh the data:
```bash
cd data/covid
curl -L --compressed 'https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/cases_deaths/full_data.csv' -o owid_cases_deaths_all.csv
curl -L --compressed 'https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv' -o owid_vaccinations_all.csv
```

## Parameters vs data
Every parameter in [`config.R`](config.R) carries its source:

| Parameter | Value | Source |
|---|---|---|
| Population & age structure | 206.14M; 43.5/26.7/20.1/7.0/2.7% | UN WPP 2019 (2020) |
| Contact matrix | Prem 2021 Nigeria, collapsed to 5 bands | Prem et al. PLoS Comput Biol 2021 — `derive_contact_matrix.R` |
| Vaccine price | US$4.06/dose (US$3 AZ-like + 10% freight, 15% wastage) | Ruiz et al. PLOS Glob Public Health 2023 |
| Delivery cost | US$0.78/dose (Nigeria, 9 states) | Noh et al. BMC Health Serv Res 2024 |
| Hospital / ICU cost | US$325 / US$2,889 per admission | Torres-Rueda et al. BMJ Glob Health 2021, Table SR1 (Nigeria) |
| ICU fraction of admissions | 32% (95% CI 26–38) | Abate et al. PLoS One 2020 |
| Life expectancy (YLL) | 63.8/50.2/34.4/19.9/9.9 y | WHO GHO 2019 life table, Nigeria |
| GDP per capita | US$2,787.5 (2021) | World Bank WDI |
| Price year | 2021 US$ via US CPI-U | BLS |

IFR/IHR, vaccine effectiveness and disability weights remain international
literature values (cited in `config.R`).

## Run it
```bash
Rscript analysis/covid_vax_impact/run.R
```
Requires R packages: `deSolve, ggplot2, dplyr, tidyr, readr` (all installed).
Calibration is cached to `outputs/covid_vax_impact/calibration.rds`; delete it
(or edit `config.R`/`model.R`/`calibrate.R`) to force a refit.

## Files
| File | Role |
|---|---|
| `config.R` | Parameters (cited assumptions + flagged placeholders) |
| `data.R` | Loads real Nigeria cases/deaths/vaccination; no synthetic fallback |
| `model.R` | Age-structured SEIR-V ODE system + runner (`deSolve`) |
| `calibrate.R` | Fits β₀, death ascertainment, latent NPI trajectory to deaths |
| `scenarios.R` | Observed rollout vs no-vax + alternative strategies |
| `economics.R` | DALYs (YLL+YLD), costs, cost-effectiveness / ICER |
| `run.R` | Orchestrator → CSV tables, plots, `RESULTS_SUMMARY.md` |
| `sensitivity.R` | Attack-rate re-calibration sweep (30–70%) + one-way tornado (~35 min) |
| `build_supplement.R` | Regenerates the Supplementary appendix (Tables S1–S4) from outputs |
| `verify_references.R` | Checks every manuscript reference against PubMed / Crossref |
| `renumber_references.R` | Renumbers manuscript references by order of first citation (Vancouver) |
| `derive_contact_matrix.R` | Downloads Prem et al. (2021) Nigeria matrix and collapses it to the 5 model bands |

## Outputs (`outputs/covid_vax_impact/`)
- `headline_results.csv`, `scenario_comparison.csv`, `economic_evaluation.csv`
- `nigeria_observed_daily.csv`, `data_provenance.json`
- `fig_calibration_fit.png`, `fig_deaths_counterfactual.png`,
  `fig_scenarios_deaths_averted.png`, `fig_fitted_npi.png`
- `RESULTS_SUMMARY.md`

## Changelog
- **2026-10-21 — costing correction.** Programme cost was previously charged on
  persons vaccinated (first doses, 74.9M) instead of all doses administered
  (105.8M). `data.R` now loads both series: first doses drive the S→V flow
  (unchanged, so calibration and all health outcomes are unchanged); total doses
  drive cost. Base case: programme cost US$846M (was US$599M), US$12,373 per DALY
  averted (was US$8,686) — still not cost-effective at 0.5×GDP per capita.

- **2026-10-21 — sourced inputs.** Replaced placeholder contact matrix, unit
  costs, ICU fraction, life expectancy, age structure and GDP per capita with
  the sourced values above; model re-calibrated. Base case: 11,974 deaths
  averted, 154,114 DALYs, programme cost US$512M, US$2,987 per DALY averted
  (above 0.5× GDP per capita; within the legacy 1–3× GDP range).

## Modelling notes
- Deterministic compartmental model: compartments are continuous, so internal
  values are fractional by construction; all **reported** person-counts are
  rounded to whole people. Whole-person stochastic dynamics would require a
  stochastic/agent-based model.
- The counterfactual reuses the *same* calibrated transmission and NPI
  trajectory, changing only the vaccination programme, so averted estimates are
  internally consistent.
- Deaths are calibrated with a fitted ascertainment fraction (reported =
  ρ × true), so results are reported on both the modelled-true and reported scales.

## Methods declarations (state these in the manuscript)
- **Calibration target is MONTHLY reported deaths**, not daily or weekly.
  Nigeria's daily/weekly death counts are small and irregular (zero days,
  reporting-batch spikes, weekend effects); monthly aggregation preserves the
  epidemic wave structure while averaging administrative noise. The cumulative
  death total is anchored separately so aggregation does not bias the level.
- **Death under-ascertainment** is estimated as a single fraction ρ (reported =
  ρ × true); results are reported on both the modelled-true and reported scales.
- **Attack-rate anchor (identifiability)**: reported deaths alone cannot separate
  transmission amplitude from death under-ascertainment (they trade off along a
  ridge — without an anchor the true-burden and vaccine-impact estimates are not
  identified and can swing by an order of magnitude). We anchor the cumulative
  infection attack rate at a reference date to a **seroprevalence** estimate
  (`config.R`; placeholder 50% by end-2021 — replace with a Nigerian serosurvey).
  This is the single most influential assumption and must carry a sensitivity
  analysis in the manuscript.
- **NPI/mobility** enters as a latent transmission multiplier fitted during
  calibration (~bimonthly knots, linearly interpolated, roughness-penalised).
- **Seasonal forcing**: transmission is scaled by a sinusoid with fitted
  amplitude and phase, representing Nigeria's dry-season/harmattan (Dec–Feb)
  peaks that drove the Jan-2021 and Jan-2022 waves.
- **Parameters vs data**: severity, VE, contacts, costs and DALY weights are
  literature-based assumptions (see `config.R`); only the epidemic and rollout
  series are empirical.

## Limitations
Single national patch (no state-level structure yet); waning immunity and
reinfection not modelled explicitly; variant timing is approximate; NPI is a
fitted latent, not measured mobility. Point estimates only — add PSA/credible
intervals for publication. These are extension points, not blockers.
