#!/usr/bin/env Rscript
# ============================================================================
# data.R -- Load REAL, verifiable Nigeria epidemiological data
# ----------------------------------------------------------------------------
# Sources (public, citable, refresh-able):
#   * Cases & deaths : Our World in Data `cases_deaths/full_data.csv`,
#                      which compiles WHO / NCDC official reports.
#   * Vaccinations   : Our World in Data `vaccinations/vaccinations.csv`,
#                      which compiles NPHCDA / WHO official reports.
# We NEVER synthesise the epidemic or the rollout. If a required file is
# missing or contains no Nigeria rows, we STOP with instructions -- no
# placeholder series is ever substituted. Literature-based epidemiological
# PARAMETERS (IFR, VE, contacts, DALY weights) live in config.R and are cited
# there; those are modelling assumptions, not data.
# ============================================================================

DATA_DIR <- file.path("data", "covid")
CASES_FILE <- file.path(DATA_DIR, "owid_cases_deaths_all.csv")
VAX_FILE   <- file.path(DATA_DIR, "owid_vaccinations_all.csv")

.need_file <- function(path, url) {
  if (!file.exists(path))
    stop(sprintf(paste0(
      "Required REAL data file not found:\n  %s\n",
      "Download it (public, verifiable) e.g.:\n  curl -L --compressed '%s' -o %s\n",
      "This pipeline does NOT fabricate epidemiological data."),
      path, url, path), call. = FALSE)
}

# clamp negative daily values (retrospective data corrections) to 0
.clamp <- function(x) { x[is.na(x)] <- 0; x[x < 0] <- 0; x }

load_nigeria_data <- function(CFG, country = "Nigeria") {
  .need_file(CASES_FILE,
    "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/cases_deaths/full_data.csv")
  .need_file(VAX_FILE,
    "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv")

  # model day grid
  n_days <- as.integer(CFG$date_end - CFG$date_start)
  grid <- data.frame(date = CFG$date_start + 0:n_days, day = 0:n_days)

  # ---- cases & deaths ----
  cd <- read.csv(CASES_FILE, stringsAsFactors = FALSE)
  cd <- cd[cd$location == country, ]
  if (nrow(cd) == 0) stop(sprintf("No '%s' rows in %s", country, CASES_FILE), call. = FALSE)
  cd$date <- as.Date(cd$date)
  cd <- cd[, c("date", "new_cases", "new_deaths", "total_cases", "total_deaths")]
  m <- merge(grid, cd, by = "date", all.x = TRUE)
  m <- m[order(m$day), ]
  death_daily <- .clamp(m$new_deaths)
  case_daily  <- .clamp(m$new_cases)

  # ---- vaccinations (persons newly protected per day = S -> V flow) ----
  vx <- read.csv(VAX_FILE, stringsAsFactors = FALSE)
  vx <- vx[vx$location == country, ]
  if (nrow(vx) == 0) stop(sprintf("No '%s' rows in %s", country, VAX_FILE), call. = FALSE)
  vx$date <- as.Date(vx$date)
  # prefer the daily first-dose series; else difference cumulative people_vaccinated
  if ("daily_people_vaccinated" %in% names(vx) &&
      any(!is.na(vx$daily_people_vaccinated))) {
    vx_daily_src <- vx[, c("date", "daily_people_vaccinated")]
    names(vx_daily_src)[2] <- "doses"
  } else {
    vx <- vx[order(vx$date), ]
    pv <- .clamp(vx$people_vaccinated)
    vx_daily_src <- data.frame(date = vx$date, doses = c(pv[1], diff(pv)))
  }
  mv <- merge(grid, vx_daily_src, by = "date", all.x = TRUE)
  mv <- mv[order(mv$day), ]
  doses_daily <- .clamp(mv$doses)
  # no vaccination before the programme start (guards data artefacts)
  doses_daily[grid$date < CFG$vax_start] <- 0

  # total doses ADMINISTERED (all doses, incl. second/booster) -- drives
  # programme COST only; persons_vaccinated above drives the S -> V flow.
  if ("daily_vaccinations" %in% names(vx) && any(!is.na(vx$daily_vaccinations))) {
    td_src <- vx[, c("date", "daily_vaccinations")]
  } else {
    vx <- vx[order(vx$date), ]
    tv <- .clamp(vx$total_vaccinations)
    td_src <- data.frame(date = vx$date, daily_vaccinations = c(tv[1], diff(tv)))
  }
  td <- merge(grid, td_src, by = "date", all.x = TRUE)
  td <- td[order(td$day), ]
  doses_total_daily <- .clamp(td$daily_vaccinations)
  doses_total_daily[grid$date < CFG$vax_start] <- 0

  prov <- list(
    country = country,
    window = c(as.character(CFG$date_start), as.character(CFG$date_end)),
    cases_source = "OWID cases_deaths/full_data.csv (WHO/NCDC)",
    vax_source   = "OWID vaccinations/vaccinations.csv (NPHCDA/WHO)",
    downloaded_utc = as.character(Sys.time()),
    observed_total_reported_deaths = sum(death_daily),
    observed_total_reported_cases  = sum(case_daily),
    observed_total_persons_vaccinated = sum(doses_daily),
    observed_total_doses_administered = sum(doses_total_daily),
    n_days = n_days
  )

  # write auditable, Nigeria-only extracts + provenance
  out_dir <- file.path("outputs", "covid_vax_impact")
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  readr::write_csv(
    data.frame(date = grid$date, day = grid$day,
               reported_new_cases = case_daily,
               reported_new_deaths = death_daily,
               persons_vaccinated = doses_daily,
               doses_administered = doses_total_daily),
    file.path(out_dir, "nigeria_observed_daily.csv"))
  writeLines(jsonlite_or_manual(prov),
             file.path(out_dir, "data_provenance.json"))

  list(grid = grid, death_daily = death_daily, case_daily = case_daily,
       doses_daily = doses_daily, doses_total_daily = doses_total_daily,
       provenance = prov)
}

# tiny JSON writer so we don't hard-depend on jsonlite
jsonlite_or_manual <- function(x) {
  if (requireNamespace("jsonlite", quietly = TRUE))
    return(jsonlite::toJSON(x, auto_unbox = TRUE, pretty = TRUE))
  kv <- vapply(names(x), function(k) {
    v <- x[[k]]
    if (length(v) > 1) v <- paste0("[", paste(sprintf('"%s"', v), collapse = ", "), "]")
    else if (is.character(v)) v <- sprintf('"%s"', v)
    sprintf('  "%s": %s', k, v)
  }, character(1))
  paste0("{\n", paste(kv, collapse = ",\n"), "\n}")
}

invisible(TRUE)
