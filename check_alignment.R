#!/usr/bin/env Rscript
# ============================================================================
# check_alignment.R -- final consistency audit of manuscript vs model outputs
# ----------------------------------------------------------------------------
# Fails loudly (non-zero exit) if any of these do not hold:
#   1. every headline result appears in the text exactly as in the outputs,
#      and the abstract repeats the same numbers as the Results;
#   2. Table 2 matches scenario_comparison.csv cell by cell;
#   3. no stale numbers or placeholder language survive from earlier drafts;
#   4. every superscript citation points to an existing reference, every
#      reference is cited, and numbering follows order of first citation;
#   5. the supplement's economic table matches economic_evaluation.csv.
#
# Usage: Rscript analysis/covid_vax_impact/check_alignment.R manuscript.docx supplement.md
# ============================================================================

suppressMessages({ library(officer); library(xml2) })
args <- commandArgs(trailingOnly = TRUE)
RES  <- file.path("outputs", "covid_vax_impact")
n0   <- function(x) format(round(as.numeric(x)), big.mark = ",", scientific = FALSE, trim = TRUE)
fails <- character(0); ok <- 0L
check <- function(cond, msg) { if (isTRUE(cond)) ok <<- ok + 1L else fails <<- c(fails, msg) }

doc  <- read_docx(args[1]); ds <- docx_summary(doc)
body <- paste(ds$text[ds$content_type == "paragraph"], collapse = "\n")
abs_ <- ds$text[grepl("^Results: ", ds$text)][1]
H  <- read.csv(file.path(RES, "headline_results.csv")); h <- setNames(H$value, H$metric)
E  <- read.csv(file.path(RES, "economic_evaluation.csv")); e <- E[E$basis == "modelled_true_burden", ]
S  <- read.csv(file.path(RES, "scenario_comparison.csv"))
AR <- read.csv(file.path(RES, "sens_attack_rate.csv"))

# ---- 1. headline numbers in text and abstract ------------------------------
key <- c(deaths = n0(h[["Deaths averted (modelled true)"]]), dalys = n0(h[["DALYs averted"]]),
         hosp = n0(h[["Hospitalisations averted"]]), icu = n0(h[["ICU admissions averted"]]),
         cpd = paste0("US$", n0(h[["Cost per DALY averted (USD)"]])),
         cost = paste0("US$", round(h[["Programme cost (USD)"]] / 1e6), " million"),
         net = paste0("US$", round(h[["Net cost (USD)"]] / 1e6), " million"),
         true_obs = n0(S$true_deaths[S$scenario == "observed"]),
         true_nov = n0(S$true_deaths[S$scenario == "no_vaccination"]),
         ar_lo = n0(min(AR$deaths_averted)), ar_hi = n0(max(AR$deaths_averted)))
for (k in names(key)) {
  check(grepl(key[[k]], body, fixed = TRUE), sprintf("text lacks %s = %s", k, key[[k]]))
  check(grepl(key[[k]], abs_, fixed = TRUE), sprintf("abstract lacks %s = %s", k, key[[k]]))
}
check(grepl(n0(e$yll_averted), body, fixed = TRUE), "YLL missing")
check(grepl(n0(h[["Deaths averted (reported scale)"]]), body, fixed = TRUE), "reported-scale deaths missing")
check(grepl(paste0("US$", n0(min(AR$cost_per_daly_usd))), body, fixed = TRUE) &&
      grepl(paste0("US$", n0(max(AR$cost_per_daly_usd))), body, fixed = TRUE), "attack-rate cost/DALY range missing")

# ---- 2. Table 2 vs scenario_comparison.csv ----------------------------------
tabs <- ds[ds$content_type == "table cell", ]
t2 <- tabs[tabs$doc_index == max(tabs$doc_index), ]
cell <- function(r, c) t2$text[t2$row_id == r & t2$cell_id == c]
lab <- c("No vaccination" = "no_vaccination", "Observed rollout" = "observed",
         "Started 3 months earlier" = "earlier_3months", "70% coverage" = "coverage_70",
         "Adults ≥50 years first" = "elderly_first", "Working-age adults first" = "adults_first")
for (r in unique(t2$row_id)) {
  nm <- cell(r, 1); if (!length(nm) || !nm[1] %in% names(lab)) next; nm <- nm[1]
  s <- S[S$scenario == lab[[nm]], ]
  check(identical(cell(r, 2), n0(s$true_deaths)), paste("Table 2 true deaths:", nm))
  check(identical(cell(r, 3), n0(s$deaths_averted_vs_novax)), paste("Table 2 deaths averted:", nm))
  check(identical(cell(r, 4), n0(s$infections_averted_vs_novax)), paste("Table 2 infections averted:", nm))
}

# ---- 3. stale values / placeholder language ---------------------------------
stale <- c("5,162", "67,087", "8,686", "12,373", "US$599", "US$846", "US$8 per dose",
           "stylised", "≈US$2,100", "US$1,050",
           "not cost-effective by any conventional standard", "several thousand deaths", "ve vs", " a 8", " a 11", "late December", "  ")
for (s in stale) check(!grepl(s, body, fixed = TRUE), sprintf("stale text present: '%s'", s))
check(!grepl("placeholder(?! (series|brackets))", body, perl = TRUE), "placeholder language present")
check(!grepl("[0-9]·[0-9]", body), "mid-dot decimal present")

# ---- 4. citations ------------------------------------------------------------
b <- docx_body_xml(doc)
blocks <- xml_children(xml_find_first(b, "//w:body")); txts <- vapply(blocks, xml_text, "")
style <- vapply(blocks, function(x) { s <- xml_attr(xml_find_first(x, "w:pPr/w:pStyle"), "val"); ifelse(is.na(s), "", s) }, "")
ref_at <- which(trimws(txts) == "References")[1]
start  <- which(grepl("^Heading[12]$", style))[1]
sup <- unlist(lapply(blocks[start:(ref_at - 1)], function(x)
  xml_text(xml_find_all(x, ".//w:r[w:rPr/w:vertAlign[@w:val='superscript']]"))))
sup <- sup[grepl("^[0-9,–-]+$", sup)]
cited <- unlist(lapply(sup, function(s) unlist(lapply(strsplit(s, ",")[[1]], function(t) {
  ab <- as.integer(strsplit(t, "[-–]")[[1]]); if (length(ab) == 2) seq(ab[1], ab[2]) else ab }))))
after <- (ref_at + 1):length(blocks)
stop_at <- after[grepl("^Heading[12]$", style[after]) | trimws(txts[after]) == "Declarations"][1]
refs <- after[after < stop_at & vapply(after, function(i) length(xml_find_all(blocks[[i]], "w:pPr/w:numPr")) > 0, TRUE)]
n_ref <- length(refs)
check(all(cited %in% seq_len(n_ref)), sprintf("citation outside 1..%d: %s", n_ref, paste(setdiff(cited, seq_len(n_ref)), collapse = ",")))
check(all(seq_len(n_ref) %in% cited), sprintf("uncited references: %s", paste(setdiff(seq_len(n_ref), cited), collapse = ",")))
# order of first citation, counting each table at its first call-out ("Table N"), as Vancouver requires
sup_of <- function(x) {
  v <- xml_text(xml_find_all(x, ".//w:r[w:rPr/w:vertAlign[@w:val='superscript']]"))
  as.integer(unlist(strsplit(v[grepl("^[0-9,]+$", v)], ",")))
}
tbl_blocks <- which(xml_name(blocks) == "tbl")
seen <- integer(0); done <- logical(length(tbl_blocks))
for (i in start:(ref_at - 1)) {
  if (xml_name(blocks[[i]]) == "tbl") next
  seen <- c(seen, sup_of(blocks[[i]]))
  for (k in seq_along(tbl_blocks)) if (!done[k] && grepl(paste0("\\bTable ", k, "\\b"), txts[i])) {
    seen <- c(seen, sup_of(blocks[[tbl_blocks[k]]])); done[k] <- TRUE
  }
}
check(!is.unsorted(unique(seen)), "references not numbered in order of first citation")
check(identical(sort(unique(cited)), seq_len(n_ref)), "reference numbers not contiguous")

# ---- 5. supplement -------------------------------------------------------------
if (length(args) >= 2 && file.exists(args[2])) {
  sm <- paste(readLines(args[2], warn = FALSE), collapse = "\n")
  for (v in c(e$programme_cost_usd, e$net_cost_usd, e$cost_per_daly_averted, e$dalys_averted, AR$cost_per_daly_usd))
    check(grepl(n0(v), sm, fixed = TRUE), paste("supplement lacks", n0(v)))
  check(!grepl("stylised|placeholder|[0-9]·[0-9]", sm), "supplement has stale wording")
  check(grepl("Table S5", sm, fixed = TRUE), "supplement lacks Table S5")
}

cat(sprintf("%d checks passed, %d failed\n", ok, length(fails)))
if (length(fails)) { cat(paste0(" - ", fails), sep = "\n"); quit(status = 1) }
