#!/usr/bin/env Rscript
# ============================================================================
# verify_references.R -- check every manuscript reference against PubMed/Crossref
# ----------------------------------------------------------------------------
# Reads the reference list from the manuscript .docx, extracts each PMID and
# DOI, and compares the cited title, DOI, year, volume, issue, first page and
# first author with the PubMed record (Crossref for DOI-only entries).
# A reference passes only if every field matches. Nothing is ever "fixed" or
# guessed: mismatches are written out for a human to resolve.
#
# Usage:   Rscript analysis/covid_vax_impact/verify_references.R manuscript_v5.docx
# Output:  outputs/covid_vax_impact/reference_check.csv
# ============================================================================

suppressMessages({ library(officer); library(jsonlite) })

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) stop("Usage: Rscript verify_references.R <manuscript.docx>", call. = FALSE)
OUT <- file.path("outputs", "covid_vax_impact")
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

# ---- 1. pull reference paragraphs (text after the 'References' heading) ------
paras <- docx_summary(read_docx(args[1]))
paras <- paras[paras$content_type == "paragraph", ]
start <- which(trimws(paras$text) == "References")[1]
if (is.na(start)) stop("No 'References' heading found", call. = FALSE)
stop_at <- which(trimws(paras$text) %in% c("Declarations", "Supplementary material") &
                 seq_len(nrow(paras)) > start)[1]
if (is.na(stop_at)) stop_at <- nrow(paras) + 1
txt  <- paras$text[(start + 1):(stop_at - 1)]
refs <- txt[grepl("PMID:|doi:|https?://", txt)]

norm <- function(x) gsub("[^a-z0-9]", "", tolower(x))
grab <- function(x, pat) { m <- regmatches(x, regexpr(pat, x, perl = TRUE)); if (length(m)) m else NA_character_ }

pmids <- sub("PMID:\\s*", "", vapply(refs, grab, "", pat = "PMID:\\s*\\d+"))
dois  <- sub("\\.$", "", sub("doi:", "", vapply(refs, grab, "", pat = "doi:\\S+")))

# ---- 2. PubMed esummary in one batch -----------------------------------------
ids <- unique(na.omit(pmids))
es  <- fromJSON(paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi?",
                       "db=pubmed&retmode=json&id=", paste(ids, collapse = ",")),
                simplifyVector = FALSE)$result

check_one <- function(i) {
  r <- refs[i]; pm <- pmids[i]; doi <- dois[i]
  row <- data.frame(ref = i, pmid = pm, doi = doi, status = "OK", problems = "",
                    cited = substr(r, 1, 160), stringsAsFactors = FALSE)
  probs <- character(0)
  if (!is.na(pm)) {
    s <- es[[pm]]
    if (is.null(s) || !is.null(s$error)) {
      probs <- "PMID not found in PubMed"
    } else {
      pdoi <- ""
      for (a in s$articleids) if (a$idtype == "doi") pdoi <- a$value
      if (!grepl(substr(norm(s$title), 1, 50), norm(r), fixed = TRUE)) probs <- c(probs, paste("title:", s$title))
      if (is.na(doi) || tolower(doi) != tolower(pdoi)) probs <- c(probs, paste("doi:", pdoi))
      if (!grepl(substr(s$pubdate, 1, 4), r, fixed = TRUE)) probs <- c(probs, paste("year:", s$pubdate))
      if (nzchar(s$volume) && !grepl(s$volume, r, fixed = TRUE)) probs <- c(probs, paste("volume:", s$volume))
      if (nzchar(s$issue) && !grepl(paste0("(", s$issue, ")"), r, fixed = TRUE)) probs <- c(probs, paste("issue:", s$issue))
      fp <- strsplit(s$pages, "-")[[1]][1]
      if (!is.na(fp) && nzchar(fp) && !grepl(fp, r, fixed = TRUE)) probs <- c(probs, paste("pages:", s$pages))
      au <- strsplit(s$sortfirstauthor, " ")[[1]][1]
      if (nzchar(au) && !grepl(norm(au), norm(substr(r, 1, 80)), fixed = TRUE)) probs <- c(probs, paste("first author:", s$sortfirstauthor))
    }
  } else if (!is.na(doi)) {
    cr <- tryCatch(fromJSON(paste0("https://api.crossref.org/works/", doi), simplifyVector = FALSE)$message,
                   error = function(e) NULL)
    if (is.null(cr)) probs <- "DOI does not resolve in Crossref"
    else if (!grepl(substr(norm(gsub("<[^>]+>", "", cr$title[[1]])), 1, 30), norm(r), fixed = TRUE))
      probs <- paste("title:", cr$title[[1]])
  } else {
    row$status <- "WEB SOURCE"; row$problems <- "no PMID/DOI: check URL manually"
    return(row)
  }
  if (length(probs)) { row$status <- "CHECK"; row$problems <- paste(probs, collapse = " | ") }
  row
}

res <- do.call(rbind, lapply(seq_along(refs), check_one))
write.csv(res, file.path(OUT, "reference_check.csv"), row.names = FALSE)
cat(sprintf("%d references: %d OK, %d CHECK, %d web sources\n", nrow(res),
            sum(res$status == "OK"), sum(res$status == "CHECK"), sum(res$status == "WEB SOURCE")))
