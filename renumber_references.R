#!/usr/bin/env Rscript
# ============================================================================
# renumber_references.R -- Vancouver renumbering by order of first citation
# ----------------------------------------------------------------------------
# Scans the manuscript in reading order up to the 'References' heading. A
# table's citations are counted where the text first mentions it ("Table 1"),
# not where the table physically sits. Then it
#   * rewrites every superscript citation (main text and tables),
#   * reorders the numbered reference list to match (one continuous list),
#   * updates "ref N" mentions inside Word comments,
#   * writes the old -> new mapping to outputs/covid_vax_impact/reference_map.csv.
# Uncited references are kept at the end of the list and reported.
#
# Usage:   Rscript analysis/covid_vax_impact/renumber_references.R in.docx out.docx
# ============================================================================

suppressMessages({ library(officer); library(xml2) })
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) stop("Usage: renumber_references.R in.docx out.docx", call. = FALSE)

x    <- read_docx(args[1])
body <- docx_body_xml(x)
SUP  <- "w:r[w:rPr/w:vertAlign[@w:val='superscript']]"

# ---- parse / format citation strings --------------------------------------
expand <- function(s) {
  unlist(lapply(strsplit(gsub("\\s", "", s), ",")[[1]], function(tok) {
    ab <- as.integer(strsplit(tok, "[-–]")[[1]])
    if (length(ab) == 2) seq(ab[1], ab[2]) else ab
  }))
}
is_cite <- function(s) grepl("^[0-9,– -]+$", s)

# ---- 1. reading order: body blocks, tables inserted at first callout --------
blocks <- xml_children(xml_find_first(body, "//w:body"))
kinds  <- xml_name(blocks)
texts  <- vapply(blocks, xml_text, "")
ref_at <- which(kinds == "p" & trimws(texts) == "References")[1]
style <- vapply(blocks, function(b) {
  s <- xml_attr(xml_find_first(b, "w:pPr/w:pStyle"), "val"); ifelse(is.na(s), "", s) }, "")
# start after the title/author block so affiliation superscripts are never touched
start_at <- which(kinds == "p" & grepl("^Heading[12]$", style))[1]
if (is.na(start_at)) start_at <- 1
if (is.na(ref_at)) stop("No 'References' heading", call. = FALSE)

tbl_idx <- which(kinds == "tbl")
caption_no <- function(i) {            # table number from the nearest caption above it
  for (j in rev(seq_len(i - 1))) {
    m <- regmatches(texts[j], regexpr("^Table [0-9]+", texts[j]))
    if (length(m)) return(sub("Table ", "", m))
  }
  NA_character_
}
tbl_no  <- vapply(tbl_idx, caption_no, "")
# fall back to physical order if captions are not directly above the tables
if (any(is.na(tbl_no)) || anyDuplicated(tbl_no)) tbl_no <- as.character(seq_along(tbl_idx))

order_seen <- integer(0)
note <- function(node) {
  for (r in xml_find_all(node, paste0(".//", SUP))) {
    s <- xml_text(r)
    if (is_cite(s)) for (n in expand(s)) if (!n %in% order_seen) order_seen <<- c(order_seen, n)
  }
}
pending <- setNames(as.list(tbl_idx), tbl_no)
for (i in start_at:(ref_at - 1)) {
  if (kinds[i] == "tbl") next                      # handled at its callout
  note(blocks[[i]])
  for (k in names(pending)) {
    if (grepl(paste0("\\bTable ", k, "\\b"), texts[i])) {
      note(blocks[[pending[[k]]]]); pending[[k]] <- NULL
    }
  }
}
for (k in names(pending)) note(blocks[[pending[[k]]]])   # tables never called out

# ---- 2. the reference list (numbered paragraphs after the heading) ----------
after <- (ref_at + 1):length(blocks)
# the list ends at the next Heading 1/2 or a 'Declarations' paragraph
stop_at <- after[kinds[after] == "p" & (grepl("^Heading[12]$", style[after]) |
                                         trimws(texts[after]) == "Declarations")][1]
if (!is.na(stop_at)) after <- after[after < stop_at]
numbered <- after[vapply(after, function(i) kinds[i] == "p" &&
                   length(xml_find_all(blocks[[i]], "w:pPr/w:numPr")) > 0, TRUE)]
n_ref <- length(numbered)

bad <- setdiff(order_seen, seq_len(n_ref))
if (length(bad)) stop("Cited numbers with no reference: ", paste(bad, collapse = ","), call. = FALSE)
uncited <- setdiff(seq_len(n_ref), order_seen)
new_order <- c(order_seen, uncited)                # new_order[new] = old
map <- integer(n_ref); map[new_order] <- seq_len(n_ref)   # map[old] = new

# ---- 3. rewrite superscripts between the first heading and the reference list
for (i in start_at:(ref_at - 1)) {
  for (r in xml_find_all(blocks[[i]], paste0(".//", SUP))) {
    s <- xml_text(r)
    if (!is_cite(s)) next
    t <- xml_find_first(r, "w:t")
    xml_text(t) <- paste(sort(unique(map[expand(s)])), collapse = ",")
  }
}

# ---- 4. reorder list: one continuous list, drop interim sub-headings --------
first <- numbered[1]; numId <- xml_attr(xml_find_first(blocks[[first]], "w:pPr/w:numPr/w:numId"), "val")
between <- setdiff(seq(first, max(numbered)), numbered)
for (i in between) if (kinds[i] == "p" && nzchar(trimws(texts[i]))) xml_remove(blocks[[i]])
anchor <- xml_find_first(blocks[[first]], "preceding-sibling::*[1]")
moved  <- lapply(numbered[new_order], function(i) blocks[[i]])
for (nd in moved) xml_remove(nd)
for (nd in rev(moved)) {
  xml_set_attr(xml_find_first(nd, "w:pPr/w:numPr/w:numId"), "w:val", numId, ns = xml_ns(body))
  xml_add_sibling(anchor, nd, .where = "after")
}

print(x, target = args[2])

# ---- 5. "ref N" mentions in comments (patched after saving: officer rewrites comments.xml)
tmpd <- tempfile(); dir.create(tmpd)
utils::unzip(args[2], exdir = tmpd)
cfile <- file.path(tmpd, "word", "comments.xml")
if (file.exists(cfile)) {
  cx <- read_xml(cfile); changed <- FALSE
  for (t in xml_find_all(cx, "//w:t", xml_ns(cx))) {
    s <- xml_text(t)
    if (!grepl("\\bref [0-9]+", s)) next
    m <- gregexpr("\\bref [0-9]+", s)[[1]]
    hits <- regmatches(s, list(m))[[1]]
    regmatches(s, list(m)) <- list(paste("ref", map[as.integer(sub("ref ", "", hits))]))
    xml_text(t) <- s; changed <- TRUE
  }
  if (changed) {
    write_xml(cx, cfile)
    files <- list.files(tmpd, recursive = TRUE, all.files = TRUE)
    files <- c("[Content_Types].xml", setdiff(files, "[Content_Types].xml"))
    out <- normalizePath(args[2]); unlink(out)
    zip::zip(out, files = files, root = tmpd)
  }
}
dir.create(file.path("outputs", "covid_vax_impact"), showWarnings = FALSE, recursive = TRUE)
mp <- data.frame(old = seq_len(n_ref), new = map)
write.csv(mp, file.path("outputs", "covid_vax_impact", "reference_map.csv"), row.names = FALSE)
moved_n <- sum(mp$old != mp$new)
cat(sprintf("%d references; %d renumbered; uncited: %s\n", n_ref, moved_n,
            if (length(uncited)) paste(uncited, collapse = ",") else "none"))
