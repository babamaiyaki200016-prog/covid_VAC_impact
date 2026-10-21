#!/usr/bin/env Rscript
# ============================================================================
# build_figures_doc.R -- main-figures document with captions taken verbatim
# from the manuscript, so figures and text can never disagree.
# ----------------------------------------------------------------------------
# Usage:  Rscript analysis/covid_vax_impact/build_figures_doc.R manuscript.docx out_dir
# Writes: out_dir/figures_v5.md (+ .docx via pandoc), images copied to out_dir/figures/
# ============================================================================

suppressMessages(library(officer))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) stop("Usage: build_figures_doc.R manuscript.docx out_dir", call. = FALSE)
RES <- file.path("outputs", "covid_vax_impact"); OUTD <- args[2]

txt <- docx_summary(read_docx(args[1]))$text
caption <- function(n) {
  hit <- txt[grepl(sprintf("^Figure %d\\. ", n), txt)]
  if (!length(hit)) stop("No caption for Figure ", n, call. = FALSE)
  hit[which.max(nchar(hit))]                      # the full in-text caption, not the short legend
}
figs <- c("fig_calibration_fit.png", "fig_deaths_counterfactual.png",
          "fig_scenarios_deaths_averted.png", "figS3_attack_rate_sensitivity.png")
dir.create(file.path(OUTD, "figures"), showWarnings = FALSE, recursive = TRUE)
invisible(file.copy(file.path(RES, figs), file.path(OUTD, "figures", figs), overwrite = TRUE))

L <- c("# Main figures", "",
       "*Companion figures document to the manuscript. Supplementary figures S1–S4 are in the Supplementary appendix. Each caption appears directly beneath its figure.*", "")
for (i in seq_along(figs)) {
  cap <- caption(i)
  L <- c(L, "---", "", sprintf("![](figures/%s)", figs[i]), "",
         sub("^(Figure \\d+\\.)", "**\\1**", cap), "")
}
md <- file.path(OUTD, "figures_v5.md")
writeLines(L, md, useBytes = TRUE)
if (nzchar(Sys.which("pandoc"))) {
  old <- setwd(OUTD); on.exit(setwd(old))
  system2("pandoc", c("figures_v5.md", "-o", "figures_v5.docx", "--resource-path=."))
}
