#!/usr/bin/env Rscript
# ============================================================================
# derive_contact_matrix.R -- Nigeria contact matrix from Prem et al. (2021)
# ----------------------------------------------------------------------------
# Downloads the published synthetic all-setting contact matrix for Nigeria
# (16 five-year age groups, 0-4 ... 75+; rows = age of contactor, columns =
# age of contactee, mean contacts per person per day) and collapses it to the
# model's five age bands, weighting contactor groups by their 2020 population
# (UN WPP 2019, as distributed with the same repository):
#
#     C[A, B] = sum_{i in A} N_i * sum_{j in B} c[i, j]  /  sum_{i in A} N_i
#
# Source: Prem K, et al. PLoS Comput Biol 2021;17(7):e1009098 (PMID 34310590),
#         https://github.com/kieshaprem/synthetic-contact-matrices
# Output: outputs/covid_vax_impact/contact_matrix_prem2021_nga.csv
#         (values are copied into config.R; rerun this script to reproduce)
#
# Usage:  Rscript analysis/covid_vax_impact/derive_contact_matrix.R
# ============================================================================

REPO <- "https://github.com/kieshaprem/synthetic-contact-matrices/raw/master/generate_synthetic_matrices"
tmp  <- tempfile(fileext = ".rdata")
download.file(file.path(REPO, "output/syntheticmatrices/contact_all.rdata"), tmp, quiet = TRUE, mode = "wb")
env <- new.env(); load(tmp, envir = env)
c16 <- env$contact_all[["NGA"]]
stopifnot(identical(dim(c16), c(16L, 16L)))

read_pop <- function(sex) {
  f <- tempfile(fileext = ".csv")
  download.file(file.path(REPO, sprintf("input/pop/popage_%s2020.csv", sex)), f, quiet = TRUE)
  read.csv(f, check.names = FALSE, stringsAsFactors = FALSE)
}
pf <- read_pop("female"); pm <- read_pop("male")
row_nga <- function(p) {                          # thousands, 21 groups age0 ... age100
  r <- p[trimws(p[[1]]) == "Nigeria", paste0("age", seq(0, 100, 5))]
  stopifnot(nrow(r) == 1)
  as.numeric(r[1, ])
}
pop21 <- row_nga(pf) + row_nga(pm)
pop16 <- c(pop21[1:15], sum(pop21[16:21]))        # 75+ as in the Prem matrices

bands <- list("0-14" = 1:3, "15-29" = 4:6, "30-49" = 7:10, "50-64" = 11:13, "65+" = 14:16)
C5 <- matrix(NA_real_, 5, 5, dimnames = list(names(bands), names(bands)))
for (a in seq_along(bands)) for (b in seq_along(bands)) {
  i <- bands[[a]]; j <- bands[[b]]
  C5[a, b] <- sum(pop16[i] * rowSums(c16[i, j, drop = FALSE])) / sum(pop16[i])
}

OUT <- file.path("outputs", "covid_vax_impact")
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
write.csv(round(C5, 3), file.path(OUT, "contact_matrix_prem2021_nga.csv"))
print(round(C5, 2))
cat(sprintf("Mean contacts/person/day (population-weighted): %.2f\n",
            sum(rowSums(c16) * pop16) / sum(pop16)))
