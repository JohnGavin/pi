#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(rix))

parse_field <- function(desc_raw, field) {
  if (!field %in% colnames(desc_raw)) {
    return(character())
  }

  pkgs <- strsplit(desc_raw[, field], ",\\s*|\\n\\s*")[[1]]
  pkgs <- gsub("\\s*\\([^)]+\\)", "", trimws(pkgs))
  pkgs[nzchar(pkgs) & !is.na(pkgs)]
}

desc_raw <- read.dcf("DESCRIPTION")
desc_deps <- unique(c(parse_field(desc_raw, "Imports"), parse_field(desc_raw, "Suggests")))
dev_extras <- c("mirai", "nanonext", "usethis", "gert", "gh", "pkgdown", "styler", "air", "spelling")
r_pkgs <- sort(unique(c(desc_deps, dev_extras)))

rix::rix(
  r_pkgs = r_pkgs,
  system_pkgs = NULL,
  date = "2026-01-05",
  ide = "none",
  overwrite = TRUE
)
