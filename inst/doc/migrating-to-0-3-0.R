## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>",
  error    = TRUE   # allow expect_error demonstrations to render
)

## ----setup--------------------------------------------------------------------
library(mpindex)

## ----dotted-error-------------------------------------------------------------
# Old (0.2.x) — now errors
define_mpi_specs(
  "path/to/specs.csv",
  .uid            = "uuid",
  .poverty_cutoff = 1/3
)

## ----no-specs-error-----------------------------------------------------------
define_deprivation(df_household, drinking_water, cutoff = drinking_water == 2)

## ----deprecated-fn------------------------------------------------------------
# Soft-deprecated: raises a warning but still works
specs <- use_global_mpi_specs()

## ----aggregation-error--------------------------------------------------------
define_mpi_specs(
  system.file("extdata", "global-mpi-specs.csv", package = "mpindex"),
  aggregation = "region"
)

