## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----eval = FALSE-------------------------------------------------------------
# install.packages("survey")   # only needed once

## ----setup--------------------------------------------------------------------
library(mpindex)
library(survey)

## -----------------------------------------------------------------------------
mpi_specs <- global_mpi_specs(uid = "uuid")

## -----------------------------------------------------------------------------
set.seed(42)
n <- nrow(df_household)

df_hh <- df_household
df_hh$hh_weight <- runif(n, 0.8, 2.5)   # sampling weight
df_hh$strata    <- sample(c("urban", "rural"), n, replace = TRUE)
df_hh$psu       <- sample(1:30, n, replace = TRUE)  # primary sampling unit

## -----------------------------------------------------------------------------
deprivations <- list(
  nutrition         = deprived(undernourished == 1 & age < 70,
                               .data = df_household_roster, collapse_fn = max),
  child_mortality   = deprived(with_child_died == 1),
  year_schooling    = deprived(completed_6yrs_schooling == 2,
                               .data = df_household_roster, collapse_fn = max),
  school_attendance = deprived(attending_school == 2 & age %in% 5:24,
                               .data = df_household_roster, collapse_fn = max),
  cooking_fuel      = deprived(cooking_fuel %in% c(4:6, 9)),
  sanitation        = deprived(toilet > 1),
  drinking_water    = deprived(drinking_water == 2),
  electricity       = deprived(electricity == 2),
  housing           = deprived(
    roof %in% c(5, 7, 9) | walls %in% c(5, 8, 9, 99) == 2 | floor %in% c(5, 6, 9)
  ),
  assets = deprived(!(
    (asset_tv + asset_telephone + asset_mobile_phone + asset_computer +
       asset_animal_cart + asset_bicycle + asset_motorcycle +
       asset_refrigerator) > 1 &
      (asset_car + asset_truck) > 0
  ))
)

## -----------------------------------------------------------------------------
mpi_simple <- compute_mpi(
  df_hh,
  mpi_specs = mpi_specs,
  deprivations = deprivations,
  weight = "hh_weight"
)

mpi_simple$index$k_33

## -----------------------------------------------------------------------------
mpi_simple_inf <- compute_mpi(
  df_hh,
  mpi_specs = mpi_specs,
  deprivations = deprivations,
  weight = "hh_weight",
  inference = TRUE
)

mpi_simple_inf$index$k_33[, c("headcount_ratio", "headcount_ratio_se",
                               "mpi", "mpi_se")]

## -----------------------------------------------------------------------------
mpi_weighted <- compute_mpi(
  df_hh,
  mpi_specs = mpi_specs,
  deprivations = deprivations,
  weight = "hh_weight",
  strata = "strata",
  cluster = "psu"
)

mpi_weighted$index$k_33

## ----eval = FALSE-------------------------------------------------------------
# compute_mpi(df_hh, mpi_specs = mpi_specs, deprivations = deprivations,
#             weight = "hh_weight", strata = "strata", cluster = "psu",
#             .fpc    = "stratum_size")

## -----------------------------------------------------------------------------
svy <- svydesign(
  ids     = ~psu,
  strata  = ~strata,
  weights = ~hh_weight,
  nest    = TRUE,         # PSU IDs restart within each stratum
  data    = df_hh
)

mpi_from_design <- compute_mpi(
  df_hh,
  mpi_specs = mpi_specs,
  deprivations = deprivations,
  survey_design = svy
)

mpi_from_design$index$k_33

## -----------------------------------------------------------------------------
mpi_inference <- compute_mpi(
  df_hh,
  mpi_specs = mpi_specs,
  deprivations = deprivations,
  weight = "hh_weight",
  strata = "strata",
  cluster = "psu",
  inference = TRUE
)

mpi_inference$index$k_33

## ----eval = FALSE-------------------------------------------------------------
# compute_mpi(..., inference = TRUE, ci_level = 0.90)   # 90% CI

## -----------------------------------------------------------------------------
mpi_by_class <- compute_mpi(
  df_hh,
  mpi_specs = mpi_specs,
  deprivations = deprivations,
  weight = "hh_weight",
  by = class,
  inference = TRUE
)

mpi_by_class$index$k_33

## -----------------------------------------------------------------------------
mpi_unweighted <- compute_mpi(df_household, mpi_specs, deprivations)

cat("Unweighted H:", round(mpi_unweighted$index$k_33$headcount_ratio, 4), "\n")
cat("Weighted   H:", round(mpi_weighted$index$k_33$headcount_ratio,   4), "\n")

## ----eval = FALSE-------------------------------------------------------------
# mpi_result <- compute_mpi(
#   df_hh,
#   mpi_specs    = mpi_specs,
#   deprivations = deprivation_profile,  # pre-assembled list from define_deprivation()
#   weight = "hh_weight",
#   strata = "strata",
#   cluster = "psu",
#   inference = TRUE
# )

## ----eval = FALSE-------------------------------------------------------------
# save_mpi(mpi_inference, mpi_specs = mpi_specs, filename = "MPI Weighted Results")

