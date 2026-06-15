## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>"
)

## ----eval = FALSE-------------------------------------------------------------
# install.packages("mpindex")

## ----eval = FALSE-------------------------------------------------------------
# # install.packages("devtools")
# devtools::install_github("yng-me/mpindex")

## ----setup--------------------------------------------------------------------
library(mpindex)

## -----------------------------------------------------------------------------
mpi_specs <- global_mpi_specs(uid = "uuid", unit_of_analysis = 'households')

## ----eval = FALSE-------------------------------------------------------------
# specs_file <- system.file("extdata", "global-mpi-specs.csv", package = "mpindex")
# mpi_specs  <- define_mpi_specs(specs_file, uid = "uuid", unit_of_analysis = 'households')

## ----echo = FALSE-------------------------------------------------------------
specs_file <- system.file("extdata", "global-mpi-specs.csv", package = "mpindex")
read.csv(specs_file) |>
  gt::gt() |>
  gt::tab_header(
    title    = "Global MPI — Dimensions, Indicators, and Weights",
    subtitle = "Source: OPHI MPI Methodological Note 49 (2020)"
  ) |>
  gt::tab_options(table.width = "100%", table.font.size = 12) |>
  gt::fmt_number(columns = 4, decimals = 3)

## ----eval = FALSE-------------------------------------------------------------
# mpi_specs <- define_mpi_specs(
#   mpi_specs_file = "path/to/my-specs.csv",
#   uid = "household_id",
#   poverty_cutoffs = 1/3 # default: a household needs >= 33% weighted deprivation score to be MPI poor
# )

## ----eval = FALSE-------------------------------------------------------------
# mpi_specs <- define_mpi_specs(
#   "path/to/my-specs.csv",
#   uid = "household_id",
#   poverty_cutoffs = c(0.20, 1/3, 0.50)   # 20%, 33%, 50%
# )

## ----message = FALSE, warning = FALSE-----------------------------------------
library(dplyr)

glimpse(df_household)

## -----------------------------------------------------------------------------
glimpse(df_household_roster)

## -----------------------------------------------------------------------------
mpi_result <- compute_mpi(
  df_household,
  mpi_specs = mpi_specs,
  deprivations = list(

    # --- Health ---
    nutrition = deprived(
      undernourished == 1 & age < 70,   # deprived if any member under 70 is undernourished
      .data        = df_household_roster,
      collapse_fn = max                 # household is deprived if any member is deprived
    ),
    child_mortality = deprived(with_child_died == 1),

    # --- Education ---
    year_schooling = deprived(
      completed_6yrs_schooling == 2,
      .data        = df_household_roster,
      collapse_fn = max
    ),
    school_attendance = deprived(
      attending_school == 2 & age %in% 5:24,
      .data        = df_household_roster,
      collapse_fn = max
    ),

    # --- Living Standards ---
    cooking_fuel   = deprived(cooking_fuel %in% c(4:6, 9)),
    sanitation     = deprived(toilet > 1),
    drinking_water = deprived(drinking_water == 2),
    electricity    = deprived(electricity == 2),
    housing        = deprived(
      roof %in% c(5, 7, 9) | walls %in% c(5, 8, 9, 99) == 2 | floor %in% c(5, 6, 9)
    ),
    assets = deprived(!(
      (asset_tv + asset_telephone + asset_mobile_phone + asset_computer +
         asset_animal_cart + asset_bicycle + asset_motorcycle +
         asset_refrigerator) > 1 &
        (asset_car + asset_truck) > 0
    ))
  )
)

## -----------------------------------------------------------------------------
names(mpi_result)

## ----eval = FALSE-------------------------------------------------------------
# compute_mpi(
#   df_household,
#   mpi_specs = mpi_specs,
#   deprivations = list(...),
#   by = class        # column in df_household
# )

## ----eval = FALSE-------------------------------------------------------------
# dp <- list()
# 
# dp$nutrition <- df_household_roster |>
#   define_deprivation(
#     indicator = nutrition,
#     cutoff = undernourished == 1 & age < 70,
#     collapse_fn = max,
#     mpi_specs = mpi_specs
#   )
# 
# dp$drinking_water <- df_household |>
#   define_deprivation(
#     indicator = drinking_water,
#     cutoff = drinking_water == 2,
#     mpi_specs = mpi_specs
#   )
# 
# # ... define all remaining indicators, then:
# mpi_result <- compute_mpi(df_household, mpi_specs = mpi_specs, deprivations = dp)

## ----eval = FALSE-------------------------------------------------------------
# mpi_result$index$k_33

## ----echo = FALSE-------------------------------------------------------------
mpi_result$index$k_33 |>
  gt::gt() |>
  gt::tab_header(title = "MPI — 33% Poverty Cutoff") |>
  gt::fmt_number(columns = 2:4, decimals = 3) |>
  gt::tab_options(table.width = "100%", table.font.size = 12)

## ----eval = FALSE-------------------------------------------------------------
# mpi_result$contribution$k_33

## ----echo = FALSE-------------------------------------------------------------
gtx <- function(.gt, .decimals = 1, .offset = 0) {
  d01_cp <- 2:3  + .offset
  d02_cp <- 4:5  + .offset
  d03_cp <- 6:11 + .offset
  .gt |>
    gt::tab_spanner(label = "Health",           columns = d01_cp) |>
    gt::tab_spanner(label = "Education",        columns = d02_cp) |>
    gt::tab_spanner(label = "Living Standards", columns = d03_cp) |>
    gt::fmt_number(columns = c(d01_cp, d02_cp, d03_cp), decimals = .decimals) |>
    gt::tab_options(table.font.size = 12)
}

mpi_result$contribution$k_33 |>
  gt::gt() |>
  gt::tab_header(title = "Contribution to MPI by Indicator — 33% Poverty Cutoff") |>
  gtx()

## ----eval = FALSE-------------------------------------------------------------
# mpi_result$headcount_ratio$uncensored   # deprivation rate — all households
# mpi_result$headcount_ratio$k_33         # deprivation rate — poor households only

## ----echo = FALSE-------------------------------------------------------------
mpi_result$headcount_ratio$uncensored |>
  dplyr::ungroup() |>
  gt::gt() |>
  gt::tab_header(title = "Uncensored Headcount Ratio (all households)") |>
  gtx(.decimals = 3)

## ----echo = FALSE-------------------------------------------------------------
mpi_result$headcount_ratio$k_33 |>
  dplyr::ungroup() |>
  gt::gt() |>
  gt::tab_header(title = "Censored Headcount Ratio (poor households only, k = 33%)") |>
  gtx(.decimals = 3)

## ----echo=FALSE---------------------------------------------------------------
mpi_result <- compute_mpi(
  df_household,
  mpi_specs = mpi_specs,
  deprivations = list(

    # --- Health ---
    nutrition = deprived(
      undernourished == 1 & age < 70,   # deprived if any member under 70 is undernourished
      .data        = df_household_roster,
      collapse_fn = max                 # household is deprived if any member is deprived
    ),
    child_mortality = deprived(with_child_died == 1),

    # --- Education ---
    year_schooling = deprived(
      completed_6yrs_schooling == 2,
      .data        = df_household_roster,
      collapse_fn = max
    ),
    school_attendance = deprived(
      attending_school == 2 & age %in% 5:24,
      .data        = df_household_roster,
      collapse_fn = max
    ),

    # --- Living Standards ---
    cooking_fuel   = deprived(cooking_fuel %in% c(4:6, 9)),
    sanitation     = deprived(toilet > 1),
    drinking_water = deprived(drinking_water == 2),
    electricity    = deprived(electricity == 2),
    housing        = deprived(
      roof %in% c(5, 7, 9) | walls %in% c(5, 8, 9, 99) == 2 | floor %in% c(5, 6, 9)
    ),
    assets = deprived(!(
      (asset_tv + asset_telephone + asset_mobile_phone + asset_computer +
         asset_animal_cart + asset_bicycle + asset_motorcycle +
         asset_refrigerator) > 1 &
        (asset_car + asset_truck) > 0
    ))
  ),
  include_deprivation_matrix = TRUE
)

## ----eval = FALSE-------------------------------------------------------------
# mpi_result <- compute_mpi(
#   df_household,
#   mpi_specs = mpi_specs,
#   deprivations = list(...),
#   include_deprivation_matrix = TRUE
# )
# 
# mpi_result$deprivation_matrix |> head()

## ----echo = FALSE-------------------------------------------------------------
mpi_result$deprivation_matrix$uncensored |>
  dplyr::ungroup() |>
  head() |>
  gt::gt() |>
  gt::tab_header(title = "Deprivation Matrix — first 6 households (uncensored)") |>
  gtx(.decimals = 0, .offset = 1) |>
  gt::fmt_number(columns = 3, decimals = 3)

## ----eval = FALSE-------------------------------------------------------------
# mpi_result$deprivation_matrix$k_33 |> head()

## ----echo = FALSE-------------------------------------------------------------
mpi_result$deprivation_matrix$k_33 |>
  dplyr::ungroup() |>
  head() |>
  gt::gt() |>
  gt::tab_header(title = "Deprivation Matrix — first 6 households (k = 33% cutoff)") |>
  gtx(.decimals = 0, .offset = 1) |>
  gt::fmt_number(columns = 3, decimals = 3)

## ----eval = FALSE-------------------------------------------------------------
# save_mpi(mpi_result, mpi_specs = mpi_specs, filename = "MPI Results")

## ----eval = FALSE-------------------------------------------------------------
# save_mpi(
#   mpi_result,
#   mpi_specs = mpi_specs,
#   filename = "MPI Results",
#   include_specs = TRUE
# )

## ----eval = FALSE-------------------------------------------------------------
# library(mpindex)
# 
# # 1. Load specifications
# mpi_specs <- global_mpi_specs(uid = "uuid", unit_of_analysis = 'households')
# 
# # 2. Compute the MPI
# mpi_result <- compute_mpi(
#   df_household,
#   mpi_specs = mpi_specs,
#   deprivations = list(
#     nutrition = deprived(undernourished == 1 & age < 70, .data = df_household_roster, collapse_fn = max),
#     child_mortality = deprived(with_child_died == 1),
#     year_schooling = deprived(completed_6yrs_schooling == 2, .data = df_household_roster, collapse_fn = max),
#     school_attendance = deprived(attending_school == 2 & age %in% 5:24, .data = df_household_roster, collapse_fn = max),
#     cooking_fuel = deprived(cooking_fuel %in% c(4:6, 9)),
#     sanitation = deprived(toilet > 1),
#     drinking_water = deprived(drinking_water == 2),
#     electricity = deprived(electricity == 2),
#     housing = deprived(roof %in% c(5, 7, 9) |walls %in% c(5, 8, 9, 99) == 2 | floor %in% c(5, 6, 9)),
#     assets = deprived(
#       !(asset_tv +
#         asset_telephone +
#         asset_mobile_phone +
#         asset_computer +
#         asset_animal_cart +
#         asset_bicycle +
#         asset_motorcycle +
#         asset_refrigerator) > 1 &
#       (asset_car + asset_truck) > 0
#     )
#   )
# )
# 
# # 3. Inspect results
# mpi_result$index$k_33          # headline MPI, H, A
# mpi_result$contribution$k_33   # indicator contributions (%)
# mpi_result$headcount_ratio$k_33
# mpi_result$deprivation_matrix$k_33
# 
# # 4. Save to Excel
# save_mpi(mpi_result, mpi_specs = mpi_specs, filename = "MPI Results")

