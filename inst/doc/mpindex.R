## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval=F------------------------------------------------------------------
#  install.packages('mpindex')

## ---- eval=F------------------------------------------------------------------
#  devtools::install_github('yng-me/mpindex')

## ----setup--------------------------------------------------------------------
library(mpindex)

## -----------------------------------------------------------------------------
system.file("extdata", package = "mpindex") |> list.files()

## -----------------------------------------------------------------------------
specs_file <- system.file("extdata", "global-mpi-specs.csv", package = "mpindex")

## ---- echo=F------------------------------------------------------------------
read.csv(specs_file) |> 
  gt::gt() |> 
  gt::tab_header(
    title = 'Global MPI – Dimensions, Indicators, Deprivation Cutoffs, and Weights'
  ) |> 
  gt::tab_options(
    table.width = '100%',
    table.font.size = 12,
  ) |> 
  gt::tab_footnote('Source: Alkire, S., Kanagaratnam, U. and Suppa, N. (2020). ‘The global Multidimensional Poverty Index (MPI): 2020 revision’, OPHI MPI Methodological Note 49, Oxford Poverty and Human Development Initiative, University of Oxford.')

## -----------------------------------------------------------------------------
specs_file <- system.file("extdata", "global-mpi-specs.csv", package = "mpindex")
mpi_specs <- define_mpi_specs(specs_file)

## ---- eval=F------------------------------------------------------------------
#  mpi_specs <- define_mpi_specs(
#    .mpi_specs_file = specs_file,
#    .poverty_cutoffs = c(1/3, 0.2, 0.8)
#  )

## -----------------------------------------------------------------------------
mpi_specs <- define_mpi_specs(
  .mpi_specs_file = specs_file, 
  .uid = 'uuid'
)

## ---- eval=F------------------------------------------------------------------
#  mpi_specs <- define_mpi_specs(
#    .mpi_specs_file = specs_file,
#    .poverty_cutoffs = c(1/3, 0.2, 0.8),
#    .uid = 'uuid',
#    .aggregation = 'class'
#  )

## -----------------------------------------------------------------------------
options(mpi_specs = mpi_specs)

## ---- warning=F, message=F----------------------------------------------------
library(dplyr)

glimpse(df_household)

## -----------------------------------------------------------------------------
glimpse(df_household_roster)

## -----------------------------------------------------------------------------
deprivation_profile <- list()

## -----------------------------------------------------------------------------
deprivation_profile$nutrition <- df_household_roster |> 
  define_deprivation(
    .indicator = nutrition,
    .cutoff = undernourished == 1 & age < 70,
    .collapse = TRUE
  )

## -----------------------------------------------------------------------------
deprivation_profile$child_mortality <- df_household |> 
  define_deprivation(
    .indicator = child_mortality,
    .cutoff = with_child_died == 1
  )

## -----------------------------------------------------------------------------
deprivation_profile$year_schooling <- df_household_roster |> 
  define_deprivation(
    .indicator = year_schooling,
    .cutoff = completed_6yrs_schooling == 2,
    .collapse = TRUE
  )

## -----------------------------------------------------------------------------
deprivation_profile$school_attendance <- df_household_roster |> 
  define_deprivation(
    .indicator = school_attendance,
    .cutoff = attending_school == 2 & age %in% c(5:24),
    .collapse = TRUE
  )

## -----------------------------------------------------------------------------
deprivation_profile$cooking_fuel <- df_household |> 
  define_deprivation(
    .indicator = cooking_fuel,
    .cutoff = cooking_fuel %in% c(4:6, 9)
  )

## -----------------------------------------------------------------------------
deprivation_profile$sanitation <- df_household |> 
  define_deprivation(
    .indicator = sanitation,
    .cutoff = toilet > 1
  )

## -----------------------------------------------------------------------------
deprivation_profile$drinking_water <- df_household |> 
  define_deprivation(
    .indicator = drinking_water,
    .cutoff = drinking_water == 2
  )

## -----------------------------------------------------------------------------
deprivation_profile$electricity <- df_household |> 
  define_deprivation(
    .indicator = electricity,
    .cutoff = electricity == 2
  )

## -----------------------------------------------------------------------------
deprivation_profile$housing <- df_household |> 
  define_deprivation(
    .indicator = housing,
    .cutoff = roof %in% c(5, 7, 9) | walls %in% c(5, 8, 9, 99) == 2 | floor %in% c(5, 6, 9)
  )

## -----------------------------------------------------------------------------
deprivation_profile$assets <- df_household |> 
  mutate_at(vars(starts_with('asset_')), ~ if_else(. > 0, 1L, 0L)) |> 
  mutate(
    asset_phone = if_else(
      (asset_telephone + asset_mobile_phone) > 0, 
      1L, 
      0L
    )
  ) |> 
  mutate(
    with_hh_conveniences = (
      asset_tv + asset_phone + asset_computer + 
        asset_animal_cart + asset_bicycle + 
        asset_motorcycle + asset_refrigerator) > 1,
    with_mobility_assets = (asset_car + asset_truck) > 0
  ) |> 
  define_deprivation(
    .indicator = assets,
    .cutoff = !(with_hh_conveniences & with_mobility_assets)
  )

## -----------------------------------------------------------------------------
mpi_result <- df_household |>
  compute_mpi(deprivation_profile)

names(mpi_result)

## ---- eval=F------------------------------------------------------------------
#  mpi_result$index

## ---- echo=F------------------------------------------------------------------
mpi_result$index |>
  gt::gt() |> 
  gt::tab_header(
    title = 'MPI Results using 33% Poverty Cutoff'
  ) |> 
  gt::fmt_number(
    columns = 2:4,
    decimals = 3
  ) |> 
  gt::tab_options(
    table.width = '100%',
    table.font.size = 12,
  )

## ---- eval=F------------------------------------------------------------------
#  mpi_result$contribution

## ---- echo=F------------------------------------------------------------------
mpi_result$contribution |> 
  rename_all(~ stringr::str_remove(., '^(Health|Education|Living Standards)>')) |> 
  gt::gt() |> 
  gt::tab_header(
    title = 'Contribution by Dimenstion and Indicator to MPI using 33% Poverty Cutoff'
  ) |> 
  gt:: tab_spanner(
    label = "Health",
    columns = 2:3
  ) |> 
  gt:: tab_spanner(
    label = "Education",
    columns = 4:5
  ) |> 
  gt:: tab_spanner(
    label = "Living Standards",
    columns = 6:11
  ) |> 
  gt::fmt_number(
    columns = 2:11,
    decimals = 1
  ) |> 
  gt::tab_options(
    table.font.size = 12,
  )

## ---- eval=F------------------------------------------------------------------
#  mpi_result$headcount_ratio$uncensored

## ---- echo=F------------------------------------------------------------------
mpi_result$headcount_ratio$uncensored |> 
  rename_all(~ stringr::str_remove(., '^(Health|Education|Living Standards)>')) |> 
  gt::gt() |> 
  gt::tab_header(
    title = 'Uncensored Headcount Ratio'
  ) |> 
  gt:: tab_spanner(
    label = "Health",
    columns = 2:3
  ) |> 
  gt:: tab_spanner(
    label = "Education",
    columns = 4:5
  ) |> 
  gt:: tab_spanner(
    label = "Living Standards",
    columns = 6:11
  ) |> 
  gt::fmt_number(
    columns = 2:11,
    decimals = 3
  ) |> 
  gt::tab_options(
    table.font.size = 12,
  )

## ---- eval=F------------------------------------------------------------------
#  mpi_result$headcount_ratio$censored

## ---- echo=F------------------------------------------------------------------
mpi_result$headcount_ratio$censored |> 
  rename_all(~ stringr::str_remove(., '^(Health|Education|Living Standards)>')) |> 
  gt::gt() |> 
  gt::tab_header(
    title = 'Censored Headcount Ratio using 33% Poverty Cutoff'
  ) |> 
  gt:: tab_spanner(
    label = "Health",
    columns = 2:3
  ) |> 
  gt:: tab_spanner(
    label = "Education",
    columns = 4:5
  ) |> 
  gt:: tab_spanner(
    label = "Living Standards",
    columns = 6:11
  ) |> 
  gt::fmt_number(
    columns = 2:11,
    decimals = 3
  ) |> 
  gt::tab_options(
    table.font.size = 12,
  )

## ---- eval=F------------------------------------------------------------------
#  mpi_result$deprivation_matrix$uncensored |> head()

## ---- echo=F------------------------------------------------------------------
mpi_result$deprivation_matrix$uncensored |> 
  head() |> 
  rename_all(~ stringr::str_remove(., '^(Health|Education|Living Standards)>')) |> 
  gt::gt() |> 
  gt::tab_header(
    title = 'Uncensored Deprivation Matrix '
  ) |> 
  gt:: tab_spanner(
    label = "Health",
    columns = 2:3
  ) |> 
  gt:: tab_spanner(
    label = "Education",
    columns = 4:5
  ) |> 
  gt:: tab_spanner(
    label = "Living Standards",
    columns = 6:11
  ) |> 
  gt::fmt_number(
    columns = 2,
    decimals = 3
  ) |> 
  gt::tab_options(
    table.font.size = 12,
  )

## ---- eval=F------------------------------------------------------------------
#  mpi_result$deprivation_matrix$censored |> head()

## ---- echo=F------------------------------------------------------------------
mpi_result$deprivation_matrix$censored |> 
  head() |> 
  rename_all(~ stringr::str_remove(., '^(Health|Education|Living Standards)>')) |> 
  gt::gt() |> 
  gt::tab_header(
    title = 'Censored Deprivation Matrix using 33% Poverty Cutoff'
  ) |> 
  gt:: tab_spanner(
    label = "Health",
    columns = 2:3
  ) |> 
  gt:: tab_spanner(
    label = "Education",
    columns = 4:5
  ) |> 
  gt:: tab_spanner(
    label = "Living Standards",
    columns = 6:11
  ) |> 
  gt::fmt_number(
    columns = 2,
    decimals = 3
  ) |> 
  gt::tab_options(
    table.font.size = 12,
  )

## ---- eval=F------------------------------------------------------------------
#  save_mpi(mpi_result, .filename = 'MPI Sample Output')

## ---- eval=F------------------------------------------------------------------
#  # ----------------------------------
#  # Load MPI specs from the built-in specs file
#  specs_file <- system.file("extdata", "global-mpi-specs.csv", package = "mpindex")
#  mpi_specs <- define_mpi_specs(specs_file, .uid = 'uuid')
#  options(mpi_specs = mpi_specs)
#  
#  # ----------------------------------
#  # Create an empty list to store deprivation profile for each indicator
#  deprivation_profile <- list()
#  
#  deprivation_profile$nutrition <- df_household_roster |>
#    define_deprivation(
#     .indicator = nutrition,
#     .cutoff = undernourished == 1 & age < 70,
#     .collapse = TRUE
#    )
#  
#  deprivation_profile$child_mortality <- df_household |>
#    define_deprivation(
#     .indicator = child_mortality,
#     .cutoff = with_child_died == 1
#    )
#  
#  deprivation_profile$year_schooling <- df_household_roster |>
#    define_deprivation(
#     .indicator = year_schooling,
#     .cutoff = completed_6yrs_schooling == 2,
#     .collapse = TRUE
#    )
#  
#  deprivation_profile$school_attendance <- df_household_roster |>
#    define_deprivation(
#     .indicator = school_attendance,
#     .cutoff = attending_school == 2 & age %in% c(5:24),
#     .collapse = TRUE
#    )
#  
#  deprivation_profile$cooking_fuel <- df_household |>
#    define_deprivation(
#     .indicator = cooking_fuel,
#     .cutoff = cooking_fuel %in% c(4:6, 9)
#    )
#  
#  deprivation_profile$sanitation <- df_household |>
#    define_deprivation(
#     .indicator = sanitation,
#     .cutoff = toilet > 1
#    )
#  
#  deprivation_profile$drinking_water <- df_household |>
#    define_deprivation(
#     .indicator = drinking_water,
#     .cutoff = drinking_water == 2
#    )
#  
#  deprivation_profile$electricity <- df_household |>
#    define_deprivation(
#     .indicator = electricity,
#     .cutoff = electricity == 2
#    )
#  
#  deprivation_profile$housing <- df_household |>
#    define_deprivation(
#     .indicator = housing,
#     .cutoff = roof %in% c(5, 7, 9) |
#       walls %in% c(5, 8, 9, 99) == 2 |
#       floor %in% c(5, 6, 9)
#    )
#  
#  deprivation_profile$assets <- df_household |>
#    dplyr::mutate_at(
#      dplyr::vars(dplyr::starts_with('asset_')),
#      ~ dplyr::if_else(. > 0, 1L, 0L)
#    ) |>
#    dplyr::mutate(
#     asset_phone = dplyr::if_else(
#       (asset_telephone + asset_mobile_phone) > 0,
#       1L,
#       0L
#     )
#    ) |>
#    dplyr::mutate(
#     with_hh_conveniences = (
#       asset_tv + asset_phone + asset_computer +
#         asset_animal_cart + asset_bicycle +
#         asset_motorcycle + asset_refrigerator) > 1,
#     with_mobility_assets = (asset_car + asset_truck) > 0
#    ) |>
#    define_deprivation(
#     .indicator = assets,
#     .cutoff = !(with_hh_conveniences & with_mobility_assets)
#    )
#  
#  # ----------------------------------
#  # Compute the MPI
#  mpi_result <- df_household |>
#    compute_mpi(deprivation_profile)
#  
#  # ----------------------------------
#  # You may also save your output into an Excel file
#  save_mpi(mpi_result, .filename = 'MPI Sample Output')

