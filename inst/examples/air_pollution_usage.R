# Air pollution indicator usage examples
#
# This script shows how to use climatehealth::air_pollution_do_analysis().
#
# It covers:
# 1) A basic run using standard column names
# 2) A run using custom additional covariates
# 3) Running multiple reference standards
# 4) Saving outputs to disk
#
# Assumes climatehealth is installed. If not, see optional install/load block.

# ------------------------------------------------------------------------------
# Optional install/load (if needed)
# ------------------------------------------------------------------------------
# Preferred (match the main README instructions):
# install.packages("devtools")
# devtools::load_all(path = "{path/to/climatehealth}")
#
# Alternative:
# install.packages("devtools")
# devtools::install_github("onssoschi/climatehealth")
# library(climatehealth)


# ------------------------------------------------------------------------------
# Input data used in these examples
# ------------------------------------------------------------------------------
#
# Replace these placeholders with paths on your machine.

data_path <- "path-to-your-air-pollution-csv"
output_dir <- "path-to-folder-where-you-want-outputs"

# Example local paths
# data_path <- "D:/soschi/data/air_pollution_health_data.csv"
# output_dir <- "D:/soschi/data/air_pollution_outputs"

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
}


# ------------------------------------------------------------------------------
# Expected input columns
# ------------------------------------------------------------------------------
#
# By default, air_pollution_do_analysis() expects columns named:
# - date
# - region
# - pm25
# - deaths
# - population
# - humidity
# - precipitation
# - tmax
# - wind_speed
#
# If your dataset uses different column names, map them through the corresponding
# function arguments.


# ------------------------------------------------------------------------------
# Example A: Basic run using standard column names
# ------------------------------------------------------------------------------
#
# This is the simplest workflow when your dataset already uses the default names.

air_basic_res <- climatehealth::air_pollution_do_analysis(
  data_path = data_path,
  save_outputs = FALSE,
  run_descriptive = TRUE,
  run_power = TRUE
)

# Useful returned object fields may include:
# air_basic_res$data_raw
# air_basic_res$data_with_lags
# air_basic_res$meta_results
# air_basic_res$analysis_results


# ------------------------------------------------------------------------------
# Example B: Custom column mappings and extra covariates
# ------------------------------------------------------------------------------
#
# Use this when your dataset does not use the package defaults for column names,
# or when you want to include extra categorical or continuous covariates.

air_custom_res <- climatehealth::air_pollution_do_analysis(
  data_path = data_path,
  date_col = "observation_date",
  region_col = "district_name",
  pm25_col = "pm25_mean",
  deaths_col = "all_cause_deaths",
  population_col = "population_total",
  humidity_col = "rel_humidity",
  precipitation_col = "rainfall_mm",
  tmax_col = "max_temp",
  wind_speed_col = "wind_ms",
  categorical_others = c("sex", "urban_rural"),
  continuous_others = c("ozone", "no2"),
  save_outputs = FALSE,
  run_descriptive = TRUE,
  run_power = FALSE
)


# ------------------------------------------------------------------------------
# Example C: Multiple reference standards
# ------------------------------------------------------------------------------
#
# Use reference_standards to compare attributable burden against more than one
# PM2.5 threshold in a single run.

air_multi_ref_res <- climatehealth::air_pollution_do_analysis(
  data_path = data_path,
  reference_standards = list(
    list(value = 15, name = "WHO"),
    list(value = 25, name = "National")
  ),
  save_outputs = FALSE,
  run_descriptive = FALSE,
  run_power = TRUE
)


# ------------------------------------------------------------------------------
# Example D: Filter to selected years or regions
# ------------------------------------------------------------------------------
#
# You can limit the analysis to specific years or regions before model fitting.

air_filtered_res <- climatehealth::air_pollution_do_analysis(
  data_path = data_path,
  years_filter = 2019:2021,
  regions_filter = c("North", "South"),
  include_national = FALSE,
  save_outputs = FALSE,
  run_descriptive = FALSE,
  run_power = FALSE
)


# ------------------------------------------------------------------------------
# Example E: Save outputs to disk
# ------------------------------------------------------------------------------
#
# When save_outputs = TRUE, the function will create output files under the
# specified output directory.

air_saved_res <- climatehealth::air_pollution_do_analysis(
  data_path = data_path,
  output_dir = output_dir,
  save_outputs = TRUE,
  run_descriptive = TRUE,
  run_power = TRUE,
  moving_average_window = 3L
)


# ------------------------------------------------------------------------------
# Common workflow rules
# ------------------------------------------------------------------------------
#
# 1. If your dataset already uses the default column names, you only need to
#    provide data_path for a basic run.
#
# 2. Use categorical_others and continuous_others for optional covariates.
#
# 3. Use reference_standards to evaluate more than one PM2.5 threshold.
#
# 4. Set save_outputs = TRUE to write results to output_dir.
