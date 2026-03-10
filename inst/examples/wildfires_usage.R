# Wildfire indicator usage examples
#
# This script shows how to use the wildfire indicator through
# climatehealth::wildfire_do_analysis().
#
# It covers:
# 1) A basic RR-only analysis with no population input
# 2) A region-level AF/AN analysis when the dataset already has a `pop` column
# 3) A region-level AF/AN analysis when the population column has a different name
# 4) Saving outputs directly into a fixed directory
# 5) Saving outputs into a timestamped run subdirectory
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

health_data_path <- "path-to-your-health-or-health-climate-csv"
output_base <- "path-to-folder-where-you-want-outputs"

# Example local paths
# health_data_path <- "D:/soschi/data/wildfire_health_data.csv"
# output_base <- "D:/soschi/data/wildfire_outputs"

if (!dir.exists(output_base)) {
  dir.create(output_base, recursive = TRUE, showWarnings = FALSE)
}


# ------------------------------------------------------------------------------
# Expected input columns
# ------------------------------------------------------------------------------
#
# For a basic run without wildfire-data joins, the input CSV should contain:
# - date: daily date column
# - region: region name column
# - temp_mean: daily mean temperature column
# - deaths: daily health outcome count column
# - pm25: wildfire-related PM2.5 column
#
# Population is only required if you set:
# calc_relative_risk_by_region = TRUE
#
# If population is needed, either:
# - include a column already named `pop`, or
# - include another population column and pass its name through population_col


# ------------------------------------------------------------------------------
# Example A: Basic RR-only analysis
# ------------------------------------------------------------------------------
#
# Use this when you only want relative risk outputs.
# Population data are not required for this workflow.

rr_only_res <- climatehealth::wildfire_do_analysis(
  health_path = health_data_path,
  join_wildfire_data = FALSE,
  date_col = "date",
  region_col = "region",
  mean_temperature_col = "temp_mean",
  health_outcome_col = "deaths",
  pm_2_5_col = "pm25",
  wildfire_lag = 3,
  temperature_lag = 1,
  spline_temperature_lag = 0,
  spline_temperature_degrees_freedom = 6,
  calc_relative_risk_by_region = FALSE,
  save_fig = FALSE,
  save_csv = FALSE
)

# Returned object fields
# rr_only_res$RR_results
# rr_only_res$AF_AN_results
# rr_only_res$AR_PM_monthly
#
# In an RR-only run:
# - RR_results should contain the fitted relative risk outputs
# - AF_AN_results is expected to be NULL
# - AR_PM_monthly is expected to be NULL


# ------------------------------------------------------------------------------
# Example B: Region-level AF/AN analysis when the dataset already has `pop`
# ------------------------------------------------------------------------------
#
# Use this when your input CSV already contains a population column named `pop`.
# In that case, do not pass population_col unless you want to be explicit.

af_an_with_pop_res <- climatehealth::wildfire_do_analysis(
  health_path = health_data_path,
  join_wildfire_data = FALSE,
  date_col = "date",
  region_col = "region",
  mean_temperature_col = "temp_mean",
  health_outcome_col = "deaths",
  pm_2_5_col = "pm25",
  calc_relative_risk_by_region = TRUE,
  save_fig = FALSE,
  save_csv = FALSE
)

# In this workflow:
# - RR_results contains the relative risk outputs
# - AF_AN_results contains region-level attributable fraction / number outputs
# - AR_PM_monthly contains joined monthly AR and PM outputs


# ------------------------------------------------------------------------------
# Example C: Region-level AF/AN analysis when the population column has
# a different name
# ------------------------------------------------------------------------------
#
# If your CSV does not use `pop`, pass the external population column name
# through population_col. The wildfire workflow will standardize it internally.

af_an_external_pop_res <- climatehealth::wildfire_do_analysis(
  health_path = health_data_path,
  join_wildfire_data = FALSE,
  date_col = "date",
  region_col = "region",
  mean_temperature_col = "temp_mean",
  health_outcome_col = "deaths",
  population_col = "population_total",
  pm_2_5_col = "pm25",
  calc_relative_risk_by_region = TRUE,
  save_fig = FALSE,
  save_csv = FALSE
)


# ------------------------------------------------------------------------------
# Example D: Save outputs directly into a fixed directory
# ------------------------------------------------------------------------------
#
# This is the default output behavior.
# Results are written directly into output_folder_path, and model validation
# outputs are written into output_folder_path/model_validation.

fixed_output_res <- climatehealth::wildfire_do_analysis(
  health_path = health_data_path,
  join_wildfire_data = FALSE,
  date_col = "date",
  region_col = "region",
  mean_temperature_col = "temp_mean",
  health_outcome_col = "deaths",
  pm_2_5_col = "pm25",
  calc_relative_risk_by_region = TRUE,
  population_col = "population_total",
  save_fig = TRUE,
  save_csv = TRUE,
  output_folder_path = output_base,
  create_run_subdir = FALSE
)

# Typical files created directly under output_base may include:
# - RR_lag_estimates.csv
# - wildfire_health_monthly_estimates.csv
# - wildfire_health_yearly_estimates.csv
# - PDFs for RR and other plots
# - model_validation/vif_results.csv
# - model_validation/qaic_results.csv


# ------------------------------------------------------------------------------
# Example E: Save each run into a timestamped subdirectory
# ------------------------------------------------------------------------------
#
# Use this when you want to keep each run separate so a new run does not
# overwrite the previous one.
#
# Important:
# - create_run_subdir must be set to TRUE
# - output_folder_path must point to an existing parent directory
#
# Example output path created by the function:
# output_base/wildfires_analysis_20260310_154512/

timestamped_output_res <- climatehealth::wildfire_do_analysis(
  health_path = health_data_path,
  join_wildfire_data = FALSE,
  date_col = "date",
  region_col = "region",
  mean_temperature_col = "temp_mean",
  health_outcome_col = "deaths",
  pm_2_5_col = "pm25",
  calc_relative_risk_by_region = TRUE,
  population_col = "population_total",
  save_fig = TRUE,
  save_csv = TRUE,
  output_folder_path = output_base,
  create_run_subdir = TRUE
)


# ------------------------------------------------------------------------------
# Example F: Using external wildfire data joins
# ------------------------------------------------------------------------------
#
# If your health data do not already contain wildfire-related PM2.5 values,
# set join_wildfire_data = TRUE and provide:
# - ncdf_path: NetCDF path for wildfire-related PM2.5
# - shp_path: shapefile path for geography boundaries
# - shape_region_col: region column name in the shapefile
#
# Example:
#
# joined_data_res <- climatehealth::wildfire_do_analysis(
#   health_path = health_data_path,
#   join_wildfire_data = TRUE,
#   ncdf_path = "path-to-wildfire-pm25.nc",
#   shp_path = "path-to-shapefile.shp",
#   date_col = "date",
#   region_col = "region",
#   shape_region_col = "shape_region_name",
#   mean_temperature_col = "temp_mean",
#   health_outcome_col = "deaths",
#   population_col = "population_total",
#   calc_relative_risk_by_region = TRUE,
#   save_fig = TRUE,
#   save_csv = TRUE,
#   output_folder_path = output_base,
#   create_run_subdir = TRUE
# )


# ------------------------------------------------------------------------------
# Common workflow rules
# ------------------------------------------------------------------------------
#
# 1. If calc_relative_risk_by_region = FALSE:
#    - population data are not required
#
# 2. If calc_relative_risk_by_region = TRUE:
#    - population data are required
#    - use a `pop` column directly, or map another name through population_col
#
# 3. If create_run_subdir = FALSE:
#    - outputs go directly into output_folder_path
#
# 4. If create_run_subdir = TRUE:
#    - output_folder_path must be supplied
#    - the function creates a timestamped subdirectory for that run
#
# 5. If save_fig = FALSE and save_csv = FALSE:
#    - the analysis still runs, but no files are written to disk
