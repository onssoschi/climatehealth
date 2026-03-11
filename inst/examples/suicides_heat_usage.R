# Suicides heat indicator usage examples
#
# This script shows how to use climatehealth::suicides_heat_do_analysis().
#
# It covers:
# 1) A basic run
# 2) A run with extra independent and control variables
# 3) A meta-analysis run across regions
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

data_path <- "path-to-your-suicides-heat-csv"
output_dir <- "path-to-folder-where-you-want-outputs"

# Example local paths
# data_path <- "D:/soschi/data/suicides_heat_data.csv"
# output_dir <- "D:/soschi/data/suicides_heat_outputs"

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
}


# ------------------------------------------------------------------------------
# Expected input columns
# ------------------------------------------------------------------------------
#
# The input dataset should contain, at minimum:
# - a date column
# - a region column (or allow region_col = NULL if not regionally split)
# - a temperature exposure column
# - a health outcome column
# - a population column


# ------------------------------------------------------------------------------
# Example A: Basic run
# ------------------------------------------------------------------------------

suicides_basic_res <- climatehealth::suicides_heat_do_analysis(
  data_path = data_path,
  date_col = "date",
  region_col = "region",
  temperature_col = "tmean",
  health_outcome_col = "suicides",
  population_col = "population",
  country = "National",
  meta_analysis = FALSE,
  save_fig = FALSE,
  save_csv = FALSE
)


# ------------------------------------------------------------------------------
# Example B: Add independent and control variables
# ------------------------------------------------------------------------------

suicides_covariate_res <- climatehealth::suicides_heat_do_analysis(
  data_path = data_path,
  date_col = "date",
  region_col = "region",
  temperature_col = "tmean",
  health_outcome_col = "suicides",
  population_col = "population",
  independent_cols = c("humidity", "rainfall"),
  control_cols = c("dow", "holiday_flag"),
  meta_analysis = FALSE,
  save_fig = FALSE,
  save_csv = FALSE
)


# ------------------------------------------------------------------------------
# Example C: Meta-analysis across regions
# ------------------------------------------------------------------------------

suicides_meta_res <- climatehealth::suicides_heat_do_analysis(
  data_path = data_path,
  date_col = "date",
  region_col = "region",
  temperature_col = "tmean",
  health_outcome_col = "suicides",
  population_col = "population",
  country = "National",
  meta_analysis = TRUE,
  save_fig = FALSE,
  save_csv = FALSE
)


# ------------------------------------------------------------------------------
# Example D: Save outputs to disk
# ------------------------------------------------------------------------------
#
# Note: this workflow creates a timestamped subdirectory inside output_folder_path.

suicides_saved_res <- climatehealth::suicides_heat_do_analysis(
  data_path = data_path,
  date_col = "date",
  region_col = "region",
  temperature_col = "tmean",
  health_outcome_col = "suicides",
  population_col = "population",
  country = "National",
  meta_analysis = TRUE,
  save_fig = TRUE,
  save_csv = TRUE,
  output_folder_path = output_dir
)


# ------------------------------------------------------------------------------
# Common workflow rules
# ------------------------------------------------------------------------------
#
# 1. population_col is required for this workflow.
#
# 2. Set meta_analysis = TRUE to pool estimates across regions.
#
# 3. save_fig = TRUE and save_csv = TRUE require output_folder_path.
#
# 4. The function creates a timestamped output folder under output_folder_path.
