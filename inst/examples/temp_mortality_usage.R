# Temperature mortality indicator usage examples
#
# This script shows how to use climatehealth::temp_mortality_do_analysis().
#
# It covers:
# 1) A basic single-dataset run
# 2) A run with additional independent and control variables
# 3) A meta-analysis run across regions
# 4) Saving figures and CSV outputs
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

data_path <- "path-to-your-temperature-mortality-csv"
output_dir <- "path-to-folder-where-you-want-outputs"

# Example local paths
# data_path <- "D:/soschi/data/temp_mortality_data.csv"
# output_dir <- "D:/soschi/data/temp_mortality_outputs"

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
}


# ------------------------------------------------------------------------------
# Expected input columns
# ------------------------------------------------------------------------------
#
# The input dataset should contain, at minimum:
# - a date column
# - a region column
# - a temperature exposure column
# - a dependent health outcome column
# - a population column
#
# You can optionally include other variables and pass them through:
# - independent_cols
# - control_cols


# ------------------------------------------------------------------------------
# Example A: Basic run
# ------------------------------------------------------------------------------

temp_basic_res <- climatehealth::temp_mortality_do_analysis(
  data_path = data_path,
  date_col = "date",
  region_col = "region",
  temperature_col = "tmean",
  dependent_col = "deaths",
  population_col = "population",
  country = "National",
  meta_analysis = FALSE,
  save_fig = FALSE,
  save_csv = FALSE
)

# Useful returned object fields may include:
# temp_basic_res$qaic_results
# temp_basic_res$vif_results
# temp_basic_res$rr_results
# temp_basic_res$attr_high_results
# temp_basic_res$attr_low_results


# ------------------------------------------------------------------------------
# Example B: Add independent and control variables
# ------------------------------------------------------------------------------
#
# Use independent_cols and control_cols when you want to adjust for additional
# explanatory variables present in the input dataset.

temp_covariate_res <- climatehealth::temp_mortality_do_analysis(
  data_path = data_path,
  date_col = "date",
  region_col = "region",
  temperature_col = "tmean",
  dependent_col = "deaths",
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
#
# Set meta_analysis = TRUE when the dataset contains multiple regions and you
# want pooled meta-analytic outputs.

temp_meta_res <- climatehealth::temp_mortality_do_analysis(
  data_path = data_path,
  date_col = "date",
  region_col = "region",
  temperature_col = "tmean",
  dependent_col = "deaths",
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

temp_saved_res <- climatehealth::temp_mortality_do_analysis(
  data_path = data_path,
  date_col = "date",
  region_col = "region",
  temperature_col = "tmean",
  dependent_col = "deaths",
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
# 1. population_col is always required for this workflow.
#
# 2. Set meta_analysis = TRUE if you want pooled outputs across regions.
#
# 3. save_fig = TRUE and save_csv = TRUE require output_folder_path.
#
# 4. The function creates a timestamped output folder under output_folder_path.
