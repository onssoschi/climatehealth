# Descriptive statistics module usage examples
#
# This script shows how to use the updated descriptive statistics module:
# 1) Directly from the package (run_descriptive_stats)
# 2) Via the API-style wrapper (run_descriptive_stats_api)
#
# Assumes climatehealth is installed. If not, see optional install/load block.

# ------------------------------------------------------------------------------
# Optional install/load (if needed)
# ------------------------------------------------------------------------------
# install.packages("devtools")
# devtools::install_github("onssoschi/climatehealth")
# library(climatehealth)


# ------------------------------------------------------------------------------
# Input data used in these examples
# ------------------------------------------------------------------------------

data_path <- "path-to-your-csv-file"
output_base <- "path-to-folder-where-you-want-outputs: e.g. data/outputs"

# Example based on my local path setup
# data_path <- "D:/soschi/data/synthetic_suicide_climate_data_1.csv"
# output_base <- "D:/soschi/data/outputs"

# Ensure output directory exists (or set create_base_dir=TRUE below)
if (!dir.exists(output_base)) {
  dir.create(output_base, recursive = TRUE, showWarnings = FALSE)
}


# ------------------------------------------------------------------------------
# Example A: Run descriptive stats directly from the package
# ------------------------------------------------------------------------------

# Alternative import:
#
# library(climatehealth)
# run_descriptive_stats(...)

# Minimal direct call (single dataframe, minimal plots)
df <- read.csv(data_path)

direct_min_res <- climatehealth::run_descriptive_stats(
  data = df,
  output_path = output_base,
  aggregation_column = "region",
  dependent_col = "suicides",
  independent_cols = c("tmean", "hum", "sun", "rainfall"),
  plot_corr_matrix = TRUE,
  plot_dist = TRUE,
  plot_ma = FALSE,
  plot_na_counts = TRUE,
  plot_scatter = TRUE,
  plot_box = TRUE,
  plot_seasonal = FALSE,
  plot_regional = FALSE,
  plot_total = FALSE,
  detect_outliers = FALSE,
  calculate_rate = FALSE,
  run_id = NULL,
  create_base_dir = TRUE
)

# Returned object fields
# direct_min_res$base_output_path
# direct_min_res$run_id
# direct_min_res$run_output_path
# direct_min_res$region_output_paths


# Advanced direct call (single dataframe, all major outputs enabled)
direct_adv_res <- climatehealth::run_descriptive_stats(
  data = df,
  output_path = output_base,
  aggregation_column = "region",
  population_col = "population",
  dependent_col = dependent_col,
  independent_cols = c("tmean", "hum", "sun", "rainfall"),
  units = c(
    suicides = "count",
    tmean = "C",
    hum = "%",
    sun = "hours",
    rainfall = "mm",
    population = "people"
  ),
  plot_corr_matrix = TRUE,
  correlation_method = "pearson",
  plot_dist = TRUE,
  plot_ma = TRUE,
  ma_days = 30,
  ma_sides = 1,
  timeseries_col = "date",
  plot_na_counts = TRUE,
  plot_scatter = TRUE,
  plot_box = TRUE,
  plot_seasonal = TRUE,
  plot_regional = TRUE,
  plot_total = TRUE,
  detect_outliers = TRUE,
  calculate_rate = TRUE,
  run_id = NULL,
  create_base_dir = TRUE
)


# Optional: pass a list of dataframes directly
df_by_region <- split(df, df$region)
direct_list_res <- climatehealth::run_descriptive_stats(
  data = df_by_region,
  output_path = output_base,
  aggregation_column = "region",
  dependent_col = dependent_col,
  independent_cols = c("tmean", "hum"),
  plot_corr_matrix = TRUE,
  plot_dist = TRUE,
  plot_ma = FALSE,
  create_base_dir = TRUE
)


# ------------------------------------------------------------------------------
# Example B: Run descriptive stats via API-style wrapper
# ------------------------------------------------------------------------------

# This wrapper is designed for API/plumber-friendly inputs.
# It accepts:
# - a CSV path string, or
# - a list object that can be converted to a data.frame

# Minimal API call (CSV path input)
api_min_res <- climatehealth::run_descriptive_stats_api(
  data = data_path,
  output_path = output_base,
  aggregation_column = "region",
  dependent_col = "suicides", #specify exact outcome variable in your dataset
  independent_cols = c("tmean", "hum", "sun", "rainfall"),
  plot_corr_matrix = TRUE,
  plot_dist = TRUE,
  plot_ma = FALSE,
  plot_na_counts = TRUE,
  plot_scatter = TRUE,
  plot_box = TRUE,
  plot_seasonal = FALSE,
  plot_regional = FALSE,
  plot_total = FALSE,
  detect_outliers = FALSE,
  calculate_rate = FALSE,
  create_base_dir = TRUE
)


# Advanced API call (list payload input)
df_payload <- as.list(df)
api_adv_res <- climatehealth::run_descriptive_stats_api(
  data = df_payload,
  output_path = output_base,
  aggregation_column = "region",
  population_col = "population",
  dependent_col = "suicides", #specify exact outcome variable in your dataset
  independent_cols = c("tmean", "hum", "sun", "rainfall"),
  units = c(
    suicides = "count",
    tmean = "C",
    hum = "%",
    sun = "hours",
    rainfall = "mm",
    population = "people"
  ),
  plot_corr_matrix = TRUE,
  correlation_method = "pearson",
  plot_dist = TRUE,
  plot_ma = TRUE,
  ma_days = 30,
  ma_sides = 1,
  timeseries_col = "date",
  plot_na_counts = TRUE,
  plot_scatter = TRUE,
  plot_box = TRUE,
  plot_seasonal = TRUE,
  plot_regional = TRUE,
  plot_total = TRUE,
  detect_outliers = TRUE,
  calculate_rate = TRUE,
  run_id = NULL,
  create_base_dir = TRUE
)


# ------------------------------------------------------------------------------
# Example C: Access through a plumber endpoint
# ------------------------------------------------------------------------------
# Assumes your plumber route maps request fields to run_descriptive_stats_api()
# arguments (same field names as used below).

library(httr2)

base_url <- "http://localhost:8000"
endpoint_path <- "/run_descriptive_stats"
endpoint_url <- paste0(base_url, endpoint_path)

# Minimal plumber payload
plumber_min_payload <- list(
  data = df_payload,
  output_path = output_base,
  aggregation_column = "region",
  dependent_col = "suicides", #specify exact outcome variable in your dataset
  independent_cols = c("tmean", "hum", "sun", "rainfall"),
  plot_corr_matrix = TRUE,
  plot_dist = TRUE,
  plot_ma = FALSE,
  plot_na_counts = TRUE,
  plot_scatter = TRUE,
  plot_box = TRUE,
  plot_seasonal = FALSE,
  plot_regional = FALSE,
  plot_total = FALSE,
  detect_outliers = FALSE,
  calculate_rate = FALSE,
  create_base_dir = TRUE
)

plumber_min_res <- request(endpoint_url) |>
  req_method("POST") |>
  req_body_json(plumber_min_payload) |>
  req_perform() |>
  resp_body_json()

# Advanced plumber payload
plumber_adv_payload <- list(
  data = df_payload,
  output_path = output_base,
  aggregation_column = "region",
  population_col = "population",
  dependent_col = "suicides", #specify exact outcome variable in your dataset
  independent_cols = c("tmean", "hum", "sun", "rainfall"),
  units = c(
    suicides = "count",
    tmean = "C",
    hum = "%",
    sun = "hours",
    rainfall = "mm",
    population = "people"
  ),
  plot_corr_matrix = TRUE,
  correlation_method = "pearson",
  plot_dist = TRUE,
  plot_ma = TRUE,
  ma_days = 30,
  ma_sides = 1,
  timeseries_col = "date",
  plot_na_counts = TRUE,
  plot_scatter = TRUE,
  plot_box = TRUE,
  plot_seasonal = TRUE,
  plot_regional = TRUE,
  plot_total = TRUE,
  detect_outliers = TRUE,
  calculate_rate = TRUE,
  run_id = NULL,
  create_base_dir = TRUE
)

plumber_adv_res <- request(endpoint_url) |>
  req_method("POST") |>
  req_body_json(plumber_adv_payload) |>
  req_perform() |>
  resp_body_json()
