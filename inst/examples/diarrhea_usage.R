# Diarrhea indicator usage examples
#
# This script shows how to use climatehealth::diarrhea_do_analysis().
#
# It covers:
# 1) A standard direct run
# 2) A run with optional SPI / NDVI covariates
# 3) A run that saves outputs to disk
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
# This workflow expects three main inputs:
# - processed health data
# - processed climate data
# - a spatial file such as a shapefile

health_data_path <- "path-to-your-diarrhea-health-data"
climate_data_path <- "path-to-your-diarrhea-climate-data"
map_path <- "path-to-your-spatial-file"
output_dir <- "path-to-folder-where-you-want-outputs"

# Example local paths
# health_data_path <- "D:/soschi/data/diarrhea_health.csv"
# climate_data_path <- "D:/soschi/data/diarrhea_climate.csv"
# map_path <- "D:/soschi/data/shapes/diarrhea_map.shp"
# output_dir <- "D:/soschi/data/diarrhea_outputs"

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
}


# ------------------------------------------------------------------------------
# Expected column mapping
# ------------------------------------------------------------------------------
#
# The workflow requires you to map the relevant columns from the health dataset,
# climate dataset, and spatial layer. Common examples include:
# - region and district identifiers
# - year and month
# - case count
# - total population
# - temperature and rainfall variables
# - a geometry column in the spatial data


# ------------------------------------------------------------------------------
# Example A: Standard run
# ------------------------------------------------------------------------------

diarrhea_basic_res <- climatehealth::diarrhea_do_analysis(
  health_data_path = health_data_path,
  climate_data_path = climate_data_path,
  map_path = map_path,
  region_col = "region",
  district_col = "district",
  date_col = NULL,
  year_col = "year",
  month_col = "month",
  case_col = "diarrhea_cases",
  case_type = "diarrhea",
  tot_pop_col = "tot_pop",
  tmin_col = "tmin",
  tmean_col = "tmean",
  tmax_col = "tmax",
  rainfall_col = "rainfall",
  r_humidity_col = "r_humidity",
  runoff_col = "runoff",
  geometry_col = "geometry",
  max_lag = 2,
  nk = 2,
  basis_matrices_choices = c("tmax", "rainfall", "r_humidity"),
  inla_param = c("tmax", "rainfall"),
  param_term = "rainfall",
  level = "district",
  save_fig = FALSE,
  save_csv = FALSE,
  save_model = FALSE
)


# ------------------------------------------------------------------------------
# Example B: Include optional SPI and NDVI inputs
# ------------------------------------------------------------------------------

diarrhea_extended_res <- climatehealth::diarrhea_do_analysis(
  health_data_path = health_data_path,
  climate_data_path = climate_data_path,
  map_path = map_path,
  region_col = "region",
  district_col = "district",
  date_col = NULL,
  year_col = "year",
  month_col = "month",
  case_col = "diarrhea_cases",
  case_type = "diarrhea_under_five",
  tot_pop_col = "tot_pop",
  tmin_col = "tmin",
  tmean_col = "tmean",
  tmax_col = "tmax",
  rainfall_col = "rainfall",
  r_humidity_col = "r_humidity",
  runoff_col = "runoff",
  geometry_col = "geometry",
  spi_col = "spi",
  ndvi_col = "ndvi",
  max_lag = 2,
  nk = 2,
  basis_matrices_choices = c("tmax", "rainfall", "r_humidity", "spi"),
  inla_param = c("tmax", "rainfall", "spi"),
  param_term = "rainfall",
  level = "district",
  save_fig = FALSE,
  save_csv = FALSE,
  save_model = FALSE
)


# ------------------------------------------------------------------------------
# Example C: Save outputs to disk
# ------------------------------------------------------------------------------
#
# Note: this workflow creates a timestamped subdirectory inside output_dir.

diarrhea_saved_res <- climatehealth::diarrhea_do_analysis(
  health_data_path = health_data_path,
  climate_data_path = climate_data_path,
  map_path = map_path,
  region_col = "region",
  district_col = "district",
  date_col = NULL,
  year_col = "year",
  month_col = "month",
  case_col = "diarrhea_cases",
  case_type = "diarrhea",
  tot_pop_col = "tot_pop",
  tmin_col = "tmin",
  tmean_col = "tmean",
  tmax_col = "tmax",
  rainfall_col = "rainfall",
  r_humidity_col = "r_humidity",
  runoff_col = "runoff",
  geometry_col = "geometry",
  max_lag = 2,
  nk = 2,
  basis_matrices_choices = c("tmax", "rainfall"),
  inla_param = c("tmax", "rainfall"),
  param_term = "rainfall",
  level = "district",
  save_fig = TRUE,
  save_csv = TRUE,
  save_model = TRUE,
  output_dir = output_dir
)


# ------------------------------------------------------------------------------
# Common workflow rules
# ------------------------------------------------------------------------------
#
# 1. This workflow currently requires case_type at the exported interface.
#
# 2. level must be one of:
#    - "country"
#    - "region"
#    - "district"
#
# 3. save_fig = TRUE or save_csv = TRUE require output_dir.
#
# 4. The function creates a timestamped output folder under output_dir.
