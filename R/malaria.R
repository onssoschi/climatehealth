#' Code for calculating Malaria disease cases attributable to extreme rainfall
#' and extreme temperature

#' Run Full Malaria–Climate Analysis Pipeline
#'
#' @description
#' The `Malaria_do_analysis()` function executes the complete workflow for analyzing
#' the association between malaria cases and climate variables. It integrates
#' health, climate, and spatial data; fits spatio-temporal models using INLA;
#' and generates a suite of diagnostic and inferential outputs, including plots
#' and attributable risk estimates.
#'
#' @param health_data_path Character. Path to the processed health data file.
#' @param climate_data_path Character. Path to the processed climate data file.
#' @param map_path Character. Path to the spatial data file (e.g., shapefile).
#' @param region_col Character. Column name for the region variable.
#' @param district_col Character. Column name for the district variable.
#' @param date_col Character (optional). Column name for the date variable.
#' Defaults to `NULL`.
#' @param year_col Character. Column name for the year variable.
#' @param month_col Character. Column name for the month variable.
#' @param malaria_case_col Character. Column name for malaria case counts.
#' @param tot_pop_col Character. Column name for total population.
#' @param tmin_col Character. Column name for minimum temperature.
#' @param tmean_col Character. Column name for mean temperature.
#' @param tmax_col Character. Column name for maximum temperature.
#' @param rainfall_col Character. Column name for cumulative monthly rainfall.
#' @param r_humidity_col Character. Column name for relative humidity.
#' @param runoff_col Character. Column name for monthly runoff data.
#' @param geometry_col Character. Column name of the geometry column in the
#' shapefile (usually `"geometry"`).
#' @param spi_col Character (optional). Column name for the Standardized
#' Precipitation Index (SPI). Defaults to `NULL`.
#' @param ndvi_col Character (optional). Column name for the Normalized Difference
#' Vegetation Index (NDVI). Defaults to `NULL`.
#' @param max_lag Numeric. Maximum temporal lag to include in the distributed
#' lag model (e.g., `2`–`4`). Defaults to `4`.
#' @param basis_matrices_choices Character vector. Specifies which climate variables
#' to include in the basis matrix (e.g., `c("tmax", "rainfall", "r_humidity")`).
#' @param inla_param Character vector. Specifies exposure variables included in
#' the INLA model (e.g., `c("tmin", "rainfall", "r_humidity")`).
#' @param param_term Character or vector. Exposure variable(s) of primary interest
#' for relative risk and attribution (e.g., `"tmax"`, `"rainfall"`).
#' @param level Character. Spatial disaggregation level; must be one of
#' `"country"`, `"region"`, or `"district"`.
#' @param param_threshold Numeric. Threshold above which exposure is considered
#' “attributable.” Defaults to `1`.
#' @param filter_year Integer or vector (optional). Year(s) to filter the data by.
#' Defaults to `NULL`.
#' @param family Character. Probability distribution for the outcome variable.
#' Options include `"poisson"` (default) and `"nbinomial"` for a negative binomial model.
#' @param group_by_year Logical. Whether to group attributable metrics by year.
#' Defaults to `FALSE`.
#' @param config Logical. Whether to enable additional INLA model configurations.
#'  Defaults to `TRUE`.
#' @param save_csv Logical. If `TRUE`, saves intermediate datasets to CSV.
#' Defaults to `TRUE`.
#' @param save_model Logical. If `TRUE`, saves fitted INLA model results.
#' Defaults to `TRUE`.
#' @param save_fig Logical. If `TRUE`, saves generated plots. Defaults to `TRUE`.
#' @param output_dir Character. Directory where output files (plots, datasets, maps)
#' are saved. Defaults to `NULL`.
#'
#' @return A named list containing:
#' \itemize{
#'   \item `inla_result` – Fitted INLA model object and summaries.
#'   \item `VIF` – Variance Inflation Factor results for multicollinearity assessment.
#'   \item `rr_df` – Relative risk results dataset.
#'   \item `attr_frac_num` – Attributable risk summary table.
#' }
#'
#' @export
malaria_do_analysis <- function(
    health_data_path,
    climate_data_path,
    map_path,
    region_col,
    district_col,
    date_col = NULL,
    year_col,
    month_col,
    malaria_case_col,
    tot_pop_col,
    tmin_col,
    tmean_col,
    tmax_col,
    rainfall_col,
    r_humidity_col,
    runoff_col,
    geometry_col,
    spi_col = NULL,
    ndvi_col = NULL,
    max_lag = 4,
    basis_matrices_choices,
    inla_param,
    param_term,
    level,
    param_threshold = 1,
    filter_year = NULL,
    family = "poisson",
    group_by_year = FALSE,
    config = FALSE,
    save_csv = FALSE,
    save_model = FALSE,
    save_fig = FALSE,
    output_dir = NULL) {
  # Simple output validation
  if (is.null(output_dir) & (save_fig | save_csv)) {
    stop("'output_dir' must be provided is 'save_fig' or save_csv' are TRUE.")
  }
  if (!is.null(output_dir)) {
    # Check output dir exists
    check_file_exists(output_dir, TRUE)
    # Create a centralised output dir
    new_fpath <- file.path(
      output_dir,
      paste0("malaria_analysis_", format(Sys.time(), "%d_%m_%Y_%H_%M"))
    )
    if (!is.null(new_fpath)) {
      (
        dir.create(new_fpath)
      )
    }
    output_dir <- new_fpath
  }

  # level validation
  level <- tolower(level)
  acceptable_levels <- c("country", "region", "district")
  if (!(level %in% acceptable_levels)) {
    stop(paste0(
      "Level must be one of ",
      paste0(acceptable_levels, collapse = ", ")
    ))
  }

  # Input validation (IF makes API exception)
  if (is.character(health_data_path)) {
    check_file_exists(health_data_path, TRUE)
  }
  if (is.character(climate_data_path)) {
    check_file_exists(climate_data_path, TRUE)
  }
  check_file_exists(map_path, TRUE)

  # Get combined data
  combined_data <- combine_health_climate_data(
    health_data_path = health_data_path,
    climate_data_path = climate_data_path,
    map_path = map_path,
    region_col = region_col,
    district_col = district_col,
    date_col = date_col,
    year_col = year_col,
    month_col = month_col,
    case_col = malaria_case_col,
    case_type = "malaria",
    tot_pop_col = tot_pop_col,
    tmin_col = tmin_col,
    tmean_col = tmean_col,
    tmax_col = tmax_col,
    rainfall_col = rainfall_col,
    r_humidity_col = r_humidity_col,
    geometry_col = geometry_col,
    runoff_col = runoff_col,
    ndvi_col = ndvi_col,
    spi_col = spi_col,
    max_lag = max_lag,
    output_dir = output_dir
  )
  # Plot time series
  if (level == "country") {
    plot_health_climate_timeseries(
      combined_data$data,
      param_term = "malaria",
      level = "country",
      case_type = "malaria",
      filter_year = filter_year,
      save_fig = save_fig,
      output_dir = output_dir
    )
    plot_health_climate_timeseries(
      combined_data$data,
      param_term = "tmax",
      level = "country",
      case_type = "malaria",
      filter_year = filter_year,
      save_fig = save_fig,
      output_dir = output_dir
    )
    plot_health_climate_timeseries(
      combined_data$data,
      param_term = "rainfall",
      level = "country",
      case_type = "malaria",
      filter_year = filter_year,
      save_fig = save_fig,
      output_dir = output_dir
    )
  }

  # Check for multicolinearity
  if (save_csv) {
    VIF <- check_and_write_vif(
      data = combined_data$data,
      inla_param = inla_param,
      max_lag = max_lag,
      basis_matrices_choices = basis_matrices_choices,
      case_type = "malaria",
      output_dir = output_dir
    )
  } else {
    VIF <- check_diseases_vif(
      data = combined_data$data,
      inla_param = inla_param,
      max_lag = max_lag,
      basis_matrices_choices = basis_matrices_choices,
      case_type = "malaria"
    )
  }

  # Fitting the model
  inla_result <- run_inla_models(
    combined_data = combined_data,
    basis_matrices_choices = basis_matrices_choices,
    inla_param = inla_param,
    case_type = "malaria",
    max_lag = max_lag,
    output_dir = output_dir,
    save_model = save_model,
    family = family,
    config = config
  )

  # Plot seasonality
  plot_monthly_random_effects(
    combined_data,
    model = inla_result$model,
    output_dir = output_dir,
    save_fig = save_fig
  )

  # Spatial random effect
  plot_yearly_spatial_random_effect(
    combined_data = combined_data,
    model = inla_result$model,
    case_type = "malaria",
    save_fig = save_fig,
    output_dir = output_dir
  )
  # Contour plots
  contour_plot(
    data = combined_data$data,
    param_term = param_term,
    max_lag = max_lag,
    model = inla_result$model,
    level = level,
    filter_year = filter_year,
    case_type = "malaria",
    save_fig = save_fig,
    output_dir = output_dir
  )

  # Relative risk map plots
  plot_rr_map(
    combined_data = combined_data,
    model = inla_result$model,
    param_term = param_term,
    max_lag = max_lag,
    level = level,
    filter_year = filter_year,
    case_type = "malaria",
    output_dir = output_dir,
    save_fig = save_fig
  )

  # Relative risk plot
  rr_data <- plot_relative_risk(
    data = combined_data$data,
    model = inla_result$model,
    param_term = param_term,
    max_lag = max_lag,
    level = level,
    filter_year = filter_year,
    case_type = "malaria",
    output_dir = output_dir,
    save_csv = save_csv,
    save_fig = save_fig
  )
  rr_df <- rr_data[["RR"]]

  # attribution fraction and number
  attr_frac_num <- attribution_calculation(combined_data$data,
    param_term = param_term,
    model = inla_result$model,
    param_threshold = param_threshold,
    max_lag = max_lag,
    level = level,
    case_type = "malaria",
    filter_year = filter_year,
    group_by_year = group_by_year,
    save_csv = save_csv,
    output_dir = output_dir
  )
  # Attributable number plots
  plot_attribution_metric(
    attr_data = attr_frac_num,
    param_term = param_term,
    level = level,
    metrics = "AR_Number",
    case_type = "malaria",
    filter_year = filter_year,
    save_fig = save_fig,
    output_dir = output_dir
  )
  # Attributable fraction plots
  plot_attribution_metric(
    attr_data = attr_frac_num,
    param_term = param_term,
    level = level,
    metrics = "AR_Fraction",
    case_type = "malaria",
    filter_year = filter_year,
    save_fig = save_fig,
    output_dir = output_dir
  )
  # Attributable rate plots
  plot_attribution_metric(
    attr_data = attr_frac_num,
    param_term = param_term,
    level = level,
    filter_year = filter_year,
    metrics = "AR_per_100k",
    case_type = "malaria",
    save_fig = save_fig,
    output_dir = output_dir
  )
  # structure and return results
  res <- list(
    inla_result = inla_result,
    VIF = VIF,
    rr_df = rr_df,
    an_ar_results = attr_frac_num
  )
  return(res)
}
