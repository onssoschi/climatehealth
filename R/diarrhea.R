#' Code for calculating Diarrhea disease cases attributable to extreme
#' precipitation and extreme temperature

#' Run Full Diarrhea-Climate Analysis Pipeline
#'
#' @description
#' The `diarrhea_do_analysis` function runs the complete analysis workflow
#' by combining multiple functions to analyze the association between diarrhea
#' cases and climate variables. It processes health, climate, and spatial data,
#' fits models, generates plots, and calculates attributable risk.
#'
#' @param health_data_path Data frame containing the processed health data.
#' @param climate_data_path Data frame containing the processed climate data.
#' @param map_path Data frame containing the spatial map data (shapefile or equivalent).
#' @param region_col Character. Name of the column containing region names.
#' @param district_col Character. Name of the column containing district names.
#' @param date_col Character. Name of the column containing the date. Defaults to NULL.
#' @param year_col Character. Name of the column containing the year.
#' @param month_col Character. Name of the column containing the month.
#' @param diarrhea_case_col Character. Name of the column containing diarrhea case counts.
#' @param tot_pop_col Character. Name of the column containing total population.
#' @param tmin_col Character. Name of the column containing minimum temperature.
#' @param tmean_col Character. Name of the column containing mean temperature.
#' @param tmax_col Character. Name of the column containing maximum temperature.
#' @param rainfall_col Character. Name of the column containing cumulative
#' monthly rainfall.
#' @param r_humidity_col Character. Name of the column containing relative humidity.
#' @param runoff_col Character. Name of the column containing monthly runoff
#' data.
#' @param geometry_col Character. Name of the geometry column in the shapefile
#' (usually "geometry").
#' @param spi_col Character. Name of the column containing the Standardized
#' Precipitation Index. Defaults to NULL.
#' @param max_lag Numeric. Maximum lag to consider in the model
#' (typically 2 to 4). Defaults to 2.
#' @param inla_param A character vector specifying the confounding exposures to
#' be included in the model. Possible values are "tmax","tmin", "rainfall",
#' "r_humidity", and "runoff".
#' @param basis_matrices_choices Character vector specifying basis matrix
#' parameters to include in the model (e.g., "tmax", "tmin", "rainfall",
#' "r_humidity", "spi").
#' @param param_term Character vector specifying the exposure variables of interest
#' (e.g., "tmax", "rainfall").
#' @param level Character. Spatial disaggregation level: "country", "region", or "district".
#' @param param_threshold Numeric. Threshold above which exposure is considered,
#' "attributable". Can take floats. Defaults to 1.
#' @param filter_year Integer. The year to filter to data to. Defaults to NULL.
#' @param family Character. The probability distribution for the response
#' variable. The user may also have thepossibility to choose "nbinomial" for a
#' negative binomial distribution. Defaults to "poisson".
#' @param config Boolean. Enable additional model configurations. Defaults to FALSE.
#' @param save_csv Boolean. If TRUE, saves the resultant datasets. Defaults to FALSE.
#' @param save_fig Boolean. If TRUE, saves the generated plots. Defaults to FALSE.
#' @param output_dir Character. The path to the directory where outputs
#' (e.g., plots, maps, datasets) should be saved.
#'
#' @return A list containing:
#' \itemize{
#'   \item Model output from INLA
#'   \item Monthly random effects plot
#'   \item Yearly random effects plot
#'   \item Contour plot
#'   \item Relative risk map
#'   \item Relative risk plot
#'   \item Attributable fraction and number summary
#' }
#'
#' @export
diarrhea_do_analysis <- function(health_data_path,
                                 climate_data_path,
                                 map_path,
                                 region_col,
                                 district_col,
                                 date_col= NULL,
                                 year_col,
                                 month_col,
                                 diarrhea_case_col,
                                 tot_pop_col,
                                 tmin_col,
                                 tmean_col,
                                 tmax_col,
                                 rainfall_col,
                                 r_humidity_col,
                                 runoff_col,
                                 geometry_col,
                                 spi_col = NULL,
                                 max_lag = 2,
                                 inla_param,
                                 basis_matrices_choices,
                                 param_term,
                                 level,
                                 param_threshold = 1,
                                 filter_year = NULL,
                                 family = "poisson",
                                 config = FALSE,
                                 save_csv = FALSE,
                                 save_fig = FALSE,
                                 output_dir = NULL){

  # Simple output validation
  if (is.null(output_dir) & (save_fig | save_csv)) {
    stop("'output_dir' must be provided if 'save_fig' or save_csv' are TRUE.")
  }
  check_file_exists(output_dir, TRUE)

  # level validation
  level <- tolower(level)
  acceptable_levels = c("country", "region", "district")
  if (!(level %in% acceptable_levels)) {
    stop(paste0("Level must be one of ", paste0(acceptable_levels, collapse=", ")))
  }

  # Input validation (IF makes API exception)
  if (is.character(health_data_path)) {
    check_file_exists(health_data_path, TRUE)
  }
  if (is.character(climate_data_path)) {
    check_file_exists(climate_data_path, TRUE)
  }
  check_file_exists(map_path, TRUE)#

  # Get combined data
  combined_data <- combine_health_climate_data(
    health_data_path,
    climate_data_path,
    map_path,
    region_col,
    district_col,
    date_col,
    year_col,
    month_col,
    diarrhea_case_col,
    "diarrhea",
    tot_pop_col,
    tmin_col,
    tmean_col,
    tmax_col,
    rainfall_col,
    r_humidity_col,
    geometry_col,
    runoff_col,
    NULL,
    spi_col,
    max_lag,
    output_dir
  )

  # Plot time series
  plot_diarrhea <- plot_health_climate_timeseries(
    combined_data$data,
    param_term= level,
    level = "country",
    case_type = "diarrhea",
    filter_year = filter_year,
    save_fig = save_fig,
    output_dir = output_dir
  )
  plot_tmax <- plot_health_climate_timeseries(
    combined_data$data,
    param_term= "tmax",
    level = level,
    case_type = "diarrhea",
    filter_year = filter_year,
    save_fig = save_fig,
    output_dir = output_dir
  )
  plot_rainfall <- plot_health_climate_timeseries(
    combined_data$data,
    param_term= "rainfall",
    level = level,
    case_type = "diarrhea",
    filter_year = filter_year,
    save_fig = save_fig,
    output_dir = output_dir
  )

  # Create base matrice
  basis <- set_cross_basis(combined_data$data, FALSE)

  # Check for multicolinearity
  if (save_csv) {
    VIF <- check_and_write_vif(
      data=combined_data$data,
      inla_param=inla_param,
      basis_matrices_choices=basis_matrices_choices,
      case_type="diarrhea",
      output_dir=output_dir
    )
  } else {
    VIF <- check_diseases_vif(
      data=combined_data$data,
      inla_param=inla_param,
      basis_matrices_choices=basis_matrices_choices,
      case_type="diarrhea"
    )
  }

  # Fit the model
  inla_result <- run_inla_models(
    combined_data=combined_data,
    basis_matrices_choices=basis_matrices_choices,
    inla_param=inla_param,
    case_type = "diarrhea",
    output_dir=output_dir,
    save_csv=save_csv,
    family=family,
    config=config
  )

  # Plot seasonality
  reff_plot_monthly <- plot_monthly_random_effects(
    combined_data=combined_data,
    model=inla_result$model,
    output_dir=output_dir,
    save_fig=save_fig
  )

  # Spatial random effect
  reff_plot_yearly <- plot_yearly_spatial_random_effect(
    combined_data=combined_data,
    model=inla_result$model,
    case_type="diarrhea",
    save_fig=save_fig,
    output_dir=output_dir
  )

  # Contour plots
  contour_plot <- contour_plot(
    data=combined_data$data,
    param_term=param_term,
    model=inla_result$model,
    level=level,
    filter_year=filter_year,
    case_type="diarrhea",
    save_fig=save_fig,
    output_dir=output_dir,
  )

  # Relative risk map plots
  rr_map_plot <- plot_rr_map(
    combined_data=combined_data,
    model=inla_result$model,
    param_term=param_term,
    level=level,
    filter_year=filter_year,
    case_type="diarrhea",
    output_dir=output_dir,
    save_fig=save_fig
  )

  # Relative risk plot
  rr_data <- plot_relative_risk(
    data=combined_data$data,
    model=inla_result$model,
    param_term=param_term,
    level=level,
    filter_year=filter_year,
    case_type="diarrhea",
    output_dir=output_dir,
    save_csv=save_csv,
    save_fig=save_fig
  )
  rr_plot <- rr_data[["plots"]]
  rr_df <- rr_data[["RR"]]

  # Attributable fractions and numbers
  attr_frac_num <- attribution_calculation(
    combined_data$data,
    param_term=param_term,
    model=inla_result$model,
    level=level,
    param_threshold=param_threshold,
    filter_year=filter_year,
    case_type="diarrhea",
    output_dir=output_dir,
    save_csv=save_csv
  )

  #AN, AF, and AR plot
  plot_AR_Num <- plot_attribution_metric(
    attr_data=attr_frac_num,
    level=level,
    metrics="AR_Number",
    filter_year=filter_year,
    param_term=param_term,
    case_type="diarrhea",
    save_fig=save_fig,
    output_dir=output_dir
  )

  plot_AR_Fr <- plot_attribution_metric(
    attr_data=attr_frac_num,
    level=level,
    metrics="AR_Fraction",
    filter_year=filter_year,
    param_term=param_term,
    case_type="diarrhea",
    save_fig=save_fig,
    output_dir=output_dir
  )

  plot_AR_per_100k <- plot_attribution_metric(
    attr_data=attr_frac_num,
    level=level,
    metrics="AR_per_100k",
    filter_year=filter_year,
    param_term=param_term,
    case_type="diarrhea",
    save_fig=save_fig,
    output_dir=output_dir
  )

  res <- list(
    plot_diarrhea = plot_diarrhea,
    plot_tmax = plot_tmax,
    plot_rainfall = plot_rainfall,
    VIF = VIF,
    inla_result = inla_result,
    reff_plot_monthly = reff_plot_monthly,
    reff_plot_yearly = reff_plot_yearly,
    contour_plot = contour_plot,
    rr_map_plot = rr_map_plot,
    rr_plot = rr_plot,
    rr_df = rr_df,
    attr_frac_num = attr_frac_num,
    plot_AR_Num = plot_AR_Num,
    plot_AR_Fr = plot_AR_Fr,
    plot_AR_per_100k = plot_AR_per_100k
  )

  return(res)
}
