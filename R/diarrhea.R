#' Code for calculating Diarrhea disease cases attributable to extreme
#' precipitation and extreme temperature

#' Run Full diarrhea-Climate Analysis Pipeline
#'
#' @description
#' The `diarrhea_do_analysis` function runs the complete analysis workflow
#' by combining multiple functions to analyze the association between diarrhea
#' cases and climate variables. It processes health, climate, and spatial data,
#' fits models, generates plots, and calculates attributable risk.
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
#' @param case_col Character. Column name for diarrhea case counts.
#' @param case_type Character. Type of diarrhea cases (e.g., `"diarrhea"`,
#' `"diarrhea_under_five"`).
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
#' @param nk Numeric. Number of internal knots for the natural spline of
#' each predictor, controlling its flexibility: \code{nk = 0} produces a linear
#' effect with one basis column, \code{nk = 1} generates a simple spline with two
#' columns, \code{nk = 2} yields a more flexible spline with three columns,
#' and higher values of \code{nk} further increase flexibility but may also
#' raise collinearity among spline terms. Defaults to 1.
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
                                 case_col,
                                 case_type,
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
                                 max_lag = 2,
                                 nk=2,
                                 basis_matrices_choices,
                                 inla_param,
                                 param_term,
                                 level,
                                 param_threshold = 1,
                                 filter_year = NULL,
                                 family = "poisson",
                                 group_by_year = FALSE,
                                 config = TRUE,
                                 save_csv = TRUE,
                                 save_model=TRUE,
                                 save_fig = TRUE,
                                 cumulative = FALSE,
                                 output_dir =NULL){

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

  # get combined data
  combined_data <- combine_health_climate_data(health_data_path,
                                               climate_data_path,
                                               map_path,
                                               region_col,
                                               district_col,
                                               date_col,
                                               year_col,
                                               month_col,
                                               case_col,
                                               case_type,
                                               tot_pop_col,
                                               tmin_col,
                                               tmean_col,
                                               tmax_col,
                                               rainfall_col,
                                               r_humidity_col,
                                               geometry_col,
                                               runoff_col,
                                               ndvi_col,
                                               spi_col,
                                               max_lag,
                                               output_dir)
  # Plot time series
  plot_diarrhea <- NULL
  plot_tmax <- NULL
  plot_rainfall <- NULL
  plot_rhumidity <-NULL
  if (level=="country") {
    plot_diarrhea <- plot_health_climate_timeseries(
      combined_data$data,
      param_term = "diarrhea",
      level = "country",
      case_type = "diarrhea",
      filter_year = filter_year,
      save_fig = save_fig,
      output_dir = output_dir
    )
    plot_tmax <- plot_health_climate_timeseries(
      combined_data$data,
      param_term = "tmax",
      level = "country",
      case_type = "diarrhea",
      filter_year = filter_year,
      save_fig = save_fig,
      output_dir = output_dir
    )
    plot_rainfall <- plot_health_climate_timeseries(
      combined_data$data,
      param_term = "rainfall",
      level = "country",
      case_type = "diarrhea",
      filter_year = filter_year,
      save_fig = save_fig,
      output_dir = output_dir
    )
    plot_rhumidity <- plot_health_climate_timeseries(
      combined_data$data,
      param_term = "r_humidity",
      level = "country",
      case_type = "diarrhea",
      filter_year = filter_year,
      save_fig = save_fig,
      output_dir = output_dir
    )
  }
  # create base matrice
  basis <- set_cross_basis(combined_data$data, case_type, max_lag, nk)

  # Check for multicolinearity
  if (save_csv) {
    VIF <- check_and_write_vif(
      data=combined_data$data,
      inla_param=inla_param,
      max_lag=max_lag,
      basis_matrices_choices=basis_matrices_choices,
      case_type="diarrhea",
      output_dir=output_dir
    )
  } else {
    VIF <- check_diseases_vif(
      data=combined_data$data,
      inla_param=inla_param,
      max_lag=max_lag,
      basis_matrices_choices=basis_matrices_choices,
      case_type="diarrhea"
    )
  }

  # Fitting the model
  inla_result <- run_inla_models(
    combined_data=combined_data,
    basis_matrices_choices=basis_matrices_choices,
    inla_param=inla_param,
    max_lag=max_lag,
    case_type = "diarrhea",
    output_dir=output_dir,
    save_model=save_model,
    family=family,
    config=config
  )

  # Plot seasonality
  reff_plot_monthly <- plot_monthly_random_effects(
    combined_data,
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
  contour_plot_diarrhea <- contour_plot(
    data=combined_data$data,
    param_term=param_term,
    max_lag=max_lag,
    model=inla_result$model,
    level=level,
    filter_year=filter_year,
    case_type="diarrhea",
    save_fig=save_fig,
    output_dir=output_dir
  )

  # Relative risk map plots
  rr_map_plot <- plot_rr_map(
    combined_data=combined_data,
    model=inla_result$model,
    param_term=param_term,
    max_lag=max_lag,
    level="district",
    filter_year=filter_year,
    case_type="diarrhea",
    output_dir=output_dir,
    save_csv =save_csv,
    save_fig=save_fig,
    cumulative = FALSE
  )
  # Cumulative risk map
  cum_rr_map <- plot_rr_map(
    combined_data = combined_data,
    model = inla_result$model,
    param_term = param_term,
    max_lag=max_lag,
    level = "region",
    case_type = case_type,
    output_dir = output_dir,
    save_fig = TRUE,
    save_csv = TRUE,
    cumulative = TRUE
  )

  # Relative risk plot
  rr_data <- plot_relative_risk(
    data=combined_data$data,
    model=inla_result$model,
    param_term=param_term,
    max_lag=max_lag,
    level=level,
    filter_year=filter_year,
    case_type="diarrhea",
    output_dir=output_dir,
    save_csv=save_csv,
    save_fig=save_fig
  )
  rr_plot <- rr_data[["plots"]]
  rr_df <- rr_data[["RR"]]

  # attribution fraction and number
  attr_frac_num <- attribution_calculation(
    combined_data$data,
    param_term=param_term,
    model=inla_result$model,
    param_threshold=param_threshold,
    max_lag=max_lag,
    level= level,
    case_type="diarrhea",
    filter_year=filter_year,
    group_by_year = group_by_year,
    save_csv=save_csv,
    output_dir=output_dir)

  plot_AR_Num <-plot_attribution_metric(attr_data = attr_frac_num,
                                        param_term=param_term,
                                        level= level,
                                        metrics = "AR_Number",
                                        case_type="diarrhea",
                                        filter_year = filter_year,
                                        save_fig =save_fig,
                                        output_dir = output_dir)

  plot_AR_Fr <-plot_attribution_metric(attr_data = attr_frac_num,
                                       param_term=param_term,
                                       level= level,
                                       metrics = "AR_Fraction",
                                       case_type="diarrhea",
                                       filter_year = filter_year,
                                       save_fig =save_fig,
                                       output_dir = output_dir)

  plot_AR_per_100k <-plot_attribution_metric(attr_data = attr_frac_num,
                                             param_term=param_term,
                                             level= level,
                                             filter_year = filter_year,
                                             metrics = "AR_per_100k",
                                             case_type="diarrhea",
                                             save_fig =save_fig,
                                             output_dir = output_dir)
  # Average monthly attribution plot
  plot_avg_AR_Num<-plot_avg_monthly(attr_data = attr_frac_num,
                                    level = level,
                                    metric = "AR_Number",
                                    data = combined_data$data,
                                    param_term = param_term,
                                    save_fig = TRUE,
                                    output_dir = output_dir )

  plot_avg_AR_per_100k<-plot_avg_monthly(attr_data = attr_frac_num,
                                         level = level,
                                         metric = "AR_per_100k",
                                         data = combined_data$data,
                                         param_term = param_term,
                                         save_fig = TRUE,
                                         output_dir = output_dir )

  res <- list(plot_diarrhea = plot_diarrhea,
              plot_tmax = plot_tmax,
              plot_rainfall = plot_rainfall,
              inla_result = inla_result,
              reff_plot_monthly = reff_plot_monthly,
              reff_plot_yearly = reff_plot_yearly,
              contour_plot = contour_plot_diarrhea,
              rr_map_plot = rr_map_plot,
              cum_rr_map=cum_rr_map,
              rr_plot = rr_plot,
              rr_df = rr_df,
              attr_frac_num = attr_frac_num,
              plot_AR_num = plot_AR_Num,
              plot_AR_frac = plot_AR_Fr,
              plot_AR_per_100k = plot_AR_per_100k,
              plot_avg_AR_Num = plot_avg_AR_Num,
              plot_avg_AR_per_100k = plot_avg_AR_per_100k)

  return(res)
}
