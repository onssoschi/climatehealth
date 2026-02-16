# Shared functions across the Diarrhea and Malaria indicators

#' Ensure that the `case_type` parameter is valid
#'
#' @description Ensures that the case_type parameter is either malaria or
#' diarrhea to comply with supported indicators.
#'
#' @param case_type Character. The value of the case_type parameter.
#'
#' @return Character. The lower case_type.
#'
#' @keywords internal
validate_case_type <- function(case_type) {
  # Ensure case_type is an accepted type
  accepted_cases <- c("diarrhea", "malaria")
  case_type <- tolower(case_type)
  if (!(case_type %in% accepted_cases)) {
    stop("'case_type' must be one of ", paste0(accepted_cases, collapse=", "))
  }
  return(case_type)
}


#' Read in and format country map data
#'
#' @description: Read in a shape file, rename columns and create the
#' adjacency matrix for spatiotemporal analysis.
#'
#' @param map_path The path to the country's geographic data (shape file "sf" data).
#' @param region_col Character. The region column in the dataframe.
#' @param district_col Character. The district column in the dataframe.
#' @param geometry_col  Character. The geometry column in the dataframe.
#' @param output_dir  Character. The path to output the processed adjacency
#' (neighboring) matrix, and the map graph.
#'
#' @return
#' \itemize{
#'  \item 'map' The processed map
#'  \item 'nb.map'
#'  \item 'graph_file'
#'  }
#'
#' @keywords internal
load_and_process_map <- function(map_path,
                                 region_col,
                                 district_col,
                                 geometry_col,
                                 output_dir = NULL){
  # Load and process map
  map <- sf::read_sf(map_path) %>%
    select(region = !!sym(region_col),
           district = !!sym(district_col),
           geometry = !!sym(geometry_col)) %>%
    mutate(geometry = sf::st_make_valid(geometry))

  # Create adjacency matrix
  nb_file <- if (!is.null(output_dir)) file.path(output_dir, "nbfile") else NULL
  g_file <- if (!is.null(output_dir)) file.path(output_dir, "map.graph")else tempfile(pattern = "map", fileext = ".graph")

  if (!is.null(nb_file) && file.exists(nb_file)) {
    nb.map <- spdep::read.gal(nb_file)
  } else {
    nb.map <- spdep::poly2nb(sf::as_Spatial(map$geometry), snap = 1e-4)
    if (!is.null(nb_file)) spdep::write.nb.gal(nb.map, nb_file)
  }

  if (is.null(output_dir) || !file.exists(g_file)) {
    spdep::nb2INLA(g_file, nb.map)
  }

  return(list(map = map, nb.map = nb.map, graph_file = g_file))
}


#' Read in and format health data - diseases cases type
#'
#' @description Read in a csv file containing a monthly time series of health
#' outcomes and population data. Renames columns and creates time variables for
#' spatiotemporal analysis.
#'
#' @param health_data_path Path to a csv file containing a monthly time series of data
#' for health outcome case type, which may be disaggregated by sex (under five case or
#' above five case), and by Region and District.
#' @param region_col Character. Name of the column in the dataframe that contains
#' the region names.
#' @param district_col Character. Name of the column in the dataframe that
#' contains the district names.
#' @param date_col Character. Name of the column in the dataframe that contains
#' the date. Defaults to NULL.
#' @param year_col Character. Name of the column in the dataframe that contains
#' the year.
#' @param month_col Character. Name of the column in the dataframe that contains
#' the month.
#' @param case_col Character. Name of the column in the dataframe
#' that contains the disease cases to be considered.
#' @param case_type Character. The type of disease that the case column refers
#' to. Must be one of 'diarrhea' or 'malaria'.
#' @param tot_pop_col Character. Name of the column in the dataframe that contains
#' the total population.
#'
#' @return A dataframe with formatted and renamed columns.
#'
#' @keywords internal
load_and_process_data <- function(health_data_path,
                                  region_col,
                                  district_col,
                                  date_col = NULL,
                                  year_col = NULL,
                                  month_col = NULL,
                                  case_col,
                                  case_type,
                                  tot_pop_col) {
  # Create dataframe from vector/list if data comes from the API
  if (is.data.frame(health_data_path)) {
    data <- health_data_path
  } else {
    ext <- tolower(xfun::file_ext(health_data_path))
    # Load data based on file extension
    data <- switch(ext,
                   "rds" = readr::read_rds(health_data_path),
                   "csv" = readr::read_csv(health_data_path, show_col_types = FALSE),
                   "xlsx" = readxl::read_excel(health_data_path),
                   stop("Unsupported file type: must be .rds, .csv, or .xlsx")
    )
  }

  # Ensure case_type is an accepted type
  case_type <- validate_case_type(case_type)
  # create date columns if needed
  if (is.null(date_col) & (is.null(month_col) | is.null(year_col))) {
    stop("If no date column is provided, you must provide both the year and the month columns.")
  }
  if (!is.null(date_col)) {
    data <- data %>% rename(
      date = date_col
    ) %>%
      mutate(
        year = lubridate::year(date),
        month = lubridate::month(date)
      )
  }
  # Rename columns accordingly
  case_sym <- rlang::sym(case_type)
  data <- data %>% rename(
    year = all_of(year_col),
    month = all_of(month_col),
    region = all_of(region_col),
    district = all_of(district_col),
    !!case_sym := all_of(case_col),
    tot_pop = all_of(tot_pop_col)
  ) %>% select(
    all_of(c("region", "district", "year", "month", case_type, "tot_pop"))
  )
  return(data)
}


#' Read in and format climate data
#'
#' @description Read in a monthly time series of climate data, rename
#' columns and create lag variable for spatiotemporal and DLNM analysis. The
#' climate data should start a year before a start year in the health data to
#' allow the lag variables calculation.
#'
#' @param climate_data_path Path to a csv file containing a monthly time series of data
#' for climate variables, which may be disaggregated by district.
#' @param district_col Character. Name of the column in the dataframe that
#' contains the region names.
#' @param year_col Character. Name of the column in the dataframe that
#' contains the Year.
#' @param month_col Character. Name of the column in the dataframe that
#' contains the month.
#' @param tmin_col Character. Name of the column in the dataframe that
#' contains the minimum temperature data.
#' @param tmean_col Character. Name of the column in the dataframe that
#' contains the average temperature.
#' @param tmax_col Character. Name of the column in the dataframe that
#' contains the maximum temperature.
#' @param rainfall_col Character. Name of the column in the dataframe that
#' contains the cumulative monthly rainfall.
#' @param r_humidity_col Character. Name of the column in the dataframe that
#' contains the relative humidity.
#' @param runoff_col Character. Name of the column in the dataframe that
#' contains the monthly runoff water data. Defaults to NULL.
#' @param ndvi_col Character. Name of column containing the Normalized Difference
#' Vegetation Index (ndvi) data. Defaults to NULL.
#' @param spi_col Character. Name of the column in the dataframe that
#' contains the standardized precipitation index. Defaults to NULL.
#' @param max_lag Character. Number corresponding to the maximum lag to be
#' considered for the delay effect. It should be between 2 an 4. Defaults to 4.
#'
#' @return climate dataframe with formatted and renamed columns, and the lag
#' variables
#'
#' @keywords internal
load_and_process_climatedata <- function(climate_data_path,
                                         district_col,
                                         year_col,
                                         month_col,
                                         tmin_col,
                                         tmean_col,
                                         tmax_col,
                                         rainfall_col,
                                         r_humidity_col,
                                         runoff_col= NULL,
                                         ndvi_col = NULL,
                                         spi_col = NULL,
                                         max_lag ){

  if (is.data.frame(climate_data_path)) {
    data <- climate_data_path
  } else {
    ext <- tolower(xfun::file_ext(climate_data_path))
    # Load data based on file extension
    data <- switch(ext,
                   "rds" = readr::read_rds(climate_data_path),
                   "csv" = readr::read_csv(climate_data_path, show_col_types = FALSE),
                   "xlsx" = readxl::read_excel(climate_data_path),
                   stop("Unsupported file type: must be .rds, .csv, or .xlsx")
    )
  }

  # Map columns to standard names, excluding NULLs
  var_map <- list(district = district_col, year = year_col, month = month_col,
                  tmin = tmin_col,tmean = tmean_col,tmax = tmax_col,
                  rainfall = rainfall_col, r_humidity = r_humidity_col,
                  runoff = runoff_col,spi = spi_col,ndvi = ndvi_col
  )

  var_map <- var_map[!sapply(var_map, is.null)]
  selected_cols <- as.character(var_map)
  rename_vec <- setNames(selected_cols, names(var_map))

  # Select and rename
  climate_data <- data %>%
    dplyr::select(all_of(selected_cols)) %>%
    dplyr::rename(!!!rename_vec)

  # Ensure time order if possible
  climate_data <- climate_data %>%
    dplyr::arrange(district, year, month)

  # Function to create lagged variables by district
  create_lags <- function(df, var, max_lag) {
    df <- df %>%
      dplyr::group_by(district) %>%
      dplyr::arrange(year, month, .by_group = TRUE) %>%
      dplyr::mutate(
        dplyr::across(
          .cols = all_of(var),
          .fns = list(!!!setNames(
            lapply(1:max_lag, function(i) ~dplyr::lag(., i)),
            paste0("lag", 1:max_lag)
          ))
        )
      ) %>%
      dplyr::ungroup()
    return(df)
  }
  # Variables to lag
  vars_to_lag <- intersect(names(rename_vec),
                           c("tmin", "tmean", "tmax", "rainfall",
                             "r_humidity", "runoff", "ndvi", "spi"))

  # Create lagged versions for each variable
  for (var in vars_to_lag) {
    lagged_df <- create_lags(climate_data, var, max_lag)
    # Append lag columns
    lag_cols <- grep(paste0("^", var, "_lag"), names(lagged_df), value = TRUE)
    climate_data[, lag_cols] <- lagged_df[, lag_cols]
  }

  return(climate_data)
}


#' Read in and combine climate and health data
#'
#' @description Read and combine climate and health data prepared for the
#' spatiotemporal and DLNM analysis.
#'
#' @param health_data_path The path to the health data.
#' @param climate_data_path The path to the climate data.
#' @param map_path The path to the relevant map data.
#' @param region_col Character. Name of the column in the dataframe that contains
#' the region names.
#' @param district_col Character. Name of the column in the dataframe that
#' contains the region names.
#' @param date_col Character. Name of the column in the dataframe that contains
#' the date. Defaults to NULL.
#' @param year_col Character. Name of the column in the dataframe that contains
#' the Year.
#' @param month_col Character. Name of the column in the dataframe that contains
#' the Month.
#' @param case_col Character. Name of the column in the dataframe
#' that contains the disease cases to be considered.
#' @param case_type Character. The type of disease that the case column refers
#' to. Must be one of 'diarrhea' or 'malaria'.
#' @param tot_pop_col Character. Name of the column in the dataframe that
#' contains the total population.
#' @param tmin_col Character. Name of the column in the dataframe that
#' contains the minimum temperature data.
#' @param tmean_col Character. Name of the column in the dataframe that
#' contains the average temperature.
#' @param tmax_col Character. Name of the column in the dataframe that
#' contains the maximum temperature.
#' @param rainfall_col Character. Name of the column in the dataframe that
#' contains the cumulative monthly rainfall.
#' @param r_humidity_col Character. Name of the column in the dataframe that
#' contains the relative humidity.
#' @param geometry_col is the Name of the geometry column in the shapefile
#' (usually "geometry").
#' @param runoff_col Character. Name of the column in the dataframe that
#' contains the monthly runoff water data. Defaults to NULL.
#' @param ndvi_col Character. Name of column containing the Normalized Difference
#' Vegetation Index (ndvi) data. Defaults to NULL.
#' @param spi_col Character. Name of the column in the dataframe that
#' contains the standardized precipitation index. Defaults to NULL.
#' @param max_lag Character. Number corresponding to the maximum lag to be
#' considered for the delay effect. It should be between 2 an 4. Defaults to 2.
#' @param output_dir Path to folder where the processed map data should be
#' saved. Defaults to NULL.
#'
#' @returns A list of dataframes containing the map, nb.map, data, grid_data, summary
#'
#' @keywords internal
combine_health_climate_data <- function(health_data_path,
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
                                        runoff_col = NULL,
                                        ndvi_col = NULL,
                                        spi_col = NULL,
                                        max_lag,
                                        output_dir = NULL){

  case_type <- validate_case_type(case_type)
  # Load data
  health_data <- load_and_process_data(health_data_path, region_col,
                                       district_col, date_col, year_col,
                                       month_col, case_col,case_type,
                                       tot_pop_col)

  climate_data <- load_and_process_climatedata(climate_data_path, district_col,
                                               year_col, month_col, tmin_col,
                                               tmean_col, tmax_col, rainfall_col,
                                               r_humidity_col, runoff_col,ndvi_col,
                                               spi_col, max_lag)

  map_data <- load_and_process_map(map_path, region_col, district_col,
                                   geometry_col, output_dir)

  joinby_vars <- c("district", "year", "month")
  data <- health_data %>%
    dplyr::left_join(climate_data, by = dplyr::join_by(!!!syms(joinby_vars))) %>%
    dplyr::distinct() %>%
    dplyr::group_by(.data$region, .data$district) %>%
    dplyr::mutate(time = (.data$year - min(.data$year)) * 12 + .data$month) %>%
    dplyr::ungroup()

  grid_data <- data %>%
    dplyr::select(all_of(c("region", "district"))) %>%
    dplyr::distinct() %>%
    dplyr::group_by(.data$region) %>%
    dplyr::mutate(
      region_code = dplyr::cur_group_id(),
      district_number = dplyr::row_number(),
      district_code = as.integer(paste0(.data$region_code, .data$district_number))
    ) %>%
    dplyr::ungroup()

  data <- dplyr::left_join(data, grid_data, by = c("region", "district")) %>%
    dplyr::arrange(.data$region_code, .data$district_code)

  map <- dplyr::left_join(map_data$map, grid_data, by = c("region", "district")) %>%
    dplyr::arrange(.data$region_code, .data$district_code)

  grid_data <- dplyr::rename(grid_data, name = .data$region, code_num = .data$region_code)

  summary_stats <- list(
    tmin = summary(data$tmin),
    tmax = summary(data$tmax),
    rainfall = summary(data$rainfall),
    rhumidity = summary(data$r_humidity)
  )

  return_list <- list(
    map = map,
    nb.map = map_data$nb.map,
    graph_file = map_data$graph_file,
    data = data,
    grid_data = grid_data,
    summary = summary_stats
  )
  return(return_list)
}


#' Plot Time Series of Health and Climate Variables
#'
#' @description Generate time series plots for combined health and climate data
#' prepared for spatiotemporal and DLNM analysis. Supports aggregation
#' at the country, region, or district level.
#'
#' @param data A data frame containing the combined health and climate data.
#' @param param_term Character. The variable to plot (e.g., tmax,
#' tmean, tmin, Malaria). Use "all" to include all available variables.
#' @param level Character. Aggregation level: one of "country", "region", or "district".
#' Defaults to "country".
#' @param filter_year Optional numeric vector to filter data by year(s). Defaults to NULL.
#' @param case_type Character. The type of disease that the case column refers
#' to. Must be one of 'diarrhea' or 'malaria'.
#' @param year Optional numeric vector to filter data by year(s). Defaults to NULL.
#' @param save_fig Boolean. Whether to save the figure as a PDF. Defaults to FALSE.
#' @param output_dir Character. Directory path to save the figure. Default to NULL
#'
#' @return A ggplot object.
#'
#' @keywords internal
plot_health_climate_timeseries <- function(data,
                                           param_term,
                                           level = "country",
                                           filter_year = NULL,
                                           case_type,
                                           save_fig = FALSE,
                                           output_dir = NULL) {

  case_type <- validate_case_type(case_type)
  vars_all <- c(case_type, "tmin","tmean","tmax","rainfall","r_humidity","runoff")
  vars_to_plot <- if (length(param_term) == 1 && param_term == "all") vars_all else param_term

  if (!is.null(filter_year)) data <- dplyr::filter(data, .data$year %in% filter_year)
  data <- dplyr::mutate(data, date = as.Date(paste(.data$year, .data$month, 1, sep = "-")))

  missing <- setdiff(vars_to_plot, names(data))
  if (length(missing)) stop("Missing columns: ", paste(missing, collapse = ", "))

  group_var <- switch(tolower(level), country=NULL, region="region", district="district",
                      stop("Invalid level"))

  agg <- data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c("date", group_var)))) %>%
    dplyr::summarise(dplyr::across(dplyr::all_of(vars_to_plot),
                                   \(x) mean(x, na.rm=TRUE)), .groups="drop") %>%
    tidyr::pivot_longer(dplyr::all_of(vars_to_plot), names_to="variable", values_to="value")

  if (!is.null(group_var)) agg <- dplyr::rename(agg, group = dplyr::all_of(group_var))

  # Variable labels with units & descriptive case_type
  var_labels <- c(
    tmin="Minimum temperature (\u00B0C)", tmean="Mean temperature (\u00B0C)", tmax="Maximum temperature (\u00B0C)",
    rainfall="Rainfall (mm)", r_humidity="Relative humidity (%)", runoff="Runoff (mm)",
    setNames(paste("Average Monthly", gsub("_"," ", case_type), "cases"), case_type)
  )

  y_label <- if (length(vars_to_plot)==1) var_labels[vars_to_plot] else "Value (see panel units)"
  title_text <- if (length(vars_to_plot)==1) paste("Time Series of", vars_to_plot) else
    if (length(vars_to_plot)==length(vars_all)) "Time Series of All Health & Climate Variables" else
      paste("Time Series of", paste(vars_to_plot, collapse=", "))

  p <- ggplot2::ggplot(agg, ggplot2::aes(date, value)) +
    ggplot2::geom_line(ggplot2::aes(color = if (!is.null(group_var)) group), linewidth=1) +
    ggplot2::facet_wrap(~variable, scales="free_y",
                        ncol=1, labeller=ggplot2::labeller(variable=var_labels)) +
    ggplot2::scale_x_date(date_breaks="4 month", date_labels="%Y-%m") +
    ggplot2::labs(title=title_text, x="Date", y=y_label) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.title=ggplot2::element_blank(),
      axis.text.x=ggplot2::element_text(angle=45,hjust=1,size=12))

  if (save_fig) ggplot2::ggsave(
    file.path(output_dir,
              paste0("timeseries_", paste(vars_to_plot, collapse="_"), "_",
                     level,".pdf")), p, width=12, height=7
    )
  p
}


#' Create a cross-basis matrix set for DLNM analysis
#'
#' @description
#' Generates cross-basis matrices for lagged climate variables in a dataset,
#' for use in Distributed Lag Nonlinear Models (DLNM).
#'
#' @param data A dataset returned from \code{combine_health_climate_data()},
#' including lagged variables like \code{tmax_lag1}, \code{tmin_lag1}, etc.
#' @param max_lag Character. Number corresponding to the maximum lag to be
#' considered for the delay effect. It should be between 2 an 4. Defaults to 2.
#' @param nk Numeric. Number of internal knots for the natural spline of
#' each predictor, controlling its flexibility: \code{nk = 0} produces a linear
#' effect with one basis column, \code{nk = 1} generates a simple spline with two
#' columns, \code{nk = 2} yields a more flexible spline with three columns,
#' and higher values of \code{nk} further increase flexibility but may also
#' raise collinearity among spline terms. Defaults to 1
#'
#' @return A list of cross-basis matrices including the basis matrix for maximum
#' temperature, minimun temperature, cumulative rainfall, and relative humidity.
#'
#' @keywords internal
set_cross_basis <- function(data, max_lag = 2, nk = 2) {

  nlag <- max_lag

  var_defs <- list(
    tmax="tmax_lag", tmin="tmin_lag", tmean="tmean_lag",
    rainfall="rainfall_lag", r_humidity="r_humidity_lag",
    runoff="runoff_lag", ndvi="ndvi_lag", spi="spi_lag"
  )
  vars <- lapply(names(var_defs), function(var) {
    expected_cols <- c(var, paste0(var_defs[[var]], seq_len(nlag)))

    # skip if variable absent
    if (!var %in% names(data)) return(NULL)

    # stop if lag columns missing
    miss <- setdiff(expected_cols, names(data))
    if (length(miss) > 0)
      stop(var, ": missing lag columns -> ", paste(miss, collapse=", "))

    # Correct lag order
    x <- data[, expected_cols, drop = FALSE]

    # check dimension
    stopifnot(ncol(x) == nlag + 1)

    x
  })

  names(vars) <- names(var_defs)
  vars <- Filter(Negate(is.null), vars)

  basis_matrices <- lapply(names(vars), function(var) {

    x <- vars[[var]]

    cb <- dlnm::crossbasis(
      x,
      lag = c(0, nlag),
      argvar = list(fun="ns",
                    knots=dlnm::equalknots(as.numeric(as.matrix(x)), nk)),
      arglag = list(fun="ns",
                    knots=dlnm::equalknots(0:nlag, nk))
    )

    colnames(cb) <- paste0("basis_", var, ".", colnames(cb))
    cb
  })

  names(basis_matrices) <- names(vars)
  basis_matrices
}



#' Create indices for INLA models
#'
#' @description: For the INLA model, there is a need to set-up regions index,
#' district index, and year index. This function create these indices using the
#' dataset, ndistrict and nregion.
#'
#' @param data is the dataframe containing district_code, region_code, and year
#' columns from the combine_health_climate_data() function.
#' @param case_type Character. The type of disease that the case column refers
#' to. Must be one of 'diarrhea' or 'malaria'.
#'
#' @returns The modified data with the created indices.
#'
#' @keywords internal
create_inla_indices <- function(data, case_type) {
  # Ensure case type is one of the supported indicators
  case_type <- validate_case_type(case_type)
  # Define needed variables
  ntime <- length(unique(data$time))
  nyear <- length(unique(data$year))
  ndistrict <- length(unique(data$district_code))
  nregion <- length(unique(data$region_code))

  # define the offset variable based on the population data
  overall_rate <- sum(data[[case_type]], na.rm = TRUE) / sum(data$tot_pop, na.rm = TRUE)
  data$E <- overall_rate * data$tot_pop # Expected counts
  data$SIR <- data[[case_type]] / data$E  # Standardized Incidence Ratio

  # Create district index
  data$district_index <- rep(1:ndistrict, length.out = nrow(data))

  # Assign district indices based on unique district codes
  unique_districts <- unique(data$district_code)
  for (j in 1:ndistrict) {
    data$district_index[data$district_code == unique_districts[j]] <- j
  }

  # Create region index
  data$region_index <- NA

  # Assign region indices based on unique region codes
  unique_regions <- unique(data$region_code)
  for (j in 1:nregion) {
    data$region_index[data$region_code == unique_regions[j]] <- j
  }

  # Create year index (first_year is the First year in the data set, is set to 1)
  data$year_index <- data$year - (min(data$year)-1)

  return(data)
}


#' Check multicollinearity using VIF and Condition Number
#'
#' @description
#' This function checks for multicollinearity among DLNM basis variables
#' (specifically tmax and rainfall) and selected confounders (r_humidity, runoff,
#' tmin, ndvi) using the Variance Inflation Factor (VIF) and condition number (Kappa).
#'
#' @param data A data frame from `combined_health_climate_data()` function,
#' containing the columns: `tmax`,`rainfall`, `r_humidity`, `runoff`, `tmin`,`ndvi`,
#' and must be compatible with `set_cross_basis()` for generating DLNM matrices.
#' @param inla_param Character vector of parameter names representing all
#' climate variables to consider excluding the `basis_matrices_choices` parameter.
#' @param basis_matrices_choices Character vector specifying the main exposure variables
#' that should be included as DLNM basismatrices, and should be excluded from the `inla_param`.
#' It might be `tmax`for temperature exposure and `rainfall` if rainfall exposure.
#' @param case_type Character. The type of disease that the case column refers
#' to. Must be one of 'diarrhea' or 'malaria'.
#'
#' @return A list with:
#' \describe{
#'   \item{vif}{Named numeric vector of VIF values.}
#'   \item{condition_number}{The condition number (Kappa) of the design matrix.}
#'   \item{interpretation}{A qualitative interpretation of collinearity level.}
#' }
#'
#' @keywords internal
check_diseases_vif <- function(data,
                               inla_param,
                               max_lag,
                               nk,
                               basis_matrices_choices,
                               case_type) {

  # validate case type
  case_type <- validate_case_type(case_type)
  # get inla indices and cross basis
  data  <- create_inla_indices(data, case_type)
  basis <- set_cross_basis(data,max_lag,nk)
  # assign variables
  vars_basis <- Filter(Negate(is.null), basis[basis_matrices_choices])
  vars_data  <- setdiff(inla_param, basis_matrices_choices)
  # detect missing values are raise errors
  miss_basis <- setdiff(basis_matrices_choices, names(vars_basis))
  miss_data  <- setdiff(vars_data, names(data))
  if (length(miss_basis)) stop("Missing in basis: ", paste(miss_basis, collapse = ", "))
  if (length(miss_data))  stop("Missing in data: ", paste(miss_data, collapse = ", "))

  X <- cbind(do.call(cbind, vars_basis), data[vars_data])
  X <- as.data.frame(X[complete.cases(X), ])
  colnames(X) <- make.names(colnames(X), unique = TRUE)

  vif_vals <- car::vif(lm(rep(1, nrow(X)) ~ ., data = X))
  cond_num <- kappa(scale(X), exact = TRUE)

  list(
    vif = vif_vals,
    condition_number = cond_num,
    interpretation = if (cond_num < 10) {
      "Low collinearity"
    } else if (cond_num < 30) {
      "Moderate collinearity"
    } else {
      "High collinearity"
    }
  )
}


#' Check multicollinearity using VIF and Condition Number and write the results
#' to file.
#'
#' @description
#' This function checks for multicollinearity among DLNM basis variables
#' (specifically tmax and rainfall) and selected confounders (r_humidity, runoff,
#' tmin, ndvi) using the Variance Inflation Factor (VIF) and condition number (Kappa).
#'
#' @param data A data frame from `combined_health_climate_data()` function,
#' containing the columns: `tmax`,`rainfall`, `r_humidity`, `runoff`, `tmin`,`ndvi`,
#' and must be compatible with `set_cross_basis()` for generating DLNM matrices.
#' @param inla_param Character vector of parameter names representing all
#' climate variables to consider excluding the `basis_matrices_choices` parameter.
#' @param basis_matrices_choices Character vector specifying the main exposure variables
#' that should be included as DLNM basismatrices, and should be excluded from the `inla_param`.
#' It might be `tmax`for temperature exposure and `rainfall` if rainfall exposure.
#' @param case_type Character. The type of disease that the case column refers
#' to. Must be one of 'diarrhea' or 'malaria'.
#' @param output_dir Character. The output directory to save the VIF results to.
#' Results are saved as 'VIF_results.csv'.
#'
#' @return A list with:
#' \describe{
#'   \item{vif}{Named numeric vector of VIF values.}
#'   \item{condition_number}{The condition number (Kappa) of the design matrix.}
#'   \item{interpretation}{A qualitative interpretation of collinearity level.}
#' }
#'
#' @keywords internal
check_and_write_vif <- function(data,
                                inla_param,
                                max_lag,
                                nk,
                                basis_matrices_choices,
                                case_type,
                                output_dir
) {
  # Calculate VIF
  VIF <- check_diseases_vif(data=data,
                            inla_param=inla_param,
                            max_lag=max_lag,
                            nk=nk,
                            basis_matrices_choices=basis_matrices_choices,
                            case_type=case_type
  )
  # Create output DF
  VIF$vif <- rbind(data.frame(VIF$vif),
                   data.frame(
                     row.names=c("condition_number", "interpretation"),
                     VIF.vif=c(VIF$condition_number, VIF$interpretation)
                   )
  )
  # Create FPATH
  fpath <- file.path(output_dir, "VIF_results.csv")
  file_connection <- file(fpath)
  # Write to file
  write.csv(file=fpath, VIF$vif)
  # Return values
  return(VIF)
}

#' Run models of increasing complexity in INLA: Fit a baseline model including
#' spatiotemporal random effects.
#'
#' @description: Create and run multiple INLA (Integrated Nested
#' Laplace Approximation) models to the dataset, evaluates them using
#' DIC (Deviance Information Criterion), and identifies the best-fitting model.
#'
#' @param combined_data A dataframe resulting from `combine_health_climate_data()` function.
#' @param basis_matrices_choices A character vector specifying the basis matrix
#' parameters to be included in the model. Possible values are `tmax`, and `rainfall`.
#' @param inla_param A character vector specifying the confounding exposures to
#' be included in the model. Possible values are `tmax`,`tmin`, `rainfall`,
#' `r_humidity`, `runoff`, `ndvi`, Etc.
#' @param case_type Character. The type of disease that the case column refers
#' to. Must be one of `diarrhea` or `malaria`.
#' @param output_dir Character. The path to save model output to.  Defaults to NULL.
#' @param save_model Boolean. Whether to save the results as a CSV. Defaults to
#' FALSE.
#' @param family Character. The probability distribution for the response
#' variable. The user may also have the possibility to choose `nbinomial` for a
#' negative binomial distribution. Defaults to`poisson`.
#' @param config Boolean. Enable additional model configurations. Defaults to FALSE.
#'
#' @returns A list containing the model, baseline_model, and the dic_table.
#'
#' @keywords internal
run_inla_models <- function(combined_data,
                            basis_matrices_choices,
                            inla_param,
                            max_lag,
                            nk,
                            case_type,
                            output_dir = NULL,
                            save_model = FALSE,
                            family = "nbinomial",
                            config = FALSE) {

  if (save_model && is.null(output_dir)) stop("output_dir must be provided if save_model = TRUE")
  case_type <- validate_case_type(case_type)
  if (!requireNamespace("INLA", quietly = TRUE)) {
    stop(
      "INLA is not installed. Run climatehealth::install_INLA to install the ",
      "package."
    )
  }

  data <- create_inla_indices(combined_data$data, case_type)
  basis <- set_cross_basis(combined_data$data,max_lag,nk)
  graph_file <- combined_data$graph_file

  prior <- list(prec = list(prior = "pc.prec", param = c(0.5 / 0.31, 0.01)))
  ct_sym <- as.name(case_type)
  # Base model structure
  base_formula <- substitute(
    case_type ~ 1 +
      f(month, replicate = region_index, model = "rw1", cyclic = TRUE,
        constr = TRUE, scale.model = TRUE, hyper = prior) +
      f(district_index, model = "bym2", replicate = year_index,
        graph = graph_file, scale.model = TRUE, hyper = prior),
    list(case_type = ct_sym)
  )
  base_formula <- as.formula(base_formula)
  if (is.null(basis_matrices_choices)) basis_matrices_choices <- character(0)

  valid_basis <- Filter(function(x) !is.null(basis[[x]]), basis_matrices_choices)
  basis_terms <- if (length(valid_basis) > 0) paste0("basis$", valid_basis) else character()
  raw_vars <- intersect(inla_param, names(data))
  all_terms <- c(basis_terms, raw_vars)

  full_formula <- if (length(all_terms) > 0)
    update(base_formula, as.formula(paste("~ . +", paste(all_terms, collapse = " + "))))
  else base_formula

  # Optional control.family for Negative binomial
  cfam <- NULL
  if (tolower(family) %in% c("nbinomial", "nbinomial2")) {
    cfam <- list(hyper = list(theta = list(prior = "loggamma", param = c(1, 0.01))))
  }

  fit <- function(f) INLA::inla.rerun(INLA::inla(
    f,data = data,family = family,offset = log(data$E),
    control.family = cfam, control.inla = list(strategy = "adaptive"),
    control.compute = list(dic = TRUE, config = config,
                           cpo = TRUE, return.marginals = FALSE),
    control.fixed = list(correlation.matrix = TRUE, prec.intercept = 1, prec = 1),
    control.predictor = list(link = 1, compute = TRUE),verbose = FALSE
  ))

  baseline_model <- fit(base_formula)
  model <- fit(full_formula)

  if (save_model) {
    save(model, file = file.path(output_dir,
                                 paste0("model_with_",
                                        paste(c(valid_basis, raw_vars),
                                              collapse = "_"), ".RData")))
  }

  dic_table <- data.table::data.table(
    Model = paste(c(valid_basis, raw_vars), collapse = " + "),
    DIC = round(model$dic$dic, 0),
    LogScore = round(mean(-log(model$cpo$cpo[model$cpo$failure == 0]), na.rm = TRUE), 3),
    LPML = round(sum(log(model$cpo$cpo[model$cpo$failure == 0]), na.rm = TRUE), 2)
  )

  list(model = model, baseline_model = baseline_model, dic_table = dic_table)
}


#' Visualise monthly random effects for selected INLA model
#'
#' @description
#' Generates and saves a plot of monthly random effects for different
#' regions, visualizing their contribution to Malaria Incidence Rate.
#'
#' @param combined_data Data list from combine_health_climate_data() function.
#' @param model The fitted model object.
#' @param save_fig Boolean. Whether to save the plot as an output. Defaults to
#' FALSE.
#' @param output_dir Character. The path to save the visualisation to. Defaults to NULL.
#'
#' @return THe monthly random effects plot.
#'
#' @keywords internal
plot_monthly_random_effects <- function(combined_data,
                                        model,
                                        save_fig = FALSE,
                                        output_dir = NULL) {
  # Validate output_dir if saving
  if (save_fig & is.null(output_dir)) {
    stop("output_dir must be provided if save_fig = TRUE")
  }

  data <- combined_data$data
  grid_data <- combined_data$grid_data
  map <- combined_data$map

  # Create data frame for monthly random effects per region
  month_effects <- data.frame(
    region_code = rep(unique(data$region_code), each = 12),
    month = model$summary.random$month
  )

  month_effects <- month_effects %>%
    dplyr::left_join(
      grid_data %>%
        dplyr::select(-all_of(c("district", "district_code"))) %>%
        dplyr::distinct(),
      by = c("region_code" = "code_num")
    )

  month_effects <- map %>%
    dplyr::select(-"district") %>%
    dplyr::distinct() %>%
    dplyr::left_join(month_effects, by = c("region" = "name"))

  p <- month_effects %>%
    ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      ggplot2::aes(
        x = .data$month.ID,
        ymin = .data$`month.0.025quant`,
        ymax = .data$`month.0.975quant`
      ),
      fill = "cadetblue4", alpha = 0.5
    ) +
    ggplot2::geom_line(
      ggplot2::aes(x = .data$month.ID, y = .data$month.mean),
      col = "cadetblue4"
    ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
    ggplot2::xlab("Month") +
    ggplot2::ylab("Contribution to log(DIR)") +
    ggplot2::scale_y_continuous() +
    ggplot2::scale_x_continuous(
      breaks = c(1, 4, 7, 10),
      labels = c("Jan", "Apr", "Jul", "Oct")
    ) +
    ggplot2::theme_bw() +
    ggplot2::facet_wrap(~.data$region)

  if (save_fig) {
    ggplot2::ggsave(
      filename = file.path(output_dir, "monthly_random_effects.pdf"),
      plot = p, height = 30, width = 25, units = "cm"
    )
  }
  return(p)
}


#' Visualize yearly spatial random effect of the Diseases Incidence Rate (DIR).
#'
#' @description
#' Generates and saves plots of yearly spatial random effect of the diseases
#' incidence rate at district level.
#'
#' @param combined_data Data list `from combine_health_climate_data()` function.
#' @param model The fitted model from `run_inla_models()` function.
#' @param case_type Character. The type of disease that the case column refers
#' to. Must be one of `diarrhea` or `malaria`.
#' @param save_fig Boolean. Whether to save the plot as an output. Defaults to
#' FALSE.
#' @param output_dir Character. The path to save the fitted model results to. Defaults
#' to NULL.
#'
#' @return The yearly space random effect for the disease incidence rate plot.
#'
#' @keywords internal
plot_yearly_spatial_random_effect <- function(combined_data ,
                                              model,
                                              case_type,
                                              save_fig = FALSE,
                                              output_dir = NULL) {
  # Validate case type
  case_type <- validate_case_type(case_type)
  # Validate output_dir if saving
  if (save_fig && is.null(output_dir)) {
    stop("output_dir must be provided if save_fig = TRUE")
  }
  # Prepare data
  data <- create_inla_indices(combined_data$data, case_type=case_type)
  grid_data <- combined_data$grid_data
  map <- combined_data$map
  ntime <- length(unique(data$time))
  nyear <- length(unique(data$year))
  ndistrict <- length(unique(data$district_code))
  # Extract spatial random effects
  space <- data.table::data.table(model$summary.random$district_index)
  space$year <- rep(min(data$year):max(data$year), each = 2 * ndistrict, length.out=nrow(space))
  space$re <- rep(c(rep(1, ndistrict), rep(2, ndistrict)), nyear, length.out=nrow(space))
  space <- space[space$re == 1, ]
  space$district_code <- rep(unique(data$district_code), nyear, length.out=nrow(space))
  # Merge with spatial map
  space <- left_join(map, space, by = c("district_code" = "district_code"))
  # Plot
  space_effects <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = space, ggplot2::aes(fill = mean), color = "black", size = 0.1) +
    ggplot2::scale_fill_gradient2(
      low = "green", mid = "white", high = "red",
      midpoint = 0,
      limits = c(min(space$mean, na.rm = TRUE), max(space$mean, na.rm = TRUE)),
      name = "Contribution\n to log(DIR)"
    ) +
    ggplot2::theme_void() +
    ggplot2::facet_wrap(~year, ncol = 4)

  # Save if required
  if (save_fig) {
    ggplot2::ggsave(file.path(output_dir, "spatial_random_effects_per_year.pdf"),
                    plot = space_effects, height = 30, width = 25, units = "cm")
  }

  return(space_effects)
}


#' A function to predict relative risk at country, region, and district level
#'
#' @description Produces cumulative relative risk at country, region and
#' district level from analysis.
#'
#' @param data Data list from `combine_health_climate_data()` function.
#' @param param_term A character vector or list containing parameter terms such
#' as `tmax` (maximum temperature) and `rainfall` (rainfall exposure).
#' @param model The fitted model from run_inla_models() function.
#' @param level Character. The spatial disaggregation level.
#' Can take one of the following values: `country`, `region`, or `district`.
#' @param case_type Character. The type of disease that the case column refers
#' to. Must be one of `diarrhea` or `malaria`.
#'
#' @return A dataframe containing cumulative relative risk at the chosen level.
#'
#' @keywords internal
get_predictions <- function(data,
                            param_term,
                            max_lag,
                            nk,
                            model,
                            level,
                            case_type){
  # Validate case type
  case_type <- validate_case_type(case_type)
  # loading the best model
  data <- create_inla_indices(data, case_type)

  # getting basis matrices
  basis_matrices <- set_cross_basis(data,max_lag,nk)

  # Extract full coef and vcov for the region
  coef <- model$summary.fixed$mean
  vcov <- model$misc$lincomb.derived.covariance.matrix

  # Find positions of terms associated with tmax crossbasis
  indt <- grep(paste("basis", param_term, sep = "_"), model$names.fixed)

  if (level == "country"){
    # Extract predictions from the tmax DLNM centered on overall mean Tmax
    predt <- dlnm::crosspred(basis_matrices[[param_term]], coef = coef[indt],
                             vcov = vcov[indt, indt], model.link = "log",
                             bylag = 0.25, cen = round(mean(data[[param_term]],
                                                            na.rm = TRUE), 0))

  } else if (tolower(level) == "region"){
    # Iterate over unique regions
    regions <- unique(data$region)
    predt <- regions %>%
      lapply(function(regi){
        region_data <- subset(data, data$region == regi)
        # Extract predictions from the tmax DLNM centered on overall mean Tmax
        mean_param <- round(mean(region_data[[param_term]], na.rm = TRUE), 0)
        predt <- dlnm::crosspred(basis_matrices[[param_term]], coef = coef[indt],
                                 vcov = vcov[indt, indt],
                                 model.link = "log", bylag = 0.25, cen = mean_param)
        return(predt)
      })
    names(predt) <- regions
  } else if (tolower(level) == "district"){
    # Iterate over unique districts
    districts <- unique(data$district)
    predt <- districts %>%
      lapply(function(dist){
        # Filter data for the current district
        district_data <- data %>% filter(data$district == dist)
        # Extract predictions from the tmax DLNM centered on overall mean Tmax
        mean_param <- round(mean(district_data[[param_term]], na.rm = TRUE), 0)
        predt <- dlnm::crosspred(basis_matrices[[param_term]], coef = coef[indt],
                                 vcov = vcov[indt, indt],
                                 model.link = "log", bylag = 0.25, cen = mean_param)
      })
    names(predt) <- districts
  }
  return(predt)
}


#' Create and plot the exposure-lag-response relationship (contour plot) at country,
#' region or district level for each disease cases type (`diarrhea` and `malaria`).
#'
#' @description: Generates a contour plot showing the exposure-lag-response
#' relationship of the exposure `tmax` and `rainfall` and the diseases case type.
#'
#' @param data Data list from `combine_health_climate_data()` function.
#' @param param_term A character vector or list containing parameter terms such
#' as `tmax` (temperature exposure) and `rainfall`(rainfall exposure).
#' Default to `tmax`.
#' @param model The fitted model from the `run_inla_models()` function.
#' @param level A character vector specifying the geographical disaggregation.
#' Can take one of the following values: "country", "region", or "district".
#' @param case_type Character. The type of disease that the case column refers
#' to. Must be one of `diarrhea` or `malaria`.
#' @param filter_year Integer. The year to filter to data to. Defaults to NULL.
#' @param save_fig Boolean. Whether to save the outputted plot. Defaults to
#' FALSE.
#' @param output_dir The path to save the visualisation to. Defaults to NULL
#'
#' @return contour plot at country, Region and District level
#'
#' @keywords internal
contour_plot <- function(data,
                         param_term,
                         model,
                         level,
                         max_lag,
                         nk,
                         case_type,
                         filter_year = NULL,
                         save_fig = FALSE,
                         output_dir = NULL) {
  case_type <- validate_case_type(case_type)
  if (save_fig && is.null(output_dir)) {
    stop("'output_dir' must be provided if save_fig = TRUE")
  }

  if (!is.null(filter_year)) {
    if (!"year" %in% names(data)) stop("'year' column not found in data.")
    data <- filter(data, year %in% filter_year)
  }
  predt <- get_predictions(data,
                           param_term=param_term,
                           max_lag=max_lag,
                           nk=nk,
                           model,
                           level=level,
                           case_type=case_type)

  plot_contour <- function(x, y, z, title) {
    nlag <- max(x)
    pal <- rev(RColorBrewer::brewer.pal(11, "PRGn"))
    levels <- pretty(range(z, na.rm = TRUE), 20)
    col1 <- colorRampPalette(pal[1:6])
    col2 <- colorRampPalette(pal[6:11])
    cols <- c(col1(sum(levels <= 1)), col2(sum(levels > 1)))

    filled.contour(
      x, y, z,
      xlab = "Lag (Month)",
      ylab = ifelse(param_term == "tmax", "Temperature (\u00b0C)",
                    ifelse(param_term == "rainfall", "Rainfall (mm)", param_term)),
      main = title,
      col = cols,
      levels = levels,
      plot.axes = {
        axis(1, at = 0:nlag, labels = 0:nlag)
        axis(2)
      }
    )
  }

  nlag <- data %>% select(contains(param_term)) %>% ncol() - 1
  lag_seq <- seq(0, nlag, 0.25)

  if (save_fig) {
    output_file <- file.path(
      output_dir,
      paste0("contour_plot_", param_term, "_", level,
             if (!is.null(filter_year))
               paste0("_", paste(filter_year, collapse = "_")), ".pdf")
    )
    pdf(output_file, width = 8, height = 8)
  }

  if (tolower(level) == "country") {
    plot_contour(lag_seq, predt$predvar, t(predt$matRRfit),
                 title = "Contour Plot for Country")
  } else {
    groups <- if (tolower(level) == "region") unique(data$region)
    else unique(data$district)
    for (grp in groups) {
      plot_contour(lag_seq, predt[[grp]]$predvar, t(predt[[grp]]$matRRfit),
                   title = paste("Contour Plot for", grp))
    }
  }
  if (save_fig) dev.off()
}


#' Plot Relative Risk Map at sub-national Level
#'
#' @description
#' Generates a map of the relative risk of the diseases cases associated with climate
#' hazards, including extreme temperature and cumulative rainfall, at a specified
#' geographical level (district or region).
#'
#' @param combined_data A list returned from the `combine_health_climate_data()`
#' function. This list should include both the health-climate data and the map data.
#' @param model The fitted model object returned from the `run_inla_models()` function.
#' @param param_term A character vector or list specifying the climate parameters
#' (e.g., `tmax` for maximum temperature, `rainfall` for precipitation) to
#' include in the map. Defaults to `tmax`.
#' @param level A character string indicating the spatial aggregation level.
#' Options are `region` or `district`. Defaults to `district`.
#' @param case_type Character. The type of disease that the case column refers
#' to. Must be one of `diarrhea` or `malaria`.
#' @param filter_year Integer. The year to filter to data to. Defaults to NULL.
#' @param output_dir Character. The directory path where the output PDF file
#' should be saved. Defaults to NULL.
#' @param save_fig Boolean. If TRUE, saves the plot to the specified directory.
#' Defaults to FALSE.
#' @param cumulative Boolean. If TRUE, plot and save cumulative risk of all year
#' for the specific exposure at region and district level. Defaults to FALSE.
#'
#' @return Relative risk map at the chosen level.
#'
#' @keywords internal
plot_rr_map <- function(combined_data,
                        model,
                        param_term = "tmax",
                        max_lag,
                        nk,
                        level = "district",
                        case_type,
                        filter_year = NULL,
                        output_dir = NULL,
                        save_fig = FALSE,
                        save_csv = FALSE,
                        cumulative = FALSE) {

  # Validate case_type
  case_type <- validate_case_type(case_type)
  data <- combined_data$data
  map <- combined_data$map
  stopifnot("year" %in% names(data))

  if ((save_fig || save_csv) && is.null(output_dir)) {
    stop("output_dir must be provided if save_fig = TRUE or save_csv = TRUE")
  }

  level <- tolower(level)
  grouping_var <- ifelse(level == "district", "district",
                         ifelse(level == "region", "region", "country"))
  years <- if (is.null(filter_year)) sort(unique(data$year)) else filter_year

  # create regional boundaries & labels
  if ("region" %in% names(map)) {
    region_map <- map %>%
      dplyr::group_by(region) %>%
      dplyr::summarise(geometry = sf::st_union(geometry), .groups = "drop")

    region_labels <- region_map %>%
      sf::st_point_on_surface() %>%
      dplyr::mutate(
        lon = sf::st_coordinates(geometry)[, 1],
        lat = sf::st_coordinates(geometry)[, 2]
      )
  } else {
    region_map <- NULL
    region_labels <- NULL
  }

  # Get RR per year
  get_rr_df <- function(yr) {
    pred <- get_predictions(filter(data, year == yr), param_term, max_lag, nk,
                            model, level, case_type)
    purrr::map_dfr(names(pred), function(name) {
      vals <- pred[[name]]
      if (anyNA(vals$allRRfit)) return(NULL)
      tibble(
        !!grouping_var := name,
        RR = median(vals$allRRfit, na.rm = TRUE),
        RR_low = quantile(vals$allRRfit, 0.025, na.rm = TRUE),
        RR_high = quantile(vals$allRRfit, 0.975, na.rm = TRUE),
        year = yr
      )
    })
  }

  # Collect RR data
  rr_list <- lapply(years, get_rr_df)
  rr_all <- bind_rows(rr_list)

  if (cumulative) {
    rr_all <- rr_all %>%
      dplyr::group_by(across(all_of(grouping_var))) %>%
      dplyr::summarise(
        RR = median(RR, na.rm = TRUE),
        RR_low = median(RR_low, na.rm = TRUE),
        RR_high = median(RR_high, na.rm = TRUE),
        .groups = "drop"
      )
    rr_range <- range(rr_all$RR, na.rm = TRUE)
  } else {
    rr_range <- range(rr_all$RR, na.rm = TRUE)
  }

  # Optionally save CSV
  if (save_csv) {
    csv_file <- file.path(output_dir,
                          paste0("RR_", param_term, "_", level,
                                 ifelse(cumulative, "_cumulative", ""), ".csv"))
    write.csv(rr_all, csv_file, row.names = FALSE)

    if (cumulative && level != "country") {
      country_rr <- rr_all %>%
        dplyr::summarise(
          RR = median(RR, na.rm = TRUE),
          RR_low = median(RR_low, na.rm = TRUE),
          RR_high = median(RR_high, na.rm = TRUE)
        )
      write.csv(country_rr,
                file.path(output_dir,
                          paste0("RR_", param_term, "_country_cumulative.csv")),
                row.names = FALSE)
    }
  }
  # Plotting
  if (cumulative) {

    map_rr <- left_join(map, rr_all, by = grouping_var)

    plot <- ggplot2::ggplot(map_rr) +
      ggplot2::geom_sf(ggplot2::aes(fill = RR),
                       color = "grey40", size = 0.15) +

      # Regional boundaries
      {if (!is.null(region_map))
        ggplot2::geom_sf(data = region_map,
                         fill = NA, color = "black", size = 1)} +

      # region labels
      {if (!is.null(region_labels))
        ggplot2::geom_text(
          data = region_labels,
          ggplot2::aes(x = lon, y = lat, label = region),
          size = 3, fontface = "bold")} +

      ggplot2::scale_fill_gradient2(
        low = "blue", mid = "white", high = "red",
        midpoint = 1, limits = rr_range,
        na.value = "grey80", name = "RR"
      ) +
      ggplot2::coord_sf(expand = FALSE) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_line(color = "gray"),
        panel.grid.minor = ggplot2::element_blank(),
        axis.text = ggplot2::element_text(color = "black"),
        axis.ticks = ggplot2::element_line(color = "black")
      ) +
      ggplot2::labs(
        title = paste("Cumulative Relative Risk of", tools::toTitleCase(case_type), "by District"),
        subtitle = paste("Exposure:", param_term),
        x = "Longitude",
        y = "Latitude"
      )

    if (save_fig) {
      ggplot2::ggsave(file.path(output_dir,
                                paste0("RR_map_", param_term, "_", level, "_cumulative.pdf")),
                      plot, width = 10, height = 8)
    }

  } else {

    plots <- lapply(seq_along(years), function(i) {

      map_rr <- left_join(map, rr_list[[i]], by = grouping_var)

      ggplot2::ggplot(map_rr) +
        ggplot2::geom_sf(ggplot2::aes(fill = RR),
                         color = "grey40", size = 0.15) +

        # regional boundaries
        {if (!is.null(region_map))
          ggplot2::geom_sf(data = region_map,
                           fill = NA, linetype = "dashed", color = "black", size = 0.6)} +

        # region labels
        {if (!is.null(region_labels))
          ggplot2::geom_text(
            data = region_labels,
            ggplot2::aes(x = lon, y = lat, label = region),
            size = 3, fontface = "bold")} +

        ggplot2::scale_fill_gradient2(
          low = "blue", mid = "white", high = "red",
          midpoint = 1, limits = rr_range,
          na.value = "grey80", name = "RR"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          panel.grid = ggplot2::element_blank(),
          axis.text = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank()
        ) +
        ggplot2::labs(
          title = paste("Year:", years[i]),
          subtitle = paste("Exposure:", param_term)
        )
    })

    plot <- patchwork::wrap_plots(plots) +
      patchwork::plot_annotation(
        title = paste("Relative Risk of", tools::toTitleCase(case_type), "by District"),
        subtitle = paste("Exposure:", param_term),
        theme = ggplot2::theme(plot.title = ggplot2::element_text(size = 16, face = "bold"))
      )

    if (save_fig) {
      ggplot2::ggsave(file.path(output_dir,
                                paste0("RR_map_", param_term, "_", level, "_all_years.pdf")),
                      plot, width = 14, height = 10)
    }
  }

  return(plot)
}


#' Read in Relative Risk plot at country, Region, and District level
#'
#' @description Plots the relative risk of Malaria cases by the maximum
#' temperature and cumulative rainfall at country, Region and District level
#'
#' @param data Data list from combine_health_climate_data() function.
#' @param model The fitted model from run_inla_models() function.
#' @param param_term A character vector or list containing parameter terms such
#' as `tmax` (temperature exposure) and `rainfall` (rainfall exposure).
#' Default to `tmax`.
#' @param level A character vector specifying the geographical disaggregation.
#' Can take one of the following values: `country`, `region`, or `district`.
#' Default to `country`.
#' @param case_type Character. The type of disease that the case column refers
#' to. Must be one of `"diarrhea"` or `"malaria"`.
#' @param filter_year Integer. The year to filter to data to. This gives the
#' possibility to user to have the plot for a specific year. When Defaults to NULL,
#' it provides the plot by grouping all the years in the dataset.
#' @param output_dir Character. The path where the PDF file will be saved. Default to NULL.
#' @param save_csv Boolean. If TRUE, saves the RR data to the specified directory.
#' Defaults to FALSE.
#' @param save_fig Boolean. If TRUE, saves the plot to the specified directory.
#' Defaults to FALSE.
#'
#' @return Relative risk plot at country, region, and district levels.
#'
#' @keywords internal
plot_relative_risk <- function(data,
                               model,
                               param_term,
                               max_lag,
                               nk,
                               level,
                               case_type,
                               filter_year = NULL,
                               output_dir = NULL,
                               save_csv = FALSE,
                               save_fig = FALSE) {
  case_type <- validate_case_type(case_type)
  if (!"year" %in% names(data)) stop("'year' column not found in data.")

  level <- tolower(level)

  if (save_fig) {
    if (is.null(output_dir)) stop("output_dir must be provided if save_fig = TRUE")
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  }

  output_pdf <- if (save_fig) {
    file.path(output_dir, paste0("RR_", param_term, "_", level, "_all_plots.pdf"))
  } else {
    NULL
  }

  csv_output_path <- if (save_csv) {
    file.path(output_dir, paste0("RR_", param_term, "_", level, "_all_plots.csv"))
  } else {
    NULL
  }

  build_plot <- function(pred, title) {
    if (anyNA(pred$allRRfit)) return(NULL)
    ggplot2::ggplot(
      dplyr::tibble(
        x = pred$predvar,
        y = pred$allRRfit,
        ymin = pred$allRRlow,
        ymax = pred$allRRhigh
      ),
      ggplot2::aes(x, y)
    ) +
      ggplot2::geom_line(color = "blue", linewidth = 1) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = ymin, ymax = ymax), fill = "blue", alpha = 0.3) +
      ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "gray", linewidth = 0.5) +
      ggplot2::labs(title = title, x = param_term, y = "Relative Risk") +
      ggplot2::theme_minimal() +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 9))
  }

  all_predictions <- list()

  if (level == "country") {
    if (is.null(filter_year)) {
      data_all <- data
      pred <- get_predictions(data_all, param_term, max_lag, nk, model, level, case_type)
      if (is.list(pred) && !is.null(names(pred)) && length(pred) == 1) {
        pred <- pred[[1]]
      }
      all_predictions[["All Years"]] <- pred

      x_breaks <- pretty(range(pred$predvar, na.rm = TRUE), n = 6)
      x_limits <- range(x_breaks)
      param_sym <- rlang::sym(param_term)

      rr_plot <- ggplot2::ggplot() +
        ggplot2::geom_line(data = dplyr::tibble(x = pred$predvar, y = pred$allRRfit),
                           ggplot2::aes(x = x, y = y), color = "red", linewidth = 1) +
        ggplot2::geom_ribbon(data = dplyr::tibble(x = pred$predvar, ymin = pred$allRRlow, ymax = pred$allRRhigh),
                             ggplot2::aes(x = x, ymin = ymin, ymax = ymax), fill = "red", alpha = 0.3) +
        ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "black", linewidth = 0.5) +
        ggplot2::scale_x_continuous(limits = x_limits, breaks = x_breaks) +
        ggplot2::labs(title = "Relative Risk Curve (All Years Combined)", y = "Relative Risk") +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.title.x = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank(),
          axis.title.y = ggplot2::element_text(color = "black", size=11),
          plot.title = ggplot2::element_text(size = 11)
        )

      # Compute histogram counts manually to find max count
      hist_counts <- ggplot2::ggplot_build(
        ggplot2::ggplot(data_all, ggplot2::aes(x = !!param_sym)) +
          ggplot2::geom_histogram(binwidth = 1, boundary = 0)
      )$data[[1]]

      max_count <- max(hist_counts$count, na.rm = TRUE)
      y_breaks <- pretty(c(0, max_count), n = 3)
      y_limits <- range(y_breaks)

      X_label <- switch(param_term,
                        tmax     = "Maximum Temperature (\u00B0C)",
                        rainfall = "Rainfall (mm)",
                        r_humidity = "Relative Humidity (%)",
                        param_term)

      hist_plot <- ggplot2::ggplot(data_all, ggplot2::aes(x = !!param_sym)) +
        ggplot2::geom_histogram(binwidth = 1, boundary = 0, fill = "skyblue", color = "black", alpha = 0.6) +
        ggplot2::scale_x_continuous(limits = x_limits, breaks = x_breaks) +
        ggplot2::scale_y_continuous(name = "Frequency", limits = y_limits, breaks = y_breaks, position = "right") +
        ggplot2::labs(x = X_label) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.title.y = ggplot2::element_text(color = "skyblue"))

      plot_all_years <- rr_plot / hist_plot +
        patchwork::plot_layout(heights = c(2, 1))

      if (save_fig && !is.null(output_pdf)) {
        grDevices::pdf(output_pdf, width = 10, height = 6)
        print(plot_all_years)
        grDevices::dev.off()
      }
      if (save_csv && !is.null(csv_output_path)) {
        utils::write.csv(
          dplyr::tibble(
            predvar = pred$predvar,
            allRRfit = pred$allRRfit,
            allRRlow = pred$allRRlow,
            allRRhigh = pred$allRRhigh
          ),
          csv_output_path, row.names = FALSE
        )
      }

      return(list(plots = plot_all_years, RR = all_predictions))
    }

    filter_year <- sort(unique(filter_year))
    plots <- lapply(filter_year, function(yr) {
      pred <- get_predictions(dplyr::filter(data, year == yr), param_term,
                              max_lag, nk, model, level, case_type)
      all_predictions[[as.character(yr)]] <- pred
      build_plot(pred, as.character(yr))
    }) %>% purrr::keep(~ !is.null(.))

    if (save_fig && !is.null(output_pdf)) {
      grDevices::pdf(output_pdf, width = 14, height = 10)
      purrr::walk(plots, print)
      grDevices::dev.off()
    }

    if (save_csv && !is.null(csv_output_path)) {
      flat_df <- dplyr::bind_rows(lapply(names(all_predictions), function(yr) {
        df <- all_predictions[[yr]]
        dplyr::tibble(
          year = as.numeric(yr),
          predvar = df$predvar,
          allRRfit = df$allRRfit,
          allRRlow = df$allRRlow,
          allRRhigh = df$allRRhigh
        )
      }))
      utils::write.csv(flat_df, csv_output_path, row.names = FALSE)
    }

    return(list(
      plots = patchwork::wrap_plots(plots) +
        patchwork::plot_annotation(
          title = "Exposure-Response Curves by Country",
          subtitle = paste(param_term, "Years:", paste(filter_year, collapse = ", "))
        ),
      RR = all_predictions
    ))
  }

  if (level %in% c("region", "district")) {
    group_plots <- list()

    if (is.null(filter_year)) {
      preds <- get_predictions(data, param_term, max_lag, nk, model, level, case_type)
      all_predictions[["All Years"]] <- preds
      for (grp in names(preds)) {
        p <- build_plot(preds[[grp]], grp)
        if (!is.null(p)) {
          group_plots[[grp]] <- list(p)
        }
      }
    } else {
      for (yr in filter_year) {
        preds <- get_predictions(dplyr::filter(data, year == yr), param_term, max_lag, nk,
                                 model, level, case_type)
        all_predictions[[as.character(yr)]] <- preds
        for (grp in names(preds)) {
          p <- build_plot(preds[[grp]], paste0(grp, " (", yr, ")"))
          if (!is.null(p)) {
            group_plots[[grp]] <- c(group_plots[[grp]], list(p))
          }
        }
      }
    }

    if (save_fig && !is.null(output_pdf)) {
      grDevices::pdf(output_pdf, width = 12, height = 9)
      all_plots <- unlist(group_plots, recursive = FALSE)
      if (length(all_plots) > 0) {
        plot_pages <- split(all_plots, ceiling(seq_along(all_plots) / 16))
        for (page in plot_pages) {
          print(
            patchwork::wrap_plots(page, ncol = 4, nrow = 4) +
              patchwork::plot_annotation(
                title = paste("Exposure-Response Curves by", tools::toTitleCase(level)),
                subtitle = if (is.null(filter_year)) "All Years Combined"
                else paste(param_term, "Years:", paste(filter_year, collapse = ", "))
              )
          )
        }
      }
      grDevices::dev.off()
    }

    if (save_csv && !is.null(csv_output_path)) {
      flat_df <- dplyr::bind_rows(lapply(names(all_predictions), function(yr) {
        preds <- all_predictions[[yr]]
        dplyr::bind_rows(lapply(names(preds), function(grp) {
          df <- preds[[grp]]
          dplyr::tibble(
            year = yr,
            group = grp,
            predvar = df$predvar,
            allRRfit = df$allRRfit,
            allRRlow = df$allRRlow,
            allRRhigh = df$allRRhigh
          )
        }))
      }))
      utils::write.csv(flat_df, csv_output_path, row.names = FALSE)
    }

    return(list(plots = group_plots, RR = all_predictions))
  }
}


#' Calculate Attributable Metrics for Climate-Health Associations.
#'
#' @description
#' Computes the attributable number, fraction, and rate of cases associated with
#' specific exposure variables (e.g., temperature or rainfall) using fitted INLA models.
#' The function estimates these metrics at the desired spatial aggregation level
#' (country, region, or district) and optionally disaggregates by month or year.
#'
#' @param data A data frame or list returned by the `combine_health_climate_data()`
#' function, containing health outcome, population, and exposure data.
#' @param param_term Character. The exposure variable term to evaluate (e.g.,`"tmax"` for
#' maximum temperature, `"rainfall"` for precipitation). Defaults to `"tmax"`.
#' @param model The fitted INLA model object returned by the `run_inla_models()` function.
#' @param level Character. The spatial disaggregation level. Can take one of
#' the following values: `"country"`, `"region"`, or `"district"`.
#' @param param_threshold Numeric. Threshold above which relative risks (RR) are
#' considered attributable. Defaults to `1`.
#' @param filter_year Integer. The year to filter to data to. Defaults to NULL.
#' @param group_by_year Logical. Whether to aggregate results by year (`TRUE`) or
#' by year and month (`FALSE`). Defaults to `FALSE`.
#' @param case_type Character. The type of disease that the case column refers
#' to. Must be one of `"diarrhea"` or `"malaria"`.
#' @param save_csv Logical. Whether to save the generated attribution metrics to file.
#' Default is `FALSE`.
#' @param output_dir Optional. Directory path to save the output metrics if
#' `save_fig = TRUE`
#'
#' @return A tibble containing the following columns:
#' \itemize{
#'   \item Grouping variables depending on the `level` and `group_by_year` settings.
#'   \item `MRT`: Minimum risk temperature (or equivalent reference exposure).
#'   \item `AR_Number`, `AR_Number_LCI`, `AR_Number_UCI`: Estimated, lower, and upper
#'   bounds of the attributable number of cases.
#'   \item `AR_Fraction`, `AR_Fraction_LCI`, `AR_Fraction_UCI`: Estimated, lower, and
#'   upper bounds of the attributable fraction (%).
#'   \item `AR_per_100k`, `AR_per_100k_LCI`, `AR_per_100k_UCI`: Estimated, lower, and
#'   upper bounds of the attributable rate per 100,000 population.
#' }
#'
#' @keywords internal
attribution_calculation <- function(data,
                                    param_term,
                                    model,
                                    level,
                                    param_threshold = 1,
                                    max_lag,
                                    nk,
                                    filter_year = NULL,
                                    group_by_year = FALSE,
                                    case_type,
                                    output_dir = NULL,
                                    save_csv = FALSE) {

  ## Input checks
  case_type <- validate_case_type(case_type)
  level <- tolower(level)

  ## Optional year filtering
  if (!is.null(filter_year)) {
    stopifnot("year" %in% names(data),
              all(filter_year %in% unique(data$year)))
    data <- dplyr::filter(data, year %in% filter_year)
  }

  ## Global centering (internal)
  cen_val_global <- median(data[[param_term]], na.rm = TRUE)

  ## Model components
  coef_mean <- model$summary.fixed$mean
  vcov_full <- model$misc$lincomb.derived.covariance.matrix

  indt <- grep(paste0("basis_", param_term), model$names.fixed)
  if (length(indt) == 0)
    stop("No basis terms found for ", param_term)

  ## INLA indices & cross-basis
  data <- create_inla_indices(data, case_type)
  basis_matrices <- set_cross_basis(data, max_lag, nk)

  ## Grouping logic
  grp_vars <- switch(level,
                     "country"  = if (group_by_year) c("year","month") else c("year","month"),
                     "region"   = if (group_by_year) c("region", "year") else c("region", "year", "month"),
                     "district" = if (group_by_year) c("region", "district", "year")
                     else c("region", "district", "year", "month"),
                     stop("Invalid level")
  )

  ## Global RR curve
  pred_global <- dlnm::crosspred(
    basis_matrices[[param_term]],
    coef  = coef_mean[indt],
    vcov = vcov_full[indt, indt],
    model.link = "log",
    bylag = 0.25,
    cen = cen_val_global
  )

  ## Metric computation (group-specific MER)
  compute_metrics <- function(df, pred) {

    df <- df[!is.na(df[[param_term]]) &
               !is.na(df[[case_type]]) &
               !is.na(df$tot_pop), ]

    if (nrow(df) == 0) return(NULL)

    ## Interpolate RR at observed exposures
    rr_fit  <- approx(pred$predvar, pred$allRRfit,  xout = df[[param_term]], rule = 2)$y
    rr_low  <- approx(pred$predvar, pred$allRRlow,  xout = df[[param_term]], rule = 2)$y
    rr_high <- approx(pred$predvar, pred$allRRhigh, xout = df[[param_term]], rule = 2)$y

    ## MER definition
    idx <- which(rr_fit > param_threshold)
    if (length(idx) == 0) {
      MER_L <- NA
      MER_U <- NA
    } else {
      MER_L <- min(df[[param_term]][idx], na.rm = TRUE)
      MER_U <- max(df[[param_term]][idx], na.rm = TRUE)
    }

    tot_pop <- sum(df$tot_pop, na.rm = TRUE)
    cases   <- df[[case_type]]

    get_vals <- function(rr) {

      id <- which(df[[param_term]] >= MER_L &
                    df[[param_term]] <= MER_U &
                    rr > param_threshold)

      if (length(id) == 0) return(c(0, 0, 0))

      af <- 1 - 1 / mean(rr[id])
      an <- af * sum(cases[id], na.rm = TRUE)
      ar <- (an / tot_pop) * 1e5

      c(af, an, ar)
    }

    fit  <- get_vals(rr_fit)
    low  <- get_vals(rr_low)
    high <- get_vals(rr_high)

    safe_ceiling <- function(x) ifelse(is.na(x), NA, ceiling(x))

    list(
      MER_Lower = MER_L,
      MER_Upper = MER_U,
      AR_Number = safe_ceiling(fit[2]),
      AR_Number_LCI = safe_ceiling(low[2]),
      AR_Number_UCI = safe_ceiling(high[2]),
      AR_Fraction = round(fit[1] * 100, 2),
      AR_Fraction_LCI = round(low[1] * 100, 2),
      AR_Fraction_UCI = round(high[1] * 100, 2),
      AR_per_100k = round(fit[3], 2),
      AR_per_100k_LCI = round(low[3], 2),
      AR_per_100k_UCI = round(high[3], 2)
    )
  }

  ## Attribution by group
  res <- data %>%
    dplyr::group_by(dplyr::across(all_of(grp_vars))) %>%
    dplyr::group_split() %>%
    purrr::map_dfr(~{
      r <- compute_metrics(.x, pred_global)
      if (is.null(r)) return(NULL)

      tibble::tibble(
        !!!.x[1, grp_vars],
        MER_Lower = r$MER_Lower,
        MER_Upper = r$MER_Upper,
        AR_Number = r$AR_Number,
        AR_Number_LCI = r$AR_Number_LCI,
        AR_Number_UCI = r$AR_Number_UCI,
        AR_Fraction = r$AR_Fraction,
        AR_Fraction_LCI = r$AR_Fraction_LCI,
        AR_Fraction_UCI = r$AR_Fraction_UCI,
        AR_per_100k = r$AR_per_100k,
        AR_per_100k_LCI = r$AR_per_100k_LCI,
        AR_per_100k_UCI = r$AR_per_100k_UCI
      )
    })

  ## Overall country-level attribution (all years combined)
  overall_country <- NULL

  if (level == "country" && is.null(filter_year) && !group_by_year) {

    r_all <- compute_metrics(data, pred_global)

    if (!is.null(r_all)) {
      overall_country <- tibble::tibble(
        level = "country",
        period = "All_years",
        MER_Lower = r_all$MER_Lower,
        MER_Upper = r_all$MER_Upper,
        AR_Number = r_all$AR_Number,
        AR_Number_LCI = r_all$AR_Number_LCI,
        AR_Number_UCI = r_all$AR_Number_UCI,
        AR_Fraction = r_all$AR_Fraction,
        AR_Fraction_LCI = r_all$AR_Fraction_LCI,
        AR_Fraction_UCI = r_all$AR_Fraction_UCI,
        AR_per_100k = r_all$AR_per_100k,
        AR_per_100k_LCI = r_all$AR_per_100k_LCI,
        AR_per_100k_UCI = r_all$AR_per_100k_UCI
      )
    }
  }

  ## Save output (optional)
  if (save_csv && !is.null(output_dir)) {

    if (!dir.exists(output_dir))
      dir.create(output_dir, recursive = TRUE)

    readr::write_csv(
      res,
      file.path(
        output_dir,
        paste0(
          "attribution_", level, "_", param_term,
          if (group_by_year) "_yearly.csv" else "_monthly.csv"
        )
      )
    )

    if (!is.null(overall_country)) {
      readr::write_csv(
        overall_country,
        file.path(
          output_dir,
          paste0("attribution_country_", param_term, "_overall.csv")
        )
      )
    }
  }

  res
}


#' Plot Attributable Health Metrics Across Spatial and Temporal Levels
#'
#' @description
#' Visualizes attributable health metrics (e.g., attributable number, fraction, or rate)
#' derived from `attribution_calculation()` across different spatial scales and time periods.
#' The function automatically adapts plots to the selected spatial level (`country`, `region`,
#' or `district`) and handles both single- and multi-year visualizations.
#' It supports faceted, grouped, or aggregated visualizations and can optionally
#' save output plots as PDF files.
#'
#' @param attr_data A data frame or tibble containing attribution results, typically
#' generated by the `attribution_calculation()` function. Must include relevant columns
#' such as `year`, `region`, `district`, `AR_Number`, `AR_Fraction`, and `AR_per_100k`.
#' @param level Character. The spatial level for plotting. One of `"country"`,
#' `"region"`, or `"district"`. Determines the type and granularity of plots.
#' @param metrics Character vector specifying which metrics to plot.
#' Options include `"AR_Number"`, `"AR_Fraction"`, and `"AR_per_100k"`. Multiple metrics can be plotted.
#' @param filter_year Optional integer or vector of integers to restrict the plots
#' to specific years. Defaults to `NULL` (all available years are included).
#' @param param_term Character. The exposure variable term to evaluate (e.g., `"tmax"` for
#' maximum temperature, `"rainfall"` for precipitation). Used for labeling the plots.
#' @param case_type Character. The type of disease that the case column refers to
#' (e.g., `"malaria"` or `"diarrhea"`). Used in titles and y-axis labels.
#' @param save_fig Logical. If `TRUE`, saves all generated plots as PDF files
#' to the specified directory. Defaults to `FALSE`.
#' @param output_dir Optional string. Directory path where output PDF files will be saved
#' when `save_fig = TRUE`. If the directory does not exist, it will be created automatically.
#'
#' @details
#' This function produces publication-ready plots of attributable metrics:
#' \itemize{
#'   \item **Country level:** Time series line plots with 95% confidence ribbons.
#'   \item **Region/District level (no filter):** Horizontal bar plots showing aggregated
#'   metrics, grouped by administrative unit.
#'   \item **Region/District level (multi-year):** Grouped bar plots comparing metrics across years.
#' }
#'
#' The function automatically adjusts y-axis limits, formats numeric labels with commas,
#' and includes optional text annotations (e.g., showing both attributable numbers and fractions).
#' When `save_fig = TRUE`, one PDF file is created per metric and spatial level, and each file
#' may contain multiple pages if many regions or districts are present.
#'
#' @return
#' A named list of `ggplot` or `patchwork` plot objects, grouped by metric.
#' Each element corresponds to one metric (`"AR_Number"`, `"AR_Fraction"`, `"AR_per_100k"`)
#' and may include one or more plots, depending on the level and year filters.
#'
#' @keywords internal
plot_attribution_metric <- function(attr_data,
                                    level = c("country", "region", "district"),
                                    metrics = c("AR_Number", "AR_Fraction", "AR_per_100k"),
                                    filter_year = NULL,
                                    param_term,
                                    case_type,
                                    save_fig = FALSE,
                                    output_dir = NULL) {
  # validation
  if (is.null(param_term)) stop("'param_term' must be provided.")
  case_type <- validate_case_type(case_type)
  level <- tolower(level)
  if (level == "country" && !is.null(filter_year)) {
    warning("If level == 'country', filter_year must be NULL.")
    return(NULL)
  }
  metrics <- match.arg(metrics, several.ok = TRUE)

  param_label <- switch(tolower(param_term),
                        tmax = "Extreme Temperature",
                        rainfall = "Extreme Rainfall",
                        param_term)

  if (!is.null(filter_year)) {
    if (!"year" %in% names(attr_data)) stop("'year' column not found in data.")
    attr_data <- dplyr::filter(attr_data, year %in% filter_year)
  }

  y_title_lookup <- c(
    AR_per_100k = "Attributable rate",
    AR_Fraction = "Attributable Fraction (%)",
    AR_Number = "Attributable Number"
  )

  title_lookup <- c(
    AR_per_100k = paste0(tools::toTitleCase(case_type), " cases per 100,000 attributable to ", param_label),
    AR_Fraction = paste0(tools::toTitleCase(case_type), " Attributable Fraction (%) due to ", param_label),
    AR_Number = paste0("Number of ", tools::toTitleCase(case_type), " cases attributable to ", param_label)
  )

  formatter_lookup <- list(
    AR_per_100k = scales::label_comma(),
    AR_Fraction = scales::label_comma(),
    AR_Number = scales::label_comma()
  )

  aggregate_attr_data <- function(data, group_var) {
    dplyr::group_by(data, .data[[group_var]]) %>%
      dplyr::summarise(
        dplyr::across(matches("^AR_Number(_LCI|_UCI)?$"), ~ sum(.x, na.rm = TRUE)),
        dplyr::across(matches("^AR_(Fraction|per_100k)(_LCI|_UCI)?$"), ~ mean(.x, na.rm = TRUE)),
        .groups = "drop"
      )
  }

  if (is.null(filter_year)) {
    if (level == "country") {
      attr_data <- aggregate_attr_data(attr_data, "year")
    } else if (level %in% c("region", "district")) {
      if (!(level %in% names(attr_data))) stop(paste0("'", level, "' column not found in data."))
      attr_data <- aggregate_attr_data(attr_data, level)
    }
  }

  plots <- purrr::map(metrics, function(metric) {
    lci_col <- paste0(metric, "_LCI")
    uci_col <- paste0(metric, "_UCI")
    x_var <- if (level %in% c("region", "district") && is.null(filter_year)) level else "year"

    title <- title_lookup[[metric]]
    y_formatter <- formatter_lookup[[metric]]
    y_label <- y_title_lookup[[metric]]

    attr_data_plot <- attr_data %>%
      dplyr::filter(!is.na(.data[[metric]]),
                    !is.na(.data[[lci_col]]),
                    !is.na(.data[[uci_col]]))

    # --- Country-level plot with CI ---
    if (level == "country" && is.null(filter_year)) {
      attr_data_plot$year <- factor(attr_data_plot$year)

      # adjust y limits based on metric
      y_limits <- if (metric == "AR_Fraction") {
        c(0, 1.8*max(attr_data_plot[[metric]], na.rm = TRUE))
      } else if (metric == "AR_Number") {
        c(0, 1.8*max(attr_data_plot[[metric]], na.rm = TRUE))
      } else {
        c(0, 1.8*max(attr_data_plot[[metric]], na.rm = TRUE))
      }

      # Add AF value in parentheses for AN only
      attr_data_plot <- attr_data_plot %>%
        mutate(label_text = if (metric == "AR_Number") {
          paste0(round(.data[[metric]], 1), " (", round(AR_Fraction, 2), "%)")
        } else {
          round(.data[[metric]], 2)
        })

      p <- ggplot2::ggplot(attr_data_plot, ggplot2::aes(x = year, y = .data[[metric]], group = 1)) +
        ggplot2::geom_line(color = "steelblue", linewidth = 1) +
        ggplot2::geom_point(color = "steelblue", size = 2) +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = .data[[lci_col]], ymax = .data[[uci_col]]),
                             alpha = 0.2, fill = "steelblue") +
        ggplot2::geom_text(ggplot2::aes(label = label_text), vjust = -1.2, size = 2) +
        ggplot2::labs(title = paste(title, " (95% CI)"), y = y_label, x = "Year") +
        ggplot2::scale_y_continuous(labels = y_formatter, limits = y_limits) +
        ggplot2::theme_minimal(base_size = 10) +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 12, face = "bold"))

      if (save_fig && !is.null(output_dir)) {
        if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
        ggplot2::ggsave(filename = file.path(output_dir, paste0("plot_", metric, "_", param_term, "_country.pdf")),
                        plot = p, width = 8, height = 5)
      }
      return(p)
    }

    # --- Region/District bar plots ---
    if (level %in% c("region", "district") && is.null(filter_year)) {
      attr_data_plot <- attr_data_plot %>%
        dplyr::arrange(dplyr::desc(.data[[metric]])) %>%
        dplyr::mutate(!!level := factor(.data[[level]], levels = unique(.data[[level]]))) %>%
        dplyr::mutate(label_text = if (metric == "AR_Number") {
          paste0(round(.data[[metric]], 1), " (", round(.data$AR_Fraction, 2), "%)")
        } else {
          paste0(round(.data[[metric]], 1))
        })

      # dynamic y-axis limits
      max_y <- max(attr_data_plot[[metric]], na.rm = TRUE)
      y_limits <- if (metric == "AR_Fraction") {
        c(0, 1.8*max(attr_data_plot[[metric]], na.rm = TRUE))
      } else if (metric == "AR_Number") {
        c(0, 1.8*max(attr_data_plot[[metric]], na.rm = TRUE))
      } else {
        c(0, 1.8*max(attr_data_plot[[metric]], na.rm = TRUE))
      }

      district_plots <- attr_data_plot %>%
        split(ceiling(seq_along(attr_data_plot[[level]]) / 30)) %>%
        purrr::map(~ {
          ggplot2::ggplot(.x, ggplot2::aes(x = .data[[level]], y = .data[[metric]])) +
            ggplot2::geom_col(fill = "steelblue", width = 0.6) +
            ggplot2::geom_text(ggplot2::aes(label = label_text), hjust = -0.1, size = 2.8) +
            ggplot2::coord_flip() +
            ggplot2::labs(x = tools::toTitleCase(level), y = y_label) +
            ggplot2::scale_y_continuous(labels = y_formatter, limits = y_limits) +
            ggplot2::theme_minimal(base_size = 10) +
            ggplot2::theme(axis.text.y = ggplot2::element_text(size = 7),
                           plot.title = ggplot2::element_text(hjust = 0.5, size = 9))
        })

      if (save_fig && !is.null(output_dir)) {
        if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
        pdf_file <- file.path(output_dir, paste0("plot_", metric, "_", param_term, "_", level, ".pdf"))
        grDevices::pdf(pdf_file, width = 11, height = 8)
        for (i in seq_along(district_plots)) {
          merged_plot <- patchwork::wrap_plots(district_plots[i], ncol = 1) +
            patchwork::plot_annotation(
              title = paste(title, "by", tools::toTitleCase(level)),
              theme = ggplot2::theme(plot.title = ggplot2::element_text(size = 10, face = "bold", hjust = 0.5))
            )
          print(merged_plot)
        }
        grDevices::dev.off()
      }
      return(district_plots)
    }

    # --- Region/District grouped by year (no CI) ---
    if (!is.null(filter_year) && length(filter_year) >= 1 && level %in% c("region", "district")) {
      attr_data_plot <- attr_data_plot %>%
        group_by(.data[[level]], year) %>%
        summarise(
          AR_Fraction = mean(AR_Fraction, na.rm = TRUE),
          across(matches("^AR_Number(_LCI|_UCI)?$"), ~ sum(.x, na.rm = TRUE)),
          across(matches("^AR_(per_100k)(_LCI|_UCI)?$"), ~ mean(.x, na.rm = TRUE)),
          .groups = "drop"
        ) %>%
        mutate(label_text = if (metric == "AR_Number") {
          paste0(round(.data[[metric]], 1), " (", round(.data$AR_Fraction, 2), "%)")
        } else {
          paste0(round(.data[[metric]], 2))
        })

      level_vals <- attr_data_plot %>%
        group_by(.data[[level]]) %>%
        summarise(avg = mean(.data[[metric]], na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(avg)) %>%
        pull(.data[[level]])

      attr_data_plot[[level]] <- factor(attr_data_plot[[level]], levels = level_vals)
      split_levels <- split(level_vals, ceiling(seq_along(level_vals) / 20))

      group_plots <- purrr::map(split_levels, function(subset_levels) {
        df <- dplyr::filter(attr_data_plot, .data[[level]] %in% subset_levels)

        # dynamic limits again
        max_y <- max(df[[metric]], na.rm = TRUE)
        y_limits <- if (metric == "AR_Fraction") {
          c(0, 1.5*max(attr_data_plot[[metric]], na.rm = TRUE))
        } else if (metric == "AR_Number") {
          c(0, 1.5*max(attr_data_plot[[metric]], na.rm = TRUE))
        } else {
          c(0, 1.5*max(attr_data_plot[[metric]], na.rm = TRUE))
        }

        ggplot(df, aes(x = .data[[level]], y = .data[[metric]], fill = factor(year))) +
          geom_col(position = position_dodge(width = 0.8)) +
          geom_text(aes(label = label_text),
                    position = position_dodge(width = 0.8), vjust = -0.3, size = 3.5) +
          scale_y_continuous(labels = y_formatter, limits = y_limits) +
          labs(x = tools::toTitleCase(level), y = y_label, fill = "Year") +
          {if (length(unique(df$year)) == 1) {
            scale_fill_manual(values = c("steelblue"))
          } else {scale_fill_brewer(palette = "Set2")}} +
          theme_minimal(base_size = 12) +
          theme(axis.text.x = element_text(angle = 70, hjust = 1, size = 12),
                axis.text.y = element_text(size = 12),
                plot.margin = margin(t = 5, r = 5, b = 50, l = 5))
      })

      if (save_fig && !is.null(output_dir)) {
        if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
        pdf_file <- file.path(output_dir, paste0("plot_", metric, "_", param_term, "_Year_", level, ".pdf"))
        pdf(pdf_file, width = 11, height = 8)
        for (i in seq_along(group_plots)) {
          merged <- patchwork::wrap_plots(group_plots[i], ncol = 1) +
            patchwork::plot_annotation(
              title = paste(title, "by Year and", tools::toTitleCase(level)),
              theme = theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5))
            )
          print(merged)
        }
        grDevices::dev.off()
      }
      return(group_plots)
    }
  })

  return(plots)
}


#' Plot Average Monthly Attributable Health Metrics with Climate Overlays
#'
#' @description
#' Visualizes average monthly attributable health metrics (e.g., attributable number,
#' fraction, or rate) derived from attribution analyses across different spatial scales.
#' The function automatically adapts plots to the selected spatial level (`country`,
#' `region`, or `district`) and summarizes seasonal patterns using monthly aggregation.
#' Optionally, corresponding monthly climate variables can be overlaid on a secondary
#' axis to support joint interpretation of health impacts and climate seasonality.
#'
#' @param data A data frame or tibble containing attributable health metrics, typically
#' generated by an attribution workflow. Must include at least `month` and the selected
#' metric column (`AR_Number`, `AR_Fraction`, or `AR_per_100k`), as well as spatial
#' identifiers (`region` or `district`) when applicable.
#' @param level Character. The spatial level for plotting. One of `"country"`,
#' `"region"`, or `"district"`. Determines whether a single national plot or multiple
#' subnational plots are produced.
#' @param metric Character. The attributable metric to visualize. One of
#' `"AR_Number"`, `"AR_Fraction"`, or `"AR_per_100k"`. Controls aggregation rules,
#' axis labeling, and numeric formatting.
#' @param c_data A data frame containing monthly climate variables corresponding
#' to the same spatial and temporal resolution as `data`. When provided together with
#' `param_term`, climate information is overlaid on a secondary y-axis.
#' @param param_term Character string specifying the climate exposure variable
#' (e.g., `"tmax"` for maximum temperature or `"rainfall"` for precipitation). Used for
#' climate extraction and axis labeling.
#' @param filter_year Optional integer or vector of integers to restrict the analysis
#' to specific years prior to monthly aggregation. Defaults to `NULL`, in which case
#' all available years are included.
#' @param save_fig Logical. If `TRUE`, saves the generated plots as a PDF file.
#' Defaults to `FALSE`.
#' @param output_dir Optional character string specifying the directory where output
#' PDF files will be saved when `save_fig = TRUE`. The directory is created automatically
#' if it does not exist.
#'
#' @details
#' This function produces publication-ready visualizations of average monthly
#' attributable health metrics:
#' \itemize{
#'   \item **Country level:** A single bar plot summarizing national average monthly
#'   attribution patterns.
#'   \item **Region/District level:** One bar plot per administrative unit, showing
#'   average monthly attribution, with automatic pagination when many units are present.
#'   \item **Climate overlay (optional):** Monthly climate exposure plotted as a line
#'   on a secondary y-axis to facilitate comparison with seasonal health impacts.
#' }
#'
#' Metric-specific aggregation rules (sum or mean) and numeric formatting are applied
#' automatically. Axis limits and breaks are dynamically adjusted to improve readability.
#' When `save_fig = TRUE`, a single PDF file is created per metric and spatial level,
#' with multiple pages used for region- or district-level outputs when necessary.
#'
#' @return
#' A named list of `ggplot` objects. Each element corresponds to the country or an
#' individual region or district and contains a monthly attribution plot. The list
#' is returned invisibly when plots are saved to file.
#'
#' @keywords internal

plot_avg_monthly <- function(attr_data,
                             level = c("country","region","district"),
                             metrics = c("AR_Number","AR_per_100k","AR_Fraction"),
                             data,
                             param_term,
                             filter_year = NULL,
                             save_fig = FALSE,
                             output_dir = NULL) {

  level <- tolower(level)
  if (save_fig && !dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  if (length(dev.list()) > 0) dev.off()

  # Loop over metrics if multiple
  all_plots <- list()

  for (metric in metrics) {

    # Labels and aggregation
    climate_label <- switch(param_term,
                            tmax="Temperature (\u00B0C)",
                            rainfall="Rainfall (mm)",
                            param_term)
    agg_fun <- switch(metric, AR_Number=sum, AR_per_100k=mean, AR_Fraction=mean)
    y_lab <- switch(metric,
                    AR_Number="Attributable Number",
                    AR_per_100k="AR per 100,000",
                    AR_Fraction="Attributable Fraction")
    y_fmt <- switch(metric,
                    AR_Number=scales::label_comma(),
                    AR_per_100k=scales::label_number(0.1),
                    AR_Fraction=scales::label_number(0.01))
    title_lab <- switch(metric,
                        AR_Number="Under-five Diarrhea Attributable Number",
                        AR_per_100k="Under-five Diarrhea Attributable Rate per 100,000 population",
                        AR_Fraction="Under-five Diarrhea Attributable Fraction")

    # Filter by year
    if (!is.null(filter_year)) {
      attr_data <- dplyr::filter(attr_data, year %in% filter_year)
      data <- dplyr::filter(data, year %in% filter_year)
    }

    group_col <- switch(level, country=NULL, region="region", district="district")
    groups <- if (is.null(group_col)) list(NULL) else sort(unique(attr_data[[group_col]]))

    # Climate overlay function
    add_climate <- function(p, ydat, cdat) {
      pad <- 0.05 * max(ydat$value, na.rm=TRUE)
      rng <- range(cdat$climate, na.rm=TRUE)
      if(diff(rng) == 0) rng[2] <- rng[1] + 1
      sf <- (max(ydat$value, na.rm=TRUE) + 2*pad) / diff(rng)
      cdat$climate_scaled <- (cdat$climate - min(rng)) * sf + pad
      p + ggplot2::geom_line(data=cdat,
                             ggplot2::aes(x=month, y=climate_scaled, group=1),
                             color="red", linewidth=1) +
        ggplot2::scale_y_continuous(labels=y_fmt,
                                    breaks=scales::pretty_breaks(5),
                                    expand=ggplot2::expansion(mult=c(0,0.05)),
                                    sec.axis=ggplot2::sec_axis(~(. - pad)/sf + min(rng),
                                                               name=climate_label,
                                                               breaks=scales::pretty_breaks(5)))
    }

    # Generate plots
    plots <- lapply(groups, function(g) {
      df <- if(is.null(group_col)) attr_data else dplyr::filter(attr_data, .data[[group_col]] == g)
      ydat <- df %>%
        dplyr::group_by(month) %>%
        dplyr::summarise(value = agg_fun(.data[[metric]], na.rm=TRUE), .groups = "drop") %>%
        dplyr::mutate(month = factor(month, 1:12, month.abb))

      p <- ggplot2::ggplot(ydat, ggplot2::aes(month, value)) +
        ggplot2::geom_col(fill="steelblue") +
        ggplot2::labs(x="Month", y=y_lab, subtitle=if(!is.null(g)) g else NULL,
                      title=if(is.null(g)) title_lab else NULL) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.title.y.left=ggplot2::element_text(size=13, face="bold"),
                       axis.title.y.right=ggplot2::element_text(size=13, face="bold"),
                       axis.text.y.left=ggplot2::element_text(size=16, face="bold"),
                       axis.text.y.right=ggplot2::element_text(size=16, face="bold"),
                       axis.text.x=ggplot2::element_text(size=14, face="bold"),
                       plot.title=ggplot2::element_text(hjust=0.5, face="bold", size=14),
                       plot.subtitle=ggplot2::element_text(hjust=0.5, face="bold", size=12))

      # Climate overlay
      cdat <- data %>%
        dplyr::filter(if(is.null(group_col)) TRUE else .data[[group_col]] == g) %>%
        dplyr::group_by(month) %>%
        dplyr::summarise(climate = mean(.data[[param_term]], na.rm=TRUE), .groups="drop") %>%
        dplyr::mutate(month = factor(month, 1:12, month.abb))
      p <- add_climate(p, ydat, cdat)

      p
    })

    names(plots) <- if(is.null(group_col)) "Country" else groups
    all_plots[[metric]] <- plots
  }

  # Save / print all plots
  for (metric_name in names(all_plots)) {
    plots <- all_plots[[metric_name]]
    title_lab <- switch(metric_name,
                        AR_Number="Under-five Diarrhea Attributable Number",
                        AR_per_100k="Under-five Diarrhea Attributable Rate per 100,000 population",
                        AR_Fraction="Under-five Diarrhea Attributable Fraction")

    if(save_fig && !is.null(output_dir)) {
      grDevices::pdf(file.path(output_dir,
                               paste0("monthly_", metric_name, "_", level, ".pdf")),
                     width=12, height=12)
      if(level == "country") {
        lapply(plots, print)
      } else {
        plot_pages <- split(plots, ceiling(seq_along(plots)/9))
        for(pg in plot_pages) print(
          patchwork::wrap_plots(pg, ncol=3, nrow=3) +
            patchwork::plot_annotation(title = title_lab,
                                       theme = ggplot2::theme(
                                         plot.title = ggplot2::element_text(hjust=0.5, face="bold", size=12))))
      }
      dev.off()
    } else lapply(plots, print)
  }

  invisible(all_plots)
}

