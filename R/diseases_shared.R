# Shared functions across the Diarrhea and Malaria indicators

#' Read in and format health data
#'
#' @description Reads in a csv file containing a monthly time series of health
#' outcomes and population data. Renames columns and creates time variables for
#' spatiotemporal analysis.
#'
#' @param health_data_path Path to a csv file containing a monthly time series 
#' of data for disease outcomes, which may be disaggregated by sex (under five 
#' case or above five case), and by Region and District.
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
#' to. Must be one of 'diarrhea' or 'malaria'
#' @param tot_pop_col Character. Name of the column in the dataframe that contains
#' the total population.
#'
#' @return A dataframe with formatted and renamed columns.
#'
#' @export
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
                   "rds" = read_rds(health_data_path),
                   "csv" = read_csv(health_data_path, show_col_types = FALSE),
                   "xlsx" = readxl::read_excel(health_data_path),
                   stop("Unsupported file type: must be .rds, .csv, or .xlsx")
    )
  }

  # Ensure case_type is an accepted type
  accepted_cases <- c("diarrhea", "malaria")
  case_type <- tolower(case_type)
  if (!(case_type %in% accepted_cases)) {
    stop("'case_type' must be one of ", paste0(accepted_cases, collapse=", "))
  }

  # Create date columns if needed
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
    year = year_col,
    month = month_col,
    region = region_col,
    district = district_col,
    case_sym = case_col,
    tot_pop = tot_pop_col
  ) %>% select(
    all_of(c("region", "district", "year", "month", case_type, "tot_pop"))
  )
  return(data)
}

#' Read in and format country map data
#'
#' @description: Read in a shape file, rename columns and create the
#' adjacency matrix for spatiotemporal analysis.
#'
#' @param map_path The path to the country's shape file "sf" data.
#' @param region_col Character. The region column in the dataset.
#' @param district_col Character. The district column in the dataset.
#' @param geometry_col  Character. The geometry column in the dataset.
#' @param output_dir  Character. The path to output the processed map data to.
#'
#' @return
#' \itemize{
#'  \item 'map' The processed map
#'  \item 'nb.map'
#'  \item 'graph_file'
#'  }
#'
#' @export
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
  g_file <- if (!is.null(output_dir)) file.path(output_dir, "map.graph")
  else tempfile(pattern = "map", fileext = ".graph")

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
#' @param cvh_col Character. Name of column containing CHV.
#' @param spi_col Character. Name of the column in the dataframe that
#' contains the standardized precipitation index. Defaults to NULL.
#' @param max_lag Character. Number corresponding to the maximum lag to be
#' considered for the delay effect. It should be between 2 an 4. Defaults to 4.
#'
#' @return climate dataframe with formatted and renamed columns, and the lag
#' variables
#'
#' @export
load_and_process_climatedata <- function(climate_data_path,
                                         district_col,
                                         year_col,
                                         month_col,
                                         tmin_col,
                                         tmean_col,
                                         tmax_col,
                                         rainfall_col,
                                         r_humidity_col,
                                         runoff_col = NULL,
                                         cvh_col = NULL,
                                         spi_col = NULL,
                                         max_lag = 4){

  if (is.data.frame(climate_data_path)) {
    data <- climate_data_path
  } else {
    ext <- tolower(xfun::file_ext(climate_data_path))
    # Load data based on file extension
    data <- switch(ext,
                   "rds" = read_rds(climate_data_path),
                   "csv" = read_csv(climate_data_path, show_col_types = FALSE),
                   "xlsx" = readxl::read_excel(climate_data_path),
                   stop("Unsupported file type: must be .rds, .csv, or .xlsx")
    )
  }

  # Map columns to standard names, excluding NULLs
  var_map <- list(district = district_col, year = year_col, month = month_col,
                  tmin = tmin_col, tmean = tmean_col, tmax = tmax_col,
                  rainfall = rainfall_col, r_humidity = r_humidity_col,
                  runoff = runoff_col, cvh = cvh, spi = spi_col)

  var_map <- var_map[!sapply(var_map, is.null)]
  selected_cols <- as.character(var_map)
  rename_vec <- setNames(selected_cols, names(var_map))

  # Select and rename
  climate_data <- data %>%
    dplyr::select(all_of(selected_cols)) %>%
    dplyr::rename(!!!rename_vec)

  # Function to create lagged variables
  create_lags <- function(df, var, max_lag) {
    for (i in 1:max_lag) {
      df[[paste0(var, "_lag", i)]] <- dplyr::lag(df[[var]], i)
    }
    return(df)
  }

  # Determine variables to lag
  vars_to_lag <- intersect(names(rename_vec),
                           c("tmin", "tmean", "tmax", "rainfall",
                             "r_humidity", "runoff", "spi"))

  # Create lagged data
  lagged_data <- lapply(vars_to_lag,
                        function(var) create_lags(climate_data[var], var, max_lag))

  # Bind all
  final_data <- dplyr::bind_cols(climate_data[c("district", "year", "month")],
                                 lagged_data)

  return(final_data)
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
#' to. Must be one of 'diarrhea' or 'malaria'
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
#' @param cvh_col Character. Name of column containing CHV.
#' @param spi_col Character. Name of the column in the dataframe that
#' contains the standardized precipitation index. Defaults to NULL.
#' @param max_lag Character. Number corresponding to the maximum lag to be
#' considered for the delay effect. It should be between 2 an 4. Defaults to 2.
#' @param output_dir Path to folder where the processed map data should be
#' saved. Defaults to NULL.
#'
#' @returns A list of dataframes containing the map, nb.map, data, grid_data, summary
#'
#' @export
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
                                        cvh_col = NULL,
                                        spi_col = NULL,
                                        max_lag = 2,
                                        output_dir = NULL){
  # Ensure case_type is an accepted type
  accepted_cases <- c("diarrhea", "malaria")
  case_type <- tolower(case_type)
  if (!(case_type %in% accepted_cases)) {
    stop("'case_type' must be one of ", paste0(accepted_cases, collapse=", "))
  }
  if (case_type=="diarrhea") cvh_col <- NULL

  health_data <- load_and_process_data(
    health_data_path = health_data_path, 
    region_col = region_col,
    district_col = district_col,
    date_col = date_col, 
    year_col = year_col,
    month_col = month_col, 
    case_col = case_col,
    case_type = case_type,
    tot_pop_col = tot_pop_col
  )

  climate_data <- load_and_process_climatedata(
    climate_data_path = climate_data_path,
    district_col = district_col,
    year_col = year_col,
    month_col = month_col,
    tmin_col = tmin_col,
    tmean_col = tmean_col,
    tmax_col = tmax_col,
    rainfall_col = rainfall_col,
    r_humidity_col = r_humidity_col,
    runoff_col = runoff_col,
    cvh_col = cvh_col,
    spi_col = spi_col,
    max_lag = max_lag
  )

  map_data <- load_and_process_map(
    map_path = map_path,
    region_col = region_col,
    district_col = district_col,
    geometry_col = geometry_col,
    output_dir = output_dir
  )

  # Merge health + climate
  data <- health_data %>%
    left_join(climate_data, by = join_by(district, year, month)) %>%
    distinct() %>%
    group_by(region, district) %>%
    mutate(time = (year - min(year)) * 12 + month) %>%
    ungroup()

  # Build grid codes
  grid_data <- data %>%
    select(region, district) %>%
    distinct() %>%
    group_by(region) %>%
    mutate(
      region_code = cur_group_id(),
      district_number = row_number(),
      district_code = as.integer(paste0(region_code, district_number))
    ) %>% ungroup()

  # Attach codes
  data <- left_join(data, grid_data, by = c("region", "district")) %>%
    arrange(region_code, district_code)

  map <- left_join(map_data$map, grid_data, by = c("region", "district")) %>%
    arrange(region_code, district_code)

  grid_data <- rename(grid_data, name = region, code_num = region_code)

  # Summary stats
  summary_stats <- list(
    tmin = summary(data$tmin),
    tmax = summary(data$tmax),
    rainfall = summary(data$rainfall),
    rhumidity = summary(data$r_humidity)
  )

  # Return combined
  return_list <- list(
    map = map,
    nb.map = map_data$nb.map,
    graph_file = map_data$graph_file,
    data = data, grid_data = grid_data,
    summary = summary_stats
  )
  return(return_list)
}