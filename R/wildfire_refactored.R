# Refactore code for the 'wildfire' indicator

#' Read in health data and rename columns
#'
#' @description Reads in a CSV file for a daily time series of health data and
#' renames columns to standard names. Creates columns for day of week, month,
#' and year columns derived from the date.
#'
#' @param health_path Path to a CSV file containing a daily time series of data
#' for a particular health outcome, which may be disaggregated by region.
#' @param date_col Character. Name of the column in the dataframe that contains
#' the date. Date column should be in YYYY-MM-DD or YYYY/MM/DD format.
#' @param mean_temperature_col Character. Name of the column in the dataframe
#' that contains the mean temperature column.
#' @param health_outcome_col Character. Name of the column in the dataframe that
#' contains the health outcome count column (e.g. number of deaths, hospital
#' admissions)
#' @param region_col Character. Name of the column in the dataframe that contains
#' the region names. Defaults to NULL.
#' @param rh_col Character. Name of the column containing relative humidity 
#' values. Defaults to NULL.
#' @param wind_speed_col Character. Name of the column containing wind speed.
#' Defaults to NULL.
#'
#' @returns Dataframe with formatted and renamed columns
#'
#' @keywords internal
read_and_format_data <- function(
    health_path,
    date_col,
    mean_temperature_col,
    health_outcome_col,
    region_col = NULL,
    rh_col = NULL,
    wind_speed_col = NULL
) {
    # Read input dataset
    df <- climatehealth::read_input_data(health_path)
    # Fill optional cols where neccesary
    if (is.null(region_col)) {
        region_col <- "regnames"
        df <- df %>% dplyr::mutate(regnames = "no_region")
    }
    if (is.null(rh_col)) {
        rh_col <- "rh"
        df <- df %>% dplyr::mutate(rh = NA)
    }
    if (is.null(wind_speed_col)) {
        wind_speed_col <- "wind_speed"
        df <- df %>% dplyr::mutate(wind_speed = NA)
    }
    # Date format identification
    date_function <- lubridate::ymd
    if (grepl("^\\d{2}/\\d{2}/\\d{4}$", df[[date_col]][1])) {
        date_function <- lubridate::dmy
    }
    # Data set pre processing
    df <- df %>%
        dplyr::rename(
            date = date_col,
            tmean = mean_temperature_col,
            health_outcome = health_outcome_col,
            regnames = region_col,
            rh = rh_col,
            wind_speed = wind_speed_col
        ) %>% dplyr::mutate(
            date = date_function(date),
            year = lubridate::year(date),
            month = lubridate::month(date),
            day = lubridate::day(date),
            dow = as.character(lubridate::wday(date, label = TRUE))
        )
    return(df)
}

#' Extract mean values for shapefile regions from NetCDF file
#'
#' @description Takes a NetCDF file of gridded wildfire data and shapefile for
#' geographical regions and extracts mean values for each shapefile region.
#'
#' Information on NetCDF files:
#' https://climatedataguide.ucar.edu/climate-tools/NetCDF#:~:text=An%20nc4%20files%20is%20a,readily%20handle%20netCDF%2D4%20files.
#'
#' We use a daily time series of gridded wildfire-related
#' PM2.5 concentration from the Finnish Meteorological Institute's SILAM-CTM
#' model. This is available open-source:
#' https://doi.org/10.57707/fmi-b2share.d1cac971b3224d438d5304e945e9f16c.
#'
#' @param ncdf_path Character. Path to a NetCDF file
#' @param shp_path Character. Path to a shapefile .shp of the geographical 
#' boundaries for which to extract mean values of wildfire-related PM2.5
#' @param region_col Character. The name of the column containing region data
#' in the shapefile. Defaults to 'region'
#' @param output_value_col Character. The name of the value column to include
#' in the output. Defaults to mean_PM_FRP
#'
#' @returns Dataframe containing a daily time series with mean wildfire-related
#' PM2.5 values for each region
#'
#' @keywords internal
extract_means_for_geography <- function(
    ncdf_path,
    shp_path,
    region_col = "region",
    output_value_col = "mean_PM_FRP"
) {
    # Confirm terra is installed
    if (!requireNamespace("terra", quietly = TRUE)) {
        stop(
            "'terra' is not installed. Run climatehealth::install_terra() to ",
            "install the package."
        )
    }

    # Load NetCDF and extract time metadata
    nc <- ncdf4::nc_open(ncdf_path)
    nc_raster <- terra::rast(ncdf_path)
    time <- ncdf4::ncvar_get(nc, "time")
    time_origin <- ncdf4::ncatt_get(nc, "time", "units")$value
    match <- regexpr("\\d{4}-\\d{2}-\\d{2}", time_origin)
    origin_date <- regmatches(time_origin, match)
    time_obs <- as.POSIXct(time, origin = origin_date, tz = "GMT")
    names(nc_raster) <- time_obs

    # Read shapefile and align CRS
    shp <- sf::st_read(shp_path, quiet = TRUE)
    shp <- sf::st_transform(shp, raster::crs(nc_raster))

    # Validate region_col exists
    if (!region_col %in% names(shp)) {
        stop(sprintf("Column '%s' not found in shapefile.", region_col))
    }

    # Rename region column to 'regnames'
    shp <- shp %>% dplyr::rename(regnames = !!region_col)

    # Extract mean values
    extracted <- cbind(shp, exactextractr::exact_extract(nc_raster, shp, 'mean'))

    # Keep only 'regnames' and raster columns
    raster_cols <- setdiff(names(extracted), names(shp))
    extracted <- extracted[ , c("regnames", raster_cols)]

    # Reshape to long format
    extracted_long <- reshape2::melt(extracted, id.vars = "regnames")

    # Rename columns
    extracted_long <- extracted_long %>%
        dplyr::rename(
            date = "variable",
            !!output_value_col := "value"
        )

    # Parse date from raster layer names
    extracted_long$date <- stringr::str_sub(extracted_long$date, start = 6, end = 15)
    extracted_long$date <- as.Date(extracted_long$date, format = "%Y.%m.%d")

    # Ensure output_value_col is present
    if (!output_value_col %in% names(extracted_long)) {
        stop("Mean value column missing after reshaping.")
    }

    return(extracted_long)
}

#' Join health and climate data
#'
#' @description Joins a daily time series of climate data with a daily time
#' series of health data.
#'
#' @param climate_data Character. Dataframe containing a daily time series of 
#' climate data, which may be disaggregated by region.
#' @param health_path Character. Path to a CSV file containing a daily time 
#' series of data for a particular health outcome, which may be disaggregated by
#' region.
#' @param region_col Character. Name of the region column in both datasets.
#' Defaults to 'regnames'
#' @param date_col Character. Name of the date column in both datasets.
#' Defaults to 'date'
#' @param exposure_col Character. Name of the column in the climate data 
#' containing the exposure column (e.g., PM2.5) in kilograms. Defaults to
#' 'mean_PM_FRP'.
#'
#' @returns Dataframe containing a daily time series of the joined
#' climate and health data.
#'
#' @keywords internal
join_health_and_climate_data <- function(
    climate_data,
    health_data,
    region_col = "regnames",
    date_col = "date",
    exposure_col = "mean_PM_FRP"
) {
    # Join both datasets
    df_joined <- dplyr::left_join(
        health_data,
        climate_data,
        by = c(region_col, date_col)
    )
    # Ensure valid exposure column
    df_joined <- df_joined %>%
        dplyr::filter(!is.na(.data[[exposure_col]]))
    df_joined[is.finite(.data[[exposure_col]]), ]
    # Convert exposure units from kg to microgram
    df_joined <- df_joined %>%
        dplyr::mutate(
            mean_PM_FRP = (.data[[exposure_col]] * 1e9), # convert kg to microgram
            regnames = as.factor(.data[[region_col]])
        )

    return(df_paired)
}

#' Load wildfire and health data
#'
#' @description Loads a dataframe containing a daily time series of health and
#' climate data, which may be disaggregated by region.
#'
#' @param health_path Path to a CSV file containing a
#' daily time series of data for a particular health outcome, which may be
#' disaggregated by region. If this does not include a column with
#' wildfire-related PM2.5, use join_wildfire_data = TRUE to join these data.
#' @param ncdf_path Path to a NetCDF file containing a daily time series of
#' gridded wildfire-related PM2.5 concentration data.
#' @param shp_path Path to a shapefile .shp of the geographical boundaries for
#' which to extract mean values of wildfire-related PM2.5
#' @param join_wildfire_data Boolean. If TRUE, a daily time series of
#' wildfire-related PM2.5 concentration is joined to the health data. If FALSE,
#' the data set is loaded without any additional joins.
#' @param date_col Character. Name of the column in the dataframe that contains
#' the date.
#' @param region_col Character. Name of the column in the dataframe that 
#' contains the region names.
#' @param mean_temperature_col Character. Name of the column in the dataframe
#' that contains the mean temperature column.
#' @param health_outcome_col Character. Name of the column in the dataframe that
#' contains the health outcome count column (e.g. number of deaths, hospital
#' admissions)
#' @param rh_col Character. Name of the column containing relative humidity 
#' values. Defaults to NULL.
#' @param wind_speed_col Character. Name of the column containing wind speed.
#' Defaults to NULL.
#' @param pm_2_5_col Character. The name of the column containing PM2.5 values
#' in micrograms. Thsi is only required if health data isn't joined. 
#' Defaults to NULL.
#'
#' @returns Dataframe containing a daily time series of climate and health data.
#'
#' @keywords internal
load_wildfire_data <- function(
    health_path,
    ncdf_path,
    shp_path,
    join_wildfire_data = TRUE,
    date_col,
    region_col,
    mean_temperature_col,
    health_outcome_col,
    rh_col = NULL,
    wind_speed_col = NULL,
    pm_2_5_col = NULL
) {
    # Load health data
    health_df <- read_and_format_data(
        health_path = health_path,
        date_col = date_col,
        mean_temperature_col = mean_temperature_col,
        health_outcome_col = health_outcome_col,
        region_col = region_col,
        rh_col = rh_col,
        wind_speed_col = wind_speed_col,
    )
    # Skip wildfire data join if not required
    if (!join_wildfire_data) {
        # Validate exposure column
        if (is.null(pm_2_5_col)) stop("PM2.5 column missing.")
        # Normalise column name
        health_df <- health_df %>%
            dplyr::rename(mean_PM_FRP = pm_2_5_col)
        return(health_df)
    }
    # Obtain wildfire data
    wildfire_df <- extract_means_for_geography(
        ncdf_path = ncdf_path,
        shapefile_path = shp_path,
        region_col = region_col
    )
    # Join wildfire data to climate data
    joined_df <- join_health_and_climate_data(
        climate_data = wildfire_df,
        health_data = health_df,
        region_col = region_col,
        date_col = date_col
    )
    return(wildfire_data)
}

#' Create lagged columns and provide the mean value.
#'
#' @description Creates new columns containing lagged values over n rows and
#' determine the mean of the lagged column.
#'
#' @param data Dataframe containing a daily time series of climate and health
#' data
#' @param lagcol Character. The column to lag.
#' @param nlags Character. How many rows to obtain a lag from.
#'
#' @returns Dataframe with added columns for lagged values and mean(s) of those
#' lags.
#'
#' @keywords internal
get_lags_and_means <- function(data, lagcol, nlags) {
    lag_cols <- paste0(lagcol, "_l", 0:num)
    # create lagged columns
    for (i in seq_along(lag_cols)) {
        data[[lag_cols[i]]] <- dplyr::lag(data[[lagcol]], i)
    }
    # get mean(s)
    for (i in seq_along(lag_cols)) {
        data[
            [paste0(lagcols[i], "_mean")]
        ] <- rowMeans(data[lag_cols])
    }
    return(data)
}

#' Generate lagged values for predictor variables
#'
#' @description Generates new variables in a dataframe for lags and means over
#' lag periods.
#'
#' @param data Dataframe containing a daily time series of climate and health
#' data
#' @param wildfire_lag Integer. The number of days for which to calculate the
#' lags for wildfire PM2.5. Default is 3.
#' @param temperature_lag Integer. The number of days for which to calculate
#' the lags for temperature. Default is 1.
#'
#' @returns Dataframe with added columns for lagged temperature and
#' wildfire-related PM2.5 concentration
#'
#' @keywords internal
create_lagged_variables <- function(
    data,
    wildfire_lag = 3,
    temperature_lag = 1
) {
    # Subset the data in to regions
    df_list <- split(data, f = data$regnames)
    # Create lags for each region
    for (i in seq(length(df_list))) {
        region_data <- df_list[[i]]
        region_data <- get_lags_and_means(
            data=region_data,
            lagcol="mean_PM_FRP",
            nlags=wildfire_lag
        )
        region_data <- get_lags_and_means(
            data=region_data,
            lagcol="tmean",
            nlags=temperature_lag
        )
        df_list[[i]] <- region_data
    }
    # Combine region-level data
    df_all <- do.call(rbind, df_list)
    row.names(df_all) <- NULL
    return(df_all)
}

#' Generate splines for temperature variable
#'
#' @description Generates temperature splines for each region
#'
#' @param data Dataframe containing a daily time series of climate and health
#' data
#' @param nlag Integer. The number of days of lag in the temperature
#' variable from which to generate splines. Default is 0 (unlagged temperature
#' variable). Defaults to 0.
#' @param degrees_freedom Integer. Degrees of freedom for the spline(s). 
#' Defaults to 6.
#'
#' @returns Dataframe with additional column for temperature spline.
#'
#' @keywords internal
create_temperature_splines <- function(
    data,
    nlag = 0,
    degrees_freedom = 6
) {
    # Validate inputs
    if (degrees_freedom<1) stop("Degrees of freedom must be >= 1.")
    if (nlag<0) stop("nlag must be >= 0.")
    lagcol <- paste0("tmean_1", temperature_lag, "_tmean")
    if (!(lagcol %in% colnames(data))) stop(paste(lagcol, "not found in dataset."))
    # Create splines
    df_list <- split(data, f = data$regnames)
    for (i in seq_along(df_list)) {
        region_data <- df_list[[i]]
        region_data$ns.tmean <- splines::ns(
            x = region_data[[temperature_column]],
            df = degrees_freedom
        )
        df_list[[i]] <- region_data
    }
    # Combine region level data in to one dataframe
    df_all <- do.call(rbind, df_list)
    row.names(df_all) <- NULL
    return(df_all)
}


