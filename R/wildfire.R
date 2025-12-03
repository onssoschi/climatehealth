#' Code for producing analysis for health effects of extreme weather events - wildfires

#' Read in and format health data
#'
#' @description Reads in a CSV file for a daily time series of health and climate data,
#' renames columns to standard names. Creates columns for day of week, month,
#' and year columns derived from the date.
#'
#' @param health_path Path to a CSV file containing a daily time series of data
#' for a particular health outcome and climate variables, which may be disaggregated by region.
#' @param date_col Character. Name of the column in the dataframe that contains
#' the date. Date column should be in YYYY-MM-DD or YYYY/MM/DD format.
#' @param mean_temperature_col Character. Name of the column in the dataframe
#' that contains the daily mean temperature column.
#' @param health_outcome_col Character. Name of the column in the dataframe that
#' contains the daily health outcome count (e.g. number of deaths, hospital
#' admissions)
#' @param region_col Character. Name of the column in the dataframe that contains
#' the region names. Defaults to NULL.
#' @param rh_col Character. Name of the column in the dataframe that contains daily relative humidity
#' values. Defaults to NULL.
#' @param wind_speed_col Character. Name of the column in the dataframe that contains daily wind speed.
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
    wind_speed_col = NULL) {
  # Read input dataset
  df <- read_input_data(health_path)
  # Fill optional cols where neccesary
  if (is.null(region_col)) {
    region_col <- "region"
    df <- df %>% dplyr::mutate(region = "no_region")
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
  if (grepl("^\\d{2}[-/]\\d{2}[-/]\\d{4}$", df[[date_col]][1])) {
    date_function <- lubridate::dmy
  }
  # Subset needed columns
  # Subset needed columns
  needed_cols <- c(
    date_col,
    mean_temperature_col,
    health_outcome_col,
    region_col,
    rh_col,
    wind_speed_col
  )
  standard_cols <- c(
    "date", "tmean", "health_outcome", "region", "rh", "wind_speed"
  )
  for (i in seq_along(standard_cols)) {
    std_col <- standard_cols[i]
    need_col <- needed_cols[i]
    if (!identical(std_col, need_col) && std_col %in% names(df)) {
      df[[std_col]] <- NULL
    }
  }
  # Data set pre processing
  df <- df %>%
    dplyr::rename(
      date = all_of(date_col),
      tmean = all_of(mean_temperature_col),
      health_outcome = all_of(health_outcome_col),
      region = all_of(region_col),
      rh = all_of(rh_col),
      wind_speed =all_of(wind_speed_col)
    ) %>%
    dplyr::mutate(
      date = date_function(date),
      year = lubridate::year(date),
      month = lubridate::month(date),
      day = lubridate::day(date),
      dow = as.character(lubridate::wday(date, label = TRUE))
    )
  return(df)
}

#' Extract mean wildfire PM2.5 values for shapefile regions from NetCDF file
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
#' @param ncdf_path Path to a NetCDF file
#' @param shp_path Path to a shapefile .shp of the geographical
#' boundaries for which to extract mean values of wildfire-related PM2.5
#' @param region_col Character. The name of the column containing region data
#' in the shapefile. Defaults to 'region'
#' @param output_value_col Character. The name of the value column to include
#' in the output. Defaults to mean_PM
#'
#' @returns Dataframe containing a daily time series with mean wildfire-related
#' PM2.5 values for each region
#'
#' @keywords internal
extract_means_for_geography <- function(
    ncdf_path,
    shp_path,
    region_col = "region",
    output_value_col = "mean_PM") {
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

  # Rename region column to 'region'
  shp <- shp %>% dplyr::rename(region = !!region_col)

  # Extract mean values
  extracted <- cbind(shp, exactextractr::exact_extract(nc_raster, shp, "mean"))

  # Keep only 'region' and raster columns
  raster_cols <- setdiff(names(extracted), names(shp))
  extracted <- extracted[, c("region", raster_cols)]

  # Reshape to long format
  extracted_long <- reshape2::melt(extracted, id.vars = "region")

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
#' @description Joins a daily time series of wildfire PM2.5 data with a daily time
#' series of health data.
#'
#' @param climate_data Character. Dataframe containing a daily time series of
#' climate data, which may be disaggregated by region.
#' @param health_data Character. Path to a CSV file containing a daily time
#' series of data for a particular health outcome, which may be disaggregated by
#' region.
#' @param region_col Character. Name of the region column in both datasets.
#' Defaults to 'region'
#' @param date_col Character. Name of the date column in both datasets.
#' Defaults to 'date'
#' @param exposure_col Character. Name of the column in the climate data
#' containing the exposure column (e.g., PM2.5) in kilograms. Defaults to
#' 'mean_PM'.
#'
#' @returns Dataframe containing a daily time series of the joined
#' climate and health data.
#'
#' @keywords internal
join_health_and_climate_data <- function(
    climate_data,
    health_data,
    region_col = "region",
    date_col = "date",
    exposure_col = "mean_PM") {
  # Join both datasets
  df_joined <- dplyr::left_join(
    health_data,
    climate_data,
    by = c(region_col, date_col)
  )
  # Ensure valid exposure column
  df_joined <- df_joined %>%
    dplyr::filter(!is.na(.data[[exposure_col]]))
  # Convert exposure units from kg to microgram
  df_joined <- df_joined %>%
    dplyr::mutate(
      mean_PM = (.data[[exposure_col]] * 1e9), # convert kg to microgram
      region = as.factor(.data[[region_col]])
    )

  return(df_joined)
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
#' @param rh_col Character. Name of the column in the dataframe that
#' contains daily relative humidity values.Defaults to NULL.
#' @param wind_speed_col Character. Name of the column in the dataframe that
#' contains the daily windspeed values.Defaults to NULL.
#' @param pm_2_5_col Character. The name of the column containing PM2.5 values
#' in micrograms. This is only required if health data isn't joined.
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
    pm_2_5_col = NULL) {
  # Load health data
  health_df <- read_and_format_data(
    health_path = health_path,
    date_col = date_col,
    mean_temperature_col = mean_temperature_col,
    health_outcome_col = health_outcome_col,
    region_col = region_col,
    rh_col = rh_col,
    wind_speed_col = wind_speed_col
  )
  # Skip wildfire data join if not required
  if (!join_wildfire_data) {
    # Normalise column name
    health_df <- health_df %>%
      dplyr::rename(mean_PM = all_of(pm_2_5_col))
    return(health_df)
  }
  # Obtain wildfire data
  wildfire_df <- extract_means_for_geography(
    ncdf_path = ncdf_path,
    shp_path = shp_path,
    region_col = region_col
  )
  # Join wildfire data to climate data
  joined_df <- join_health_and_climate_data(
    climate_data = wildfire_df,
    health_data = health_df,
    region_col = region_col,
    date_col = date_col
  )
  return(joined_df)
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
  lag_cols <- paste0(lagcol, "_l", 0:nlags)
  # create lagged columns
  for (i in seq_along(lag_cols)) {
    data[[lag_cols[i]]] <- dplyr::lag(data[[lagcol]], i - 1)
  }
  # get mean(s)
  for (i in seq_along(lag_cols)) {
    cols_to_average <- lag_cols[0:i]
    data[[paste0(lag_cols[i], "_mean")]] <- rowMeans(data[cols_to_average])
  }
  return(data)
}

#' Generate lagged values for predictor (temperature) variables
#'
#' @description Generates new variables in a dataframe for lags and means over
#' lag periods.
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
    wildfire_lag,
    temperature_lag) {
  # Subset the data in to regions
  df_list <- split(data, f = data$region)
  # Create lags for each region
  for (i in seq(length(df_list))) {
    region_data <- df_list[[i]]
    region_data <- get_lags_and_means(
      data = region_data,
      lagcol = "mean_PM",
      nlags = wildfire_lag
    )
    region_data <- get_lags_and_means(
      data = region_data,
      lagcol = "tmean",
      nlags = temperature_lag
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
#' variable from which to generate splines (unlagged temperature
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
    degrees_freedom = 6) {
  # Validate inputs
  if (degrees_freedom < 1) stop("Degrees of freedom must be >= 1.")
  if (nlag < 0) stop("nlag must be >= 0.")
  if (!("tmean" %in% colnames(data))) stop("tmean not found in dataset to create splines.")
  lagcol <- paste0("tmean_l", nlag, "_mean")
  if (!(lagcol %in% colnames(data))) stop(paste(lagcol, "not found in dataset."))
  # Create splines
  df_list <- split(data, f = data$region)
  for (i in seq_along(df_list)) {
    region_data <- df_list[[i]]
    region_data$ns.tmean <- splines::ns(
      x = region_data[[lagcol]],
      df = degrees_freedom
    )
    df_list[[i]] <- region_data
  }
  # Combine region level data in to one dataframe
  df_all <- do.call(rbind, df_list)
  row.names(df_all) <- NULL
  return(df_all)
}

#' Stratify data by time period
#'
#' @description Adds columns for strata for each region:year:month:dayofweek
#' and for the total counts of a health outcome across days in each stratum.
#'
#' @param data Dataframe containing a daily time series of climate and health
#' data. Assumes that 'data' has a 'month', 'year', 'dow' and 'region' column.
#'
#' @returns Dataframe with additional columns for stratum
#' (region:year:month:dayofweek) and for the total counts of a health outcome
#' across days in each stratum.
#'
#' @keywords internal
time_stratify <- function(data) {
  # validate columns
  required_cols <- c("month", "year", "dow", "region", "health_outcome")
  if (!all(required_cols %in% colnames(data))) {
    stop(
      paste("Data must include columns:", paste(required_cols, collapse = ", "))
    )
  }
  # Create stratified columns
  df <- split(data, f = data$region)
  for (i in seq(df)) {
    # Convert column to factors
    df[[i]]$month <- as.factor(df[[i]]$month)
    df[[i]]$year <- as.factor(df[[i]]$year)
    df[[i]]$dow <- as.factor(df[[i]]$dow)
    df[[i]]$reg_name_strata <- as.factor(
      stringr::str_replace_all(df[[i]]$region, " ", "_")
    )
    df[[i]]$stratum <- with(
      df[[i]],
      as.factor(reg_name_strata:year:month:dow)
    )
    df[[i]]$ind <- tapply(
      df[[i]]$health_outcome,
      df[[i]]$stratum,
      sum
    )[df[[i]]$stratum]
  }
  # Combine region level datasets
  df_all <- do.call(rbind, df)
  row.names(df_all) <- NULL
  return(df_all)
}

#' Save descriptive statistics
#'
#' @description Generates summary statistics for climate and health data and
#' saves them to the specified file path.
#'
#' @param data Dataframe containing a daily time series of climate and health
#' data
#' @param variables Character or character vector with variable to produce
#' summary statistics for. Must include at least 1 variable.
#' @param bin_width Integer. Width of each bin in a histogram of the outcome
#' variable. Defaults to 5.
#' @param output_dir Character. The directory to output descriptive stats to.
#' Must exist and will not be automatically created. Defaults to ".".
#'
#' @returns Prints summary statistics and a histogram of the the outcome
#' variable
#'
#' @keywords internal
descriptive_stats <- function(
    data,
    variables,
    bin_width = 5,
    output_dir = ".") {
  # validate and define file paths
  if (!file.exists(output_dir)) stop("Output directory does not exist.")
  hist_fpath <- file.path(output_dir, "health_outcome_hist.png")
  summary_fpath <- file.path(output_dir, "variable_summaries.csv")
  # create and save hist
  png(filename = hist_fpath)
  hist(
    data$health_outcome,
    breaks = seq(
      0,
      max(data$health_outcome, na.rm = TRUE) + bin_width,
      by = bin_width
    ),
    main = "Health outcome",
    xlab = "Health outcome"
  )
  dev.off()
  # create summaries, combine and output
  summary_list <- lapply(variables, function(var) {
    stats <- summary(data[[var]])
    data.frame(
      variable = var,
      statistic = names(stats),
      value = as.vector(stats),
      stringsAsFactors = FALSE
    )
  })
  summary_df <- do.call(rbind, summary_list)
  write.csv(summary_df, file = summary_fpath, row.names = FALSE)
}

#' Check variance inflation factors of predictor variables using a linear model
#'
#' @description Checks variance inflation factors of predictor variables using a
#' linear model of the predictor variables on the health outcome. Prints stats
#' if print_vif==TRUE. Raises a warning if VIF for a variables is > 2.
#'
#' @param data Dataframe containing a daily time series of climate and health
#' data.
#' @param predictors Character vector with each of the predictors to include
#' in the model. Must contain at least 2 variables.
#' @param save_csv Bool. Whether or not to save the VIF results to a CSV.
#' @param output_folder_path String. Where to save the CSV file to (if save_csv==T).
#' @param print_vif Bool, whether or not to print VIF for each predictor.
#' Defaults to FALSE.
#'
#' @returns Variance inflation factor statistics for each predictor variable.
#'
#' @keywords internal
check_wildfire_vif <- function(
    data,
    predictors,
    save_csv = FALSE,
    output_folder_path = NULL,
    print_vif = FALSE) {
  # input validation
  if (is.null(output_folder_path) && save_csv == TRUE) {
    stop("No output path provided when save_csv==T.")
  }
  if (!is.character(predictors) || !is.vector(predictors)) {
    stop("Please provide predictor variable names as a character vector")
  }
  if (length(predictors) < 2) {
    stop("Please provide at least two predictor variables")
  }
  # Define Formula
  formula <- paste("health_outcome ~", paste(predictors, collapse = "+"))
  # Loop regions
  vif_results <- list()
  for (reg in unique(data$region)) {
    region_data <- subset(data, data$region == reg)
    model <- lm(formula, data = region_data)
    vif_mod <- car::vif(model)
    if (print_vif) print(paste0("Variance inflation factor: ", round(vif_mod, 4)))
    for (var in names(vif_mod)) {
      if (vif_mod[[var]] >= 2) {
        warning(paste0(
          "Variance inflation factor for ", var, " is >= 2. ",
          "Investigation is suggested."
        ))
      }
      vif_results[[reg]] <- data.frame(
        region = reg,
        formula = formula,
        var = var,
        VIF = vif_mod
      )
    }
  }
  # conditionally save VIF results
  results_df <- unique(do.call(rbind, vif_results))
  if (save_csv) {
    fpath <- file.path(output_folder_path, "model_validation", "vif_results.csv")
    write.csv(results_df, fpath, row.names = FALSE)
  }
}

get_wildfire_lag_columns <- function(data) {
  lags <- c("mean_PM")
  lag_nums <- c("mean_PM" = 0)
  lagged_wf_cols <- grep(
    "^mean_PM_l\\d+_mean$", colnames(data),
    value = TRUE
  )
  if (length(lagged_wf_cols)) {
    matched_lags <- as.integer(
      sub("^mean_PM_l(\\d+)_mean$", "\\1", lagged_wf_cols)
    )
    lags <- c(lags, lagged_wf_cols)
    lag_nums <- c(lag_nums, setNames(matched_lags, lagged_wf_cols))
  }
  return(list(col_names = lags, lag_nums = lag_nums))
}

#' Fit quasipoisson regression models for different lags using a time-stratified
#' case-crossover approach.
#'
#' @description Fits quasipoisson regression models using \link[gnm]{gnm}
#'
#' @param data Dataframe containing a daily time series of climate and health
#' data from which to fit models.
#' @param scale_factor_wildfire_pm Numeric. The value to divide the wildfire PM2.5
#' concentration variables by for alternative interpretation of outputs.
#' Corresponds to the unit increase in wildfire PM2.5 to give the model
#' estimates and relative risks (e.g. scale_factor = 10 corresponds to estimates
#' and relative risks representing impacts of a 10 unit increase in wildfire
#' PM2.5). Setting this parameter to 0 or 1 leaves the variable unscaled.
#' @param save_fig Bool. Whether or not to save a figure showing residuals vs
#' fitted values for each lag. Defaults to FALSE.
#' @param output_folder_path String. Where to save the figure. Defaults to NULL.
#' @param print_model_summaries Bool. Whether to print the model summaries to
#' console. Defaults to FALSE.
#'
#' @returns Dataframe of relative risk and confidence intervals for
#' each lag of wildfire-related PM2.5
#'
#' @keywords internal
casecrossover_quasipoisson <- function(
    data,
    scale_factor_wildfire_pm = 10,
    wildfire_lag,
    save_fig = TRUE,
    output_folder_path = NULL,
    print_model_summaries = TRUE) {
  # scale wildfire PM if neccessary
  if (scale_factor_wildfire_pm > 0) {
    data <- data %>%
      dplyr::mutate(
        dplyr::across(
          "mean_PM" | matches("^mean_PM_l\\d+_mean$"),
          ~ . / scale_factor_wildfire_pm
        )
      )
  }
  # define lag columns
  lag_cols <- get_wildfire_lag_columns(data = data)
  lags <- lag_cols$col_names
  lag_nums <- lag_cols$lag_nums
  # get results
  results <- list()
  if (save_fig == T) {
    grid <- create_grid(length(lags))
    output_path <- file.path(
      output_folder_path,
      "model_validation",
      "wildfires_residuals_vs_fit_plot.pdf"
    )
    pdf(output_path, width = grid[1] * 4, height = grid[2] * 4)
    par(mfrow = c(grid[1], grid[2]))
  }
  for (i in lags) {
    # create model
    formula_parts <- c("health_outcome ~ splines::ns(tmean, df = 6)")
    if (!all(is.na(data$rh))) {
      formula_parts <- c(formula_parts, "splines::ns(rh, df = 3)")
    }
    if (!all(is.na(data$wind_speed))) {
      formula_parts <- c(formula_parts, "splines::ns(wind_speed, df = 3)")
    }

    formula_parts <- c(formula_parts, i)


    formula <- as.formula(paste(formula_parts, collapse = " + "))
    model <- gnm::gnm(
      formula,
      data = data,
      family = quasipoisson,
      subset = data$ind > 0,
      eliminate = data$stratum
    )
    # print summaries if required
    if (print_model_summaries) {
      print(Epi::ci.exp(model, subset = i))
      print(summary(model))
      print(paste(
        Epi::ci.exp(model, subset = i), "\n",
        summary(model),
        "\nRatio of residual deviance to degrees of freedom: ",
        model$deviance / model$df.residual
      ))
    }
    # get residuals and plot (if needed)
    devresid <- resid(model, type = "deviance")
    if (save_fig == T) {
      plot(devresid ~ model$fitted.values, main = i, col = "#f25574")
    }
    coef_pm <- summary(model)$coefficients[i, "Estimate"]
    se_pm <- summary(model)$coefficients[i, "Std. Error"]

    relative_risk <- exp(coef_pm)
    ci_lower <- exp(coef_pm - 1.96 * se_pm)
    ci_upper <- exp(coef_pm + 1.96 * se_pm)

    results[[i]] <- c(
      lag = as.integer(lag_nums[i]),
      relative_risk = relative_risk,
      ci_lower = ci_lower,
      ci_upper = ci_upper
    )
  }
  # save figure
  if (save_fig == TRUE) {
    dev.off()
  }
  # create results df and return
  results <- as.data.frame(do.call(rbind, results))
  results <- unique(results)
  rownames(results) <- NULL
  return(results)
}

#' QAIC calculation
#'
#' @description Computes the Quasi–Akaike Information Criterion (QAIC) for models,
#' enabling model comparison
#'
#' @param data Dataframe containing a daily time series of climate and health
#' data from which to fit models.
#' @param save_csv Bool. Whether or not to save the VIF results to a CSV.
#' @param output_folder_path String. Where to save the CSV file to (if save_csv==T).
#' @param print_results Logical. Whether or not to print model summaries and
#' pearson dispersion statistics. Defaults to FALSE.
#'
#' @returns Dataframe containing QAIC results for each lag.
#'
#' @keywords internal
calculate_qaic <- function(
    data,
    save_csv = FALSE,
    output_folder_path = NULL,
    print_results = FALSE) {
  # input validation
  if (is.null(output_folder_path) && save_csv == TRUE) {
    stop("No output path provided when save_csv==T.")
  }
  qaic_results <- list()
  # get WF lag columns
  lag_cols <- get_wildfire_lag_columns(data = data)
  lags <- lag_cols$col_names
  lag_nums <- lag_cols$lag_nums
  # create results holder and loop regions
  all_results <- list()
  for (reg in unique(data$region)) {
    region_data <- subset(data, data$region == reg)
    for (i in lags) {
      # define model
      number <- lag_nums[[i]]
      # create model formula
      formula_parts <- c("health_outcome ~ splines::ns(tmean, df = 6)", i)
      if (!all(is.na(data$rh))) {
        formula_parts <- c(formula_parts, "splines::ns(rh, df = 3)")
      }
      if (!all(is.na(data$wind_speed))) {
        formula_parts <- c(formula_parts, "splines::ns(wind_speed, df = 3)")
      }
      formula <- as.formula(paste(formula_parts, collapse = " + "))
      model <- gnm::gnm(formula,
        data = region_data,
        family = quasipoisson,
        subset = region_data$ind > 0,
        eliminate = region_data$stratum
      )
      pearson_chisq <- sum(residuals(model, type = "pearson")^2, na.rm = TRUE)
      dispersion <- pearson_chisq / model$df.residual
      # Number of estimated parameters
      k <- length(coef(model))
      # Dispersion parameter
      phi <- summary(model)$dispersion
      # Log-likelihood approximation
      ll <- -0.5 * model$deviance
      # QAIC formula
      qaic <- -2 * ll + 2 * phi * k
      if (print_results == TRUE) {
        print(paste("QAIC for", i, "=", qaic))
        print(paste("Pearson dispersion statistic:", round(dispersion, 3)))
      }
      qaic_results[[paste0("qaic_at_lag_", lag_nums[[i]])]] <- qaic
      qaic_results[[paste0("pearson_dispersion_at_lag_", lag_nums[[i]])]] <- round(dispersion, 3)
    }
    all_results[[reg]] <- as.data.frame(qaic_results)
    all_results[[reg]]$region <- reg
  }
  results_df <- do.call(rbind, all_results)
  results_df <- results_df %>% select(c("region", setdiff(names(results_df), "region")))
  if (save_csv == TRUE) {
    fpath <- file.path(output_folder_path, "model_validation", "qaic_results.csv")
    write.csv(results_df, fpath, row.names = FALSE)
  }
  return(results_df)
}

#' Plot relative risk results by region (if available).
#'
#' @description Plots relative risk and confidence intervals for each lag value
#' of wildfire-related PM2.5
#'
#' @param rr_data Dataframe of relative risk and confidence intervals for
#' each lag of wildfire-related PM2.5
#' @param wildfire_lag Integer. The maximum number of days for which to plot the
#' lags for wildfire PM2.5. Defaults to 3.
#' @param by_region Bool. Whether to plot RR(relative risk) by region.
#' Defaults to FALSE
#' @param save_fig Boolean. Whether to save the generated plot.
#' Defaults to FALSE.
#' @param output_folder_path Path to folder where plots should be saved.
#'
#' @returns Plot of relative risk and confidence intervals for each lag of
#' wildfire-related PM2.5
#'
#' @keywords internal
plot_RR <- function(
    rr_data,
    wildfire_lag,
    by_region = FALSE,
    save_fig = FALSE,
    output_folder_path = NULL) {
  # input validation
  if (save_fig && is.null(output_folder_path)) {
    stop("No output path provided when save_fig==T.")
  }
  # create list to collect plots
  plots <- list()
  # determine ylimits for plotting
  ylims <- c(
    min(rr_data$ci_lower) - 0.1,
    max(rr_data$ci_upper) + 0.1
  )
  # plot by region if needed
  if (by_region) {
    df_list <- split(rr_data, f = rr_data$region_name)
    for (i in seq(df_list)) {
      region_results <- df_list[[i]]
      region_name <- region_results$region_name[1]
      region_plot <- plot_RR_core(
        rr_data = region_results,
        output_folder_path = output_folder_path,
        wildfire_lag = wildfire_lag,
        save_fig = FALSE,
        region_name = region_name,
        ylims = ylims
      )
      plots[[i]] <- region_plot
    }
  } else {
    # Plot overall RR
    plot <- plot_RR_core(
      rr_data = rr_data,
      output_folder_path = output_folder_path,
      wildfire_lag = wildfire_lag,
      save_fig = FALSE,
      region_name = "All Regions"
    )
    plots[[length(plots) + 1]] <- plot
  }
  # combine and save
  combined_plots <- patchwork::wrap_plots(plots)
  if (save_fig) {
    ggplot2::ggsave(
      filename = file.path(output_folder_path, "wildfire_rr.pdf"),
      plot = combined_plots,
      width = if (length(combined_plots) == 1) 8 else 5 * length(combined_plots),
      height = if (length(combined_plots) == 1) 8 else 4 * length(combined_plots),
      limitsize = FALSE
    )
  }
  return(combined_plots)
}

#' Core functionality for plotting results of relative risk analysis.
#'
#' @description Plots relative risk and confidence intervals for each lag value
#' of wildfire-related PM2.5.
#'
#' @param rr_data Dataframe of relative risk and confidence intervals for
#' each lag of wildfire-related PM2.5.
#' @param save_fig Boolean. Whether to save the plot as an output.
#' Defaults to FALSE.
#' @param wildfire_lag Integer. The maximum number of days for which to plot the
#' lags for wildfire PM2.5. Defaults to 3.
#' @param output_folder_path Path to folder where plots should be saved.
#' Defaults to NULL.
#' @param region_name Character. The name of the region.
#' Defaults to 'All regions'.
#'
#' @returns Plot of relative risk and confidence intervals for each lag of
#' wildfire-related PM2.5.
#'
#' @keywords internal
plot_RR_core <- function(
    rr_data,
    save_fig = FALSE,
    wildfire_lag,
    output_folder_path = NULL,
    region_name = "All regions",
    ylims = NULL) {
  # input validation
  if (save_fig && is.null(output_folder_path)) {
    stop("No output path provided when save_fig==T.")
  }
  # generate labels
  labels <- c("0 days")
  if (wildfire_lag > 0) {
    additional_labels <- sapply(
      1:wildfire_lag,
      function(lag) paste("0-", lag, " days", sep = "")
    )
    labels <- c(labels, additional_labels)
  }
  # configure y limits
  if (is.null(ylims)) {
    ylims <- c(
      min(rr_data$ci_lower) - 0.1,
      max(rr_data$ci_upper) + 0.1
    )
  }
  # plot RR data
  plot <- ggplot2::ggplot(
    data = rr_data,
    ggplot2::aes(
      x = lag,
      y = .data$relative_risk,
      ymin = .data$ci_lower,
      ymax = .data$ci_upper
    )
  ) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_errorbar(width = 0.5, linewidth = 1) +
    ggplot2::geom_hline(yintercept = 1, lty = 2) +
    ggplot2::xlab("Lag") +
    ggplot2::ylab("Relative risk") +
    ggplot2::ggtitle(paste("Wildfire PM2.5: ", region_name, sep = "")) +
    ggplot2::scale_x_continuous(
      breaks = seq(0, wildfire_lag, 1), labels = labels
    ) +
    ggplot2::scale_y_continuous(
      limits = ylims
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 18),
      axis.title = ggplot2::element_text(size = 18)
    )
  # save figure if necessary
  if (save_fig == TRUE) {
    # Create output path and PDF
    formatted_region_name <- gsub(" ", "_", tolower(region_name))
    file_name <- paste(
      "wildfire_rr_", formatted_region_name, ".pdf",
      sep = ""
    )
    pdf(file.path(output_folder_path, file_name), width = 8, height = 8)
    # Add to figure
    print(plot)
    dev.off()
  }
  return(plot)
}

#' Save results of wildfire related analysis
#'
#' @description Saves a CSV file of relative risk and confidence intervals for
#' each lag value of wildfire-related PM2.5. Also optionally save results of
#' attributable numbers/fractions.
#'
#' @param rr_results Dataframe of relative risk and confidence intervals for
#' each lag of wildfire-related PM2.5.
#' @param an_ar_results Dataframe containing attributable number/fraction
#' results. Defaults to NULL.
#' @param output_folder_path Path to folder where results should be saved.
#'
#' @keywords internal
save_wildfire_results <- function(
    rr_results,
    an_ar_results = NULL,
    annual_af_an_results = NULL,
    output_folder_path) {
  # Input validation
  if (is.null(output_folder_path)) stop("Output directory required.")
  if (!file.exists(output_folder_path)) stop("Output directory does not exist")
  # Save RR results
  write.csv(
    rr_results,
    file = file.path(output_folder_path, "wildfire_rr.csv"),
    row.names = FALSE
  )
  # conditionally save AN/AR results
  if (!is.null(an_ar_results)) {
    write.csv(
      an_ar_results,
      file = file.path(output_folder_path, "wildfire_an_ar_monthly.csv"),
      row.names = FALSE
    )
    write.csv(
      annual_af_an_results,
      file = file.path(output_folder_path, "wildfire_an_ar_yearly.csv"),
      row.names = FALSE
    )
  }
}

#' Passes data to casecrossover_quasipoisson to calculate RR.
#'
#' @description Splits data by region if relative_risk_by_region==TRUE.
#' If TRUE, data for each individual region is passed to casecrossover_quasipoisson
#' to calculate RR by region. If FALSE, RR is calculated for the entire dataset.
#'
#' @param data Dataframe containing a daily time series of climate and health
#' data from which to fit models.
#' @param scale_factor_wildfire_pm Numeric. The value to divide the wildfire
#' PM2.5 concentration variables by for alternative interpretation of outputs.
#' Corresponds to the unit increase in wildfire PM2.5 to give the model
#' estimates and relative risks (e.g. scale_factor = 10 corresponds to estimates
#' and relative risks representing impacts of a 10 unit increase in wildfire
#' PM2.5). Setting this parameter to 0 or 1 leaves the variable unscaled.
#' @param calc_relative_risk_by_region Bool. Whether to calculate Relative Risk
#' by region. Defaults to FALSE.
#' @param save_fig Bool. Whether or not to save a figure showing residuals vs
#' fitted values for each lag. Defaults to FALSE.
#' @param output_folder_path String. Where to save the figure. Defaults to NULL.
#' @param print_model_summaries Bool. Whether to print the model summaries to
#' console. Defaults to FALSE.
#'
#' @returns Dataframe of relative risk and confidence intervals for
#' each lag of wildfire-related PM2.5. Split by region if calc_relative_risk_by_region
#' set to TRUE.
#'
#' @keywords internal
calculate_wildfire_rr_by_region <- function(
    data,
    scale_factor_wildfire_pm,
    calc_relative_risk_by_region = FALSE,
    save_fig = FALSE,
    output_folder_path = NULL,
    print_model_summaries = FALSE) {
  # input validation
  if (save_fig && is.null(output_folder_path)) {
    stop("No output path provided when save_fig==T.")
  }
  if (calc_relative_risk_by_region == TRUE && !("region" %in% names(data))) {
    stop("data must contain 'region' column for region level RR data.")
  }
  results_list <- list()
  if (calc_relative_risk_by_region == TRUE) {
    # split dataset and create output list
    df_list <- split(data, f = data$region)
    # Get region level RR data
    for (i in seq(df_list)) {
      region_data <- df_list[[i]]
      region_name <- names(df_list)[i]
      region_results <- casecrossover_quasipoisson(
        data = region_data,
        scale_factor_wildfire_pm = scale_factor_wildfire_pm,
        output_folder_path = output_folder_path,
        save_fig = save_fig,
        print_model_summaries = print_model_summaries
      )
      region_results$region_name <- region_name
      results_list[[i]] <- region_results
    }
  }
  # calculate and return dataset level RR
  results <- casecrossover_quasipoisson(
    data = data,
    scale_factor_wildfire_pm = scale_factor_wildfire_pm,
    output_folder_path = output_folder_path,
    save_fig = save_fig,
    print_model_summaries = print_model_summaries
  )
  results$region_name <- "All Regions"
  results_list[[length(results_list) + 1]] <- results
  # combine all regions and return
  results_all <- do.call(rbind, results_list)
  row.names(results_all) <- NULL
  results_all <- unique(results_all)
  return(results_all)
}

#' Calculate attributable numbers and fraction of a given health outcome.
#'
#' @description Takes a calculated RR and upper and lower CIs, and applies these
#' to the input data to calculate attributable fraction and attributable number,
#' along with upper and lower CIs, for each day in the input data. Uses Lag 1 RR
#' and lower/upper CIs.
#'
#' @param data Dataframe containing a daily time series of climate and health
#' data that was used to obtain rr_data.
#' @param rr_data Dataframe containing relative risk and confidence intervals,
#' calculated from input data.
#'
#' @returns A dataframe containing a daily timseries of AF and AN, including
#' upper and lower confidence intervals.
#'
#' @keywords internal
calculate_daily_AF_AN <- function(data, rr_data) {
  # dissagregate data into region level
  df_list <- split(data, f = data$region)
  # calculate values for each region
  for (i in seq(df_list)) {
    region_data <- df_list[[i]]
    region_name <- names(df_list)[i]
    # obtain RR values inc. CIs
    RR_value <- rr_data %>%
      filter(.data$lag == 0, .data$region_name == !!region_name) %>%
      pull(.data$relative_risk)
    RR_CI_lower <- rr_data %>%
      filter(.data$lag == 0, .data$region_name == !!region_name) %>%
      pull(.data$ci_lower)

    RR_CI_upper <- rr_data %>%
      filter(.data$lag == 0, .data$region_name == !!region_name) %>%
      pull(.data$ci_upper)
    # Calculate daily rescaled_RR, AF and AN
    region_data <- region_data %>%
      mutate(
        rescaled_RR = exp((log(RR_value) / 10) * .data$mean_PM),
        attributable_fraction = (.data$rescaled_RR - 1) / .data$rescaled_RR,
        attributable_number = .data$attributable_fraction * .data$health_outcome
      )
    # Repeat for upper/lower CIs
    region_data <- region_data %>%
      mutate(
        rescaled_CI_upper = exp((log(RR_CI_upper) / 10) * .data$mean_PM),
        attributable_fraction_upper = (.data$rescaled_CI_upper - 1) / .data$rescaled_CI_upper,
        attributable_number_upper = .data$attributable_fraction_upper * .data$health_outcome
      )
    region_data <- region_data %>%
      mutate(
        rescaled_CI_lower = exp((log(RR_CI_lower) / 10) * .data$mean_PM),
        attributable_fraction_lower = (.data$rescaled_CI_lower - 1) / .data$rescaled_CI_lower,
        attributable_number_lower = .data$attributable_fraction_lower * .data$health_outcome
      )
    df_list[[i]] <- region_data
  }
  # combine region level data and return
  df_all <- do.call(rbind, df_list)
  row.names(df_all) <- NULL
  return(df_all)
}

#' Summarise AF and AN numbers by region and year
#'
#' @description Takes daily data with attributable fraction and attributable
#' number and summarises by year and region.
#'
#' @param data Dataframe containing daily data including calculated AF and AN.
#' @param monthly Bool. Whether to summarise by month as well as year and region.
#' Defaults to TRUE.
#'
#' @returns Dataframe containing summarised AF and AN data, by year, region and
#' optionall month (if monthly==T).
#'
#' @keywords internal
summarise_AF_AN <- function(data, monthly = TRUE) {
  # determine group columns
  group_cols <- c("region", "year")
  if (monthly) group_cols <- c(group_cols, "month")
  # Z-score for 95% CI
  z <- 1.96
  # Calculate standard errors and variances for AN and AF
  data <- data %>%
    mutate(
      se_AN = (
        .data$attributable_number_upper - .data$attributable_number_lower
      ) / (2 * z),
      var_AN = .data$se_AN^2,
      se_AF = (
        .data$attributable_fraction_upper - .data$attributable_fraction_lower
      ) / (2 * z),
      var_AF = .data$se_AF^2
    )
  summary <- data %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(
      population = mean(.data$pop, na.rm = TRUE),
      total_attributable_number = sum(.data$attributable_number, na.rm = TRUE),
      total_variance_AN = sum(.data$var_AN, na.rm = TRUE),
      average_attributable_fraction = mean(.data$attributable_fraction, na.rm = TRUE),
      variance_AF_mean = sum(.data$var_AF, na.rm = TRUE) / n(),
      .groups = "drop"
    ) %>%
    mutate(
      # Confidence intervals for total AN
      se_total_AN = sqrt(.data$total_variance_AN),
      lower_ci_attributable_number = .data$total_attributable_number - z * .data$se_total_AN,
      upper_ci_attributable_number = .data$total_attributable_number + z * .data$se_total_AN,
      # Confidence intervals for average AF
      se_AF_mean = sqrt(.data$variance_AF_mean),
      lower_ci_attributable_fraction = .data$average_attributable_fraction - z * .data$se_AF_mean,
      upper_ci_attributable_fraction = .data$average_attributable_fraction + z * .data$se_AF_mean,
      # Deaths per 100k
      deaths_per_100k = (.data$total_attributable_number / .data$population) * 100000,
      lower_ci_deaths_per_100k = (.data$lower_ci_attributable_number / .data$population) * 100000,
      upper_ci_deaths_per_100k = (.data$upper_ci_attributable_number / .data$population) * 100000,
      year = as.numeric(as.character(.data$year)),
      region = as.character(.data$region)
    )
  if (monthly) {
    summary <- summary %>% mutate(
      month = as.numeric(as.character(.data$month))
    )
  }
  return(summary)
}

#' Plots attributable fractions and CI across years by regions
#'
#' @description Generates a PDF containing one or more plots of average attributable
#' fractions over time. If by_region is TRUE, the function creates separate
#' plots for each region. All plots are saved to a single PDF
#' file named "aggregated_AR_by_region.pdf" in the specified output_dir.
#'
#' @param data A data frame containing annual attributable fraction estimates.
#' Must include columns: year, average_attributable_fraction,
#' lower_ci_attributable_fraction, upper_ci_attributable_fraction. If
#' by_region is TRUE, must also include region.
#' @param by_region Logical. If TRUE, plots are generated per region using
#' region. Defaults to FALSE.
#' @param output_dir Character. Directory path where the PDF file will be
#' saved. Must exist. Defaults to ".".
#'
#' @return No return value. A PDF file is created.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' plot_aggregated_AF(data = my_df, by_region = TRUE, output_dir = "plots")
#' }
plot_aggregated_AF <- function(data, by_region = FALSE, output_dir = ".") {
  # input validation
  expected_cols <- c(
    "year",
    "average_attributable_fraction",
    "lower_ci_attributable_fraction",
    "upper_ci_attributable_fraction"
  )
  if (by_region == TRUE) expected_cols <- c(expected_cols, "region")
  if (!all(expected_cols %in% colnames(data))) {
    stop(
      "'data' must contain the following columns: ",
      paste(expected_cols, collapse = ", ")
    )
  }
  if (is.null(output_dir)) stop("'output_dir' is NULL.")
  if (!file.exists(output_dir)) stop("'output_dir' does not exist.")
  # set up plot
  pname <- "aggregated_AN"
  if (by_region) pname <- paste0(pname, "_by_region")
  fpath <- file.path(output_dir, paste0(pname, ".pdf"))
  plots <- list()
  # plot for full dataset
  plots[[1]] <- plot_aggregated_AF_core(data = data, region_name = "All Regions")
  # plot for regions (conditional)
  if (by_region == TRUE) {
    for (region in unique(data$region)) {
      region_data <- data[data$region == region, ]
      plots[[length(plots) + 1]] <- plot_aggregated_AF_core(data = region_data, region_name = region)
    }
  }
  # combine and save plots
  combined_plots <- patchwork::wrap_plots(plots)
  ggplot2::ggsave(
    fpath,
    combined_plots,
    width = length(plots) * 5,
    height = length(plots) * 4,
    limitsize = FALSE
  )
}

#' Create a plot of aggregated annual attributable fractions with CI
#'
#' @description Aggregates annual average attributable fraction estimates and generates a
#' ggplot showing the central estimate and CI.
#'
#' @param data A data frame with columns: year, average_attributable_fraction,
#' lower_ci_attributable_fraction, and upper_ci_attributable_fraction.
#' @param region_name Optional character string used to label the plot title
#' with a region name. Defaults to NULL.
#' @return A ggplot object showing annual attributable rates with confidence
#' intervals.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' plot_aggregated_AF_core(data = my_df, region_name = "Wales")
#' }
plot_aggregated_AF_core <- function(data, region_name = NULL) {
  # aggregate AN/AR data
  agg_data <- data %>%
    group_by(.data$year) %>%
    summarise(
      sum_total_deaths = mean(.data$average_attributable_fraction, na.rm = TRUE),
      lower_ci = mean(.data$lower_ci_attributable_fraction, na.rm = TRUE),
      upper_ci = mean(.data$upper_ci_attributable_fraction, na.rm = TRUE),
    ) %>%
    mutate(year = as.numeric(as.character(.data$year)))
  # create output plot
  title <- "Attributable Mortality Rate to Wildfire smoke-related PM2.5"
  if (!is.null(region_name)) title <- paste0(title, " (", region_name, ")")
  plot_agg_an <- ggplot2::ggplot(
    agg_data,
    ggplot2::aes(x = .data$year, y = .data$sum_total_deaths)
  ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$lower_ci, ymax = .data$upper_ci),
      alpha = 0.2,
      fill = "#4d7789"
    ) +
    ggplot2::geom_line(color = "#003c57", linewidth = 1) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::labs(
      title = title,
      x = "Year",
      y = "Attributable Rate"
    ) +
    ggplot2::theme(
      axis.line = ggplot2::element_line(linewidth = 0.5, colour = "black")
    )
  return(plot_agg_an)
}

#' Join monthly PM2.5 estimates with attributable risk data by region and time
#'
#' @description Aggregates PM2.5 data to monthly averages by region and joins it with
#' attributable risk data using year, month, and region as keys.
#'
#' @param pm_data A data frame with columns: year, month, region, mean_PM.
#' Represents monthly PM2.5 estimates.
#' @param an_ar_data A data frame with columns: year, month, region.
#' Represents attributable risk or fraction data to be joined with PM2.5
#' estimates.
#'
#' @return A data frame with monthly average PM2.5 values joined to attributable
#' risk data.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' join_ar_and_pm_monthly(pm_data = pm_df, an_ar_data = ar_df)
#' }
join_ar_and_pm_monthly <- function(
    pm_data,
    an_ar_data) {
  exp_cols_ar <- c("year", "month", "region")
  exp_cols_pm <- c("year", "month", "region", "mean_PM")
  if (!all(exp_cols_ar %in% colnames(an_ar_data))) {
    stop(paste0(
      "'an_ar_data' requires the columns: ",
      paste(exp_cols_ar, collapse = ", ")
    ))
  }
  if (!all(exp_cols_pm %in% colnames(pm_data))) {
    stop(paste0(
      "'pm_data' requires the columns: ",
      paste(exp_cols_pm, collapse = ", ")
    ))
  }
  monthly_pm25 <- pm_data %>%
    group_by(.data$region, .data$year, .data$month) %>%
    summarise(
      monthly_avg_pm25 = mean(.data$mean_PM, na.rm = TRUE),
      .groups = "drop"
    )
  joined_data <- left_join(
    an_ar_data,
    monthly_pm25,
    by = c("year", "month", "region")
  )
  return(joined_data)
}

#' Plot monthly deaths and PM2.5 concentrations with dual y-axes
#'
#' @description Aggregates data by month and creates a dual-axis plot showing average
#' deaths per 100,000 and mean PM2.5 concentrations.
#'
#' @param data A data frame with columns: month, deaths_per_100k, and
#' monthly_avg_pm25. Month names must match month.abb.
#' @param save_outputs Logical. If TRUE, saves the plot as PNG and the
#' aggregated data as CSV. Defaults to FALSE.
#' @param output_dir Character. Directory path where outputs are saved if
#' save_outputs is TRUE. Must exist. Defaults to NULL.
#'
#' @return No return value. Generates a plot and optionally saves files.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' plot_ar_pm_monthly(data = my_df, save_outputs = TRUE, output_dir = "outputs")
#' }
plot_ar_pm_monthly <- function(data, save_outputs = FALSE, output_dir = NULL) {
  # validate inputs
  if (save_outputs == TRUE && is.null(output_dir)) {
    stop("'output_dir' must be provded to save outputs.")
  }
  if (save_outputs == TRUE && !file.exists(output_dir)) {
    stop("'output_dir' must exist on disk to save outputs.")
  }
  data$month_name <- month.abb[data$month]
  # Aggregate data across all regions by year
  aggregated_data <- data %>%
    group_by(.data$month_name, .data$region) %>%
    summarise(
      mean_deaths_per_100k = mean(.data$deaths_per_100k, na.rm = TRUE),
      mean_pm = mean(.data$monthly_avg_pm25, na.rm = TRUE)
    ) %>%
    mutate(month_name = factor(.data$month_name, levels = month.abb))
  all_regions_agg <- data %>%
    group_by(.data$month_name) %>%
    summarise(
      mean_deaths_per_100k = mean(.data$deaths_per_100k, na.rm = TRUE),
      mean_pm = mean(.data$monthly_avg_pm25, na.rm = TRUE)
    ) %>%
    mutate(month_name = factor(.data$month_name, levels = month.abb))
  all_regions_agg$region <- "All Regions"
  aggregated_data <- rbind(all_regions_agg, aggregated_data)
  # Calculate scaling factor
  scale_factor <- max(aggregated_data$mean_deaths_per_100k) / max(aggregated_data$mean_pm)
  # Plot results
  all_plots <- c()
  for (reg in unique(aggregated_data$region)) {
    region_data <- subset(
      aggregated_data,
      aggregated_data$region == reg
    )
    title <- paste0(
      "Monthly Deaths and Mean PM2.5 Concentration - ",
      reg, " (", min(data$year), " - ", max(data$year), ")"
    )
    plot_ar_pm <- ggplot2::ggplot(
      region_data,
      ggplot2::aes(x = .data$month_name)
    ) +
      ggplot2::geom_bar(
        ggplot2::aes(y = .data$mean_deaths_per_100k),
        stat = "identity",
        fill = "#003c57",
        alpha = 0.7
      ) +
      ggplot2::geom_line(
        ggplot2::aes(y = .data$mean_pm * scale_factor, group = 1),
        color = "red",
        linewidth = 1
      ) +
      ggplot2::geom_point(
        ggplot2::aes(y = .data$mean_pm * scale_factor),
        color = "red", size = 1
      ) +
      ggplot2::scale_y_continuous(
        name = "Deaths per 100,000 population",
        sec.axis = ggplot2::sec_axis(
          ~ . / scale_factor,
          name = "Mean PM2.5 (ug/m^3)"
        )
      ) +
      ggplot2::labs(
        title = title,
        x = "Month"
      ) +
      ggplot2::theme_light() +
      ggplot2::theme(
        axis.line = ggplot2::element_line(linewidth = 0.5, colour = "black")
      )
    all_plots[[length(all_plots) + 1]] <- plot_ar_pm
  }
  combined_plots <- patchwork::wrap_plots(all_plots)
  if (save_outputs) {
    fpath <- file.path(output_dir, "ar_and_pm_monthly_average")
    ggplot2::ggsave(
      paste0(fpath, ".png"),
      combined_plots,
      width = length(all_plots) * 5,
      height = length(all_plots) * 4,
      limitsize = FALSE
    )
    sorted_data <- aggregated_data[
      order(
        aggregated_data$region,
        match(aggregated_data$month_name, month.abb)
      ),
    ]
    sorted_data <- sorted_data %>% select(all_of(c("region", "month_name", "mean_deaths_per_100k", "mean_pm")))
    write.csv(sorted_data, paste0(fpath, ".csv"), row.names = FALSE)
  }
  return(combined_plots)
}

#' Relative risk estimates across PM2.5 concentrations for a specified lag.
#'
#' @description Computes relative risk and confidence intervals across a range
#' of PM2.5 concentrations for a specified wildfire-related lag, using
#' log-linear extrapolation from a reference estimate.
#'
#' @param relative_risk_overall Data frame containing relative risk estimates
#' and confidence intervals for wildfire-related PM2.5 exposure at different
#' lags. Must include columns: 'lag', 'relative_risk', 'ci_lower', and
#' 'ci_upper'.
#' @param scale_factor_wildfire_pm Numeric. Scaling factor used to normalize
#' PM2.5 values to the unit of exposure used in the original relative risk
#' estimate.
#' @param wildfire_lag Integer. Lag day to filter from the input data for
#' extrapolation. Defaults to 0.
#' @param pm_vals Numeric vector. PM2.5 concentrations over which to compute
#' relative risk. Defaults to seq(0, 50, by = 1).
#'
#' @return A dataframe with columns: 'pm_levels', 'relative_risk', 'ci_lower',
#' and 'ci_upper', representing estimated relative risk and 95% confidence
#' intervals across the specified PM2.5 levels.
#'
#' @keywords internal
generate_rr_pm_overall <- function(
    relative_risk_overall,
    scale_factor_wildfire_pm,
    wildfire_lag = 0,
    pm_vals = seq(0, 50, by = 1)) {
  # filter by lag
  data_lagged <- subset(relative_risk_overall, lag == wildfire_lag)
  rr <- data_lagged$relative_risk
  ci_low <- data_lagged$ci_lower
  ci_up <- data_lagged$ci_upper
  # calculate beta and SE
  coef_pm <- log(rr)
  se_pm <- (log(ci_up) - log(ci_low)) / (2 * 1.96)
  # adjust for scale factor
  rr_vals <- exp(coef_pm * (pm_vals / scale_factor_wildfire_pm))
  rr_lower <- exp(
    (coef_pm - 1.96 * se_pm) * (pm_vals / scale_factor_wildfire_pm)
  )
  rr_upper <- exp(
    (coef_pm + 1.96 * se_pm) * (pm_vals / scale_factor_wildfire_pm)
  )
  # create results df
  rr_pm_table <- data.frame(
    pm_levels = pm_vals,
    relative_risk = round(rr_vals, 4),
    ci_lower = round(rr_lower, 4),
    ci_upper = round(rr_upper, 4)
  )
  return(rr_pm_table)
}

#' Generate Relative Risk Estimates by Region
#'
#' @description Computes relative risk estimates for wildfire-specific PM2.5 exposure
#' across regions as PM values changes.
#'
#' @param relative_risk_overall Data frame containing relative risk estimates
#' and confidence intervals for wildfire-related PM2.5 exposure at different
#' lags. Must include columns: 'lag', 'relative_risk', 'ci_lower', and
#' 'ci_upper'.
#' @param scale_factor_wildfire_pm Numeric. Scaling factor used to normalize
#' PM2.5 values to the unit of exposure used in the original relative risk
#' estimate.
#' @param wildfire_lag Integer. Lag day to filter from the input data for
#' extrapolation. Defaults to 0.
#' @param pm_vals Numeric vector. PM2.5 concentrations over which to compute
#' relative risk. Defaults to seq(0, 50, by = 1).
#'
#' @return A data frame with relative risk estimates for each region and PM value.
#'
#' @keywords internal
generate_rr_pm_by_region <- function(
    relative_risk_overall,
    scale_factor_wildfire_pm,
    wildfire_lag = 0,
    pm_vals = seq(0, 50, by = 1)) {
  results <- list()
  regions <- unique(relative_risk_overall$region_name)
  for (reg in regions) {
    region_df <- subset(
      relative_risk_overall,
      relative_risk_overall$region_name == reg
    )
    rr_pm_region <- generate_rr_pm_overall(
      relative_risk_overall = region_df,
      scale_factor_wildfire_pm = scale_factor_wildfire_pm,
      wildfire_lag = wildfire_lag,
      pm_vals = pm_vals
    )
    results[[reg]] <- cbind(
      region_name = reg,
      rr_pm_region
    )
  }
  results_all <- do.call(rbind, results)
  row.names(results_all) <- NULL
  return(results_all)
}

#' Plot relative risk by PM2.5 levels for all regions and individually
#'
#' @description Generates one or more plots showing relative risk estimates across PM2.5
#' levels. If multiple regions are present, plots are created per region and
#' for all regions combined. Optionally saves the output as a PDF.
#'
#' @param data A data frame with columns: pm_levels, relative_risk, ci_lower,
#' ci_upper, and region.
#' @param save_fig Logical. If TRUE, saves the plot(s) as a PDF file in
#' output_dir. Defaults to FALSE.
#' @param output_dir Character. Directory path where the PDF file will be
#' saved if save_fig is TRUE. Must exist. Defaults to NULL.
#'
#' @return No return value. Generates one or more plots and optionally saves
#' them to disk.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' plot_rr_by_pm(data = rr_df, save_fig = TRUE, output_dir = "outputs")
#' }
plot_rr_by_pm <- function(
    data,
    save_fig = FALSE,
    output_dir = NULL) {
  # input validation
  if (save_fig == TRUE && is.null(output_dir)) {
    stop("'output_dir' must be provided to save outputs.")
  }
  if (save_fig == TRUE && !file.exists(output_dir)) {
    stop("'output_dir' must exist on disk to save outputs.")
  }
  exp_cols <- c(
    "pm_levels",
    "relative_risk",
    "ci_lower",
    "ci_upper",
    "region_name"
  )
  if (!all(exp_cols %in% colnames(data))) {
    stop("'data' must contain these columns: ", paste0(exp_cols, collapse = ", "))
  }
  # Collect Plots
  all_plots <- list()
  ymax <- max(data$ci_upper, na.rm = TRUE) + 0.1
  ymin <- min(data$ci_lower, na.rm = TRUE) - 0.1
  # Plot region is more than one region is available
  for (region in unique(data$region_name)) {
    p <- plot_rr_by_pm_core(
      data = data[data$region_name == region, ],
      region_name = as.character(region),
      ylims = c(ymin, ymax)
    )
    all_plots[[length(all_plots) + 1]] <- p
  }
  # combine and save
  combined_plots <- patchwork::wrap_plots(all_plots)
  if (save_fig) {
    fpath <- file.path(output_dir, "rr_by_pm.pdf")
    ggplot2::ggsave(
      fpath,
      combined_plots,
      width = if (length(combined_plots) == 1) 8 else 5 * length(combined_plots),
      height = if (length(combined_plots) == 1) 8 else 4 * length(combined_plots),
      limitsize = FALSE
    )
  }
  return (combined_plots)
}

#' Create a relative risk plot across PM2.5 levels for a single region
#'
#' @description Generates a ggplot showing relative risk estimates and confidence intervals
#' across PM2.5 levels for a given region.
#'
#' @param data A data frame with columns: pm_levels, relative_risk, ci_lower,
#' and ci_upper.
#' @param region_name Optional character string used to label the plot title
#' with a region name. Defaults to "All Regions".
#' @param ylims Numeric vector of length 2 specifying y-axis limits. Defaults to
#' c(-2, 2).
#'
#' @return A ggplot object showing relative risk and CI.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' plot_rr_by_pm_core(data = rr_df, region_name = "Wales")
#' }
plot_rr_by_pm_core <- function(
    data,
    region_name = "All Regions",
    ylims = c(-2, 2)) {
  # create plot object
  title <- paste0("All-cause mortality", " (", region_name, ")")
  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data$pm_levels, y = .data$relative_risk)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$ci_lower, ymax = .data$ci_upper),
      alpha = 0.2,
      fill = "#4d7789"
    ) +
    ggplot2::geom_line(color = "#003c57", linewidth = 1) +
    ggplot2::scale_y_continuous(
      limits = ylims,
      labels = scales::number_format(accuracy = 0.01)
    ) +
    ggplot2::labs(title = title, x = "PM2.5 (ug/m3)", y = "Relative Risk") +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      axis.line = ggplot2::element_line(linewidth = 0.5, colour = "black"),
      plot.background = ggplot2::element_rect(color = "#222222", linewidth = 1),
      panel.border = ggplot2::element_rect(
        color = "#222222",
        fill = NA,
        linewidth = 0.5
      )
    )
  return(p)
}

#' Plot Attributable Risk by Region
#'
#' @description Aggregates wildfire-specific PM2.5 attributable risk (deaths per 100k)
#' by region and creates a bar plot showing the mean attributable risk per region.
#'
#' @param data A data frame containing columns:
#'   \itemize{
#'     \item \code{region}: Region names.
#'     \item \code{deaths_per_100k}: Numeric values of deaths per 100k population.
#'     \item \code{lower_ci_deaths_per_100k}: Lower bound of confidence interval.
#'     \item \code{upper_ci_deaths_per_100k}: Upper bound of confidence interval.
#'   }
#' @param output_dir A character string specifying the directory where the plot will be saved.
#'   Defaults to the current working directory (\code{"."}).
#'
#' @return A \code{ggplot} object representing the bar plot.
#'
#' @keywords internal
plot_ar_by_region <- function(data, output_dir = ".") {
  # validation
  if (is.null(output_dir)) stop("'output_dir' required.")
  if (!file.exists(output_dir)) stop("'output_dir' does not exist.")
  exp_cols <- c(
    "region",
    "deaths_per_100k",
    "lower_ci_deaths_per_100k",
    "upper_ci_deaths_per_100k"
  )
  if (!(all(exp_cols %in% colnames(data)))) {
    stop(
      "'data' must contain the following columns: ",
      paste(exp_cols, collapse = ", ")
    )
  }
  # aggregate dataset
  aggregated_data <- data %>%
    group_by(.data$region) %>%
    summarise(
      mean_deaths_per_100k = mean(.data$deaths_per_100k, na.rm = TRUE),
      mean_lower_ci_deaths_per_100k = mean(
        .data$lower_ci_deaths_per_100k,
        na.rm = TRUE
      ),
      mean_upper_ci_deaths_per_100k = mean(
        .data$upper_ci_deaths_per_100k,
        na.rm = TRUE
      )
    )
  # draw plot and save
  p <- ggplot2::ggplot(
    aggregated_data,
    ggplot2::aes(
      x = forcats::fct_reorder(
        .data$region,
        .data$mean_deaths_per_100k,
        .desc = TRUE
      ),
      y = .data$mean_deaths_per_100k
    )
  ) +
    ggplot2::geom_col(fill = "#003c57") +
    ggplot2::labs(
        title = "Deaths per 100k attributable to Wildfire-specific PM2.5",
        x = "Regions",
        y = "Deaths per 100k population"
    ) +
    ggplot2::theme_minimal(base_family = "sans") +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      axis.text = ggplot2::element_text(color = "black"),
      axis.title = ggplot2::element_text(color = "black"),
      plot.title = ggplot2::element_text(color = "black"),
      axis.line = ggplot2::element_line(linewidth = 0.5, colour = "black"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
  # save plot
  ggplot2::ggsave(
    file.path(output_dir, "ar_by_region.png"),
    p,
    width = 8,
    height = 6
  )
  return(p)
}

#' Plot Total Attributable Number by Region
#'
#' @description Aggregates wildfire smoke-related PM2.5 attributable numbers by region and
#' creates a bar plot showing the total attributable number of deaths per region.
#'
#' @param data A data frame containing columns:
#'   \itemize{
#'     \item \code{region}: Region names.
#'     \item \code{total_attributable_number}: Numeric values of attributable numbers.
#'   }
#' @param output_dir A character string specifying the directory where the plot will be saved.
#'   Defaults to the current working directory (\code{"."}).
#'
#' @return A \code{ggplot} object representing the bar plot.
#'
#' @keywords internal
plot_an_by_region <- function(data, output_dir = ".") {
  # validation
  if (is.null(output_dir)) stop("'output_dir' required.")
  if (!file.exists(output_dir)) stop("'output_dir' does not exist.")
  exp_cols <- c(
    "region",
    "total_attributable_number"
  )
  if (!(all(exp_cols %in% colnames(data)))) {
    stop(
      "'data' must contain the following columns: ",
      paste(exp_cols, collapse = ", ")
    )
  }
  # aggregate dataset
  aggregated_data <- data %>%
    group_by(.data$region) %>%
    summarise(
      sum_total_an = sum(.data$total_attributable_number, na.rm = TRUE),
    )
  # draw plot and save
  p <- ggplot2::ggplot(
    aggregated_data,
    ggplot2::aes(
      x = forcats::fct_reorder(.data$region, .data$sum_total_an, .desc = TRUE),
      y = .data$sum_total_an
    )
  ) +
    ggplot2::geom_col(fill = "#003c57") +
    ggplot2::labs(
      title = "Total attributable number of deaths to Wildfire smoke-related PM2.5",
      x = "Regions",
      y = "Attributable Number of Deaths"
    ) +
    ggplot2::theme_minimal(base_family = "sans") +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      axis.text = ggplot2::element_text(color = "black"),
      axis.title = ggplot2::element_text(color = "black"),
      plot.title = ggplot2::element_text(color = "black"),
      axis.line = ggplot2::element_line(linewidth = 0.5, colour = "black"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )

  # save plot
  ggplot2::ggsave(
    file.path(output_dir, "an_by_region.png"),
    p,
    width = 8,
    height = 6
  )
  return(p)
}


#' This is full analysis pipeline to analyse the impact of wildfire-related PM2.5 on a health
#' outcome.
#'
#' @description Runs full analysis pipeline for analysis of the impact of
#' wildfire-related PM2.5 on a health outcome using time stratified case-crossover approach
#' with conditional quasi-Poisson regression model. This function generates
#' relative risk of the mortality associated to wildfire-related PM2.5 as well as
#' attributable numbers, rates and fractions of health outcome. Model validation
#' statistics are also provided.
#'
#' @param health_path Path to a CSV file containing a daily time series of data
#' for a particular health outcome, which may be disaggregated by region. If
#' this does not include a column with wildfire-related PM2.5, use
#' join_wildfire_data = TRUE to join these data.
#' @param join_wildfire_data Boolean. If TRUE, a daily time series of
#' wildfire-related PM2.5 concentration is joined to the health data. If FALSE,
#' the data set is loaded without any additional joins. Defaults to FALSE.
#' @param ncdf_path Path to a NetCDF file containing a daily time series of
#' gridded wildfire-related PM2.5 concentration data.
#' @param shp_path Path to a shapefile .shp of the geographical boundaries for
#' which to extract mean values of wildfire-related PM2.5
#' @param date_col Character. Name of the column in the dataframe that contains
#' the date.
#' @param region_col Character. Name of the column in the dataframe that contains
#' the region names.
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
#' in micrograms. This is only required if health data isn't joined. Defaults to NULL.
#' @param wildfire_lag Integer. The number of days for which to calculate the
#' lags for wildfire PM2.5. Default is 3.
#' @param temperature_lag Integer. The number of days for which to calculate
#' the lags for temperature. Default is 1.
#' @param spline_temperature_lag Integer. The number of days of lag in the
#' temperature variable from which to generate splines. Default is 0 (unlagged
#' temperature variable).
#' @param spline_temperature_degrees_freedom Integer. Degrees of freedom for the
#' spline(s).
#' @param predictors_vif Character vector with each of the predictors to
#' include in the model. Must contain at least 2 variables. Defaults to NULL.
#' @param calc_relative_risk_by_region Bool. Whether to calculate Relative Risk by region.
#' Default: FALSE
#' @param scale_factor_wildfire_pm Numeric. The value to divide the wildfire
#' PM2.5 concentration variables by for alternative interpretation of outputs.
#' Corresponds to the unit increase in wildfire PM2.5 to give the model
#' estimates and relative risks (e.g. scale_factor = 10 corresponds to estimates
#' and relative risks representing impacts of a 10 unit increase in wildfire
#' PM2.5). Setting this parameter to 0 or 1 leaves the variable unscaled.
#' @param save_fig Boolean. Whether to save the plot as an output.
#' @param save_csv Boolean. Whether to save the results as a CSV
#' @param output_folder_path Path. Path to folder where plots and/or CSV should
#' be saved.
#' @param print_vif Bool, whether or not to print VIF (variance inflation factor)
#' for each predictor. Defaults to FALSE.
#' @param print_model_summaries Bool. Whether to print the model summaries to
#' console. Defaults to FALSE.
#'
#' @details This analysis pipeline requires a daily time series with mean wildfire PM2.5,
#' mean temperature and health outcome (all-cause mortality, respiratory, cardiovascular,
#' hospital admissions etc) with population values as a minimum.
#' This is then processed using a time stratified case crossover approach with conditional
#' Poisson case-crossover analysis and optional meta-analysis.
#' Meta-analysis is recommended if the input data is disaggregated by area.
#'
#' The model parameters have default values, which are recommended to keep as based
#' on existing studies. However, if desired these can be adjusted for sensitivity
#' analysis.
#'
#' Model validation testing is provided as a standard output from the pipeline so
#' a user can assess the quality of the model. Additionally, users can incorporate
#' extra independent variables—such as relative humidity or wind speed—directly
#' into the model for enhanced analysis.
#'
#' Further details on the input data requirements, methodology, quality information
#' and guidance on interpreting outputs can be found in the accompanying published
#' \href{https://doi.org/10.5281/zenodo.14052184}{Zenodo documentation}.
#'
#' @references to be updated
#'
#' @returns
#' \itemize{
#'   \item `rr_results` A dataframe with relative risk estimates and confidence
#'   intervals for each region.
#'   \item `rr_pm` A dataframe of relative risk estimates for wildfire-specific PM2.5 exposure
#'   across regions as PM values changes.
#'   \item `af_an_results` A dataframe containing attributable fractions, attributable numbers
#'   and deaths per 100k population for each region
#'   \item `annual_af_an_results`A dataframe containing annual attributable numbers and fractions
#'    for each region
#'   \item `calculate_qaic` A dataframe of QAIC and dispersion metrics for each model
#'   combination and geography.
#'   \item `check_wildfire_vif` A dataframe containing Variance inflation factors for
#'   each independent variables by region.
#' }
#'
#' @export
wildfire_do_analysis <- function(
    health_path,
    join_wildfire_data = FALSE,
    ncdf_path = NULL,
    shp_path = NULL,
    date_col,
    region_col,
    mean_temperature_col,
    health_outcome_col,
    rh_col = NULL,
    wind_speed_col = NULL,
    pm_2_5_col = NULL,
    wildfire_lag = 3,
    temperature_lag = 1,
    spline_temperature_lag = 0,
    spline_temperature_degrees_freedom = 6,
    predictors_vif = NULL,
    calc_relative_risk_by_region = FALSE,
    scale_factor_wildfire_pm = 10,
    save_fig = FALSE,
    save_csv = FALSE,
    output_folder_path = NULL,
    print_vif = FALSE,
    print_model_summaries = FALSE) {
  # Setup additional output DIR
  if (save_fig == TRUE && !file.exists(file.path(output_folder_path, "model_validation"))) {
    dir.create(file.path(output_folder_path, "model_validation"), recursive = TRUE)
  }
  # Read and combine datasets
  data <- load_wildfire_data(
    health_path = health_path,
    ncdf_path = ncdf_path,
    shp_path = shp_path,
    join_wildfire_data = join_wildfire_data,
    date_col = date_col,
    region_col = region_col,
    mean_temperature_col = mean_temperature_col,
    health_outcome_col = health_outcome_col,
    rh_col = rh_col,
    wind_speed_col = wind_speed_col,
    pm_2_5_col = pm_2_5_col
  )
  # Create raw pm dataframe for late
  pm_data <- data[c("month", "year", "region", "mean_PM")]
  # Create lagged variables
  data <- create_lagged_variables(
    data = data,
    wildfire_lag = wildfire_lag,
    temperature_lag = temperature_lag
  )
  # Create splines
  data <- create_temperature_splines(
    data = data,
    nlag = spline_temperature_lag,
    degrees_freedom = spline_temperature_degrees_freedom
  )
  # Stratify data by time period
  data <- time_stratify(data = data)
  # Calculate QAIC
  calculate_qaic(
    data = data,
    save_csv = save_csv,
    output_folder_path = output_folder_path,
    print_results = print_model_summaries
  )
  # Conditionally check VIF
  if (!is.null(predictors_vif)) {
    check_wildfire_vif(
      data = data,
      predictors = predictors_vif,
      save_csv = save_csv,
      output_folder_path = output_folder_path,
      print_vif = print_vif
    )
  }
  # Obtain and plot RR values
  rr_results <- calculate_wildfire_rr_by_region(
    data = data,
    scale_factor_wildfire_pm = scale_factor_wildfire_pm,
    calc_relative_risk_by_region = calc_relative_risk_by_region,
    save_fig = save_fig,
    output_folder_path = output_folder_path,
    print_model_summaries = print_model_summaries
  )
  plot_RR(
    rr_data = rr_results,
    wildfire_lag = wildfire_lag,
    by_region = calc_relative_risk_by_region,
    save_fig = save_fig,
    output_folder_path = output_folder_path
  )
  # Plot RR by PM2.5 levels
  rr_pm <- generate_rr_pm_by_region(
    relative_risk_overall = rr_results,
    scale_factor_wildfire_pm = scale_factor_wildfire_pm,
    wildfire_lag = wildfire_lag,
    pm_vals = seq(0, 50, by = 2)
  )
  plot_rr_by_pm(
    data = rr_pm,
    save_fig = save_fig,
    output_dir = output_folder_path
  )
  # Obtain and plot attributable numbers/fractions
  af_an_results <- NULL
  annual_af_an_results <- NULL
  ar_pm_monthly <- NULL
  if (calc_relative_risk_by_region == TRUE) {
    # get AN/AR
    daily_AF_AN <- calculate_daily_AF_AN(data = data, rr_data = rr_results)
    af_an_results <- summarise_AF_AN(data = daily_AF_AN)
    annual_af_an_results <- summarise_AF_AN(data = daily_AF_AN, monthly = FALSE)
    # Plot aggregated AN for all regions and individual regions
    # Plot AR and PM monthly values
    ar_pm_monthly <- join_ar_and_pm_monthly(pm_data, af_an_results)
    plot_ar_pm_monthly(ar_pm_monthly, save_fig, output_folder_path)
    # Plot AR/AN by region
    if (save_fig == TRUE) {
      plot_aggregated_AF(af_an_results, TRUE, output_folder_path)
      plot_ar_by_region(
        data = af_an_results,
        output_dir = output_folder_path
      )
      plot_an_by_region(
        data = af_an_results,
        output_dir = output_folder_path
      )
    }
  }
  # Save outputs (conditionally)
  if (save_csv == TRUE) {
    save_wildfire_results(
      rr_results = rr_results,
      an_ar_results = af_an_results,
      annual_af_an_results = annual_af_an_results,
      output_folder_path = output_folder_path
    )
  }
  return(list(
    RR_results = rr_results,
    AF_AN_results = af_an_results,
    AR_PM_monthly = ar_pm_monthly
  ))
}
