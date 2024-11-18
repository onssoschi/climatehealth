#' Read in health data and rename columns
#'
#' @description Reads in a CSV file for a daily time series of health data and
#' renames columns to standard names. This function creates day of week, month,
#' and year columns derived from the date.
#'
#' @param health_path Path to a CSV file containing a daily time series of data
#' for a particular health outcome, which may be disaggregated by region.
#' @param date_col Character. Name of the column in the dataframe that contains
#' the date. Date column should be in YYYY-MM-DD or YYYY/MM/DD format.
#' @param region_col Character. Name of the column in the dataframe that contains
#' the region names.
#' @param mean_temperature_col Character. Name of the column in the dataframe
#' that contains the mean temperature column.
#' @param health_outcome_col Character. Name of the column in the dataframe that
#' contains the health outcome count column (e.g. number of deaths, hospital
#' admissions)
#'
#' @returns Dataframe with formatted and renamed columns

read_and_format_data <- function(health_path,
                                 date_col,
                                 region_col = NULL,
                                 mean_temperature_col,
                                 health_outcome_col) {

  df <- climatehealth::read_input_data(health_path)

  if (is.null(region_col)) {

    df <- df %>%
      dplyr::mutate(regnames = "no_region")

  } else {

    df <- df %>%
      dplyr::rename(regnames = region_col)

  }

  df <- df %>%
    dplyr::rename(date = date_col,
                  tmean = mean_temperature_col,
                  health_outcome = health_outcome_col) %>%
    dplyr::mutate(date = lubridate::ymd(date),
                  year = lubridate::year(date),
                  month = lubridate::month(date),
                  day = lubridate::day(date),
                  dow = as.character(lubridate::wday(date, label = TRUE)))

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
#' @param ncdf_path Path to a NetCDF file
#' @param shp_path Path to a shapefile .shp of the geographical boundaries for
#' which to extract mean values of wildfire-related PM2.5
#'
#' @returns Dataframe containing a daily time series with mean wildfire-related
#' PM2.5 values for each region

extract_means_for_geography <- function(ncdf_path,
                                        shapefile_path) {

  # Convert NetCDF to raster and get layers (time steps)
  nc <- ncdf4::nc_open(ncdf_path)
  nc_raster <- terra::rast(ncdf_path)
  time <- ncdf4::ncvar_get(nc, "time")
  time_origin <- ncdf4::ncatt_get(nc, "time", "units")$value
  match <- regexpr("\\d{4}-\\d{2}-\\d{2}", time_origin)
  origin_date <- regmatches(time_origin, match)
  time_obs <- as.POSIXct(time,
                         origin = origin_date,
                         tz = "GMT")
  names(nc_raster) <- time_obs

  # Read shapefile and convert to same Coordinate Reference System (CRS) as
  # raster layer
  shp <- sf::st_read(shapefile_path)
  shp <- sf::st_transform(shp, raster::crs(nc_raster))

  #TODO: Generalise code below to allow for shapefiles with different column
  # names

  # Extract mean values within shapefile regions from raster
  extracted <- cbind(shp, exactextractr::exact_extract(nc_raster, shp, 'mean'))
  extracted[ , c("RGN23CD", "BNG_E", "BNG_N", "LONG",
                 "LAT", "GlobalID", "geometry")] <- list(NULL)

  extracted_long <- reshape2::melt(extracted, 'RGN23NM')
  extracted_long <- extracted_long %>%
    dplyr::rename("date" = "variable",
                  "mean_PM_FRP" = "value")

  extracted_long$date <- stringr::str_sub(extracted_long$date,
                                          start = 6,
                                          end = 15)
  extracted_long$date <- as.Date(extracted_long$date, "%Y.%m.%d")

  return(extracted_long)

}

#' Join health and climate data
#'
#' @description Joins a daily time series of climate data with a daily time
#' series of health data.
#'
#' @param climate_data Dataframe containing a daily time series of climate data,
#' which may be disaggregated by region.
#' @param health_path Path to a CSV file containing a
#' daily time series of data for a particular health outcome, which may be
#' disaggregated by region.
#' @param date_col Character. Name of the column in the dataframe that contains
#' the date.
#' @param region_col Character. Name of the column in the dataframe that contains
#' the region names.
#' @param mean_temperature_col Character. Name of the column in the dataframe
#' that contains the mean temperature column.
#' @param health_outcome_col Character. Name of the column in the dataframe that
#' contains the health outcome count column (e.g. number of deaths, hospital
#' admissions)
#'
#' @returns Dataframe containing a daily time series of the joined
#' climate and health data.

pair_with_health <- function(climate_data,
                             health_path,
                             date_col,
                             region_col = NULL,
                             mean_temperature_col,
                             health_outcome_col) {

  df <- read_and_format_data(health_path = health_path,
                             date_col = date_col,
                             region_col = region_col,
                             mean_temperature_col = mean_temperature_col,
                             health_outcome_col = health_outcome_col)

  df_paired_all <- dplyr::left_join(df, climate_data,
                                    by = c('regnames' = 'RGN23NM',
                                           'date' = 'date'))

  #TODO: output print information to a file
  # Joins will fail if CSV has wider date range or regions do not match
  print(paste0("Number of joins: ", sum(!is.na(df_paired_all$mean_PM_FRP))))
  print(paste0("Number of failed joins: ",
               sum(is.na(df_paired_all$mean_PM_FRP))))

  # Remove failed joins
  df_paired <- df_paired_all %>%
    dplyr::filter(!is.na(mean_PM_FRP))

  df_paired[is.finite(df_paired$mean_PM_FRP), ]

  df_paired <- df_paired %>%
    dplyr::mutate(mean_PM_FRP = (mean_PM_FRP * 1e9), # convert kg to microgram
                  regnames = as.factor(regnames))

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
#' @param join_wildfire_data Boolean. If TRUE, a daily time series of
#' wildfire-related PM2.5 concentration is joined to the health data. If FALSE,
#' the data set is loaded without any additional joins.
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
#'
#' @returns Dataframe containing a daily time series of
#' climate and health data.

load_wildfire_data <- function(health_path,
                               join_wildfire_data,
                               ncdf_path,
                               shp_path,
                               date_col = date_col,
                               region_col = NULL,
                               mean_temperature_col,
                               health_outcome_col) {

  if (!file.exists(health_path)) {

    stop("Health CSV file does not exist:", health_path)

  }

  wildfire_data <- tryCatch(

    {

      read_and_format_data(health_path = health_path,
                           date_col = date_col,
                           region_col = region_col,
                           mean_temperature_col = mean_temperature_col,
                           health_outcome_col = health_outcome_col)

    },

    error = function(e) {

      stop("Failed to read health CSV file: ", e$message)

    }

  )

  if (join_wildfire_data == FALSE) {

    return(wildfire_data)

  }

  df_zonal <- extract_means_for_geography(ncdf_path = ncdf_path,
                                            shapefile_path = shp_path)

  wildfire_data <- pair_with_health(climate_data = df_zonal,
                                    health_path = health_path,
                                    date_col = date_col,
                                    region_col = region_col,
                                    mean_temperature_col = mean_temperature_col,
                                    health_outcome_col = health_outcome_col)

  return(wildfire_data)
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

create_lagged_variables <- function(data,
                                    wildfire_lag = 3,
                                    temperature_lag = 1) {

  df_list <- split(data, f = data$regnames)

  for (i in seq(df_list)) {

    region_data <- df_list[[i]]

    for (num in 0:wildfire_lag) {

      region_data[[paste("wildfire_pm_l", num, sep = "")]] <-
        dplyr::lag(region_data$mean_PM_FRP, num)

    }

    for(num in 0:wildfire_lag) {

      wildfire_cols <- paste0("wildfire_pm_l", 0:num)
      region_data[[paste0("l", num, "_mean_wildfire_pm")]] <-
        rowMeans(region_data[wildfire_cols])

    }

    for (num in 0:temperature_lag) {

      region_data[[paste("tmean_l", num, sep = "")]] <-
        dplyr::lag(region_data$tmean, num)

    }

    for(num in 0:temperature_lag) {

    tmean_cols <- paste0("tmean_l", 0:num)
    region_data[[paste0("l", num, "_tmean")]] <-
      rowMeans(region_data[tmean_cols])

    }

    df_list[[i]] <- region_data

  }

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
#' @param temperature_lag Integer. The number of days of lag in the temperature
#' variable from which to generate splines. Default is 0 (unlagged temperature
#' variable).
#' @param df Integer. Degrees of freedom for the spline(s).
#'
#' @returns Dataframe with additional columns for temperature spline

create_temperature_splines <- function(data,
                                       temperature_lag = 0,
                                       degrees_freedom = 6) {

  temperature_column <- paste0("l", temperature_lag, "_tmean")

  df_list <- split(data, f = data$regnames)

  for (i in seq(df_list)) {

    region_data <- df_list[[i]]

    region_data$ns.tmean <- splines::ns(region_data[[temperature_column]],
                                        df = degrees_freedom)

    df_list[[i]] <- region_data

  }

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
#' data
#'
#' @returns Dataframe with additional columns for stratum
#' (region:year:month:dayofweek) and for the total counts of a health outcome
#' across days in each stratum.

time_stratify <- function(data) {

  df <- split(data, f = data$regnames)

  for (i in seq(df)) {

    df[[i]]$month <- as.factor(df[[i]]$month)
    df[[i]]$year  <- as.factor(df[[i]]$year)
    df[[i]]$dow   <- as.factor(df[[i]]$dow)
    df[[i]]$reg_name_strata <- as.factor(
      stringr::str_replace_all(df[[i]]$regnames, " ", "_")
      )
    df[[i]]$stratum <- with(df[[i]],
                              as.factor(reg_name_strata:year:month:dow))
    df[[i]]$ind <- tapply(df[[i]]$health_outcome,
                          df[[i]]$stratum,
                          sum)[df[[i]]$stratum]

    df[[i]] <- df[[i]] %>%
      dplyr::select(-X)
  }

  df_all <- do.call(rbind, df)
  row.names(df_all) <- NULL

  return(df_all)
  }

#' Show descriptive statistics
#'
#' @description Generates summary statistics for climate and health data.
#'
#' @param data Dataframe containing a daily time series of climate and health
#' data
#' @param variables Character or character vector with variable to produce
#' summary statistics for. Must include at least 1 variable.
#' @param bin_width Integer. Width of each bin in a histogram of the outcome
#' variable. Default is 5.
#'
#' @returns Prints summary statistics and a histogram of the the outcome
#' variable

descriptive_stats <- function(data,
                              variables,
                              bin_width = 5) {
  #TODO: output plot to a file
  graphics::hist(data$health_outcome,
                 breaks = seq(0, max(data$health_outcome) + bin_width,
                              by = bin_width),
                 main = "Health outcome",
                 xlab = "Health outcome")

  for (i in seq_along(variables)) {
    #TODO: output print information to a file
    variable_name <- variables[i]
    print(variable_name)
    print(summary(data[[variable_name]]))
    cat("\n")

  }
}

#' Scatterplot
#'
#' @description Produces a ggplot2 scatterplot of two variables x versus y.
#'
#' @param data Dataframe containing a daily time series of climate and health
#' data
#' @param xvar x variable
#' @param yvar y variable
#'
#' @returns Prints a ggplot2 scatterplot of x versus y

plot_variables <- function(data, xvar, yvar) {

  ggplot2::ggplot(data = data, ggplot2::aes(x = {{xvar}}, y = {{yvar}})) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth() +
    ggplot2::theme_bw()

}

#' Check variance inflation factors of predictor variables using a linear model
#'
#' @description Checks variance inflation factors of predictor variables using a
#' linear model of the predictor variables on the health outcome.
#'
#' @param data Dataframe containing a daily time series of climate and health
#' data.
#' @param predictors Character vector with each of the predictors to include
#' in the model. Must contain at least 2 variables.
#'
#' @returns Prints variance inflation factors for each predictor variable.

check_vif <- function(data, predictors) {

  if (!is.character(predictors)) {
    stop("Please provide predictor variable names as a character vector")
  }

  if (length(predictors) < 2) {
    stop("Please provide at least two predictor variables")
  }

  formula <- paste("health_outcome ~", paste(predictors, collapse = "+"))

  model <- lm(formula, data = data)
  vif_mod <- car::vif(model)
  #TODO: output print information to a file
  print("Variance inflation factor:")
  print(vif_mod)
}

#' Fit quasipoisson regression models for different lags using a time-stratified
#' case-crossover approach.
#'
#' @description Fits quasipoisson regression models using \link[gnm]{gnm}
#'
#' @param data Dataframe containing a daily time series of climate and health
#' data from which to fit models.
#' @param scale_factor Numeric. The value to divide the wildfire PM2.5
#' concentration variables by for alternative interpretation of outputs.
#' Corresponds to the unit increase in wildfire PM2.5 to give the model
#' estimates and relative risks (e.g. scale_factor = 10 corresponds to estimates
#' and relative risks representing impacts of a 10 unit increase in wildfire PM2.5)
#' Setting this parameter to 0 or 1 leaves the variable unscaled.
#' @param wildfire_lag Integer. The maximum number of days for which to calculate
#' lagged results for wildfire PM2.5. Default is 3.
#'
#' @returns Dataframe of relative risk and confidence intervals for
#' each lag of wildfire-related PM2.5

casecrossover_quasipoisson <- function(data,
                                       scale_factor = 10,
                                       wildfire_lag = 3) {

  if (scale_factor > 0) {
    data <- data %>%
      dplyr::mutate(
        dplyr::across(
          "mean_PM_FRP" | dplyr::ends_with("mean_wildfire_pm"),
          ~ . / scale_factor
        )
      )
  }

  lags <- c("mean_PM_FRP")
  lag_nums <- c("mean_PM_FRP" = 0)

  if (wildfire_lag > 0) {
    additional_lags <- sapply(1:wildfire_lag,
                                function(lag) paste("l",
                                                    lag,
                                                    "_mean_wildfire_pm",
                                                    sep = ""))
    additional_lag_nums <- setNames(1:wildfire_lag, paste("l",
                                                          1:wildfire_lag,
                                                          "_mean_wildfire_pm",
                                                          sep = ""))
    lags <- c(lags, additional_lags)
    lag_nums <- c(lag_nums, additional_lag_nums)
  }

  results <- list()

  for (i in lags) {

    number <- lag_nums[[i]]

    formula <- as.formula(paste("health_outcome ~ ns.tmean +", i))

    model <- gnm::gnm(formula,
                      data = data,
                      family = quasipoisson,
                      subset = ind > 0,
                      eliminate = stratum)
    #TODO: output print information to a file
    print(Epi::ci.exp(model, subset = i))
    print(summary(model))

    print(paste("Ratio of residual deviance to degrees of freedom:",
                model$deviance / model$df.residual))

    devresid <- resid(model, type = "deviance")
    #TODO: output plot to a file
    plot(devresid ~ model$fitted.values)

    coef_pm <- summary(model)$coefficients[i, "Estimate"]
    se_pm <- summary(model)$coefficients[i, "Std. Error"]

    relative_risk <- exp(coef_pm)
    ci_lower <- exp(coef_pm - 1.96 * se_pm)
    ci_upper <- exp(coef_pm + 1.96 * se_pm)

    results[[i]] <- c(lag = number, relative_risk = relative_risk,
                      ci_lower = ci_lower, ci_upper = ci_upper)
  }

  results <- as.data.frame(do.call(rbind, results))
  rownames(results) <- NULL

  return(results)

}

#' Plot results of analysis
#'
#' @description Plots relative risk and confidence intervals for each lag value
#' of wildfire-related PM2.5
#'
#' @param results Dataframe of relative risk and confidence intervals for
#' each lag of wildfire-related PM2.5
#' @param save_fig Boolean. Whether to save the plot as an output.
#' @param wildfire_lag Integer. The maximum number of days for which to plot the
#' lags for wildfire PM2.5. Default is 3.
#' @param output_folder_path Path to folder where plots should be saved.
#'
#' @returns Plot of relative risk and confidence intervals for each lag of
#' wildfire-related PM2.5

plot_results <- function(results,
                         save_fig,
                         wildfire_lag = 3,
                         output_folder_path) {

  labels <- c("0 days")

  if (wildfire_lag > 0) {
    additional_labels <- sapply(1:wildfire_lag,
                                function(lag) paste("0-", lag, " days", sep = ""))
    labels <- c(labels, additional_labels)
    }

  plot <- ggplot2::ggplot(data = results, ggplot2::aes(x = lag, y = relative_risk,
                                      ymin = ci_lower, ymax = ci_upper)) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_errorbar(width = 0.5, size = 1) +
    ggplot2::geom_hline(yintercept = 1, lty = 2) +
    ggplot2::xlab("Lag") +
    ggplot2::ylab("Relative risk") +
    ggplot2::ggtitle("Wildfire PM2.5") +
    ggplot2::scale_x_continuous(breaks = seq(0, wildfire_lag, 1), labels = labels) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text = ggplot2::element_text(size = 18),
                   axis.title = ggplot2::element_text(size = 18))

  if (save_fig == TRUE) {

    if (!is.null(output_folder_path)) {
      pdf(paste(output_folder_path, "wildfire_plot.pdf", sep = ""),
          width = 8, height = 8)
      print(plot) # NOTE: this print() is required to produce the plot pdf
      dev.off()
      climatehealth::check_file_exists(paste(
        output_folder_path, "wildfire_plot.pdf", sep = ""))
    }

  }
  return(plot)
}

#' Save results of analysis
#'
#' @description Saves a CSV file of relative risk and confidence intervals for
#' each lag value of wildfire-related PM2.5
#'
#' @param results Dataframe of relative risk and confidence intervals for
#' each lag of wildfire-related PM2.5
#' @param output_folder_path Path to folder where results should be saved.

save_results <- function(results,
                         output_folder_path) {

  if (!is.null(output_folder_path)) {

    write.csv(results, file = paste(
      output_folder_path, "wildfire_results.csv", sep = "")
      )
    climatehealth::check_file_exists(paste(
      output_folder_path, "wildfire_results.csv", sep = ""))

  } else {

    stop("Output path not specified")

  }
}

#' Run pipeline to analyse the impact of wildfire-related PM2.5 on a health
#' outcome using a time-stratified case-crossover approach.
#'
#' @description Runs full analysis pipeline for analysis of the impact of
#' wildfire-related PM2.5 on a health outcome.
#'
#' @param health_path Path to a CSV file containing a daily time series of data
#' for a particular health outcome, which may be disaggregated by region. If
#' this does not include a column with wildfire-related PM2.5, use
#' join_wildfire_data = TRUE to join these data.
#' @param join_wildfire_data Boolean. If TRUE, a daily time series of
#' wildfire-related PM2.5 concentration is joined to the health data. If FALSE,
#' the data set is loaded without any additional joins.
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
#' @param wildfire_lag Integer. The number of days for which to calculate the
#' lags for wildfire PM2.5. Default is 3.
#' @param temperature_lag Integer. The number of days for which to calculate
#' the lags for temperature. Default is 1.
#' @param spline_temperature_lag Integer. The number of days of lag in the
#' temperature variable from which to generate splines. Default is 0 (unlagged
#' temperature variable).
#' @param spline_temperature_degrees_freedom Integer. Degrees of freedom for the
#' spline(s).
#' @param variables_descriptive_stats Character or character vector with
#' variable to produce summary statistics for. Must include at least 1 variable.
#' @param bin_width_histogram Integer. Width of each bin in a histogram of the
#' outcome variable. Default is 5.
#' @param predictors_vif Character vector with each of the predictors to
#' include in the model. Must contain at least 2 variables.
#' @param scale_factor_wildfire_pm Numeric. The value to divide the wildfire
#' PM2.5 concentration variables by for alternative interpretation of outputs.
#' Corresponds to the unit increase in wildfire PM2.5 to give the model
#' estimates and relative risks (e.g. scale_factor = 10 corresponds to estimates
#' and relative risks representing impacts of a 10 unit increase in wildfire
#' PM2.5). Setting this parameter to 0 or 1 leaves the variable unscaled.
#' @param save_fig Boolean. Whether to save the plot as an output.
#' @param save_csv Boolean. Whether to save the results as a CSV
#' @param output_folder_path Path to folder where plots and/or CSV should be
#' saved.
#'
#' @returns Dataframe of relative risk and confidence intervals for
#' each lag of wildfire-related PM2.5

wildfire_do_analysis <- function(health_path,
                                 join_wildfire_data,
                                 ncdf_path,
                                 shp_path,
                                 date_col,
                                 region_col,
                                 mean_temperature_col,
                                 health_outcome_col,
                                 wildfire_lag = 3,
                                 temperature_lag = 1,
                                 spline_temperature_lag = 0,
                                 spline_temperature_degrees_freedom = 6,
                                 variables_descriptive_stats,
                                 bin_width_histogram = 10,
                                 predictors_vif,
                                 scale_factor_wildfire_pm = 10,
                                 save_fig = TRUE,
                                 save_csv = TRUE,
                                 output_folder_path) {

  data <- load_wildfire_data(health_path = health_path,
                             join_wildfire_data = join_wildfire_data,
                             ncdf_path = ncdf_path,
                             shp_path = shp_path,
                             date_col = date_col,
                             region_col = region_col,
                             mean_temperature_col = mean_temperature_col,
                             health_outcome_col = health_outcome_col)

  data <- create_lagged_variables(data = data,
                                  wildfire_lag = wildfire_lag,
                                  temperature_lag = temperature_lag)

  data <- create_temperature_splines(data = data,
                                     temperature_lag = spline_temperature_lag,
                                     degrees_freedom = spline_temperature_degrees_freedom)

  data <- time_stratify(data = data)

  descriptive_stats(data = data,
                    variables = variables_descriptive_stats,
                    bin_width = bin_width_histogram)

  check_vif(data = data,
            predictors = predictors_vif)

  results <- casecrossover_quasipoisson(data = data,
                                        scale_factor = scale_factor_wildfire_pm,
                                        wildfire_lag = wildfire_lag)

  plot_results(results = results,
               output_folder_path = output_folder_path,
               wildfire_lag = wildfire_lag,
               save_fig = save_fig)

  if (save_csv == TRUE) {
    save_results(results = results,
                 output_folder_path = output_folder_path)
  }

  return(results)
}
