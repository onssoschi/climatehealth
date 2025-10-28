# Code for producing analysis surrounding wildfire related health outcomes

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
    lag_cols <- paste0(lagcol, "_l", 0:nlags)
    # create lagged columns
    for (i in seq_along(lag_cols)) {
        data[[lag_cols[i]]] <- dplyr::lag(data[[lagcol]], i)
    }
    # get mean(s)
    for (i in seq_along(lag_cols)) {
        data[[paste0(lagcols[i], "_mean")]] <- rowMeans(data[lag_cols])
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

#' Stratify data by time period
#'
#' @description Adds columns for strata for each region:year:month:dayofweek
#' and for the total counts of a health outcome across days in each stratum.
#'
#' @param data Dataframe containing a daily time series of climate and health
#' data. Assumes that 'data' has a 'month', 'year', 'dow' and 'regnames' column.
#'
#' @returns Dataframe with additional columns for stratum
#' (region:year:month:dayofweek) and for the total counts of a health outcome
#' across days in each stratum.
#'
#' @keywords internal
time_stratify <- function(data) {
    # validate columns
    required_cols <- c("month", "year", "dow", "regnames", "health_outcome")
    if (!all(required_cols %in% colnames(data))) stop(
        paste("data must include columns:", paste(required_col, collapse=", "))
    )
    # Create stratified columns
    df <- split(data, f = data$regnames)
    for (i in seq(df)) {
        # Convert column to factors
        df[[i]]$month <- as.factor(df[[i]]$month)
        df[[i]]$year  <- as.factor(df[[i]]$year)
        df[[i]]$dow   <- as.factor(df[[i]]$dow)
        df[[i]]$reg_name_strata <- as.factor(
            stringr::str_replace_all(df[[i]]$regnames, " ", "_")
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
    output_dir = "."
) {
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

#' Creates a scatter plot.
#'
#' @description Produces a ggplot2 scatterplot of two variables x versus y.
#'
#' @param data Dataframe containing a daily time series of climate and health
#' data
#' @param xvar x variable
#' @param yvar y variable
#'
#' @returns Prints a ggplot2 scatterplot of x versus y
#'
#' @keywords internal
plot_scatter <- function(data, xvar, yvar) {
  ggplot2::ggplot(data = data, ggplot2::aes(x = {{xvar}}, y = {{yvar}})) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth() +
    ggplot2::theme_bw()
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
#' @param print_vif Bool, whether or not to print VIF for each predictor.
#' Defaults to FALSE.
#'
#' @returns Variance inflation factor statistics for each predictor variable.
#'
#' @keywords internal
check_wildfire_vif <- function(data, predictors, print_vif = FALSE) {
    # input validation
    if (!is.character(predictors) || !is.vector(predictors)) {
        stop("Please provide predictor variable names as a character vector")
    }
    if (length(predictors) < 2) {
        stop("Please provide at least two predictor variables")
    }
    # Get VIF
    formula <- paste("health_outcome ~", paste(predictors, collapse = "+"))
    model <- lm(formula, data = data)
    vif_mod <- car::vif(model)
    if (print_vif) print(paste0("Variance inflation factor:\n", vif_mod))
    for (var in names(vif_mod)) {
        if (vif_mod[[var]] >= 2) {
            warning(paste0(
                "Variance inflation factor for ", var, " is >= 2. Investigation is",
                " suggested."
            ))
        }
    }
    return(vif_mod)
}

get_wildfire_lag_columns <- function(data) {
    lags <- c("mean_PM_FRP")
    lag_nums <- c("mean_PM_FRP" = 0)
    lagged_wf_cols <- grep(
        "^mean_PM_FRP_l\\d+_mean$", colnames(data), value = TRUE
    )
    if (length(lagged_wf_cols)) {
        matched_lags <- as.integer(
            sub("^mean_PM_FRP_l(\\d+)_mean$", "\\1", matched_cols)
        )
        lags <- c(lags, matched_cols)
        lag_nums <- c(lag_nums, setNames(matched_lags, matched_cols))
    }
    return(list(col_names=lags, lag_nums=lag_nums))
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
#' and relative risks representing impacts of a 10 unit increase in wildfire 
#' PM2.5). Setting this parameter to 0 or 1 leaves the variable unscaled.
#' @param wildfire_lag Integer. The maximum number of days for which to 
#' calculate lagged results for wildfire PM2.5. Default is 3.
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
    save_fig = TRUE,
    output_folder_path = NULL,
    print_model_summaries = TRUE
) {
    # scale wildfire PM if neccessary
    if (scale_factor_wildfire_pm > 0) {
        data <- data %>%
        dplyr::mutate(
            dplyr::across(
            "mean_PM_FRP" | matches("^mean_PM_FRP_l\\d+_mean$"),
            ~ . / scale_factor_wildfire_pm
            )
        )
    }
    # define lag columns
    lag_cols <- get_wildfire_lag_columns(data=data)
    lags <- lag_cols$col_names
    lag_nums <- lag_cols$lag_nums
    # get results
    results <- list()
    if (save_fig==T) {
        grid <- create_grid(length(lags))
        output_path <- file.path(
            output_folder_path,
            "wildfires_residuals_vs_fit_plot.pdf"
        )
        pdf(output_path, width=grid[1]*4, height=grid[2]*4)
        par(mfrow=c(grid[1],  grid[2]))
    }
    for (i in lags) {
        number <- lag_nums[[i]]
        # create model
        formula <- as.formula(
            paste("health_outcome ~ splines::ns(tmean, df = 6) +", i)
        )
        model <- gnm::gnm(
            formula,
            data = data,
            family = quasipoisson,
            subset = ind > 0,
            eliminate = stratum
        )
        # print summaries if required
        if (print_model_summaries) {
            print(Epi::ci.exp(model, subset = i))
            print(summary(model))
            print(paste0(
                Epi::ci.exp(model, subset = i), "\n",
                summary(model),
                "\nRatio of residual deviance to degrees of freedom: ",
                model$deviance / model$df.residual
            ))
        }
        # get residuals and plot (if needed)
        devresid <- resid(model, type = "deviance")
        if (save_fig==T) {
            plot(devresid ~ model$fitted.values, main = i, col="#f25574")
        }
        coef_pm <- summary(model)$coefficients[i, "Estimate"]
        se_pm <- summary(model)$coefficients[i, "Std. Error"]

        relative_risk <- exp(coef_pm)
        ci_lower <- exp(coef_pm - 1.96 * se_pm)
        ci_upper <- exp(coef_pm + 1.96 * se_pm)

        results[[i]] <- c(lag = number, relative_risk = relative_risk,
                        ci_lower = ci_lower, ci_upper = ci_upper)
    }
    # save figure
    if (save_fig==TRUE) {
        dev.off()
    }
    # create results df and return
    results <- as.data.frame(do.call(rbind, results))
    rownames(results) <- NULL
    return(results)
}

#' QAIC calculation
#' 
#' @param data Dataframe containing a daily time series of climate and health
#' data from which to fit models.
#' @param print_results Logical. Whether or not to print model summaries and 
#' pearson dispersion statistics. Defaults to FALSE.
#'
#' @returns Dataframe containing QAIC results for each lag.
#'
#' @keywords internal 
calculate_qaic <- function(
    data,
    print_results = FALSE
) {

    qaic_results <- list()
    # get WF lag columns
    lag_cols <- get_wildfire_lag_columns(data=data)
    lags <- lag_cols$col_names
    lag_nums <- lag_cols$lag_nums
    for (i in lags) {
        # define model
        number <- lag_nums[[i]]
        formula <- as.formula(
            paste("health_outcome ~ splines::ns(tmean, df = 6) +", i)
        )
        model <- gnm::gnm(formula,
                        data = data,
                        family = quasipoisson,
                        subset = ind > 0,
                        eliminate = stratum)
        pearson_chisq <- sum(residuals(model, type = "pearson")^2, na.rm = TRUE)
        dispersion <- pearson_chisq / model$df.residual
        # Number of estimated parameters
        k <- length(coef(model))
        # Dispersion parameter
        phi <- summary(model)$dispersion
        # Log-likelihood approximation
        ll <- -0.5*model$deviance
        # QAIC formula
        qaic <- -2 * ll + 2 * phi * k
        if (print_results==TRUE) {
            print(paste("QAIC for", i, "=", qaic))
            print(paste("Pearson dispersion statistic:", round(dispersion,3)))
        }
        qaic_results[[i]] <- qaic
    }
    return(qaic_results)
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
    pm_vals = seq(0, 50, by = 1)
) {
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

#' Plot relative risk results by region.
#'
#' @description Plots relative risk and confidence intervals for each lag value
#' of wildfire-related PM2.5.
#'
#' @param results Dataframe of relative risk and confidence intervals for
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
    wildfire_lag = 3,
    by_region = FALSE,
    save_fig = FALSE,
    output_folder_path = NULL
) {
    # input validation
    if (save_fig && is.null(output_folder_path)) {
        stop("No output path provided when save_fig==T.")
    }
    # plot by region if needed
    if (relative_risk_by_region) {
        df_list <- split(rr_data, f = rr_data$region_name)
        plots_list <- list()
        if (save_fig) {
            pdf(
                file.path(output_folder_path, "wildfire_rr_by_region.pdf"),
                width = 8, height = 8
            )
        }
        for (i in seq(df_list)) {
            region_results <- df_list[[i]]
            region_name <- region_results$region_name[1]
            region_plot <- plot_RR_core(
                rr_data = region_results,
                output_folder_path = output_folder_path,
                wildfire_lag = wildfire_lag,
                save_fig = FALSE,
                region_name = region_name
            )
            plots_list[[i]] <- region_plot
            print(region_plot)
        }
        if (save_fig) dev.off()
        return(plots_list)

    }
    # Plot overall RR
    plot <- plot_RR_core(
        rr_data = rr_data,
        output_folder_path = output_folder_path,
        wildfire_lag = wildfire_lag,
        save_fig = save_fig
    )
    return(plot)
}

#' Core functionality for plotting results of relative risk analysis.
#'
#' @description Plots relative risk and confidence intervals for each lag value
#' of wildfire-related PM2.5.
#'
#' @param results Dataframe of relative risk and confidence intervals for
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
    wildfire_lag = 3,
    output_folder_path = NULL,
    region_name = "All regions"
) {
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
    # plot RR data
    plot <- ggplot2::ggplot(
        data = rr_data,
        ggplot2::aes(
            x = lag, y = relative_risk, ymin = ci_lower, ymax = ci_upper
        )
    ) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_errorbar(width = 0.5, size = 1) +
    ggplot2::geom_hline(yintercept = 1, lty = 2) +
    ggplot2::xlab("Lag") +
    ggplot2::ylab("Relative risk") +
    ggplot2::ggtitle(paste("Wildfire PM2.5: ", region_name, sep = "")) +
    ggplot2::scale_x_continuous(
        breaks = seq(0, wildfire_lag, 1), labels = labels
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
        axis.text = ggplot2::element_text(size = 18),
        axis.title = ggplot2::element_text(size = 18)
    )
    # save figure if neccessary
    if (save_fig == TRUE) {
        # Create ouutput path and PDF
        formatted_region_name <- gsub(" ", "_", tolower(region_name))
        file_name <- paste(
            "wildfire_rr_", formatted_region_name, ".pdf", sep = ""
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
#' @param results Dataframe of relative risk and confidence intervals for
#' each lag of wildfire-related PM2.5.
#' @param an_ar_results Dataframe containing attributable number/fraction
#' results. Defaults to NULL.
#' @param output_folder_path Path to folder where results should be saved.
#'
#' @keywords internal
save_wildfire_results <- function(
    rr_results,
    an_ar_results = NULL,
    output_folder_path
) {
    # Input validation
    if (is.null(output_folder_path)) stop("Output directory required.")
    if (!file.exists(output_folder_path)) stop("Output directory does not exist")
    # Save RR results
    write.csv(
        rr_results, file = file.path(output_folder_path, "wildfire_rr.csv")
    )
    # conditionally save AN/AR results
    if (!is.null(an_ar_results)) {
        write.csv(
            an_ar_results,
            file = file.path(output_folder_path, "wildfire_an_ar.csv")
        )
    }
}

#' Passes data to casecrossover_quasipoisson to calculate RR.
#'
#' @description Splits data by region if relative_risk_by_region==TRUE.
#' If T, data for each individual region is passed to casecrossover_quasipoisson
#' to calculate RR by region. If false RR is calculated for the entire dataset.
#'
#' @param data Dataframe containing a daily time series of climate and health
#' data from which to fit models.
#' @param scale_factor_wildfire_pm Numeric. The value to divide the wildfire 
#' PM2.5 concentration variables by for alternative interpretation of outputs.
#' Corresponds to the unit increase in wildfire PM2.5 to give the model
#' estimates and relative risks (e.g. scale_factor = 10 corresponds to estimates
#' and relative risks representing impacts of a 10 unit increase in wildfire 
#' PM2.5). Setting this parameter to 0 or 1 leaves the variable unscaled.
#' @param wildfire_lag Integer. The maximum number of days for which to calculate
#' lagged results for wildfire PM2.5. Defaults to 3.
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
calculate_wilfire_rr_by_region <- function(
    data,
    scale_factor_wildfire_pm,
    wildfire_lag,
    calc_relative_risk_by_region = FALSE,
    save_fig = FALSE,
    output_folder_path = NULL,
    print_model_summaries = FALSE
) {
    # input validation
    if (save_fig && is.null(output_folder_path)) {
        stop("No output path provided when save_fig==T.")
    }
    if (calc_relative_risk_by_region && !("regnames" %in% names(data))) {
        stop("data must contain 'regnames' column for region level RR data.")
    }
    if(calc_relative_risk_by_region) {
        # split dataset and create output list
        df_list <- split(data, f = data$regnames)
        results_list <- list()
        # Get region level RR data
        for (i in seq(df_list)) {
            region_data <- df_list[[i]]
            region_name <- names(df_list)[i]
            region_results <- casecrossover_quasipoisson(
                data = region_data,
                scale_factor_wildfire_pm = scale_factor_wildfire_pm,
                wildfire_lag = wildfire_lag,
                output_folder_path = output_folder_path,
                save_fig = save_fig,
                print_model_summaries = print_model_summaries
            )
            region_results$region_name <- region_name
            results_list[[i]] <- region_results
        }
        # combine all regions and return
        results_all <- do.call(rbind, results_list)
        row.names(results_all) <- NULL
        return(results_all)

    }
    # calculate and return dataset level RR
    results <- casecrossover_quasipoisson(
        data=data,
        scale_factor_wildfire_pm=scale_factor_wildfire_pm,
        wildfire_lag=wildfire_lag,
        output_folder_path=output_folder_path,
        save_fig=save_fig,
        print_model_summaries=print_model_summaries
    )
    return(results)
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
calculate_daily_AF_AN <- function(data, rr_data){
    # dissagregate data into region level
    df_list <- split(data, f = data$regnames)
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
                rescaled_RR = exp((log(.data$RR_value) / 10) * .data$mean_PM_FRP),
                attributable_fraction = (.data$rescaled_RR - 1) / .data$rescaled_RR,
                attributable_number = .data$attributable_fraction * .data$health_outcome
            )
        # Repeat for upper/lower CIs
        region_data <- region_data %>%
            mutate(
                rescaled_CI_upper = exp((log(.data$RR_CI_upper) / 10) * .data$mean_PM_FRP),
                attributable_fraction_upper = (.data$rescaled_CI_upper - 1) / .data$rescaled_CI_upper,
                attributable_number_upper = .data$attributable_fraction_upper * .data$health_outcome
            )
        region_data <- region_data %>%
            mutate(
                rescaled_CI_lower = exp((log(.data$RR_CI_lower) / 10) *.data$ mean_PM_FRP),
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
#'
#' @returns Dataframe containing summarised AF and AN data, by year and region.
#'
#' @keywords internal
summarise_AF_AN <- function(data) {
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
    yearly_summary <- data %>%
        group_by(regnames, year, month) %>%
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
            se_AF_mean = sqrt(variance_AF_mean),
            lower_ci_attributable_fraction = .data$average_attributable_fraction - z * .data$se_AF_mean,
            upper_ci_attributable_fraction = .data$average_attributable_fraction + z * .data$se_AF_mean,
            # Deaths per 100k
            deaths_per_100k = (.data$total_attributable_number / .data$population) * 100000,
            lower_ci_deaths_per_100k = (.data$lower_ci_attributable_number / .data$population) * 100000,
            upper_ci_deaths_per_100k = (.data$upper_ci_attributable_number / .data$population) * 100000
        )
    return(yearly_summary)
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
#' @param rh_col Character. Name of the column containing relative humidity 
#' values. Defaults to NULL.
#' @param wind_speed_col Character. Name of the column containing wind speed.
#' Defaults to NULL.
#' @param pm_2_5_col Character. The name of the column containing PM2.5 values
#' in micrograms. Thsi is only required if health data isn't joined. Defaults to NULL.
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
#' @param output_AF_AN Bool. Whether to create attributable fraction and
#' attributable number output. Only works id relative_risk_by_region = TRUE.
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
#' @returns Dataframe of relative risk and confidence intervals for
#' each lag of wildfire-related PM2.5
#'
#' @return rr_results, af_an_results
#' \itemize{
#'   \item `rr_results` A dataframe with relative risk estimates and confidence
#'   intervals for each region.
#'   \item `af_an_results` A dataframe containing attributable fractions and numbers for each region
#' }
#'
#' @export
wildfire_do_analysis <- function(
    health_path,
    join_wildfire_data = TRUE,
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
    print_model_summaries = FALSE
) {
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
    # Conditionally check VIF
    if (!is.null(predictors_vif)) {
        check_wildfire_vif(
            data = data,
            predictors = predictors_vif,
            print_vif = print_vif
        )
    }
    # Obtain and plot RR values
    rr_results <- calculate_wilfire_rr_by_region(
        data = data,
        scale_factor_wildfire_pm = scale_factor_wildfire_pm,
        wildfire_lag = wildfire_lag,
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
    # Obtain and plot attributable numbers/fractions
    af_an_results = NULL
    if (calc_relative_risk_by_region) {
        daily_AF_AN <- calculate_daily_AF_AN(data = data, rr_data = rr_results)
        af_an_results <- summarise_AF_AN(data = daily_AF_AN)
    }
    # TODO: add plotting
    # Save outputs (conditionally)
    if (save_csv == TRUE) {
        save_wildfire_results(
            rr_results = rr_results,
            an_ar_results = af_an_results,
            output_folder_path = output_folder_path
        )
    }
    return(list(rr_results, af_an_results))
}