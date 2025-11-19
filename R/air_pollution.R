#-------------------------------------------------------------------------------
#' @title R-code for I1: All-cause mortality attributable to Outdoor PM2.5.
#-------------------------------------------------------------------------------

#' Read in climate, environmental and health data and rename columns
#'
#' @description Reads in a CSV file for a daily time series of climate, environmental
#' and health data and renames them to standardised names. This function
#' creates year, month, day, and day of week columns derived from the date.
#'
#' @param data_path Path to a CSV file containing a daily time series of data.
#' @param date_col Character. Name of date column in the dataframe with format
#' YYYY-MM-DD. Defaults to "date".
#' @param region_col Character. Name of region column in the dataframe. Defaults
#' to "region".
#' @param pm25_col Character. Name of PM2.5 column in the dataframe. Defaults to
#' "pm25".
#' @param deaths_col Character. Name of all-cause mortality column in the dataframe 
#' (Note that deaths_col variable has value 1 for each recorded death). 
#' 'Defaults to "deaths"
#' @param humidity_col Character. Name of humidity column in the dataframe. Defaults
#' to "humidity".
#' @param precipitation_col Character. Name of precipitation column in the dataframe.
#' Defaults to "precipitation".
#' @param tmax_col Character. Name of maximum temperature column in the dataframe.
#' Defaults to "tmax".
#' @param tmean_col Character. Name of mean temperature column in the dataframe.
#' Defaults to NULL.
#' @param wind_speed_col Character. Name of wind speed column in the dataframe.
#' Defaults to "wind_speed".
#' @param population_col Character. Name of population column in the dataframe.
#' Defaults to "population".
#' @param age_col Character. Name of age column in the dataframe. Defaults to NULL.
#' @param sex_col Character. Name of sex column in the dataframe. Defaults to NULL.
#' @param urbanisation_col Character. Name of urbanisation column in the dataframe.
#' Defaults to NULL.
#'
#' @return Dataframe with formatted and renamed with standardized column names.
#'
#' @export
load_air_pollution_data <- function(data_path,
                                    date_col = "date",
                                    region_col = "region",
                                    pm25_col = "pm25",
                                    deaths_col = "deaths",
                                    humidity_col = "humidity",
                                    precipitation_col = "precipitation",
                                    tmax_col = "tmax",
                                    tmean_col = NULL,
                                    wind_speed_col = "wind_speed",
                                    population_col = "population",
                                    age_col = NULL,
                                    sex_col = NULL,
                                    urbanisation_col = NULL) {
  
  if (!file.exists(data_path)) {
    stop("Data file not found at: ", data_path)
  }
  
  data <- if(is.character(data_path)) read.csv(data_path) else data_path
  
  # Rename columns with validation
  if(date_col != "date" && date_col %in% names(data)) {
    data <- data %>% rename(date = !!rlang::sym(date_col))
  }
  if(region_col != "region" && region_col %in% names(data)) {
    data <- data %>% rename(region = !!rlang::sym(region_col))
  }
  if(pm25_col != "pm25" && pm25_col %in% names(data)) {
    data <- data %>% rename(pm25 = !!rlang::sym(pm25_col))
  }
  if(deaths_col != "deaths" && deaths_col %in% names(data)) {
    data <- data %>% rename(deaths = !!rlang::sym(deaths_col))
  }
  if(humidity_col != "humidity" && humidity_col %in% names(data)) {
    data <- data %>% rename(humidity = !!rlang::sym(humidity_col))
  }
  if(precipitation_col != "precipitation" && precipitation_col %in% names(data)) {
    data <- data %>% rename(precipitation = !!rlang::sym(precipitation_col))
  }
  if(tmax_col != "tmax" && tmax_col %in% names(data)) {
    data <- data %>% rename(tmax = !!rlang::sym(tmax_col))
  }
  if(!is.null(tmean_col) && tmean_col != "tmean" && tmean_col %in% names(data)) {
    data <- data %>% rename(tmean = !!rlang::sym(tmean_col))
  }
  if(wind_speed_col != "wind_speed" && wind_speed_col %in% names(data)) {
    data <- data %>% rename(wind_speed = !!rlang::sym(wind_speed_col))
  }
  if(population_col != "population" && population_col %in% names(data)) {
    data <- data %>% rename(population = !!rlang::sym(population_col))
  }
  
  # Handle age column with intelligent grouping
  if(!is.null(age_col) && age_col %in% names(data)) {
    data <- data %>% rename(age = !!rlang::sym(age_col))
    data <- as.data.table(data)
    
    # Check if age is numeric (not grouped)
    if(is.numeric(data$age)) {
      # Group the numeric ages
      data[, age := fifelse(age <= 5, "0-5",
                            fifelse(age < 60, "6-59", "60+"))]
    }
    # If age is not numeric (already grouped), keep the original grouping
  } else if(!("age" %in% names(data))) {
    # Create default grouping if no age column exists
    data <- data %>% mutate(age = "all")
  }
  
  # Handle sex column
  if(!is.null(sex_col) && sex_col %in% names(data)) {
    data <- data %>% rename(sex = !!rlang::sym(sex_col))
  } else if(!("sex" %in% names(data))) {
    data <- data %>% mutate(sex = "both")
  }
  
  # Handle urbanisation column
  if(!is.null(urbanisation_col) && urbanisation_col %in% names(data)) {
    data <- data %>% rename(urbanisation = !!rlang::sym(urbanisation_col))
  } else if(!("urbanisation" %in% names(data))) {
    data <- data %>% mutate(urbanisation = "mixed")
  }
  
  # Convert age, sex, urbanisation, region to factor
  cols_to_factor <- c("age", "sex", "urbanisation", "region")
  for (col in cols_to_factor) {
    if (col %in% names(data)) data[[col]] <- as.factor(data[[col]])
  }
  
  # Enhanced date parsing function
  universal_date <- function(x) {
    lubridate::parse_date_time(
      x,
      orders = c("ymd", "dmy", "mdy", "Ymd", "dmY", "mdY", 
                 "Y/m/d", "d/m/Y", "m/d/Y", "Y-m-d H:M:S", "d/m/Y H:M", "m/d/Y H:M")
    ) %>% as.Date()
  }
  
  data <- data %>% mutate(date = universal_date(date))
  
  # Convert to data.table for efficient aggregation
  data <- as.data.table(data)
  
  # Define grouping variables
  group_vars <- c("date", "region", "urbanisation", "sex", "age")
  
  # Define variables to aggregate (conditional on tmean presence)
  agg_vars <- c("pm25", "population", "tmax", "precipitation", "humidity", "wind_speed")
  if ("tmean" %in% names(data)) {
    agg_vars <- c(agg_vars, "tmean")
  }
  
  # Perform aggregation
  data <- data[, c(
    list(deaths = sum(deaths, na.rm = TRUE)),
    lapply(.SD, function(x) round(mean(x, na.rm = TRUE), 2))
  ), by = group_vars, .SDcols = agg_vars]
  
  # Sort and select columns
  data <- data %>% arrange(region, date)
  
  essential_cols <- c("region", "urbanisation", "date", agg_vars, "population", 
                      "deaths", "age", "sex")
  data <- data %>% select(all_of(essential_cols))
  
  # Data quality check
  missing_summary <- sapply(data, function(x) sum(is.na(x)))
  print(missing_summary[missing_summary > 0])
  
  return(data)
}


#' Create lagged values for PM2.5 variable and average lag column.
#'
#' @description Creates new variables in a dataframe for lags and means over lag
#' periods.
#'
#' @param data Dataframe containing a daily time series of health and environmental data.
#' @param max_lag Integer. The maximum lag days for outdoor PM2.5. Defaults to 2.
#'
#' @return Dataframe with added columns for lagged PM2.5 concentration.
#'
#' @export
create_air_pollution_lags <- function(data,
                                      max_lag = 2) {
  
  # Validate input
  if (!("pm25" %in% names(data))) {
    stop("pm25 column not found in data")
  }
  
  if (!("region" %in% names(data))) {
    stop("region column not found in data")
  }
  
  if (max_lag < 1) {
    stop("max_lag must be at least 1")
  }
  
  initial_rows <- nrow(data)
  
  data_with_lags <- data %>%
    group_by(region) %>%
    arrange(date)
  
  # Create individual lag variables
  for (i in 1:max_lag) {
    lag_name <- paste0("pm25_lag", as.character(i))
    data_with_lags <- data_with_lags %>%
      mutate(!!lag_name := lag(pm25, i))
  }
  
  lag_vars <- paste0("pm25_lag", 1:max_lag)
  all_lag_vars <- c("pm25", lag_vars)
  avg_lag_name <- paste0("pm25_lag0_", max_lag) 
  
  data_with_lags <- data_with_lags %>%
    mutate(!!avg_lag_name := rowMeans(across(all_of(all_lag_vars)), na.rm = FALSE))
  
  # Filter out incomplete cases
  data_with_lags <- data_with_lags %>%
    filter(if_all(all_of(all_lag_vars), ~!is.na(.))) %>%
    ungroup()
  
  final_rows <- nrow(data_with_lags)
  removed_rows <- initial_rows - final_rows
  
  # Create time variables
  data_with_lags <- data_with_lags %>%
    arrange(region, date) %>%
    group_by(region) %>%
    mutate(
      year = lubridate::year(date),
      month = lubridate::month(date),
      day = lubridate::day(date),
      dow = as.character(lubridate::wday(date, label = TRUE)),
      time = row_number()   # sequential per region
    ) %>%
    ungroup()
  
  # Get PM25 columns and validate they exist
  pm25_cols <- names(data_with_lags)[grepl("pm25", names(data_with_lags), ignore.case = TRUE)]
  
  # Select essential columns dynamically based on tmean availability
  if ("tmean" %in% names(data_with_lags)){
    essential_cols <- c("region", "urbanisation", "date", "year", "month", "day", 
                        "dow", "time", pm25_cols, "humidity", "precipitation", "tmax", 
                        "tmean", "wind_speed", "population", "deaths", "age", "sex")
  } else {
    essential_cols <- c("region", "urbanisation", "date", "year", "month", "day", 
                        "dow", "time", pm25_cols, "humidity", "precipitation", "tmax", 
                        "wind_speed", "population", "deaths", "age", "sex")
  }
  
  data_with_lags <- data_with_lags %>% select(all_of(essential_cols))
  
  return(data_with_lags)
}


#' Show descriptive statistics
#'
#' @description Generates summary statistics for climate, environmental and health data.
#'
#' @param data Dataframe containing a daily time series of climate, environmental
#' and health data
#' @param output_dir Character. Directory to save descriptive statistics.
#' Defaults to NULL.
#' @param save_outputs Logical. Whether to save outputs. Defaults to FALSE.
#' @param moving_average_window Numeric. Window size for moving average calculations.
#' Defaults to 3 (3-day moving average).
#'
#' @export
air_pollution_descriptive_stats <- function(data,
                                            save_outputs = FALSE,
                                            output_dir = NULL,
                                            moving_average_window = 3) {
  
  # Validate moving_average_window parameter
  if (!is.numeric(moving_average_window) || moving_average_window < 1) {
    stop("moving_average_window must be a positive integer")
  }
  
  # Warning for insufficient window size
  if (moving_average_window < 3) {
    warning("Window < 3 provides minimal smoothing. ",
            "Consider using window >= 3 for better results.")
  }
  
  moving_average_window <- as.integer(moving_average_window)
  
  # Define variables dynamically based on tmean availability
  has_tmean <- "tmean" %in% names(data)
  
  # Base environmental variables (always included)
  base_vars <- c("pm25", "humidity", "precipitation", "tmax", "wind_speed")
  
  # Add tmean if available
  env_vars <- if (has_tmean) c(base_vars, "tmean") else base_vars
  
  # Essential columns for analysis
  essential_cols <- c("region", "date", "deaths", env_vars)
  
  # Create national dataset
  data_n <- data %>% 
    select(all_of(essential_cols)) %>%
    group_by(date) %>%
    summarise(
      across(deaths, ~sum(., na.rm = TRUE)),
      across(where(is.numeric) & !deaths, ~mean(., na.rm = TRUE)),
      .groups = 'drop'
    )
  
  # Create regional dataset
  data_r <- data %>% 
    select(all_of(essential_cols)) %>%
    group_by(region, date) %>%
    summarise(
      across(deaths, ~sum(., na.rm = TRUE)),
      across(where(is.numeric) & !deaths, ~mean(., na.rm = TRUE)),
      .groups = 'drop'
    )
  
  # Check input params
  if (save_outputs && is.null(output_dir)) {
    stop("An output directory must be passed if save_outputs==T.")
  }
  
  # Create output dir
  if (save_outputs && !file.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # 1. HISTOGRAM OF MORTALITY
  if (save_outputs) {
    png(file.path(output_dir, "mortality_histogram.png"), width = 800, height = 600)
  }
  
  mortality_range <- range(data_n$deaths, na.rm = TRUE)
  breaks <- seq(floor(mortality_range[1]), ceiling(mortality_range[2]) + 1, by = 1)
  
  p <- ggplot(data_n, aes(x = deaths)) +
    geom_histogram(
      breaks = breaks,
      fill = "#1F77B4",
      color = "white", 
      alpha = 0.8
    ) +
    labs(
      title = "All-Cause Mortality (Countrywide)",
      subtitle = "Distribution of recorded deaths across all provinces",
      x = "Number of Deaths",
      y = "Frequency"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
      axis.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(linetype = "dashed", color = "gray85")
    )
  print(p)
  
  if (save_outputs) {
    dev.off()
  }
  
  # 2. SCATTER PLOTS
  if (save_outputs) {
    png(file.path(output_dir, "scatter_plots.png"), width = 1000, height = 800)
  }
  
  # Create scatter plots dynamically for all environmental variables
  plots <- list()
  for (i in seq_along(env_vars)) {
    xvar <- env_vars[i]
    plots[[i]] <- ggplot2::ggplot(data = data_n, ggplot2::aes(x = .data[[xvar]], y = deaths)) +
      ggplot2::geom_point(alpha = 0.6) +
      ggplot2::geom_smooth(method = "loess", color = "red") +
      ggplot2::ggtitle(paste("Deaths against", xvar)) +
      ggplot2::theme_bw()
  }
  
  # Arrange in grid dynamically
  n_plots <- length(plots)
  ncol <- ifelse(n_plots >= 3, 2, 1)
  nrow <- ceiling(n_plots / ncol)
  
  grid_plot_scatter <- grid.arrange(grobs = plots, nrow = nrow, ncol = ncol)
  print(grid_plot_scatter)
  
  if (save_outputs) {
    dev.off()
  }
  
  # 3. SUMMARY STATISTICS TABLE
  # Calculate statistics for all environmental variables dynamically
  stat_vars <- c("deaths", env_vars)
  
  regional_stats <- data_r %>%
    group_by(region) %>%
    summarise(
      across(all_of(stat_vars), 
             list(
               min = ~round(min(.x, na.rm = TRUE), 2),
               mean = ~round(mean(.x, na.rm = TRUE), 2),
               max = ~round(max(.x, na.rm = TRUE), 2),
               sd = ~round(sd(.x, na.rm = TRUE), 2),
               IQR = ~round(IQR(.x, na.rm = TRUE), 2),
               missing = ~round(sum(is.na(.x))/length(.x)*100, 2)
             ),
             .names = "{.col}_{.fn}"),
      .groups = 'drop'
    )
  
  national_stats <- data_n %>%
    summarise(
      across(all_of(stat_vars), 
             list(
               min = ~round(min(.x, na.rm = TRUE), 2),
               mean = ~round(mean(.x, na.rm = TRUE), 2),
               max = ~round(max(.x, na.rm = TRUE), 2),
               sd = ~round(sd(.x, na.rm = TRUE), 2),
               IQR = ~round(IQR(.x, na.rm = TRUE), 2),
               missing = ~round(sum(is.na(.x))/length(.x)*100, 2)
             ),
             .names = "{.col}_{.fn}"),
      .groups = 'drop'
    ) %>%
    mutate(region = "Countrywide")
  
  combined_stats <- bind_rows(regional_stats, national_stats)
  
  table_data <- combined_stats %>%
    pivot_longer(
      cols = -region,
      names_to = c("variable", "statistic"),
      names_pattern = "(.+)_(.+)",
      values_to = "value"
    ) %>%
    pivot_wider(
      names_from = c(region, statistic),
      values_from = "value"
    )
  
  # Dynamic variable labels
  variable_labels <- c(
    "deaths" = "Deaths",
    "pm25" = "PM2.5",
    "tmax" = "Max Temperature",
    "tmean" = "Mean Temperature",
    "precipitation" = "Precipitation", 
    "humidity" = "Humidity",
    "wind_speed" = "Wind Speed"
  )
  
  table_data$Variable <- ifelse(
    table_data$variable %in% names(variable_labels),
    variable_labels[table_data$variable],
    table_data$variable
  )
  
  table_formatted <- table_data %>% 
    select(Variable, everything(), -variable)
  
  print(table_formatted, n = Inf)
  
  if (save_outputs) {
    write.csv(table_formatted, 
              file.path(output_dir, "descriptive_statistics_table.csv"), 
              row.names = FALSE)
  }
  
  # 4. CORRELATION MATRIX AND TABLE
  # Use dynamic variable list for correlation
  cor_data <- data_n %>%
    select(all_of(c("deaths", env_vars))) %>%
    filter(complete.cases(.))
  
  if (nrow(cor_data) > 0) {
    cor_matrix <- cor(cor_data, method = "pearson", use = "complete.obs")
    cor_matrix_rounded <- round(cor_matrix, 3)
    
    # Apply labels dynamically
    rownames(cor_matrix_rounded) <- variable_labels[rownames(cor_matrix_rounded)]
    colnames(cor_matrix_rounded) <- variable_labels[colnames(cor_matrix_rounded)]
    
    cor_matrix_lower <- cor_matrix_rounded
    cor_matrix_lower[upper.tri(cor_matrix_lower)] <- NA
    
    cor_table_display <- as.data.frame(cor_matrix_lower)
    cor_table_display$Variable <- rownames(cor_table_display)
    cor_table_display <- cor_table_display %>% select(Variable, everything())
    cor_table_display[is.na(cor_table_display)] <- ""
    
    print(cor_table_display)
    
    if (save_outputs) {
      png(file.path(output_dir, "correlation_matrix_plot.png"), 
          width = 800, height = 800, res = 150)
      
      corrplot::corrplot(cor_matrix_rounded,
                         method = "color",
                         type = "lower",
                         order = "original",
                         tl.cex = 0.8,
                         tl.col = "black",
                         tl.srt = 45,
                         addCoef.col = "black",
                         number.cex = 0.7,
                         col = colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))(200),
                         title = "Correlation Matrix of Climate Variables and Mortality",
                         mar = c(0,0,2,0))
      
      dev.off()
    }
    
    if (save_outputs) {
      write.csv(cor_table_display, 
                file.path(output_dir, "correlation_matrix_table.csv"), 
                row.names = FALSE)
    }
  }
  
  # 5. TIME SERIES PLOTS WITH CONFIGURABLE MOVING AVERAGE
  # Calculate moving averages for all environmental variables dynamically
  national_df_ma <- data_n %>%
    arrange(date) %>%
    mutate(
      across(all_of(c("deaths", env_vars)),
             ~ zoo::rollmean(., k = moving_average_window, fill = NA, align = "center"),
             .names = "{.col}_ma{moving_average_window}")
    )
  
  ma_title_suffix <- paste0(moving_average_window, "-day Moving Average")
  
  # Create time series plots dynamically
  ts_plots <- list()
  plot_colors <- c(deaths = "#E41A1C", pm25 = "#377EB8", humidity = "#4DAF4A", 
                   precipitation = "#984EA3", tmax = "#FF7F00", 
                   tmean = "#2FdF33", wind_speed = "#0000EE")
  
  plot_labels <- c(deaths = "Number of Deaths", pm25 = "PM2.5 (µg/m³)", 
                   humidity = "Humidity (%)", precipitation = "Precipitation (mm)", 
                   tmax = "Max Temp (°C)", tmean = "Mean Temp (°C)", 
                   wind_speed = "Wind Speed (m/s)")
  
  plot_titles <- c(deaths = "Daily Deaths", pm25 = "Daily PM2.5", 
                   humidity = "Daily Humidity", precipitation = "Daily Precipitation", 
                   tmax = "Daily Max Temperature", tmean = "Daily Mean Temperature", 
                   wind_speed = "Daily Wind Speed")
  
  all_ts_vars <- c("deaths", env_vars)
  
  for (i in seq_along(all_ts_vars)) {
    var <- all_ts_vars[i]
    ma_col <- paste0(var, "_ma", moving_average_window)
    
    ts_plots[[i]] <- ggplot(national_df_ma, aes(x = date)) +
      geom_line(aes(y = .data[[var]]), color = "lightgray", alpha = 0.7, 
                linewidth = 0.3, na.rm = TRUE) +
      geom_line(aes(y = .data[[ma_col]]), color = plot_colors[var], 
                linewidth = 0.8, na.rm = TRUE) +
      labs(title = paste(plot_titles[var], "with", ma_title_suffix), 
           x = "Date", y = plot_labels[var]) +
      theme_minimal()
  }
  
  # Arrange plots in grid
  n_ts_plots <- length(ts_plots)
  ts_ncol <- 2
  ts_nrow <- ceiling(n_ts_plots / ts_ncol)
  
  grid_plot_ts <- grid.arrange(grobs = ts_plots, ncol = ts_ncol, nrow = ts_nrow)
  print(grid_plot_ts)
  
  if (save_outputs) {
    filename <- paste0("National_time_series_plots_", moving_average_window, "day_ma.png")
    png(file.path(output_dir, filename), 
        width = 1800, height = 1400, res = 150)
    grid.arrange(grobs = ts_plots, ncol = ts_ncol, nrow = ts_nrow)
    dev.off()
  }
  
  # Return summary information
  
  invisible(national_df_ma)
}


#' Fit GAM model
#'
#' @description Fits the GAM model for a single region with a natural spline
#' for climate variables.
#'
#' @param data Data to fit the model.
#' @param var_name Character. The name of the column containing PM2.5 values.
#' Defaults to "pm25"
#' @param family Character string indicating the distribution family used in the GAM.
#' Must be one of the four options "quasipoisson", "nb", "ziP", and "poisson".
#' "quasipoisson" (Appropriate for count data with overdispersion),
#' "nb" (negativebinomial which is suitable for highly dispersed count data),
#' "ziP" (zero-inflated which is suitable for count data with excess zeros (sparse data)),
#' "poisson" (Suitable for count data without overdispersion (mean = variance)).
#' Defaults to "quasipoisson"
#'
#' @return GAM model object or NULL if failed.
#'
#' @keywords internal
fit_air_pollution_gam <- function(data,
                                  var_name = "pm25",
                                  family = "quasipoisson") {
  # Data Validation
  if (nrow(data) < 500) {
    warning(
      "Insufficient data: only ",
      nrow(data),
      " observations of a recommended 500 minimum."
    )
    return(NULL)
  }
  
  # Parameter Validation
  if (!(var_name %in% names(data))) {
    stop(paste0(var_name, " not in dataset."))
  }
  
  # Check for required confounders
  required_vars <- c("deaths", "time", "tmax", "humidity", "precipitation", "dow", "population")
  missing_vars <- required_vars[!(required_vars %in% names(data))]
  if (length(missing_vars) > 0) {
    stop("Missing required variables: ", paste(missing_vars, collapse = ", "))
  }
  
  yr <- length(unique(data$year))
  dfseas <- 6
  # Create Model Formula
  GAM_formula <- as.formula(
    paste(
      "deaths ~ ", var_name,
      "+ ns(time, df =", dfseas * yr, ") + ns(tmax, df = 3) + ns(humidity, df = 3)",
      "+ ns(precipitation, df = 3) + ns(wind_speed) + dow + offset(log(population))"
    )
  )
  
  # Fit Model with error handling
  tryCatch(
    {
      model <- mgcv::gam(GAM_formula, data = data, family = family)
      return(model)
    },
    error = function(e) {
      warning("Model fitting failed: ", e$message)
      return(NULL)
    }
  )
}


#' Extract coefficient and standard error from GAM model
#'
#' @description Extracts the coefficient and standard error of the GAM model.
#'
#' @param model GAM model object from single region.
#' @param var_name Character. Column name to extract. Defaults to "pm25"
#'
#' @return List with coefficient and standard error.
#'
#' @keywords internal
extract_air_pollution_coef <- function(model,
                                       var_name = "pm25") {
  if (is.null(model)) {
    return(list(coef = NA, se = NA))
  }
  
  tryCatch(
    {
      res <- summary(model)
      coef_val <- res$p.coeff[var_name]
      se_val <- res$se[var_name]
      
      if (is.na(coef_val) || is.na(se_val)) {
        warning("Could not extract valid coefficient or SE for '", var_name, "'")
        return(list(coef = NA, se = NA))
      }
      
      return(list(
        coef = as.numeric(coef_val),
        se = as.numeric(se_val)
      ))
    },
    error = function(e) {
      warning("Failed to extract coefficient for '", var_name, "': ", e$message)
      return(list(coef = NA, se = NA))
    }
  )
}


#' Conduct two-stage meta-analysis with random effect with AF and AN calculations
#'
#' @description Performs meta-analysis across regions and calculates attributable
#' fraction and attributable number.
#'
#' @param data Input data with multiple regions
#' @param var_name Character. PM2.5 variable to analyze. Defaults to "pm25".
#' @param family Character string indicating the distribution family used in the GAM.
#' Must be one of the four options "quasipoisson", "nb", "ziP", and "poisson".
#' "quasipoisson" (Appropriate for count data with overdispersion),
#' "nb" (negativebinomial which is suitable for highly dispersed count data),
#' "ziP" (zero-inflated which is suitable for count data with excess zeros (sparse data)),
#' "poisson" (Suitable for count data without overdispersion (mean = variance)).
#' Defaults to "quasipoisson"
#'
#' @return List with meta-analysis results including AF and AN.
#'
#' @keywords internal
air_pollution_meta_analysis <- function(data,
                                        var_name = "pm25",
                                        family = "quasipoisson") {
  
  # Stage 0: Fit Regional Models
  region_results <- data %>%
    dplyr::group_by(.data$region) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      n_obs = purrr::map_dbl(.data$data, nrow),
      total_deaths = purrr::map_dbl(.data$data, ~ sum(.x$deaths, na.rm = TRUE)),
      model = purrr::map2(.data$data, .data$region, ~ {
        fit_air_pollution_gam(.x, var_name, family)
      })
    ) %>%
    dplyr::ungroup()
  
  # Stage 1: Extract Coefficients and Calculate AN/AF
  region_results <- region_results %>%
    dplyr::mutate(
      coef_results = purrr::map(.data$model, ~ extract_air_pollution_coef(.x, var_name)),
      coef_pm25 = tidytable::map_dbl(.data$coef_results, ~ .x$coef),
      se_pm25 = tidytable::map_dbl(.data$coef_results, ~ .x$se),
      rr_10ug = exp(.data$coef_pm25 * 10),
      ci_lower = exp((.data$coef_pm25 - 1.96 * .data$se_pm25) * 10),
      ci_upper = exp((.data$coef_pm25 + 1.96 * .data$se_pm25) * 10),
      af_10ug = (.data$rr_10ug - 1) / .data$rr_10ug,
      an_10ug = .data$total_deaths * .data$af_10ug
    ) %>%
    dplyr::filter(!is.na(.data$coef_pm25) & !is.na(.data$se_pm25)) %>%
    dplyr::select(-"coef_results")
  
  if (nrow(region_results) < 2) {
    warning("At least 2 regions with successful model fits needed for meta-analysis.")
    return(NULL)
  }
  
  # Stage 2: Meta Analysis
  meta_result <- metafor::rma(
    yi = region_results$coef_pm25,
    sei = region_results$se_pm25,
    data = region_results,
    method = "REML",
    slab = region_results$region
  )
  
  overall_rr <- exp((meta_result$beta) * 10)
  overall_af <- (overall_rr - 1) / overall_rr
  overall_an <- sum(region_results$total_deaths) * overall_af
  
  return(list(
    region_results = region_results,
    meta_result = meta_result,
    overall_rr = overall_rr,
    overall_ci = exp(c(meta_result$ci.lb, meta_result$ci.ub) * 10),
    overall_af = overall_af,
    overall_an = overall_an,
    heterogeneity = list(
      tau2 = meta_result$tau2,
      I2 = meta_result$I2
    )
  ))
}


#' Analyze multiple lag structures with AF and AN
#'
#' @description Implements distributed lag model. Individual lag
#' coefficients and cumulative effects are extracted from this single model.
#'
#' @param data Input data
#' @param max_lag Integer. Maximum lag days. Defaults to 2
#' @param family Character string indicating the distribution family used in the GAM.
#'
#' @return Dataframe with lag-specific results including proper cumulative effect
#'
#' @keywords internal
analyze_air_pollution_lags <- function(data,
                                       max_lag = 2,
                                       family = "quasipoisson") {
  
  yr <- length(unique(data$year))
  dfseas <- 6
  
  # Create list of all lag variables
  all_lag_vars <- c("pm25", paste0("pm25_lag", 1:max_lag))
  
  # Fit SINGLE distributed lag model for each region with ALL lags
  region_results <- data %>%
    dplyr::group_by(.data$region) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      n_obs = purrr::map_dbl(.data$data, nrow),
      total_deaths = purrr::map_dbl(.data$data, ~ sum(.x$deaths, na.rm = TRUE))
    ) %>%
    dplyr::mutate(
      model = purrr::map2(.data$data, .data$region, function(dat, reg) {
        
        if (nrow(dat) < 500) {
          warning("Insufficient data for ", reg)
          return(NULL)
        }
        
        lag_formula <- paste(all_lag_vars, collapse = " + ")
        formula_str <- paste(
          "deaths ~", lag_formula,
          "+ ns(time, df =", dfseas * yr, ") + ns(tmax, df = 3) + ns(humidity, df = 3)",
          "+ ns(precipitation, df = 3) + ns(wind_speed, df =3) + dow + offset(log(population))"
        )
        
        GAM_formula <- as.formula(formula_str)
        
        tryCatch({
          mgcv::gam(GAM_formula, data = dat, family = family)
        }, error = function(e) {
          warning("Model failed for ", reg, ": ", e$message)
          return(NULL)
        })
      })
    ) %>%
    dplyr::filter(!is.na(.data$model)) %>%
    dplyr::ungroup()
  
  if (nrow(region_results) < 2) {
    stop("At least 2 regions needed for meta-analysis")
  }
  
  individual_lag_labels <- c("Lag 0", paste0("Lag ", 1:max_lag))
  
  # Storage for coefficients and standard errors from the distributed lag model
  coef_matrix <- matrix(NA, nrow = nrow(region_results), ncol = max_lag + 1,
                        dimnames = list(region_results$region, all_lag_vars))
  se_matrix <- matrix(NA, nrow = nrow(region_results), ncol = max_lag + 1,
                      dimnames = list(region_results$region, all_lag_vars))
  
  # Extract coefficients from each region's distributed lag model
  for (i in 1:nrow(region_results)) {
    model <- region_results$model[[i]]
    if (!is.null(model)) {
      model_summary <- summary(model)
      
      for (j in seq_along(all_lag_vars)) {
        var <- all_lag_vars[j]
        if (var %in% names(coef(model))) {
          coef_matrix[i, j] <- model_summary$p.coeff[var]
          se_matrix[i, j] <- model_summary$se[var]
        }
      }
    }
  }
  
  results_list <- list()
  
  for (j in seq_along(all_lag_vars)) {
    var <- all_lag_vars[j]
    label <- individual_lag_labels[j]
    
    valid_coefs <- !is.na(coef_matrix[, j]) & !is.na(se_matrix[, j])
    
    if (sum(valid_coefs) < 2) {
      warning("Insufficient regions for ", label)
      results_list[[j]] <- data.frame(
        lag = label, rr = NA, ci_lower = NA, ci_upper = NA,
        af = NA, an = NA, tau2 = NA, I2 = NA, p_value = NA,
        is_cumulative = FALSE
      )
      next
    }
    
    meta_result <- tryCatch({
      metafor::rma(
        yi = coef_matrix[valid_coefs, j],
        sei = se_matrix[valid_coefs, j],
        method = "REML"
      )
    }, error = function(e) {
      warning("Meta-analysis failed for ", label, ": ", e$message)
      return(NULL)
    })
    
    if (!is.null(meta_result)) {
      overall_rr <- exp(meta_result$beta[1] * 10)
      overall_ci <- exp(c(meta_result$ci.lb, meta_result$ci.ub) * 10)
      overall_af <- (overall_rr - 1) / overall_rr
      
      total_deaths <- sum(region_results$total_deaths, na.rm = TRUE)
      overall_an <- overall_af * total_deaths
      
      results_list[[j]] <- data.frame(
        lag = label,
        rr = overall_rr,
        ci_lower = overall_ci[1],
        ci_upper = overall_ci[2],
        af = overall_af,
        an = overall_an,
        tau2 = meta_result$tau2,
        I2 = meta_result$I2,
        p_value = meta_result$pval,
        is_cumulative = FALSE
      )
    } else {
      results_list[[j]] <- data.frame(
        lag = label, rr = NA, ci_lower = NA, ci_upper = NA,
        af = NA, an = NA, tau2 = NA, I2 = NA, p_value = NA,
        is_cumulative = FALSE
      )
    }
  }
  
  # Sum coefficients across all lags for each region
  cumulative_coefs <- rowSums(coef_matrix, na.rm = FALSE)
  
  # Calculate SE of sum (assuming independence of estimates within model)
  cumulative_se <- sqrt(rowSums(se_matrix^2, na.rm = FALSE))
  
  valid_cumulative <- !is.na(cumulative_coefs) & !is.na(cumulative_se)
  
  if (sum(valid_cumulative) >= 2) {
    # Meta-analyze the cumulative coefficients
    meta_cumulative <- tryCatch({
      metafor::rma(
        yi = cumulative_coefs[valid_cumulative],
        sei = cumulative_se[valid_cumulative],
        method = "REML"
      )
    }, error = function(e) {
      warning("Cumulative meta-analysis failed: ", e$message)
      return(NULL)
    })
    
    if (!is.null(meta_cumulative)) {
      cumulative_rr <- exp(meta_cumulative$beta[1] * 10)
      cumulative_ci <- exp(c(meta_cumulative$ci.lb, meta_cumulative$ci.ub) * 10)
      cumulative_af <- (cumulative_rr - 1) / cumulative_rr
      
      total_deaths <- sum(region_results$total_deaths, na.rm = TRUE)
      cumulative_an <- cumulative_af * total_deaths
      
      cumulative_result <- data.frame(
        lag = paste0("Lag 0-", max_lag),
        rr = cumulative_rr,
        ci_lower = cumulative_ci[1],
        ci_upper = cumulative_ci[2],
        af = cumulative_af,
        an = cumulative_an,
        tau2 = meta_cumulative$tau2,
        I2 = meta_cumulative$I2,
        p_value = meta_cumulative$pval,
        is_cumulative = TRUE
      )
      
      results_list[[length(results_list) + 1]] <- cumulative_result
    }
  } else {
    warning("Insufficient valid cumulative coefficients")
  }
  
  # Combine all results
  final_results <- do.call(rbind, results_list)
  
  return(final_results)
}


#' Save air pollution plot with standardized dimensions
#'
#' @param plot_object ggplot or grob object to save
#' @param output_dir Character. Directory to save plot.
#' @param filename Character. Name of the file (without or with .png extension).
#' @param grid_dims List with ncol and nrow for multi-panel plots. NULL for single plots.
#'
#' @return Invisibly returns the output path
#'
#' @keywords internal
save_air_pollution_plot <- function(plot_object,
                                    output_dir,
                                    filename,
                                    grid_dims = NULL) {
  # Fixed internal parameters
  width_per_panel <- 4
  height_per_panel <- 4
  dpi <- 150
  single_plot_width <- 10
  single_plot_height <- 8
  
  # Simple Validation
  if (is.null(output_dir)) stop("'output_dir' must be provided")
  
  # Create output directory
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Calculate dimensions
  if (is.null(grid_dims)) {
    # Single plot
    fig_width <- single_plot_width
    fig_height <- single_plot_height
  } else {
    # Multi-panel plot
    fig_width <- width_per_panel * grid_dims$ncol
    fig_height <- height_per_panel * grid_dims$nrow
  }
  
  # Save plot
  if (!endsWith(filename, ".png")) {
    output_path <- file.path(output_dir, paste0(filename, ".png"))
  } else {
    output_path <- file.path(output_dir, filename)
  }
  
  ggplot2::ggsave(output_path, plot_object,
                  width = fig_width, height = fig_height, dpi = dpi
  )
  
  invisible(output_path)
}


#' Plot forest plot for PM2.5 effects by region
#'
#' @param meta_results Meta-analysis results with regional and overall estimates.
#' @param output_dir Character. Directory to save plot. Defaults to NULL.
#' @param save_plot Logical. Whether to save the plot. Defaults to FALSE.
#'
#' @return ggplot object
#'
#' @keywords internal
plot_air_pollution_forest <- function(meta_results,
                                      output_dir = NULL,
                                      save_plot = FALSE) {
  if (is.null(output_dir) && save_plot) {
    stop("Output directory must be specified if save_plot == TRUE.")
  }
  
  region_data <- meta_results$region_results %>%
    dplyr::select(
      all_of(
        c("region", "rr_10ug", "ci_lower", "ci_upper", "af_10ug", "an_10ug")
      )
    ) %>%
    dplyr::mutate(
      type = "Region",
      label = sprintf("RR: %.3f", .data$rr_10ug),
      region = as.character(.data$region)
    )
  
  overall_data <- data.frame(
    region = "Overall",
    rr_10ug = meta_results$overall_rr,
    ci_lower = meta_results$overall_ci[1],
    ci_upper = meta_results$overall_ci[2],
    af_10ug = meta_results$overall_af,
    an_10ug = meta_results$overall_an,
    type = "Overall",
    label = sprintf("RR: %.3f", meta_results$overall_rr),
    stringsAsFactors = FALSE
  )
  
  plot_data <- dplyr::bind_rows(region_data, overall_data) %>%
    dplyr::mutate(
      is_overall = .data$type == "Overall"
    )
  
  # Create factor with explicit levels for x-axis (regions)
  all_regions <- c(as.character(unique(region_data$region)), "Overall")
  plot_data$region <- factor(plot_data$region, levels = all_regions)
  
  y_min <- min(plot_data$ci_lower, na.rm = TRUE) * 0.95
  y_max <- max(plot_data$ci_upper, na.rm = TRUE) * 1.05
  
  forest_plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$region, y = .data$rr_10ug)) +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        ymin = .data$ci_lower, ymax = .data$ci_upper,
        color = .data$is_overall, linewidth = .data$is_overall
      ),
      width = 0.1
    ) +
    ggplot2::geom_point(ggplot2::aes(color = .data$is_overall, size = .data$is_overall)) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
    ggplot2::scale_color_manual(values = c("FALSE" = "blue", "TRUE" = "red")) +
    ggplot2::scale_size_manual(values = c("FALSE" = 1, "TRUE" = 1)) +
    ggplot2::scale_linewidth_manual(values = c("FALSE" = 0.6, "TRUE" = 0.6)) +
    ggplot2::ylim(c(y_min, y_max)) +
    ggplot2::labs(
      x = "Region", y = "Health Relative Risk",
      title = "PM2.5 Effects by Region",
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(hjust = 0.5),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
  
  if (save_plot) {
    save_air_pollution_plot(
      plot_object = forest_plot,
      output_dir = output_dir,
      filename = "forest_plot",
      grid_dims = NULL  # Single plot
    )
  }
  
  return(forest_plot)
}


#' Plot Relative Risk (RR) by lag
#'
#' @param lag_results RR lag analysis results.
#' @param max_lag Integer. Maximum lag days. Defaults to 2.
#' @param output_dir Character. Directory to save plot. Defaults to NULL.
#' @param save_plot Logical. Whether to save the plot. Defaults to FALSE.
#'
#' @return ggplot object
#'
#' @keywords internal
plot_air_pollution_lags <- function(lag_results,
                                    max_lag = 2,
                                    output_dir = NULL,
                                    save_plot = FALSE) {
  # Param Validation
  if (is.null(output_dir) && save_plot == TRUE) {
    stop("Output directory must be specified if save_plot==TRUE.")
  }
  
  lag_results_clean <- lag_results %>% filter(!is.na(.data$rr))
  
  if (nrow(lag_results_clean) == 0) {
    stop("No successful lag analyses to plot")
  }
  
  lag_labels <- c("Lag 0", paste0("Lag ", 1:max_lag), paste0("Lag 0-", max_lag))
  lag_results_clean$lag <- factor(lag_results_clean$lag,
                                  levels = lag_labels
  )
  
  lag_results_clean <- lag_results_clean %>%
    dplyr::mutate(label = "")
  
  y_min <- min(lag_results_clean$ci_lower, na.rm = TRUE) * 0.95
  y_max <- max(lag_results_clean$ci_upper, na.rm = TRUE) * 1.05
  
  # Add buffer if range is too small
  if ((y_max - y_min) < 0.05) {
    y_min <- y_min - 0.025
    y_max <- y_max + 0.025
  }
  
  lag_plot <- ggplot2::ggplot(lag_results_clean, ggplot2::aes(x = lag, y = .data$rr)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$ci_lower, ymax = .data$ci_upper),
                           width = 0.2, color = "darkblue", linewidth = 0.8
    ) +
    ggplot2::geom_point(size = 1.5, color = "blue") +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
    ggplot2::labs(
      x = "Lag (Day)", y = "Health Relative Risk",
      title = "RR by Lag Structure"
    ) +
    ggplot2::ylim(c(y_min, y_max)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
  
  if (save_plot) {
    save_air_pollution_plot(
      plot_object = lag_plot,
      output_dir = output_dir,
      filename = "lag_results",
      grid_dims = NULL  # Single plot
    )
  }
  
  return(lag_plot)
}


#' Generate a grid size for a certain number of plots.
#'
#' @param n_plots The number of plots required for the grid.
#'
#' @return A list containing ncol and nrow values for the grid.
#'
#' @export
calculate_air_pollution_grid_dims <- function(n_plots){
  est <- sqrt(n_plots)
  if (est == floor(est)){
    x <- y <- est
  } else {
    base <- est - floor(est)
    if (base < 0.5){
      y <- floor(est)
    }
    else {
      y <- floor(est) + 1
    }
    x <- floor(est) + 1
  }
  return(list(ncol = x, nrow = y))
}


#' Calculate AF and AN for a specific PM2.5 reference value
#'
#' @param data A region-specific dataset.
#' @param reference_pm25 Integer. PM2.5 reference value. Defaults to 15
#' (WHO guideline).
#' @param var_name Character. PM2.5 variable name. Defaults to "pm25".
#' @param family Character. Distribution family for GAM. Defaults to
#' "quasipoisson".
#'
#' @return List with AF and AN
#'
#' @keywords internal
calculate_air_pollution_af_an <- function(data,
                                          reference_pm25 = 15,
                                          var_name = "pm25",
                                          family = "quasipoisson") {
  
  # Fit model
  model <- fit_air_pollution_gam(data, var_name, family)
  
  if (is.null(model)) {
    warning("Model fitting failed for AF/AN calculation")
    return(list(af = 0, an = 0))
  }
  
  # Extract coefficient
  coef_extract <- extract_air_pollution_coef(model, var_name)
  beta <- coef_extract$coef
  
  if (is.na(beta)) {
    warning("Could not extract coefficient for AF/AN calculation")
    return(list(af = 0, an = 0))
  }
  
  pm_values <- data[[var_name]]
  deaths <- data$deaths
  
  above_ref <- pm_values > reference_pm25 & !is.na(pm_values) & !is.na(deaths)
  
  if (!any(above_ref)) {
    message("No observations above reference value of ", reference_pm25)
    return(list(af = 0, an = 0))
  }
  
  rr_t <- exp(beta * (pm_values[above_ref] - reference_pm25))
  af_t <- (rr_t - 1) / rr_t
  deaths_t <- deaths[above_ref]
  
  af_overall <- sum(deaths_t * af_t) / sum(deaths, na.rm = TRUE)
  an_overall <- af_overall * sum(deaths, na.rm = TRUE)
  
  return(list(af = af_overall, an = an_overall))
}


#' Analyze region-specific distributed lag effects with AF/AN for a chosen
#' PM2.5 reference. Calculates cumulative effects as sum of coefficients from that model.
#'
#' @param data Input data with lag variables.
#' @param reference_pm25 Numeric. PM2.5 reference value. Defaults to 15.
#' @param max_lag Integer. Maximum lag days. Defaults to 2
#' @param family Character. Distribution family for GAM. Defaults to "quasipoisson".
#'
#' @return List with region-specific and meta-analysis results
#'
#' @keywords internal
analyze_air_pollution_gam <- function(data,
                                      reference_pm25 = 15,
                                      max_lag = 2,
                                      family = "quasipoisson") {
  
  regions <- unique(data$region)
  
  all_lag_vars <- c("pm25", paste0("pm25_lag", 1:max_lag))
  individual_labels <- c("0", paste0("", 1:max_lag))
  
  region_results_list <- list()
  
  # Fit distributed lag model for each region
  for (prov in regions) {
    
    prov_data <- data %>% dplyr::filter(.data$region == prov)
    total_deaths_prov <- sum(prov_data$deaths, na.rm = TRUE)
    
    yr <- length(unique(prov_data$year))
    dfseas <- 6
    if (nrow(prov_data) < 500) {
      message("  Skipping ", prov, " - insufficient data")
      next
    }
    
    lag_formula <- paste(all_lag_vars, collapse = " + ")
    formula_str <- paste(
      "deaths ~", lag_formula,
      "+ ns(time, df =", dfseas * yr, ") + ns(tmax, df = 3) + ns(humidity, df = 3)",
      "+ ns(precipitation, df = 3) + ns(wind_speed, df = 3) + dow + offset(log(population))"
    )
    
    GAM_formula <- as.formula(formula_str)
    
    model <- tryCatch({
      mgcv::gam(GAM_formula, data = prov_data, family = family)
    }, error = function(e) {
      warning("  Model failed for ", prov, ": ", e$message)
      return(NULL)
    })
    
    if (is.null(model)) next
    
    # Initialize results for this region
    prov_results <- data.frame(
      lag_group = c(individual_labels, paste0("0-", max_lag)),
      RR = NA,
      LB = NA,
      UB = NA,
      AF = NA,
      AN = NA,
      region = prov,
      stringsAsFactors = FALSE
    )
    
    model_summary <- summary(model)
    
    lag_coefs <- numeric(length(all_lag_vars))
    lag_ses <- numeric(length(all_lag_vars))
    
    for (i in seq_along(all_lag_vars)) {
      var_name <- all_lag_vars[i]
      
      if (var_name %in% names(coef(model))) {
        beta <- model_summary$p.coeff[var_name]
        beta_se <- model_summary$se[var_name]
        
        lag_coefs[i] <- beta
        lag_ses[i] <- beta_se
        
        beta_lower <- beta - 1.96 * beta_se
        beta_upper <- beta + 1.96 * beta_se
        
        # Calculate RR and AF for this lag
        pm_var <- prov_data[[var_name]]
        deaths_var <- prov_data$deaths
        above_ref <- pm_var > reference_pm25 & !is.na(pm_var) & !is.na(deaths_var)
        
        if (any(above_ref)) {
          rr_t <- exp(beta * (pm_var[above_ref] - reference_pm25))
          rr_t_lower <- exp(beta_lower * (pm_var[above_ref] - reference_pm25))
          rr_t_upper <- exp(beta_upper * (pm_var[above_ref] - reference_pm25))
          af_t <- (rr_t - 1) / rr_t
          deaths_t <- deaths_var[above_ref]
          
          rr_overall <- sum(deaths_t * rr_t) / sum(deaths_t)
          lb_overall <- sum(deaths_t * rr_t_lower) / sum(deaths_t)
          ub_overall <- sum(deaths_t * rr_t_upper) / sum(deaths_t)
          af_overall <- sum(deaths_t * af_t) / sum(deaths_var, na.rm = TRUE)
          an_overall <- af_overall * sum(deaths_var, na.rm = TRUE)
          
          prov_results[i, c("RR", "LB", "UB", "AF", "AN")] <-
            list(rr_overall, lb_overall, ub_overall, af_overall, an_overall)
        }
      }
    }
    
    if (sum(!is.na(lag_coefs)) >= 2) {
      
      cumulative_coef <- sum(lag_coefs, na.rm = TRUE)
      cumulative_se <- sqrt(sum(lag_ses^2, na.rm = TRUE))
      
      cumulative_beta_lower <- cumulative_coef - 1.96 * cumulative_se
      cumulative_beta_upper <- cumulative_coef + 1.96 * cumulative_se
      
      # Use pm25 (lag 0) for exposure calculation
      pm_var <- prov_data$pm25
      deaths_var <- prov_data$deaths
      above_ref <- pm_var > reference_pm25 & !is.na(pm_var) & !is.na(deaths_var)
      
      if (any(above_ref)) {
        rr_t_cum <- exp(cumulative_coef * (pm_var[above_ref] - reference_pm25))
        rr_t_lower_cum <- exp(cumulative_beta_lower * (pm_var[above_ref] - reference_pm25))
        rr_t_upper_cum <- exp(cumulative_beta_upper * (pm_var[above_ref] - reference_pm25))
        af_t_cum <- (rr_t_cum - 1) / rr_t_cum
        deaths_t <- deaths_var[above_ref]
        
        rr_cum <- sum(deaths_t * rr_t_cum) / sum(deaths_t)
        lb_cum <- sum(deaths_t * rr_t_lower_cum) / sum(deaths_t)
        ub_cum <- sum(deaths_t * rr_t_upper_cum) / sum(deaths_t)
        af_cum <- sum(deaths_t * af_t_cum) / sum(deaths_var, na.rm = TRUE)
        an_cum <- af_cum * sum(deaths_var, na.rm = TRUE)
        
        # Add cumulative result
        cum_idx <- length(all_lag_vars) + 1
        prov_results[cum_idx, c("RR", "LB", "UB", "AF", "AN")] <-
          list(rr_cum, lb_cum, ub_cum, af_cum, an_cum)
      }
    }
    
    region_results_list[[prov]] <- prov_results
  }
  
  if (length(region_results_list) == 0) {
    stop("No regions with sufficient data")
  }
  
  region_gam_results <- do.call(rbind, region_results_list)
  
  # Meta-analysis across regions
  
  all_lag_labels <- c(individual_labels, paste0("0-", max_lag))
  
  meta_gam_results <- data.frame(
    lag_group = all_lag_labels,
    RR = NA,
    LB = NA,
    UB = NA,
    AF = NA,
    AN = NA,
    I2 = NA,
    Q_p = NA
  )
  
  for (lg in all_lag_labels) {
    lag_data <- region_gam_results %>%
      dplyr::filter(.data$lag_group == lg) %>%
      dplyr::filter(!is.na(.data$RR) & .data$RR > 0)
    
    if (nrow(lag_data) < 2) {
      warning("Insufficient regions for lag group ", lg)
      next
    }
    
    lag_data$yi <- log(lag_data$RR)
    lag_data$sei <- (log(lag_data$UB) - log(lag_data$LB)) / (2 * 1.96)
    
    meta_res <- tryCatch({
      metafor::rma(yi = lag_data$yi, sei = lag_data$sei, method = "REML")
    }, error = function(e) {
      warning("Meta-analysis failed for lag ", lg, ": ", e$message)
      return(NULL)
    })
    
    if (!is.null(meta_res)) {
      pooled_rr <- exp(meta_res$b)
      pooled_af <- mean(lag_data$AF, na.rm = TRUE)
      pooled_an <- mean(lag_data$AN, na.rm = TRUE)
      
      meta_gam_results[meta_gam_results$lag_group == lg, "RR"] <- pooled_rr
      meta_gam_results[meta_gam_results$lag_group == lg, "LB"] <- exp(meta_res$ci.lb)
      meta_gam_results[meta_gam_results$lag_group == lg, "UB"] <- exp(meta_res$ci.ub)
      meta_gam_results[meta_gam_results$lag_group == lg, "AF"] <- pooled_af
      meta_gam_results[meta_gam_results$lag_group == lg, "AN"] <- pooled_an
      meta_gam_results[meta_gam_results$lag_group == lg, "I2"] <- meta_res$I2
      meta_gam_results[meta_gam_results$lag_group == lg, "Q_p"] <- meta_res$QEp
    }
  }
  
  return(list(
    region_results = region_gam_results,
    meta_results = meta_gam_results
  ))
}


#' Plot GAM distributed lag results
#'
#' @param gam_results GAM analysis results from analyze_air_pollution_gam.
#' @param output_dir Character. Directory to save plot. Defaults to NULL.
#' @param max_lag Integer. Maximum lag days. Defaults to 2.
#' @param save_plot Logical. Whether to save the plot. Defaults to FALSE.
#'
#' @return List with individual plots, combined plot, and grid dimensions
#'
#' @keywords internal
plot_air_pollution_gam <- function(gam_results,
                                   output_dir = NULL,
                                   max_lag = 2,
                                   save_plot = FALSE) {
  
  if (is.null(output_dir) && save_plot) {
    stop("Output directory must be specified if save_plot == TRUE.")
  }
  
  region_results <- gam_results$region_results
  meta_results <- gam_results$meta_results
  
  individual_labels <- c("0", paste0("", 1:max_lag))
  cumulative_label <- paste0("0-", max_lag)
  all_labels <- c(individual_labels, cumulative_label)
  
  plots_list <- list()
  
  all_data <- bind_rows(
    meta_results %>% filter(!is.na(.data$RR)),
    region_results %>% filter(!is.na(.data$RR))
  )
  
  if (nrow(all_data) == 0) {
    warning("No valid results to plot")
    return(NULL)
  }
  
  global_y_min <- min(all_data$LB, na.rm = TRUE) * 0.95
  global_y_max <- max(all_data$UB, na.rm = TRUE) * 1.05
  
  # Add buffer if range is too small
  if ((global_y_max - global_y_min) < 0.05) {
    global_y_min <- global_y_min - 0.025
    global_y_max <- global_y_max + 0.025
  }
  
  # Create meta plot (national/countrywide)
  meta_plot_data <- meta_results %>%
    filter(!is.na(.data$RR)) %>%
    dplyr::mutate(lag_group = factor(.data$lag_group, levels = all_labels))
  
  if (nrow(meta_plot_data) == 0) {
    warning("No valid meta-analysis results to plot")
    return(NULL)
  }
  
  meta_plot <- ggplot2::ggplot(meta_plot_data, ggplot2::aes(x = .data$lag_group, y = .data$RR)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$LB, ymax = .data$UB), 
                           width = 0.2, color = "darkred") +
    ggplot2::geom_point(size = .5, color = "red") +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "red", size = 1) +
    ggplot2::labs(x = "Lag (Day)", y = "RR", title = "Countrywide") +
    ggplot2::ylim(global_y_min, global_y_max) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 8),
      axis.title = ggplot2::element_text(face = "bold", size = 5),
      axis.text = ggplot2::element_text(size = 5),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = .5)
    )
  
  plots_list[["Countrywide"]] <- meta_plot
  
  # Create regional plots
  regions <- unique(region_results$region)
  
  for (prov in regions) {
    prov_data <- region_results %>%
      filter(.data$region == prov) %>%
      filter(!is.na(.data$RR)) %>%
      dplyr::mutate(lag_group = factor(.data$lag_group, levels = all_labels))
    
    if (nrow(prov_data) == 0) {
      warning("No valid results for region: ", prov)
      next
    }
    
    prov_plot <- ggplot2::ggplot(prov_data, ggplot2::aes(x = .data$lag_group, y = .data$RR)) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$LB, ymax = .data$UB), 
                             width = 0.2, color = "darkblue") +
      ggplot2::geom_point(size = .5, color = "blue") +
      ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "red", size = 1) +
      ggplot2::labs(x = "Lag (days)", y = "RR", title = prov) +
      ggplot2::ylim(global_y_min, global_y_max) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, size = 10),
        axis.title = ggplot2::element_text(face = "bold", size = 8),
        axis.text = ggplot2::element_text(size = 8),
        panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 1)
      )
    
    plots_list[[prov]] <- prov_plot
  }
  
  # Calculate grid and create combined plot
  n_plots <- length(plots_list)
  grid_dims <- calculate_air_pollution_grid_dims(n_plots)
  
  combined_plot <- gridExtra::grid.arrange(
    grobs = plots_list,
    ncol = grid_dims$ncol,
    nrow = grid_dims$nrow
  )
  
  if (save_plot && !is.null(output_dir)) {
    save_air_pollution_plot(
      plot_object = combined_plot,
      output_dir = output_dir,
      filename = "distributed_lag_plots",
      grid_dims = grid_dims
    )
  }
  
  return(list(
    individual_plots = plots_list,
    combined_plot = combined_plot,
    grid_dimensions = grid_dims
  ))
}


#' Create exposure-response plots using GAM
#'
#' @param data_with_lags Data frame containing pre-processed data.
#' Must include columns: region, date, deaths, population, pm25, tmax, humidity.
#' @param meta_results List. Meta-analysis results from previous analysis (optional).
#' @param reference_pm25 Numeric value specifying the reference PM2.5 concentration.
#' Defaults to 15 (WHO guideline).
#' @param reference_name Character string describing the reference scenario.
#' Defaults to "WHO".
#' @param output_file Character. Full path for output PNG file. If NULL, output path will
#' default to 'air_pollution_results/(reference_name)_exposure_response_plots.png'.
#' @param var_name Character. Name of PM2.5 variable in dataset. Defaults to "pm25".
#' @param family Character. Distribution family for GAM. Defaults to "quasipoisson".
#'
#' @return List containing predictions, meta model, and summary statistics
#'
#' @keywords internal
create_air_pollution_exposure_plots <- function(data_with_lags,
                                                meta_results = NULL,
                                                reference_pm25 = 15,
                                                reference_name = "WHO",
                                                output_file = NULL,
                                                var_name = "pm25",
                                                family = "quasipoisson") {
  
  dfseas <- 6
  vardf <- 3
  plot_width_per_panel <- 4
  plot_height_per_panel <- 3.5
  res <- 150
  
  # Validate essential columns
  required_cols <- c("region", "date", "year", "deaths", "population", var_name)
  missing_cols <- required_cols[!(required_cols %in% colnames(data_with_lags))]
  if (length(missing_cols) > 0) {
    stop(paste0(
      "Missing required columns in 'data_with_lags': ",
      paste(missing_cols, collapse = ", ")
    ))
  }
  
  if (is.null(output_file)) {
    output_file <- paste0(
      "air_pollution_results/", tolower(reference_name),
      "_exposure_response_plots_gam.png"
    )
  }
  
  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
  
  confounders <- c("tmax", "tmean", "humidity", "precipitation", "wind_speed")
  available_confounders <- confounders[confounders %in% names(data_with_lags)]
  
  # Aggregate data by region, date, and year
  data_aggreg <- data_with_lags %>%
    dplyr::filter(!is.na(.data$date)) %>%
    dplyr::group_by(.data$region, .data$date, .data$year) %>%
    dplyr::summarise(
      deaths = sum(.data$deaths, na.rm = TRUE),
      pm25 = mean(.data[[var_name]], na.rm = TRUE),
      population = sum(.data$population, na.rm = TRUE),
      across(all_of(available_confounders), ~mean(.x, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    dplyr::arrange(.data$region, .data$date) %>%
    dplyr::group_by(.data$region) %>%
    dplyr::mutate(days = dplyr::row_number()) %>%
    dplyr::ungroup()
  
  plist <- split(data_aggreg, data_aggreg$region)
  prov <- names(plist)
  
  yr <- length(unique(data_aggreg$year))
  knots_values <- c(0.25, 0.5, 0.75)
  
  coef_matrix <- NULL
  vcov_list <- vector("list", length(prov))
  names(vcov_list) <- prov
  region_deaths <- numeric(length(prov))
  names(region_deaths) <- prov
  region_pm25_ranges <- list()
  
  all_pm25 <- unlist(lapply(plist, function(x) x$pm25))
  pm25_range_global <- range(all_pm25, na.rm = TRUE)
  
  # Fit regional models
  
  for (j in 1:length(prov)) {
    dat <- plist[[prov[j]]]
    var1 <- dat$pm25
    region_deaths[j] <- sum(dat$deaths, na.rm = TRUE)
    region_pm25_ranges[[prov[j]]] <- range(var1, na.rm = TRUE)
    
    if (nrow(dat) < 100) {
      warning("Insufficient data for region ", prov[j], ": ", nrow(dat), " observations")
      next
    }
    
    knots_pm25 <- quantile(var1, knots_values, na.rm = TRUE)
    
    # Build formula with available confounders
    confounder_terms <- if (length(available_confounders) > 0) {
      paste0("+ s(", available_confounders, ", bs = 'cr', k = 4)", collapse = " ")
    } else {
      ""
    }
    
    fmla_prov <- as.formula(paste(
      "deaths ~ offset(log(population)) +",
      "bs(pm25, df =", vardf, ", knots = knots_pm25, Boundary.knots = pm25_range_global) +",
      "s(days, bs = 'cc', k =", dfseas * yr + 1, ") +",
      "factor(year)",
      confounder_terms
    ))
    
    mod <- tryCatch(
      {
        mgcv::gam(fmla_prov, data = dat, family = quasipoisson)
      },
      error = function(e) {
        warning("Error fitting model for ", prov[j], ": ", e$message)
        return(NULL)
      }
    )
    
    if (is.null(mod)) next
    
    pm25_terms <- grep("^bs\\(pm25", names(coef(mod)), value = TRUE)
    
    if (length(pm25_terms) == 0) {
      warning("No PM2.5 terms found for ", prov[j])
      next
    }
    
    pm25_coefs <- coef(mod)[pm25_terms]
    pm25_vcov <- vcov(mod)[pm25_terms, pm25_terms]
    
    if (is.null(coef_matrix)) {
      actual_df <- length(pm25_coefs)
      coef_matrix <- matrix(
        data = NA, nrow = length(prov), ncol = actual_df,
        dimnames = list(prov)
      )
    }
    
    # Ensure consistent dimensions
    if (ncol(coef_matrix) == length(pm25_coefs)) {
      coef_matrix[j, ] <- pm25_coefs
      vcov_list[[j]] <- pm25_vcov
    } else {
      warning("Dimension mismatch for region ", prov[j], 
              ": expected ", ncol(coef_matrix), " coefficients, got ", length(pm25_coefs))
    }
    
    rm(dat, var1, knots_pm25, mod, pm25_coefs, pm25_vcov)
  }
  
  valid_rows <- which(!apply(is.na(coef_matrix), 1, all))
  if (length(valid_rows) < 2) {
    stop("Insufficient regions with successful model fits for meta-analysis")
  }
  
  # Meta-analysis
  
  meta_model <- tryCatch(
    {
      mixmeta::mixmeta(coef_matrix[valid_rows, ] ~ 1,
                       S = vcov_list[valid_rows],
                       control = list(showiter = FALSE)
      )
    },
    error = function(e) {
      stop("Error in meta-analysis: ", e$message)
    }
  )
  
  blup_results <- mixmeta::blup(meta_model, vcov = TRUE)
  
  pm25_pred_seq <- seq(pm25_range_global[1], pm25_range_global[2], by = 0.1)
  
  knots_national <- quantile(all_pm25, knots_values, na.rm = TRUE)
  
  # Create basis matrices with proper dimension handling
  actual_df <- length(meta_model$coefficients)
  
  basis_national <- splines::bs(pm25_pred_seq, 
                                df = actual_df, 
                                knots = knots_national,
                                Boundary.knots = pm25_range_global)
  
  # For reference value, ensure it's a matrix with proper dimensions
  basis_ref <- splines::bs(rep(reference_pm25, actual_df), 
                           df = actual_df, 
                           knots = knots_national,
                           Boundary.knots = pm25_range_global)
  
  # Take only the first row for reference (all rows are identical)
  if (nrow(basis_ref) > 1) {
    basis_ref <- basis_ref[1, , drop = FALSE]
  }
  
  # Ensure all dimensions match
  if (ncol(basis_national) != length(meta_model$coefficients) || 
      ncol(basis_ref) != length(meta_model$coefficients)) {
    stop(paste("Dimension mismatch:",
               "basis_national cols:", ncol(basis_national),
               "basis_ref cols:", ncol(basis_ref),
               "coefficients:", length(meta_model$coefficients)))
  }
  
  # Calculate relative risks with proper matrix operations
  pred_national <- basis_national %*% meta_model$coefficients
  ref_value <- basis_ref %*% meta_model$coefficients
  
  # Ensure ref_value is scalar for subtraction
  if (length(ref_value) > 1) {
    ref_value <- as.numeric(ref_value[1])
  }
  
  log_rr_national <- as.vector(pred_national) - as.vector(ref_value)
  rr_national <- exp(log_rr_national)
  
  # Calculate confidence intervals
  se_national <- sqrt(diag(basis_national %*% meta_model$vcov %*% t(basis_national)))
  se_ref <- sqrt(as.numeric(basis_ref %*% meta_model$vcov %*% t(basis_ref)))
  se_total <- sqrt(se_national^2 + se_ref^2)
  
  rr_low <- exp(log_rr_national - 1.96 * se_total)
  rr_high <- exp(log_rr_national + 1.96 * se_total)
  
  max_ylim <- ceiling(max(c(rr_high, 2.0), na.rm = TRUE) * 10) / 10
  max_ylim <- max(max_ylim, 1.5)  # Ensure minimum of 1.5
  
  pred_national <- list(
    predvar = pm25_pred_seq,
    allRRfit = rr_national,
    allRRlow = rr_low,
    allRRhigh = rr_high
  )
  
  n_regions <- length(valid_rows)
  n_plots <- n_regions + 1
  
  # Simple grid calculation function
  calculate_grid_dims <- function(n_plots) {
    if (n_plots <= 3) {
      return(list(nrow = 1, ncol = n_plots))
    } else if (n_plots <= 6) {
      return(list(nrow = 2, ncol = ceiling(n_plots / 2)))
    } else if (n_plots <= 12) {
      return(list(nrow = 3, ncol = ceiling(n_plots / 3)))
    } else {
      return(list(nrow = 4, ncol = ceiling(n_plots / 4)))
    }
  }
  
  grid_dims <- calculate_grid_dims(n_plots)
  
  # Create plots
  fig_width <- plot_width_per_panel * grid_dims$ncol
  fig_height <- plot_height_per_panel * grid_dims$nrow
  
  png(output_file,
      width = fig_width * res, height = fig_height * res,
      res = res, bg = "white"
  )
  
  par(
    mfrow = c(grid_dims$nrow, grid_dims$ncol), mar = c(3, 3, 3, 3),
    oma = c(4, 4, 3, 1), cex.main = 1.1, cex.lab = 0.9, cex.axis = 0.8,
    mgp = c(2, 0.7, 0), tcl = -0.3
  )
  
  predictions <- list()
  plot_count <- 0
  
  # Plot nationwide results
  plot_count <- plot_count + 1
  y_seq <- seq(0, max_ylim, by = 0.5)
  
  plot(pred_national$predvar, pred_national$allRRfit,
       type = "l", ylab = "", ylim = c(0.0, max_ylim), xlab = "", 
       xaxt = "n", yaxt = "n", main = "Nationwide", lwd = 2, col = "red"
  )
  
  # Add confidence interval
  polygon(c(pred_national$predvar, rev(pred_national$predvar)),
          c(pred_national$allRRlow, rev(pred_national$allRRhigh)),
          col = rgb(0, 0, 1, 0.2), border = NA)
  
  abline(h = 1, col = "black", lwd = 1, lty = 1)
  axis(2, at = y_seq, labels = format(y_seq, nsmall = 1), cex.axis = 0.8,
       las = 1, tck = -0.02)
  axis(1, at = seq(floor(min(all_pm25)), ceiling(max(all_pm25)), by = 20), 
       cex.axis = 0.8, tck = -0.02)
  
  # Add histogram
  breaks <- c(min(all_pm25) - 1, seq(pm25_range_global[1], pm25_range_global[2], length = 30), 
              max(all_pm25) + 1)
  hist_data <- hist(all_pm25, breaks = breaks, plot = FALSE)
  hist_data$density <- hist_data$density / max(hist_data$density) * 0.7
  hist_scaling_factor <- max_ylim / 1.8
  plot(hist_data,
       ylim = c(0, max(hist_data$density) * hist_scaling_factor),
       axes = FALSE, ann = FALSE,
       col = rgb(0.8, 0.8, 0.8, 0.5),
       border = rgb(0.7, 0.7, 0.7, 0.8),
       breaks = breaks, freq = FALSE, add = TRUE
  )
  
  counts <- pretty(hist_data$count, 3)
  prop <- max(hist_data$density) / max(hist_data$counts)
  axis(4, at = counts * prop, labels = counts, cex.axis = 0.8, las = 1, tck = -0.02)
  abline(v = reference_pm25, col = "red", lty = 2, lwd = 2)
  abline(v = quantile(all_pm25, c(0.05, 0.95)), col = grey(0.5), lty = 3, lwd = 1.5)
  
  predictions[["Nationwide"]] <- pred_national
  
  # Plot regional results
  valid_regions <- prov[valid_rows]
  
  for (i in 1:length(valid_regions)) {
    plot_count <- plot_count + 1
    region_name <- valid_regions[i]
    dat <- plist[[region_name]]
    var1 <- dat$pm25
    
    pm25_seq_region <- seq(min(var1, na.rm = TRUE), max(var1, na.rm = TRUE), by = 0.1)
    knots_region <- quantile(var1, knots_values, na.rm = TRUE)
    region_range <- range(var1, na.rm = TRUE)
    
    # Create basis matrices with consistent dimensions
    basis_region <- splines::bs(pm25_seq_region, 
                                df = actual_df, 
                                knots = knots_region, 
                                Boundary.knots = pm25_range_global)
    
    basis_ref_region <- splines::bs(rep(reference_pm25, actual_df), 
                                    df = actual_df, 
                                    knots = knots_region, 
                                    Boundary.knots = pm25_range_global)
    
    if (nrow(basis_ref_region) > 1) {
      basis_ref_region <- basis_ref_region[1, , drop = FALSE]
    }
    
    valid_idx <- which(prov[valid_rows] == region_name)
    
    # Predictions using BLUP with proper dimension handling
    pred_region <- basis_region %*% blup_results[[valid_idx]]$blup
    ref_value_region <- basis_ref_region %*% blup_results[[valid_idx]]$blup
    
    if (length(ref_value_region) > 1) {
      ref_value_region <- as.numeric(ref_value_region[1])
    }
    
    log_rr_region <- as.vector(pred_region) - (ref_value_region)
    rr_region <- exp(log_rr_region)
    
    se_region <- sqrt(diag(basis_region %*% blup_results[[valid_idx]]$vcov %*% 
                             t(basis_region)))
    se_ref_region <- sqrt(as.numeric(basis_ref_region %*% blup_results[[valid_idx]]$vcov %*% 
                                       t(basis_ref_region)))
    se_total_region <- sqrt(se_region^2 + se_ref_region^2)
    
    rr_low_region <- exp(log_rr_region - 1.96 * se_total_region)
    rr_high_region <- exp(log_rr_region + 1.96 * se_total_region)
    
    # Plot
    plot(pm25_seq_region, rr_region,
         type = "l", ylim = c(0.0, max_ylim), lwd = 2, col = "red",
         xaxt = "n", yaxt = "n", xlab = "", ylab = "",
         main = region_name
    )
    
    polygon(c(pm25_seq_region, rev(pm25_seq_region)),
            c(rr_low_region, rev(rr_high_region)),
            col = rgb(0, 0, 1, 0.2), border = NA)
    
    abline(h = 1, col = "black", lwd = 1, lty = 1)
    axis(2, at = y_seq, labels = format(y_seq, nsmall = 1), cex.axis = 0.8,
         las = 1, tck = -0.02)
    axis(1, at = seq(floor(min(var1)), ceiling(max(var1)), by = 20),
         cex.axis = 0.8, tck = -0.02)
    
    # Add histogram
    breaks_prov <- c(min(var1) - 1, seq(min(var1), max(var1), length = 30), 
                     max(var1) + 1)
    hist_prov <- hist(var1, breaks = breaks_prov, plot = FALSE)
    hist_prov$density <- hist_prov$density / max(hist_prov$density) * 0.7
    prop_prov <- max(hist_prov$density) / max(hist_prov$counts)
    counts_prov <- pretty(hist_prov$count, 3)
    
    plot(hist_prov,
         ylim = c(0, max(hist_prov$density) * hist_scaling_factor),
         axes = FALSE, ann = FALSE,
         col = rgb(0.8, 0.8, 0.8, 0.5),
         border = rgb(0.7, 0.7, 0.7, 0.8),
         breaks = breaks_prov, freq = FALSE, add = TRUE
    )
    
    axis(4, at = counts_prov * prop_prov, labels = counts_prov, cex.axis = 0.8,
         las = 1, tck = -0.02)
    abline(v = reference_pm25, col = "red", lty = 2, lwd = 2)
    abline(v = quantile(var1, c(0.05, 0.95)), col = grey(0.5), lty = 3, lwd = 1.5)
    
    predictions[[region_name]] <- list(
      predvar = pm25_seq_region,
      allRRfit = rr_region,
      allRRlow = rr_low_region,
      allRRhigh = rr_high_region
    )
  }
  
  # Fill remaining panels if needed
  if (plot_count < grid_dims$nrow * grid_dims$ncol) {
    remaining_panels <- (grid_dims$nrow * grid_dims$ncol) - plot_count
    for (i in 1:remaining_panels) {
      plot.new()
    }
  }
  
  # Add labels
  mtext(
    text = "Daily PM2.5 (\u03bc\u0067\u002f\u006d\u00b3)", side = 1, line = 2.5, 
    outer = TRUE, cex = 1.1
  )
  mtext(text = "Relative risk", side = 2, line = 2.5, outer = TRUE, cex = 1.1)
  mtext(
    text = paste0(
      reference_name, " (PM2.5 reference: ",
      reference_pm25, " \u03bc\u0067\u002f\u006d\u00b3)"
    ),
    side = 3, line = 1, outer = TRUE, cex = 1.2, font = 2
  )
  
  dev.off()
  
  invisible(list(
    predictions = predictions,
    meta_model = meta_model,
    reference_pm25 = reference_pm25,
    reference_name = reference_name,
    grid_dimensions = grid_dims,
    max_ylim = max_ylim
  ))
}



#' Full analysis pipeline for the the All-cause mortality associated to PM2.5 (Air Pollution)
#'
#' @description This function performs comprehensive analysis of the relationship
#' between PM2.5 and all-cause mortality using GAM models. It includes meta-analysis,
#' lag analysis, distributed lag modeling, and exposure-response plots with
#' attributable fraction (AF) and attributable number (AN) calculations for
#' multiple reference standards.
#'
#' @param data_path Character. Path to data file
#' @param date_col Character. Name of date column
#' @param region_col Character. Name of region column
#' @param pm25_col Character. Name of PM2.5 column
#' @param deaths_col Character. Name of deaths column
#' @param humidity_col Character. Name of humidity column
#' @param precipitation_col Character. Name of precipitation column
#' @param tmax_col Character. Name of temperature column
#' @param tmean_col Character. Name of mean temperature column. Defaults to NULL.
#' @param wind_speed_col Character. Name of wind speed column. Defaults to "wind_speed".
#' @param population_col Character. Name of population column
#' @param max_lag Integer. Maximum lag days. Defaults to 2.
#' @param var_name Character. Variable name for labeling. Defaults to "pm25".
#' @param family Character. Distribution family for GAM model. Defaults to "quasipoisson".
#' @param reference_pm25 Numeric. Reference value for AF/AN calculations. Defaults to 15.
#' @param output_dir Character. Output directory for results. Defaults to NULL.
#' @param save_outputs Logical. Whether to save plots and results. Defaults to FALSE.
#' analysis. Defaults to TRUE.
#' Defaults to TRUE.
#' @param reference_standards List of reference thresholds used in exposure-response plots.
#' Defaults include WHO (\eqn{15~\mu g/m^3}) daily air quality standards.
#'
#' @return List containing:
#' \describe{
#'   \item{data}{Processed data with lag variables}
#'   \item{meta_analysis}{Meta-analysis results with AF/AN calculations}
#'   \item{lag_analysis}{Lag-specific analysis results}
#'   \item{distributed_lag_analysis}{Distributed lag model results (if requested)}
#'   \item{plots}{List of generated plots (forest, lags, distributed lags)}
#'   \item{exposure_response_plots}{Exposure-response plots for each reference
#'   standard (if requested)}
#'   \item{reference_specific_af_an}{AF/AN calculations specific to each
#'   reference standard (if requested)}
#'   \item{descriptive_stats}{Summary statistics of key variables}
#' }
#'
#' @export
air_pollution_do_analysis <- function(data_path,
                                      date_col,
                                      region_col,
                                      pm25_col,
                                      deaths_col,
                                      humidity_col,
                                      precipitation_col,
                                      tmax_col,
                                      tmean_col,
                                      wind_speed_col,
                                      population_col,
                                      max_lag,
                                      var_name,
                                      family,
                                      reference_pm25,
                                      output_dir,
                                      save_outputs,
                                      reference_standards) {
  
  # Step 1: Load data
  data <- load_air_pollution_data(
    data_path = data_path,
    date_col = date_col,
    region_col = region_col,
    pm25_col = pm25_col,
    deaths_col = deaths_col,
    humidity_col = humidity_col,
    precipitation_col = precipitation_col,
    tmax_col = tmax_col,
    tmean_col = tmean_col, # Pass tmean_col to load_air_pollution_data
    wind_speed_col = wind_speed_col,
    population_col = population_col
  )
  
  # Step 2: Create lag variables
  data_with_lags <- create_air_pollution_lags(data, max_lag)
  
  # Step 3: Descriptive statistics
  
  desc_stats <- air_pollution_descriptive_stats(
    data_with_lags,
    output_dir = output_dir,
    save_outputs = save_outputs
  )
  
  # Step 4: Meta-analysis
  
  meta_results <- air_pollution_meta_analysis(data_with_lags, var_name, family)
  
  # Step 5: Lag analysis
  
  lag_results <- analyze_air_pollution_lags(data_with_lags, max_lag, family)
  
  # Step 6: Create forest and lag plots
  forest_plot <- plot_air_pollution_forest(
    meta_results,
    output_dir = output_dir,
    save_plot = save_outputs
  )
  
  lag_plot <- plot_air_pollution_lags(
    lag_results,
    max_lag,
    output_dir = output_dir,
    save_plot = save_outputs
  )
  
  # Step 7: Distributed lag analysis (optional)
  gam_results <- NULL
  gam_plots <- NULL
  
  gam_results <- analyze_air_pollution_gam(
    data_with_lags,
    reference_pm25, max_lag, family
  )
  
  gam_plots <- plot_air_pollution_gam(
    gam_results, 
    output_dir, 
    max_lag, 
    save_plot = save_outputs
  )
  
  # Step 8: Exposure-response plots (optional)
  exposure_response_plots <- NULL
  reference_specific_af_an <- NULL
  
  if (!is.null(meta_results)) {
    
    exposure_response_plots <- list()
    reference_specific_af_an <- list()
    
    for (ref_std in reference_standards) {
      
      ref_af_an <- analyze_air_pollution_gam(
        data_with_lags,
        reference = ref_std$value, max_lag, family
      )
      
      reference_specific_af_an[[ref_std$name]] <- ref_af_an
      
      meta_summary <- ref_af_an$meta_results %>%
        filter(.data$lag_group == "0") %>%
        dplyr::select(all_of(c("AF", "AN")))
      
      plot_results <- create_air_pollution_exposure_plots(
        data_with_lags = data_with_lags,
        meta_results = meta_results,
        reference_pm25 = ref_std$value,
        reference_name = ref_std$name,
        output_file = if (!is.null(output_dir)) {
          file.path(output_dir, paste0(tolower(ref_std$name), "_exposure_response_plots.png"))
        } else {
          file.path("air_pollution_results", paste0(tolower(ref_std$name), "_exposure_response_plots.png"))
        },
        var_name = var_name,
        family = family
      )
      
      exposure_response_plots[[ref_std$name]] <- plot_results
    }
  }
  
  # Step 9: Save results to CSV
  if (save_outputs && !is.null(output_dir)) {
    
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    
    # Save cleaned region results
    region_results_clean <- meta_results$region_results %>%
      dplyr::select(-c("data", "model"))
    
    write.csv(region_results_clean, file.path(output_dir, "region_results.csv"),
              row.names = FALSE
    )
    
    write.csv(lag_results, file.path(output_dir, "lag_results.csv"), row.names = FALSE)
    
    if (!is.null(gam_results)) {
      write.csv(gam_results$region_results, file.path(
        output_dir,
        "region_gam_results.csv"
      ),
      row.names = FALSE
      )
      
      write.csv(gam_results$meta_results, file.path(output_dir, "meta_gam_results.csv"),
                row.names = FALSE
      )
    }
    
    if (!is.null(reference_specific_af_an)) {
      for (ref_name in names(reference_specific_af_an)) {
        write.csv(
          reference_specific_af_an[[ref_name]]$meta_results,
          file.path(output_dir, paste0(tolower(ref_name), "_af_an_results.csv")),
          row.names = FALSE
        )
      }
    }
  }
  
  # Final summary
  
  return(list(
    data = data_with_lags,
    meta_analysis = meta_results,
    lag_analysis = lag_results,
    distributed_lag_analysis = gam_results,
    plots = list(
      forest = forest_plot,
      lags = lag_plot,
      distributed_lags = gam_plots
    ),
    exposure_response_plots = exposure_response_plots,
    reference_specific_af_an = reference_specific_af_an,
    descriptive_stats = desc_stats
  ))
}