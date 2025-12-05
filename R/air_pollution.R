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
#' @param wind_speed_col Character. Name of wind speed column in the dataframe.
#' Defaults to "wind_speed".
#' @param Categorical_Others Optional. Character vector of additional categorical
#' variables (e.g., "sex", "age_group"). Defaults to NULL.
#'
#' @param Continuous_Others Optional. Character vector of additional continuous
#' variables (e.g., "tmean", "population"). Defaults to NULL.
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
                                    wind_speed_col = "wind_speed",
                                    Categorical_Others= NULL,
                                    Continuous_Others= NULL
) {
  
  if (!file.exists(data_path)) {
    stop("Data file not found at: ", data_path)
  }
  
  
  # Check for missing columns
  Others <- c(Categorical_Others, Continuous_Others)# all additional variables
  data0 <- if(is.character(data_path)) read.csv(data_path) else data_path
  Col_NA <- setdiff(c(date_col,region_col,pm25_col,deaths_col, humidity_col,precipitation_col, 
                      tmax_col, wind_speed_col, Others), names(data0))
  
  if(length(Col_NA)>0 ) {
    stop(paste0("Variables not found: ", paste(Col_NA, collapse = ", "),  ". Please check your dataset."))
    
  }
  
  
  # Rename main variables using tidy evaluation
  data <- data0 %>%
    rename(
      date         = !!sym(date_col),
      region       = !!sym(region_col),
      pm25         = !!sym(pm25_col),
      deaths       = !!sym(deaths_col),
      humidity     = !!sym(humidity_col),
      precipitation= !!sym(precipitation_col),
      tmax         = !!sym(tmax_col),
      wind_speed   = !!sym(wind_speed_col)
    )%>%
    select(
      date, region, pm25, deaths, humidity, precipitation,
      tmax, wind_speed,  all_of(Others) )
  
  
  # Rename 'Others' after removing spaces
  Nospace <- if(is.null(Others)) NULL else gsub(" ","_", Others)
  if(!is.null(Others)){ names(data)[names(data) %in% Others] <- Nospace}
  
  # Enhanced date parsing function
  universal_date <- function(x) {
    lubridate::parse_date_time(
      x,
      orders = c("ymd", "dmy", "mdy", "Ymd", "dmY", "mdY", 
                 "Y/m/d", "d/m/Y", "m/d/Y", "Y-m-d H:M:S", "d/m/Y H:M", "m/d/Y H:M")
    ) %>% as.Date()
  }
  
  data <- data %>%
    dplyr::mutate(
      date = universal_date(date),
      year = lubridate::year(date),
      month = lubridate::month(date),
      day = lubridate::day(date),
      dow = as.character(lubridate::wday(date, label = TRUE)),
      time = dplyr::row_number()
    ) %>%
    dplyr::arrange(date)
  
  
  # Convert to data.table for efficient aggregation
  data <- as.data.table(data)
  
  # Define grouping variables
  group_vars <- c("date", "region","dow", Categorical_Others)
  
  # Define variables to aggregate 
  agg_vars <- setdiff(names(data), c("deaths", group_vars))# c("pm25", "tmax", "precipitation", "humidity", "wind_speed", Continuous_Others)
  
  
  # Perform aggregation
  data <- data[, c(
    list(deaths = sum(deaths, na.rm = TRUE)),
    lapply(.SD, function(x) round(mean(x, na.rm = TRUE), 2))
  ), by = group_vars, .SDcols = agg_vars]
  
  # Sort and select columns
  data <- data %>% arrange(region, date)
  
  return(data)
}



#===============================================================================
#' Create lagged values for PM2.5 variable and average lag column.
#'
#' @description Creates new variables in a dataframe for lags and means over lag
#' periods.
#'
#' @param data Dataframe from  load_air_pollution_data() containing a daily 
#'             time series of health and environmental data.
#' @param max_lag Integer. The maximum lag days for outdoor PM2.5. Defaults to 2.
#'
#' @return Dataframe with added columns for lagged PM2.5 concentration.
#'
#' @export

create_air_pollution_lags <- function( data, max_lag = 14 ) {
  # Validate input
  if (max_lag < 1) {
    stop("max_lag must be at least 1")
  }
  
  # Create lag variables
  lag_vars <- paste0("pm25_lag", 1:max_lag)
  all_lag_vars <- c("pm25", lag_vars)
  avg_lag_name <- paste0("pm25_lag0_", max_lag)
  
  # data_with_lags
  data_with_lags <- data %>%
    group_by(region) %>%
    arrange(date)
  
  # Create individual lag variables
  for (i in 1:max_lag) {
    lag_name <- paste0("pm25_lag", as.character(i))
    data_with_lags <- data_with_lags %>%
      mutate(!!lag_name := lag(pm25, i))
  }
  
  #Average lags
  data_with_lags <- data_with_lags%>%
    dplyr::mutate(!!avg_lag_name := rowMeans(across(all_of(all_lag_vars)), na.rm = FALSE))
  
  data_with_lags <- data_with_lags %>%
    filter(dplyr::if_all(all_of(all_lag_vars), ~!is.na(.))) %>%
    dplyr::ungroup()
  
  return(data_with_lags)
}


#===============================================================================
#' Show descriptive statistics
#'
#' @description Generates summary statistics for climate, environmental and health data.
#'
#' @param data Dataframe containing a daily time series of climate, environmental
#' and health data
#' @env_vars  list of all environmental variables to be used in EDA
#' @param output_dir Character. Directory to save descriptive statistics.
#' Defaults to NULL.
#' @param save_outputs Logical. Whether to save outputs. Defaults to FALSE.
#' @param moving_average_window Numeric. Window size for moving average calculations.
#' Defaults to 3 (3-day moving average).
#'
#' @export
air_pollution_descriptive_stats <- function(data, 
                                            env_lables = c(
                                              "pm25" = "PM2.5 (µg/m³)",
                                              "tmax" = "Max Temperature (°C)",
                                              "tmean" = "Mean Temperature (°C)",
                                              "precipitation" = "Precipitation (mm)", 
                                              "humidity" = "Humidity (%)",
                                              "wind_speed" = "Wind Speed (m/s)"),
                                            Base_colors = NULL, 
                                            save_outputs = FALSE,
                                            output_dir = NULL,
                                            moving_average_window = 3L
) {
  # Validate moving_average_window parameter
  if (!is.numeric(moving_average_window) || moving_average_window < 1) {
    stop("moving_average_window must be a positive integer")
  }
  # Warning moving average window size
  if (moving_average_window < 3) {
    warning("Window < 3 provides minimal smoothing. Consider using window >= 3 for better results.")
  }
  moving_average_window <- as.integer(moving_average_window)
  
  # Ensure data is a data.frame / tibble
  if (!("data.frame" %in% class(data))) stop("data must be a data.frame or tibble")
  if (!all(c("date", "deaths", "region") %in% names(data))) {
    stop("data must contain at least the columns: date, deaths, region")
  }
  
  # Determine which environmental variables to use based on env_lables and data columns
  env_vars <- intersect(names(env_lables), names(data))
  if (length(env_vars) == 0) {
    warning("No environmental variables from env_lables were found in the data. Continuing with deaths only.")
  }
  
  # Build variable lists used by the function
  all_ts_vars <- c("deaths", env_vars)
  stat_vars <- all_ts_vars
  
  # Labels and titles (only for variables present)
  plot_labels <- c(deaths = "Number of Deaths")
  plot_labels[env_vars] <- env_lables[env_vars]
  plot_titles <- c(deaths = "Daily Deaths")
  plot_titles[env_vars] <- paste("Daily", env_lables[env_vars])
  
  # Colors: ensure we have enough, name them by all_ts_vars
  if (is.null(Base_colors)) {
    Base_colors <- c( "#0000EE", "#E31A1C", "green", "yellow", "#B15928",
                      "#F0027F", "#800080", "#FF7F00", "#6A3D9A","#8DD3C7",
                      "#BEBADA", "#FB8072", "#33A02C", "#80B1D3","#FDB462",
                      "#B3DE69", "#FCCDE5", "#CCEBC5", "#1F78B4","#4DAF4A")
  }
  # Ensure enough colors
  if (length(Base_colors) < length(all_ts_vars)) {
    Base_colors <- rep(Base_colors, length.out = length(all_ts_vars))
  }
  plot_colors <- setNames(Base_colors[seq_along(all_ts_vars)], all_ts_vars)
  
  # Check save_outputs / output_dir
  if (save_outputs && is.null(output_dir)) {
    stop("An output directory must be passed if save_outputs==TRUE.")
  }
  if (save_outputs && !file.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Create national dataset: deaths sum, env_vars mean
  data_n <- data %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(
      deaths = sum(deaths, na.rm = TRUE),
      dplyr::across(all_of(env_vars), ~ mean(.x, na.rm = TRUE)),
      .groups = "drop"
    )
  
  # Create regional dataset
  data_r <- data %>%
    dplyr::group_by(region, date) %>%
    dplyr::summarise(
      deaths = sum(deaths, na.rm = TRUE),
      dplyr::across(all_of(env_vars), ~ mean(.x, na.rm = TRUE)),
      .groups = "drop"
    )
  
  # 1. HISTOGRAM OF MORTALITY
  if (save_outputs) {
    png(file.path(output_dir, "mortality_histogram.png"), width = 800, height = 600)
  }
  mortality_range <- range(data_n$deaths, na.rm = TRUE)
  breaks <- seq(floor(mortality_range[1]), ceiling(mortality_range[2]) + 1, by = 1)
  p <- ggplot2::ggplot(data_n, ggplot2::aes(x = deaths)) +
    ggplot2::geom_histogram(breaks = breaks, fill = "#1F77B4", color = "white", alpha = 0.8) +
    ggplot2::labs(title = "All-Cause Mortality (Countrywide)",
                  subtitle = "Distribution of recorded deaths across all provinces",
                  x = "Number of Deaths", y = "Frequency") +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 16, hjust = 0.5),
                   plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5, color = "gray40"),
                   axis.title = ggplot2::element_text(face = "bold"),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_line(linetype = "dashed", color = "gray85"))
  print(p)
  if (save_outputs) dev.off()
  
  # 2. SCATTER PLOTS (deaths vs each env variable)
  if (save_outputs) {
    png(file.path(output_dir, "scatter_plots.png"), width = 1000, height = 800)
  }
  plots <- list()
  if (length(env_vars) > 0) {
    for (i in seq_along(env_vars)) {
      xvar <- env_vars[i]
      plots[[i]] <- ggplot2::ggplot(data = data_n, ggplot2::aes(x = .data[[xvar]], y = deaths)) +
        ggplot2::geom_point(alpha = 0.6) +
        ggplot2::geom_smooth(method = "loess", color = "red") +
        ggplot2::ggtitle(paste("Deaths against", xvar)) +
        ggplot2::theme_bw()
    }
    n_plots <- length(plots)
    ncol <- ifelse(n_plots >= 3, 2, 1)
    nrow <- ceiling(n_plots / ncol)
    grid_plot_scatter <- gridExtra::grid.arrange(grobs = plots, nrow = nrow, ncol = ncol)
    print(grid_plot_scatter)
  } else {
    message("No environmental variables to plot scatter plots against deaths.")
  }
  if (save_outputs) dev.off()
  
  # 3. SUMMARY STATISTICS TABLE
  stat_vars <- c("deaths", env_vars)
  regional_stats <- data_r %>%
    dplyr::group_by(region) %>%
    dplyr::summarise(
      dplyr::across(all_of(stat_vars),
                    list(
                      min = ~round(min(.x, na.rm = TRUE), 2),
                      max = ~round(max(.x, na.rm = TRUE), 2),
                      sd = ~round(sd(.x, na.rm = TRUE), 2),
                      IQR = ~round(stats::IQR(.x, na.rm = TRUE), 2),
                      missing = ~round(sum(is.na(.x)) / length(.x) * 100, 2)
                    ),
                    .names = "{.col}_{.fn}"),
      .groups = "drop"
    )
  national_stats <- data_n %>%
    dplyr::summarise(
      dplyr::across(all_of(stat_vars),
                    list(
                      min = ~round(min(.x, na.rm = TRUE), 2),
                      max = ~round(max(.x, na.rm = TRUE), 2),
                      sd = ~round(sd(.x, na.rm = TRUE), 2),
                      IQR = ~round(stats::IQR(.x, na.rm = TRUE), 2),
                      missing = ~round(sum(is.na(.x)) / length(.x) * 100, 2)
                    ),
                    .names = "{.col}_{.fn}"),
      .groups = "drop"
    ) %>%
    dplyr::mutate(region = "Countrywide")
  combined_stats <- dplyr::bind_rows(regional_stats, national_stats)
  table_data <- combined_stats %>%
    tidyr::pivot_longer(cols = -region, names_to = c("variable", "statistic"),
                        names_pattern = "(.+)_(.+)", values_to = "value") %>%
    tidyr::pivot_wider(names_from = c(region, statistic), values_from = "value")
  table_data$Variable <- ifelse(table_data$variable %in% names(plot_titles),
                                plot_titles[table_data$variable], table_data$variable)
  table_formatted <- table_data %>% dplyr::select(Variable, everything(), -variable)
  print(table_formatted, n = Inf)
  if (save_outputs) {
    utils::write.csv(table_formatted, file.path(output_dir, "descriptive_statistics_table.csv"), row.names = FALSE)
  }
  
  # 4. CORRELATION MATRIX AND PLOT
  cor_data <- data_n %>% dplyr::select(dplyr::all_of(c("deaths", env_vars))) %>% tidyr::drop_na()
  if (nrow(cor_data) > 0 && ncol(cor_data) > 1) {
    cor_matrix <- stats::cor(cor_data, method = "pearson", use = "complete.obs")
    cor_matrix_rounded <- round(cor_matrix, 3)
    # apply labels if available
    rownames(cor_matrix_rounded) <- ifelse(rownames(cor_matrix_rounded) %in% names(plot_titles),
                                           plot_titles[rownames(cor_matrix_rounded)],
                                           rownames(cor_matrix_rounded))
    colnames(cor_matrix_rounded) <- ifelse(colnames(cor_matrix_rounded) %in% names(plot_titles),
                                           plot_titles[colnames(cor_matrix_rounded)],
                                           colnames(cor_matrix_rounded))
    if (save_outputs) {
      png(file.path(output_dir, "correlation_matrix_plot.png"), width = 800, height = 800, res = 150)
      corrplot::corrplot(cor_matrix_rounded, method = "color", type = "lower",
                         order = "original", tl.cex = 0.8, tl.col = "black", tl.srt = 45,
                         addCoef.col = "black", number.cex = 0.7,
                         col = grDevices::colorRampPalette(c("blue", "white", "red"))(200),
                         title = "Correlation Matrix of Climate Variables and Mortality", mar = c(0,0,2,0))
      dev.off()
    }
  }
  
  # 5. TIME SERIES PLOTS WITH MOVING AVERAGE
  national_df_ma <- data_n %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(
      dplyr::across(all_of(c("deaths", env_vars)),
                    ~ zoo::rollmean(.x, k = moving_average_window, fill = NA, align = "center"),
                    .names = "{.col}_ma{moving_average_window}")
    )
  ma_title_suffix <- paste0(moving_average_window, "-day Moving Average")
  ts_plots <- list()
  for (i in seq_along(all_ts_vars)) {
    var <- all_ts_vars[i]
    ma_col <- paste0(var, "_ma", moving_average_window)
    # Only plot if var present in national_df_ma
    if (!(var %in% names(national_df_ma))) next
    color_for_var <- ifelse(var %in% names(plot_colors), plot_colors[var], "black")
    ts_plots[[i]] <- ggplot2::ggplot(national_df_ma, ggplot2::aes(x = date)) +
      ggplot2::geom_line(ggplot2::aes(y = .data[[var]]), color = "lightgray", alpha = 0.7, linewidth = 0.3, na.rm = TRUE) +
      ggplot2::geom_line(ggplot2::aes(y = .data[[ma_col]]), color = color_for_var, linewidth = 0.8, na.rm = TRUE) +
      ggplot2::labs(title = paste(plot_titles[var], "with", ma_title_suffix), x = "Date", y = plot_labels[var]) +
      ggplot2::theme_minimal()
  }
  if (length(ts_plots) > 0) {
    ts_ncol <- 2
    ts_nrow <- ceiling(length(ts_plots) / ts_ncol)
    grid_plot_ts <- gridExtra::grid.arrange(grobs = ts_plots, ncol = ts_ncol, nrow = ts_nrow)
    print(grid_plot_ts)
    if (save_outputs) {
      filename <- paste0("National_time_series_plots_", moving_average_window, "day_ma.png")
      png(file.path(output_dir, filename), width = 1800, height = 1400, res = 150)
      gridExtra::grid.arrange(grobs = ts_plots, ncol = ts_ncol, nrow = ts_nrow)
      dev.off()
    }
  } else {
    message("No time-series plots were generated (no variables found).")
  }
  
  invisible(national_df_ma)
}



#' Fit GAM model
#'
#' @description Fit a generalized additive model (mgcv::gam) including pm25 and its lagged
#' variables (pm25_lag1, ..., pm25_lagN)
#'
#' @param data_with_lags data.frame or tibble containing the outcome, confounders
#'   and pm25 lag variables.
#' @param max_lag integer. Maximum lag to include. Default: 14
#' @param family character or family object passed to mgcv::gam. Default "quasipoisson".
#' @param conf_level numeric. Confidence level for CI (default 0.95).
#'
#' @return A list with components:
#'   - model: the fitted mgcv::gam object (or NULL if fit failed)
#'   - coef_table: data.frame with columns: lag (0 for pm25, 1..N for pm25_lag#,
#'       and "0-N" for cumulative), pm25_variable, coef, se, ci.lb, ci.ub
#'   - vcov_used_for_cumulative: logical; TRUE if vcov() was used to compute cumulative SE
#'
#' @keywords internal
fit_air_pollution_gam <- function(data_with_lags,
                                  max_lag = 14L,
                                  family = "quasipoisson"
) {
  
  # Build expected lag variable names and detect which are present
  max_lag <- as.integer(max_lag)
  if (is.na(max_lag) || max_lag < 0) stop("max_lag must be a non-negative integer")
  lag_vars_expected <- c("pm25", if (max_lag >= 1) paste0("pm25_lag", 
                                                          seq_len(max_lag)) else character())
  present_lag_vars <- intersect(lag_vars_expected, names(data_with_lags))
  if (length(present_lag_vars) == 0) stop("No pm25 variables found in dataset ", 
                                          "(expected at least 'pm25')")
  
  # Build GAM formula including present lag terms
  yr <- length(unique(data_with_lags$year))
  dfseas <- 6L
  lag_formula <- paste(present_lag_vars, collapse = " + ")
  
  GAM_formula <- as.formula(
    paste0(
      "deaths ~ ", lag_formula,
      " + s(time, k = ", dfseas * yr, ")",
      " + s(tmax, k = 3)",
      " + s(humidity, k = 3)",
      " + s(precipitation, k = 3)",
      " + s(wind_speed, k = 3)",
      " + dow + offset(log(population))"
    )
  )
  
  # Fit model
  model <- tryCatch(
    mgcv::gam(GAM_formula, data = data_with_lags, family = family),
    error = function(e) {
      warning("Model fitting failed: ", e$message)
      NULL
    }
  )
  
  # Prepare output structure
  conf_level = 0.95
  z <- stats::qnorm((1 + conf_level) / 2)
  coef_table <- NULL
  used_vcov <- FALSE
  
  if (is.null(model)) {
    return(list(model = NULL, coef_table = NULL, vcov_used_for_cumulative = FALSE))
  }
  
  # Extract coefficients and try vcov()
  coefs_all <- coef(model)
  V_full <- tryCatch(vcov(model), error = function(e) {
    warning("vcov(model) failed: ", e$message)
    NULL
  })
  
  # Helper to extract se for a named parameter
  se_from_vcov <- function(param) {
    if (!is.null(V_full) && param %in% rownames(V_full)) {
      se_val <- sqrt(as.numeric(V_full[param, param]))
      return(as.numeric(se_val))
    }
    # fallback: try summary(model)$se
    s <- tryCatch({
      ssum <- summary(model)
      if (!is.null(ssum$se) && param %in% names(ssum$se)) as.numeric(ssum$se[param]) else NA_real_
    }, error = function(e) NA_real_)
    return(as.numeric(s))
  }
  
  # Build per-lag rows
  rows <- vector("list", length = length(present_lag_vars))
  for (i in seq_along(present_lag_vars)) {
    nm <- present_lag_vars[i]
    beta <- if (nm %in% names(coefs_all)) as.numeric(coefs_all[nm]) else NA_real_
    se <- se_from_vcov(nm)
    ci_lb <- beta - z * se
    ci_ub <- beta + z * se
    lag_index <- if (nm == "pm25") 0L else as.integer(sub("^pm25_lag", "", nm))
    rows[[i]] <- data.frame(
      lag = lag_index,
      pm25_variable = nm,
      coef = beta,
      se = se,
      ci.lb = ci_lb,
      ci.ub = ci_ub,
      stringsAsFactors = FALSE
    )
  }
  coef_table <- do.call(rbind, rows)
  row.names(coef_table) <- NULL
  
  # Cumulative across available lag vars
  lag_vars_for_cum <- intersect(lag_vars_expected, names(coefs_all))
  if (length(lag_vars_for_cum) > 0) {
    coefs_sub <- as.numeric(coefs_all[lag_vars_for_cum])
    names(coefs_sub) <- lag_vars_for_cum
    cum_coef <- sum(coefs_sub, na.rm = TRUE)
    
    # try to compute cumulative variance using vcov: Var(sum) = 1' V_sub 1
    if (!is.null(V_full) && all(lag_vars_for_cum %in% rownames(V_full))) {
      V_sub <- V_full[lag_vars_for_cum, lag_vars_for_cum, drop = FALSE]
      one <- rep(1, length(lag_vars_for_cum))
      cum_var <- as.numeric(t(one) %*% V_sub %*% one)
      if (is.na(cum_var) || cum_var < 0) cum_var <- pmax(cum_var, 0)
      cum_se <- sqrt(cum_var)
      used_vcov <- TRUE
    } else {
      # fallback: naive independence assumption
      se_sub <- vapply(lag_vars_for_cum, se_from_vcov, numeric(1))
      cum_se <- sqrt(sum((se_sub)^2, na.rm = TRUE))
      used_vcov <- FALSE
      warning("vcov() unavailable or missing lag terms; cumulative SE computed ", 
              "with naive sqrt(sum(se^2)) fallback - may underestimate variance.")
    }
    
    cum_ci_lb <- cum_coef - z * cum_se
    cum_ci_ub <- cum_coef + z * cum_se
    
    cum_row <- data.frame(
      lag = paste0("0-", max_lag),
      pm25_variable = paste0("pm25_lag0_", max_lag),
      coef = cum_coef,
      se = cum_se,
      ci.lb = cum_ci_lb,
      ci.ub = cum_ci_ub,
      stringsAsFactors = FALSE
    )
    coef_table <- rbind(coef_table, cum_row)
    row.names(coef_table) <- NULL
  }
  
  # tidy ordering: numeric lags ascending and cumulative last
  coef_table$lag_order <- suppressWarnings(as.numeric(as.character(coef_table$lag)))
  coef_table$lag_order[is.na(coef_table$lag_order)] <- Inf
  coef_table <- coef_table[order(coef_table$lag_order), c("lag", "pm25_variable", "coef", "se", "ci.lb", "ci.ub")]
  
  return(list(coef_table = coef_table))
}


#' Perform meta analysis with multiple lag structures
#'
#' @description Implements distributed lag model. Individual lag
#' coefficients and cumulative effects are extracted and perform meta analysis
#'
#' @param data_with_lags Lagged data
#' @param max_lag Integer. Maximum lag days. Defaults to 14
#' @param family Character string indicating the distribution family used in the GAM.
#'
#' @return Dataframe with lag-specific results including for regional and national
#'
#' @keywords internal
air_pollution_meta_analysis <- function(data_with_lags,
                                        max_lag = 14L,
                                        family = "quasipoisson"
) {
  
  # Fit distributed-lag model per region
  region_results <- data_with_lags %>%
    dplyr::group_by(region) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      model_results = purrr::map2(data, region, ~ fit_air_pollution_gam(.x, max_lag, family))
    ) %>%
    dplyr::select(region, model_results) %>%
    dplyr::ungroup()
  
  if (nrow(region_results) < 1) stop("At least 1 region needed for meta-analysis")
  
  # Extract per-region coef_table
  all_coefs <- region_results %>%
    dplyr::mutate(coef_table = purrr::map(model_results, "coef_table")) %>%
    dplyr::select(region, coef_table) %>%
    tidyr::unnest(coef_table)
  
  # Preserve the appearance order of lag and pm25_variable
  all_coefs <- all_coefs %>%
    dplyr::mutate(
      lag = factor(as.character(lag), levels = unique(as.character(lag))),
      pm25_variable = factor(as.character(pm25_variable), levels = unique(as.character(pm25_variable)))
    )
  
  # Calculation by lag and pm25_variable
  meta_results <- all_coefs %>%
    dplyr::group_by(lag, pm25_variable) %>%
    dplyr::group_modify(~{
      valid_data <- .x %>% dplyr::filter(!is.na(coef) & !is.na(se))
      if (nrow(valid_data) < 1) {
        return(data.frame(
          coef = NA_real_, se = NA_real_, ci.lb = NA_real_, ci.ub = NA_real_,
          pval = NA_real_, I2 = NA_real_, stringsAsFactors = FALSE
        ))
      }
      res <- tryCatch({
        fit <- metafor::rma(yi = valid_data$coef, sei = valid_data$se, method = "REML")
        data.frame(
          coef = as.numeric(fit$b),
          se = as.numeric(fit$se),
          ci.lb = as.numeric(fit$ci.lb),
          ci.ub = as.numeric(fit$ci.ub),
          pval = if (!is.null(fit$pval)) as.numeric(fit$pval) else NA_real_,
          I2 = if (!is.null(fit$I2)) as.numeric(fit$I2) else NA_real_,
          stringsAsFactors = FALSE
        )
      }, error = function(e) {
        warning("Meta-analysis failed for lag ", unique(.y$lag), ": ", e$message)
        data.frame(
          coef = NA_real_, se = NA_real_, ci.lb = NA_real_, ci.ub = NA_real_,
          pval = NA_real_, I2 = NA_real_, stringsAsFactors = FALSE
        )
      })
      return(res)
    }) %>%
    dplyr::ungroup()
  
  return(list(region_results = region_results, meta_results = meta_results))
}



#' Calculate daily RR/AF/AN/AR for region-specific/national distributed lag effects 
#' for a chosen PM2.5 reference.
#'
#' @param data_with_lags Dataset. Lagged data with lag variables.
#' @param meta_results Dataset. Results from meta analysis 
#' @param ref_pm25 Numeric. PM2.5 reference value. Defaults to 15.
#' @param ref_name Character. Reference body name. Defaults to "WHO". 
#' @param max_lag Integer. Maximum lag days. Defaults to 14
#'
#' @return List with region-specific/national results for daily RR/AF/AN/AR
#'
#' @keywords internal
analyze_air_pollution_daily <- function(data_with_lags,
                                        meta_results,
                                        ref_pm25 = 15,
                                        ref_name = "WHO",
                                        max_lag = 14L) {
  
  data <- data_with_lags
  regions <- unique(as.character(data$region))
  
  all_lag_vars <- c("pm25", paste0("pm25_lag", 1:max_lag), paste0("pm25_lag0_", max_lag))
  individual_labels <- c("0", paste0("", 1:max_lag), paste0("0_", max_lag))
  z <- stats::qnorm(0.975)
  
  region_results_df <- meta_results$region_results
  pooled_df <- meta_results$meta_results
  
  # Helper to extract a coef_table from each model_results element
  get_coef_table <- function(x) {
    if (!is.null(x$coef)) return(x$coef)
    if (is.data.frame(x)) return(x)
    stop("Could not find coefficient table inside model_results element")
  }
  
  # Build a single data.frame with per-region coefficients
  region_coefs <- region_results_df %>%
    dplyr::mutate(
      coef_table = purrr::map(.data$model_results, get_coef_table)
    ) %>%
    dplyr::select(region, coef_table) %>%
    tidyr::unnest(cols = dplyr::all_of("coef_table"))
  
  # Normalize columns in region_coefs to match the lookup used later
  if ("pm25_variable" %in% names(region_coefs)) {
    region_coefs <- region_coefs %>%
      dplyr::mutate(
        pm25_variable = as.character(.data$pm25_variable),
        lag = if ("lag" %in% names(.)) as.character(.data$lag) else
          ifelse(.data$pm25_variable == "pm25", "0", sub("^pm25_lag", "", .data$pm25_variable)),
        lag = ifelse(is.na(lag), NA_character_, gsub("-", "_", lag))
      )
  }
  
  # Compute region-level results
  region_rows <- list()
  for (prov in regions) {
    prov_data <- data[data$region == prov, , drop = FALSE]
    deaths_prov <- prov_data$deaths
    population_prov <- prov_data$population
    
    for (j in seq_along(all_lag_vars)) {
      var <- all_lag_vars[j]
      lag_label <- individual_labels[j]
      
      row_coef <- region_coefs %>%
        dplyr::filter(.data$region == prov & .data$pm25_variable == var & .data$lag == lag_label)
      
      if (nrow(row_coef) == 0) {
        coef <- se <- ci.lb <- ci.ub <- NA_real_
      } else {
        coef <- row_coef$coef[1]
        se <- row_coef$se[1]
        ci.lb <- row_coef$ci.lb[1]
        ci.ub <- row_coef$ci.ub[1]
      }
      
      if (is.na(coef) || is.na(se)) {
        rr <- rr.lb <- rr.ub <- af <- af.lb <- af.ub <- an <- an.lb <- an.ub <- ar <- ar.lb <- ar.ub <- NA_real_
      } else {
        rr <- exp(coef * pmax(prov_data$pm25 - ref_pm25, 0))
        rr.lb <- exp(ci.lb * pmax(prov_data$pm25 - ref_pm25, 0))
        rr.ub <- exp(ci.ub * pmax(prov_data$pm25 - ref_pm25, 0))
        
        af <- (rr - 1) / rr
        af.lb <- (rr.lb - 1) / rr.lb
        af.ub <- (rr.ub - 1) / rr.ub
        
        an <- af * deaths_prov
        an.lb <- af.lb * deaths_prov
        an.ub <- af.ub * deaths_prov
        
        ar <- (an / population_prov) * 100000
        ar.lb <- (an.lb / population_prov) * 100000
        ar.ub <- (an.ub / population_prov) * 100000
      }
      
      region_rows[[length(region_rows) + 1]] <- tibble::tibble(
        region = prov,
        date = prov_data$date,        
        lag = lag_label,
        pm25_var = var,
        coef = coef,
        se = se,
        rr = rr, rr.lb = rr.lb, rr.ub = rr.ub,
        af = af, af.lb = af.lb, af.ub = af.ub,
        an = an, an.lb = an.lb, an.ub = an.ub,
        ar = ar, ar.lb = ar.lb, ar.ub = ar.ub,
        tot_deaths = deaths_prov,
        pop = population_prov,
        pm25_values = prov_data$pm25,
        ref_pm25 = ref_pm25,
        ref_name = ref_name
      )
    }
  }
  
  region_gam_results <- dplyr::bind_rows(region_rows)
  
  # Compute national results
  if (!"pm25_variable" %in% names(pooled_df)) {
    stop("meta_results$meta_results must include a 'pm25_variable' column.")
  }
  pooled_df2 <- pooled_df %>%
    dplyr::mutate(
      pm25_variable = as.character(.data$pm25_variable),
      coef = as.numeric(.data$coef),
      se = as.numeric(.data$se)
    ) %>%
    dplyr::mutate(
      lag = if ("lag" %in% names(pooled_df)) as.character(.data$lag) else
        ifelse(.data$pm25_variable == "pm25", "0", sub("^pm25_lag", "", .data$pm25_variable)),
      lag = ifelse(is.na(lag), NA_character_, gsub("-", "_", lag))
    ) %>%
    # compute ci if not present
    dplyr::mutate(
      ci.lb = if ("ci.lb" %in% names(.)) .data$ci.lb else .data$coef - z * .data$se,
      ci.ub = if ("ci.ub" %in% names(.)) .data$ci.ub else .data$coef + z * .data$se
    )
  
  # Find pm25-related columns in the input data
  pm25_cols <- grep("^pm25", names(data), value = TRUE)
  agg_vars <- c(pm25_cols, "tmax", "precipitation", "humidity", "wind_speed")
  
  # Aggregate national-level data
  data_national <- data %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(
      deaths = sum(.data$deaths, na.rm = TRUE),
      population = sum(.data$population, na.rm = TRUE),
      dplyr::across(dplyr::all_of(agg_vars), ~ round(mean(.x, na.rm = TRUE), 2)),
      .groups = "drop"
    )
  
  deaths_national <- data_national$deaths
  population_national <- data_national$population
  
  national_rows <- list()
  for (j in seq_along(all_lag_vars)) {
    var <- all_lag_vars[j]
    lag_label <- individual_labels[j]
    
    row_pooled <- pooled_df2 %>%
      dplyr::filter(.data$pm25_variable == var & .data$lag == lag_label)
    
    if (nrow(row_pooled) == 0) {
      coef <- se <- ci.lb <- ci.ub <- NA_real_
    } else {
      coef <- row_pooled$coef[1]
      se <- row_pooled$se[1]
      ci.lb <- row_pooled$ci.lb[1]
      ci.ub <- row_pooled$ci.ub[1]
    }
    
    if (is.na(coef) || is.na(se)) {
      rr <- rr.lb <- rr.ub <- af <- af.lb <- af.ub <- an <- an.lb <- an.ub <- ar <- ar.lb <- ar.ub <- NA_real_
    } else {
      # use the national pm25 vector (data_national$pm25) for per-date RR/AF/AN/AR
      rr <- exp(coef * pmax(data_national$pm25 - ref_pm25, 0))
      rr.lb <- exp(ci.lb * pmax(data_national$pm25 - ref_pm25, 0))
      rr.ub <- exp(ci.ub * pmax(data_national$pm25 - ref_pm25, 0))
      
      af <- (rr - 1) / rr
      af.lb <- (rr.lb - 1) / rr.lb
      af.ub <- (rr.ub - 1) / rr.ub
      
      an <- af * deaths_national
      an.lb <- af.lb * deaths_national
      an.ub <- af.ub * deaths_national
      
      ar <- (an / population_national) * 100000
      ar.lb <- (an.lb / population_national) * 100000
      ar.ub <- (an.ub / population_national) * 100000
    }
    
    national_rows[[length(national_rows) + 1]] <- tibble::tibble(
      region = "National",
      date = data_national$date,
      lag = lag_label,
      pm25_var = var,
      coef = coef,
      se = se,
      rr = rr, rr.lb = rr.lb, rr.ub = rr.ub,
      af = af, af.lb = af.lb, af.ub = af.ub,
      an = an, an.lb = an.lb, an.ub = an.ub,
      ar = ar, ar.lb = ar.lb, ar.ub = ar.ub,
      tot_deaths = deaths_national,
      pop = population_national,
      pm25_values = data_national$pm25,
      ref_pm25 = ref_pm25,
      ref_name = ref_name
    )
  }
  
  national_gam_results <- dplyr::bind_rows(national_rows)
  
  final <- rbind(region_gam_results,national_gam_results)
  row.names(final) <- NULL
  
  return(final)
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
#' @param analysis_results Processed results with RR/AF/AN/AR with lag variables
#' @param pm25_var Character. PM2.5 variable name. Defaults to "pm25".
#' @param output_dir Character. Directory to save plot. Defaults to NULL.
#' @param save_plot Logical. Whether to save the plot. Defaults to FALSE.
#'
#' @return ggplot object
#'
#' @keywords internal
plot_air_pollution_forest_by_region <- function(analysis_results,
                                                pm25_var = "pm25",
                                                output_dir = NULL,
                                                save_plot = FALSE) {
  if (is.null(output_dir) && save_plot) {
    stop("output_dir must be specified when save_plot == TRUE.")
  }
  
  # select and summarise by region
  pm25 <- pm25_var
  specific_results <- analysis_results %>%
    dplyr::filter(pm25_var == pm25) %>%
    dplyr::group_by(region, ref_name, ref_pm25, pm25_var) %>%
    dplyr::summarise(
      rr = mean(rr, na.rm = TRUE),
      rr.lb = mean(rr.lb, na.rm = TRUE),
      rr.ub = mean(rr.ub, na.rm = TRUE),
      .groups = "drop"
    )
  
  if (nrow(specific_results) == 0) {
    stop("No results for the selected pm25_var.")
  }
  
  # reference metadata (first non-NA)
  ref_name <- na.omit(unique(specific_results$ref_name))[1]
  ref_pm25 <- na.omit(unique(specific_results$ref_pm25))[1]
  
  # mark national row so it can be highlighted
  specific_results <- specific_results %>%
    dplyr::mutate(is_national = (.data$region == "National"))
  
  # Create ordered factor for region
  prov_names <- specific_results %>% filter(region != "National") 
  regions_only <- unique(prov_names$region)
  region_order <- c(regions_only, "National")
  
  specific_results <- specific_results %>%
    dplyr::mutate(region = factor(.data$region, levels = region_order))
  
  # global plotting limits
  global_max_rr <- max(specific_results$rr.ub, na.rm = TRUE)
  global_min_rr <- min(specific_results$rr.lb, na.rm = TRUE)
  max_ylim <- global_max_rr * 1.01
  min_ylim <- min(global_min_rr, 1) * 0.99
  
  forest_plot <- ggplot2::ggplot(specific_results, ggplot2::aes(x = .data$region, y = .data$rr)) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data$rr.lb, ymax = .data$rr.ub, color = .data$is_national),
      width = 0.1, linewidth = 0.6
    ) +
    ggplot2::geom_point(ggplot2::aes(color = .data$is_national), size = 2) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
    ggplot2::scale_color_manual(values = c("FALSE" = "blue", "TRUE" = "red")) +
    ggplot2::coord_cartesian(ylim = c(min_ylim, max_ylim)) +
    ggplot2::labs(
      x = "Region",
      y = "Relative Risk",
      title = sprintf('PM2.5 effects by region — Ref: "%s" = %s', ref_name, ref_pm25)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(hjust = 0.5),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
  
  if (save_plot) {
    filename <- sprintf("forest_plot_by_region_%s_%s_%s", pm25_var, ref_name, ref_pm25)
    save_air_pollution_plot(
      plot_object = forest_plot,
      output_dir = output_dir,
      filename = filename,
      grid_dims = NULL
    )
  }
  
  return(forest_plot)
}


#' Plot Relative Risk (RR) by lag
#'
#' @param analysis_results Processed results with RR/AF/AN/AR with lag variables
#' @param max_lag Integer. Maximum lag days. Defaults to 14.
#' @param output_dir Character. Directory to save plot. Defaults to NULL.
#' @param save_plot Logical. Whether to save the plot. Defaults to FALSE.
#'
#' @return ggplot object
#'
#' @keywords internal
plot_air_pollution_forest_by_lag <- function(analysis_results,
                                             max_lag = 14,
                                             output_dir = NULL,
                                             save_plot = FALSE) {
  if (is.null(output_dir) && save_plot) {
    stop("output_dir must be specified when save_plot == TRUE.")
  }
  
  # select and summarise by region
  specific_results <- analysis_results %>%
    dplyr::group_by(pm25_var, ref_name, ref_pm25) %>%
    dplyr::summarise(
      rr = mean(rr, na.rm = TRUE),
      rr.lb = mean(rr.lb, na.rm = TRUE),
      rr.ub = mean(rr.ub, na.rm = TRUE),
      .groups = "drop"
    )
  
  if (nrow(specific_results) == 0) {
    stop("No results for the selected pm25_var.")
  }
  
  # reference metadata (first non-NA)
  ref_name <- na.omit(unique(specific_results$ref_name))[1]
  ref_pm25 <- na.omit(unique(specific_results$ref_pm25))[1]
  
  # mark national row so it can be highlighted
  specific_results <- specific_results %>%
    dplyr::mutate(is_cumulative = (.data$pm25_var == paste0("pm25_lag0_", max_lag)))
  
  # Create ordered factor for pm25_var
  lag_order <- c("pm25", paste0("pm25_lag", 1:max_lag), paste0("pm25_lag0_", max_lag))
  
  specific_results <- specific_results %>%
    dplyr::mutate(pm25_var = factor(.data$pm25_var, levels = lag_order))
  
  # global plotting limits
  global_max_rr <- max(specific_results$rr.ub, na.rm = TRUE)
  global_min_rr <- min(specific_results$rr.lb, na.rm = TRUE)
  max_ylim <- global_max_rr * 1.01
  min_ylim <- min(global_min_rr, 1) * 0.99
  
  forest_plot <- ggplot2::ggplot(specific_results, ggplot2::aes(x = .data$pm25_var, y = .data$rr)) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data$rr.lb, ymax = .data$rr.ub, color = .data$is_cumulative),
      width = 0.1, linewidth = 0.6
    ) +
    ggplot2::geom_point(ggplot2::aes(color = .data$is_cumulative), size = 2) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
    ggplot2::scale_color_manual(values = c("FALSE" = "blue", "TRUE" = "red")) +
    ggplot2::coord_cartesian(ylim = c(min_ylim, max_ylim)) +
    ggplot2::labs(
      x = "Region",
      y = "Relative Risk",
      title = sprintf('PM2.5 effects by region — Ref: "%s" = %s', ref_name, ref_pm25)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(hjust = 0.5),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
  pm25_var_n <- na.omit(unique(specific_results$pm25_var))
  
  if (save_plot) {
    filename <- sprintf("forest_plot_by_lag_%s_%s", ref_name, ref_pm25)
    save_air_pollution_plot(
      plot_object = forest_plot,
      output_dir = output_dir,
      filename = filename,
      grid_dims = NULL
    )
  }
  
  return(forest_plot)
}


#===============================================================================
#' Aggregate air pollution results by region
#'
#' @description Aggregates daily analysis results to regional summaries
#'
#' @param analysis_results Results from analyze_air_pollution_daily or analyze_air_pollution_monthly
#' @param pm25_var Character. PM2.5 variable to aggregate. Defaults to cumulative lag variable
#' @param max_lag Integer. Maximum lag used in analysis
#'
#' @return Dataframe with regional aggregates
#' @keywords internal
aggregate_air_pollution_by_region <- function(analysis_results,
                                              pm25_var = NULL,
                                              max_lag = 14) {
  
  # Default to cumulative lag if not specified
  if (is.null(pm25_var)) {
    pm25_var <- paste0("pm25_lag0_", max_lag)
  }
  
  # Filter to specific PM2.5 variable
  results_filtered <- analysis_results %>%
    dplyr::filter(pm25_var == !!pm25_var)
  
  # Aggregate by region
  regional_summary <- results_filtered %>%
    dplyr::group_by(region, ref_name, ref_pm25) %>%
    dplyr::summarise(
      # Attributable Rate
      ar_per_100k = sum(ar, na.rm = TRUE),
      ar_lower = sum(ar.lb, na.rm = TRUE),
      ar_upper = sum(ar.ub, na.rm = TRUE),
      
      # Attributable Number
      an = sum(an, na.rm = TRUE),
      an_lower = sum(an.lb, na.rm = TRUE),
      an_upper = sum(an.ub, na.rm = TRUE),
      
      # Attributable Fraction
      af = mean(af, na.rm = TRUE),
      af_lower = mean(af.lb, na.rm = TRUE),
      af_upper = mean(af.ub, na.rm = TRUE),
      
      # Supporting metrics
      total_deaths = sum(tot_deaths, na.rm = TRUE),
      mean_pm25 = mean(pm25_values, na.rm = TRUE),
      population = mean(pop, na.rm = TRUE),
      
      .groups = 'drop'
    )
  
  return(regional_summary)
}

#===============================================================================
#' Aggregate air pollution results by year
#'
#' @description Aggregates daily analysis results to annual summaries
#'
#' @param analysis_results Results from analyze_air_pollution_daily or analyze_air_pollution_monthly
#' @param pm25_var Character. PM2.5 variable to aggregate
#' @param max_lag Integer. Maximum lag used in analysis
#' @param by_region Logical. Whether to also group by region
#'
#' @return Dataframe with annual aggregates
#' @keywords internal
aggregate_air_pollution_by_year <- function(analysis_results,
                                            pm25_var = NULL,
                                            max_lag = 14,
                                            by_region = FALSE) {
  
  # Default to cumulative lag if not specified
  if (is.null(pm25_var)) {
    pm25_var <- paste0("pm25_lag0_", max_lag)
  }
  
  # Add year column
  results_with_year <- analysis_results %>%
    dplyr::filter(pm25_var == !!pm25_var) %>%
    dplyr::mutate(year = lubridate::year(date))
  
  # Define grouping variables
  group_vars <- c("year", "ref_name", "ref_pm25")
  if (by_region) {
    group_vars <- c(group_vars, "region")
  }
  
  # Aggregate
  annual_summary <- results_with_year %>%
    dplyr::group_by(across(all_of(group_vars))) %>%
    dplyr::summarise(
      # Attributable Rate
      ar_per_100k = sum(ar, na.rm = TRUE),
      ar_lower = sum(ar.lb, na.rm = TRUE),
      ar_upper = sum(ar.ub, na.rm = TRUE),
      
      # Attributable Number
      an = sum(an, na.rm = TRUE),
      an_lower = sum(an.lb, na.rm = TRUE),
      an_upper = sum(an.ub, na.rm = TRUE),
      
      # Attributable Fraction
      af = mean(af, na.rm = TRUE),
      af_lower = mean(af.lb, na.rm = TRUE),
      af_upper = mean(af.ub, na.rm = TRUE),
      
      # Supporting metrics
      total_deaths = sum(tot_deaths, na.rm = TRUE),
      mean_pm25 = mean(pm25_values, na.rm = TRUE),
      population = mean(pop, na.rm = TRUE),
      
      .groups = 'drop'
    )
  
  return(annual_summary)
}

#===============================================================================
#' Aggregate air pollution results by month
#'
#' @description Aggregates daily analysis results to monthly summaries
#'
#' @param analysis_results Results from analyze_air_pollution_daily
#' @param pm25_var Character. PM2.5 variable to aggregate
#' @param max_lag Integer. Maximum lag used in analysis
#' @param by_region Logical. Whether to also group by region
#'
#' @return Dataframe with monthly aggregates
#' @keywords internal
aggregate_air_pollution_by_month <- function(analysis_results,
                                             pm25_var = NULL,
                                             max_lag = 14,
                                             by_region = FALSE) {
  
  # Default to cumulative lag if not specified
  if (is.null(pm25_var)) {
    pm25_var <- paste0("pm25_lag0_", max_lag)
  }
  
  # Add year and month columns
  results_with_time <- analysis_results %>%
    dplyr::filter(pm25_var == !!pm25_var) %>%
    dplyr::mutate(
      year = lubridate::year(date),
      month = lubridate::month(date)
    )
  
  # Define grouping variables
  group_vars <- c("year", "month", "ref_name", "ref_pm25")
  if (by_region) {
    group_vars <- c(group_vars, "region")
  }
  
  # Aggregate
  monthly_summary <- results_with_time %>%
    dplyr::group_by(across(all_of(group_vars))) %>%
    dplyr::summarise(
      # Attributable Rate
      ar_per_100k = sum(ar, na.rm = TRUE),
      ar_lower = sum(ar.lb, na.rm = TRUE),
      ar_upper = sum(ar.ub, na.rm = TRUE),
      
      # Attributable Number
      an = sum(an, na.rm = TRUE),
      an_lower = sum(an.lb, na.rm = TRUE),
      an_upper = sum(an.ub, na.rm = TRUE),
      
      # Attributable Fraction
      af = mean(af, na.rm = TRUE),
      af_lower = mean(af.lb, na.rm = TRUE),
      af_upper = mean(af.ub, na.rm = TRUE),
      
      # Supporting metrics
      total_deaths = sum(tot_deaths, na.rm = TRUE),
      mean_pm25 = mean(pm25_values, na.rm = TRUE),
      population = mean(pop, na.rm = TRUE),
      
      .groups = 'drop'
    )
  
  return(monthly_summary)
}

#===============================================================================
#' Improved plot of Attributable Rate by Region for Air Pollution
#'
#' @param analysis_results Results from analyze_air_pollution_daily
#' @param pm25_var Character. PM2.5 variable to plot
#' @param max_lag Integer. Maximum lag
#' @param output_dir Character. Directory to save plot
#' @param save_plot Logical. Whether to save
#'
#' @return ggplot object
#' @export
plot_air_pollution_ar_by_region <- function(analysis_results,
                                            pm25_var = NULL,
                                            max_lag = 14,
                                            output_dir = NULL,
                                            save_plot = FALSE) {
  
  if (save_plot && is.null(output_dir)) {
    stop("output_dir must be specified when save_plot == TRUE")
  }
  
  # Aggregate data
  regional_data <- aggregate_air_pollution_by_region(
    analysis_results,
    pm25_var,
    max_lag
  )
  
  # Get reference info
  ref_name <- unique(regional_data$ref_name)[1]
  ref_pm25 <- unique(regional_data$ref_pm25)[1]
  
  # Separate national from regional
  national_data <- regional_data %>% filter(region == "National")
  province_data <- regional_data %>% filter(region != "National")
  
  # Create plot
  p <- ggplot2::ggplot(
    province_data,
    ggplot2::aes(
      x = forcats::fct_reorder(region, ar_per_100k, .desc = TRUE),
      y = ar_per_100k
    )
  ) +
    ggplot2::geom_col(fill = "#003c57", alpha = 0.8) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = ar_lower, ymax = ar_upper),
      width = 0.3,
      color = "darkred"
    ) +
    ggplot2::labs(
      title = sprintf(
        "Deaths per 100K attributable to PM2.5 by Region\n(%s Standard: %s µg/m³)",
        ref_name, ref_pm25
      ),
      x = "Region",
      y = "Attributable Rate (per 100K population)",
      caption = "Error bars show 95% confidence intervals"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10),
      axis.title = ggplot2::element_text(face = "bold"),
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 14),
      plot.caption = ggplot2::element_text(hjust = 0.5, size = 9, color = "gray40")
    )
  
  # Add national reference line if available
  if (nrow(national_data) > 0) {
    p <- p +
      ggplot2::geom_hline(
        yintercept = national_data$ar_per_100k,
        linetype = "dashed",
        color = "darkgreen",
        linewidth = 1
      ) +
      ggplot2::annotate(
        "text",
        x = Inf,
        y = national_data$ar_per_100k,
        label = sprintf("National: %.2f", national_data$ar_per_100k),
        hjust = 1.1,
        vjust = -0.5,
        color = "darkgreen",
        fontface = "bold"
      )
  }
  
  # Save if requested
  if (save_plot) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    filename <- sprintf(
      "air_pollution_ar_by_region_%s_ref%s.png",
      ref_name, ref_pm25
    )
    
    ggplot2::ggsave(
      file.path(output_dir, filename),
      plot = p,
      width = 10,
      height = 6,
      dpi = 300,
      bg = "white"
    )
  }
  
  return(p)
}

#===============================================================================
#' Improved plot of Attributable Number by Region for Air Pollution
#'
#' @param analysis_results Results from analyze_air_pollution_daily
#' @param pm25_var Character. PM2.5 variable to plot
#' @param max_lag Integer. Maximum lag
#' @param output_dir Character. Directory to save plot
#' @param save_plot Logical. Whether to save
#'
#' @return ggplot object
#' @export
plot_air_pollution_an_by_region <- function(analysis_results,
                                            pm25_var = NULL,
                                            max_lag = 14,
                                            output_dir = NULL,
                                            save_plot = FALSE) {
  
  if (save_plot && is.null(output_dir)) {
    stop("output_dir must be specified when save_plot == TRUE")
  }
  
  # Aggregate data
  regional_data <- aggregate_air_pollution_by_region(
    analysis_results,
    pm25_var,
    max_lag
  )
  
  # Get reference info
  ref_name <- unique(regional_data$ref_name)[1]
  ref_pm25 <- unique(regional_data$ref_pm25)[1]
  
  # Separate national from regional
  province_data <- regional_data %>% filter(region != "National")
  
  # Create plot
  p <- ggplot2::ggplot(
    province_data,
    ggplot2::aes(
      x = forcats::fct_reorder(region, an, .desc = TRUE),
      y = an
    )
  ) +
    ggplot2::geom_col(fill = "#003c57", alpha = 0.8) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = an_lower, ymax = an_upper),
      width = 0.3,
      color = "darkred"
    ) +
    ggplot2::labs(
      title = sprintf(
        "Total Deaths attributable to PM2.5 by Region\n(%s Standard: %s µg/m³)",
        ref_name, ref_pm25
      ),
      x = "Region",
      y = "Attributable Number of Deaths",
      caption = "Error bars show 95% confidence intervals"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10),
      axis.title = ggplot2::element_text(face = "bold"),
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 14),
      plot.caption = ggplot2::element_text(hjust = 0.5, size = 9, color = "gray40")
    )
  
  # Save if requested
  if (save_plot) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    filename <- sprintf(
      "air_pollution_an_by_region_%s_ref%s.png",
      ref_name, ref_pm25
    )
    
    ggplot2::ggsave(
      file.path(output_dir, filename),
      plot = p,
      width = 10,
      height = 6,
      dpi = 300,
      bg = "white"
    )
  }
  
  return(p)
}

#===============================================================================
#' Plot Attributable Rate by Year for Air Pollution
#'
#' @param analysis_results Results from analyze_air_pollution_daily
#' @param pm25_var Character. PM2.5 variable to plot
#' @param max_lag Integer. Maximum lag
#' @param by_region Logical. Whether to create separate plots by region
#' @param output_dir Character. Directory to save plot
#' @param save_plot Logical. Whether to save
#'
#' @return ggplot object or list of ggplot objects
#' @export
plot_air_pollution_ar_by_year <- function(analysis_results,
                                          pm25_var = NULL,
                                          max_lag = 14,
                                          by_region = FALSE,
                                          output_dir = NULL,
                                          save_plot = FALSE) {
  
  if (save_plot && is.null(output_dir)) {
    stop("output_dir must be specified when save_plot == TRUE")
  }
  
  # Aggregate data
  annual_data <- aggregate_air_pollution_by_year(
    analysis_results,
    pm25_var,
    max_lag,
    by_region
  )
  
  # Get reference info
  ref_name <- unique(annual_data$ref_name)[1]
  ref_pm25 <- unique(annual_data$ref_pm25)[1]
  
  if (!by_region) {
    # Single plot for all regions combined
    p <- ggplot2::ggplot(
      annual_data,
      ggplot2::aes(x = year, y = ar_per_100k)
    ) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = ar_lower, ymax = ar_upper),
        alpha = 0.2,
        fill = "#4d7789"
      ) +
      ggplot2::geom_line(color = "#003c57", linewidth = 1.2) +
      ggplot2::geom_point(color = "#003c57", size = 2.5) +
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      ggplot2::labs(
        title = sprintf(
          "Annual Deaths (per 100K) attributable to PM2.5\n(%s Standard: %s µg/m³)",
          ref_name, ref_pm25
        ),
        x = "Year",
        y = "Attributable Rate (per 100K population)",
        caption = "Shaded area shows 95% confidence interval"
      ) +
      ggplot2::theme_minimal(base_size = 14) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
        axis.title = ggplot2::element_text(face = "bold"),
        plot.caption = ggplot2::element_text(hjust = 0.5, color = "gray40")
      )
    
    if (save_plot) {
      if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
      }
      
      filename <- sprintf(
        "air_pollution_ar_by_year_%s_ref%s.png",
        ref_name, ref_pm25
      )
      
      ggplot2::ggsave(
        file.path(output_dir, filename),
        plot = p,
        width = 12,
        height = 7,
        dpi = 300,
        bg = "white"
      )
    }
    
    return(p)
    
  } else {
    # Multiple plots by region
    plots <- list()
    regions <- unique(annual_data$region)
    regions <- regions[regions != "National"]  # Exclude national for regional plots
    
    for (reg in regions) {
      reg_data <- annual_data %>% dplyr::filter(region == reg)
      
      p <- ggplot2::ggplot(
        reg_data,
        ggplot2::aes(x = year, y = ar_per_100k)
      ) +
        ggplot2::geom_ribbon(
          ggplot2::aes(ymin = ar_lower, ymax = ar_upper),
          alpha = 0.2,
          fill = "#4d7789"
        ) +
        ggplot2::geom_line(color = "#003c57", linewidth = 1) +
        ggplot2::geom_point(color = "#003c57", size = 2) +
        ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
        ggplot2::labs(
          title = sprintf("%s", reg),
          x = "Year",
          y = "AR (per 100K)"
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
        )
      
      plots[[reg]] <- p
    }
    
    # Combine plots
    combined_plots <- patchwork::wrap_plots(plots, ncol = 2)
    
    if (save_plot) {
      if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
      }
      
      filename <- sprintf(
        "air_pollution_ar_by_year_by_region_%s_ref%s.png",
        ref_name, ref_pm25
      )
      
      ggplot2::ggsave(
        file.path(output_dir, filename),
        plot = combined_plots,
        width = 14,
        height = length(plots) * 3.5,
        dpi = 300,
        bg = "white",
        limitsize = FALSE
      )
    }
    
    return(plots)
  }
}

#===============================================================================
#' Plot Attributable Number by Year for Air Pollution
#'
#' @param analysis_results Results from analyze_air_pollution_daily
#' @param pm25_var Character. PM2.5 variable to plot
#' @param max_lag Integer. Maximum lag
#' @param by_region Logical. Whether to create separate plots by region
#' @param output_dir Character. Directory to save plot
#' @param save_plot Logical. Whether to save
#'
#' @return ggplot object or list of ggplot objects
#' @export
plot_air_pollution_an_by_year <- function(analysis_results,
                                          pm25_var = NULL,
                                          max_lag = 14,
                                          by_region = FALSE,
                                          output_dir = NULL,
                                          save_plot = FALSE) {
  
  if (save_plot && is.null(output_dir)) {
    stop("output_dir must be specified when save_plot == TRUE")
  }
  
  # Aggregate data
  annual_data <- aggregate_air_pollution_by_year(
    analysis_results,
    pm25_var,
    max_lag,
    by_region
  )
  
  # Get reference info
  ref_name <- unique(annual_data$ref_name)[1]
  ref_pm25 <- unique(annual_data$ref_pm25)[1]
  
  if (!by_region) {
    # Single plot for all regions combined
    p <- ggplot2::ggplot(
      annual_data,
      ggplot2::aes(x = year, y = an)
    ) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = an_lower, ymax = an_upper),
        alpha = 0.2,
        fill = "#4d7789"
      ) +
      ggplot2::geom_line(color = "#003c57", linewidth = 1.2) +
      ggplot2::geom_point(color = "#003c57", size = 2.5) +
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      ggplot2::labs(
        title = sprintf(
          "Annual Deaths attributable to PM2.5\n(%s Standard: %s µg/m³)",
          ref_name, ref_pm25
        ),
        x = "Year",
        y = "Attributable Number of Deaths",
        caption = "Shaded area shows 95% confidence interval"
      ) +
      ggplot2::theme_minimal(base_size = 14) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
        axis.title = ggplot2::element_text(face = "bold"),
        plot.caption = ggplot2::element_text(hjust = 0.5, color = "gray40")
      )
    
    if (save_plot) {
      if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
      }
      
      filename <- sprintf(
        "air_pollution_an_by_year_%s_ref%s.png",
        ref_name, ref_pm25
      )
      
      ggplot2::ggsave(
        file.path(output_dir, filename),
        plot = p,
        width = 12,
        height = 7,
        dpi = 300,
        bg = "white"
      )
    }
    
    return(p)
    
  } else {
    # Multiple plots by region
    plots <- list()
    regions <- unique(annual_data$region)
    regions <- regions[regions != "National"]
    
    for (reg in regions) {
      reg_data <- annual_data %>% dplyr::filter(region == reg)
      
      p <- ggplot2::ggplot(
        reg_data,
        ggplot2::aes(x = year, y = an)
      ) +
        ggplot2::geom_ribbon(
          ggplot2::aes(ymin = an_lower, ymax = an_upper),
          alpha = 0.2,
          fill = "#4d7789"
        ) +
        ggplot2::geom_line(color = "#003c57", linewidth = 1) +
        ggplot2::geom_point(color = "#003c57", size = 2) +
        ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
        ggplot2::labs(
          title = sprintf("%s", reg),
          x = "Year",
          y = "Attributable Deaths"
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
        )
      
      plots[[reg]] <- p
    }
    
    # Combine plots
    combined_plots <- patchwork::wrap_plots(plots, ncol = 2)
    
    if (save_plot) {
      if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
      }
      
      filename <- sprintf(
        "air_pollution_an_by_year_by_region_%s_ref%s.png",
        ref_name, ref_pm25
      )
      
      ggplot2::ggsave(
        file.path(output_dir, filename),
        plot = combined_plots,
        width = 14,
        height = length(plots) * 3.5,
        dpi = 300,
        bg = "white",
        limitsize = FALSE
      )
    }
    
    return(plots)
  }
}

#===============================================================================
#' Plot histograms for AN and AR by month
#'
#' @description Creates histogram plots for Attributable Number (AN) and 
#' Attributable Rate (AR) aggregated by month (Jan, Feb, ..., Dec)
#'
#' @param analysis_results Processed results with RR/AF/AN/AR with lag variables
#' @param pm25_var Character. PM2.5 variable name. Defaults to "pm25".
#' @param output_dir Character. Directory to save plots.
#' @param save_plot Logical. Whether to save the plots.
#'
#' @return List with ggplot objects
#' @keywords internal
plot_air_pollution_monthly_histograms <- function(analysis_results,
                                                  pm25_var = "pm25",
                                                  output_dir = NULL,
                                                  save_plot = FALSE) {
  
  if (save_plot && is.null(output_dir)) {
    stop("output_dir must be specified when save_plot == TRUE")
  }
  
  # Filter for specific PM2.5 variable and national level
  specific_results <- analysis_results %>%
    dplyr::filter(pm25_var == !!pm25_var & region == "National") %>%
    dplyr::mutate(month = lubridate::month(date, label = TRUE, abbr = FALSE))
  
  # Get reference info
  ref_name <- na.omit(unique(specific_results$ref_name))[1]
  ref_pm25 <- na.omit(unique(specific_results$ref_pm25))[1]
  
  # Aggregate by month
  monthly_agg <- specific_results %>%
    dplyr::group_by(month) %>%
    dplyr::summarise(
      an_total = sum(an, na.rm = TRUE),
      ar_aggr = sum(ar, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(month = factor(month, levels = month.name))
  
  # Plot AN by month
  an_plot <- ggplot2::ggplot(monthly_agg, ggplot2::aes(x = month, y = an_total)) +
    ggplot2::geom_bar(stat = "identity", fill = "#2E86AB", alpha = 0.8) +
    ggplot2::labs(
      title = paste("Attributable Number (AN) by Month -", ref_name, "Standard"),
      subtitle = paste("Reference PM2.5:", ref_pm25, "µg/m³"),
      x = "Month",
      y = "Total Attributable Number",
      caption = "National level aggregation"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "gray40"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      axis.title = ggplot2::element_text(face = "bold"),
      panel.grid.major.x = ggplot2::element_blank()
    ) +
    ggplot2::scale_y_continuous(labels = scales::comma)
  
  # Plot AR by month
  ar_plot <- ggplot2::ggplot(monthly_agg, ggplot2::aes(x = month, y = ar_aggr)) +
    ggplot2::geom_bar(stat = "identity", fill = "#A23B72", alpha = 0.8) +
    ggplot2::labs(
      title = paste("Aggregated Attributable Rate (AR) by Month -", ref_name, "Standard"),
      subtitle = paste("Reference PM2.5:", ref_pm25, "µg/m³"),
      x = "Month",
      y = "Aggregated AR (per 100,000 population)",
      caption = "National level aggregation"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "gray40"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      axis.title = ggplot2::element_text(face = "bold"),
      panel.grid.major.x = ggplot2::element_blank()
    )
  
  # Save plots if requested
  if (save_plot) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    safe_refname <- tolower(gsub("\\s+", "_", ref_name))
    
    ggplot2::ggsave(
      file.path(output_dir, paste0("an_monthly_histogram_", safe_refname, "_ref", ref_pm25, ".png")),
      plot = an_plot,
      width = 10,
      height = 6,
      dpi = 300,
      bg = "white"
    )
    
    ggplot2::ggsave(
      file.path(output_dir, paste0("ar_monthly_histogram_", safe_refname, "_ref", ref_pm25, ".png")),
      plot = ar_plot,
      width = 10,
      height = 6,
      dpi = 300,
      bg = "white"
    )
  }
  
  return(list(an_plot = an_plot, ar_plot = ar_plot))
}

#===============================================================================
#' Plot exposure-response relationship with confidence intervals by region
#'
#' @description Creates faceted exposure-response plots showing RR with confidence 
#' intervals across PM2.5 concentrations for each region (excluding National)
#'
#' @param analysis_results Processed results with RR/AF/AN/AR with lag variables
#' @param pm25_var Character. PM2.5 variable name. Defaults to "pm25".
#' @param ref_pm25 Numeric. Reference PM2.5 value to highlight.
#' @param output_dir Character. Directory to save plot.
#' @param save_plot Logical. Whether to save the plot.
#'
#' @return ggplot object
#' @keywords internal
plot_air_pollution_exposure_response <- function(analysis_results,
                                                 pm25_var = "pm25",
                                                 ref_pm25 = NULL,
                                                 output_dir = NULL,
                                                 save_plot = FALSE) {
  
  if (save_plot && is.null(output_dir)) {
    stop("output_dir must be specified when save_plot == TRUE")
  }
  
  # Use provided ref_pm25 or extract from analysis_results
  if (is.null(ref_pm25)) {
    ref_pm25 <- na.omit(unique(analysis_results$ref_pm25))[1]
  }
  
  # Filter for specific PM2.5 variable and exclude National
  specific_results <- analysis_results %>%
    dplyr::filter(pm25_var == !!pm25_var & region != "National") %>%
    dplyr::select(date, region, pm25_values, rr, rr.lb, rr.ub, ref_name, ref_pm25) %>%
    dplyr::distinct()
  
  if (nrow(specific_results) == 0) {
    stop("No data available for exposure-response plot by region")
  }
  
  # Get reference name
  ref_name <- na.omit(unique(specific_results$ref_name))[1]
  
  # Create faceted exposure-response plot
  exp_plot <- ggplot2::ggplot(specific_results, ggplot2::aes(x = pm25_values, y = rr)) +
    ggplot2::geom_line(color = "red", linewidth = 0.8) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = rr.lb, ymax = rr.ub),
      alpha = 0.2,
      fill = "#3498DB"
    ) +
    ggplot2::geom_vline(
      xintercept = ref_pm25,
      linetype = "dashed",
      color = "#2ECC71",
      linewidth = 0.8
    ) +
    ggplot2::geom_hline(
      yintercept = 1,
      linetype = "dashed",
      color = "#7F8C8D",
      linewidth = 0.6
    ) +
    ggplot2::facet_wrap(~ region, scales = "free_y", ncol = 2) +
    ggplot2::labs(
      title = paste("Exposure-Response Relationship by Region: PM2.5 vs Relative Risk -", ref_name, "Standard"),
      subtitle = paste("Reference guideline:", ref_pm25, "µg/m³"),
      x = "PM2.5 Concentration (µg/m³)",
      y = "Relative Risk (RR)",
      caption = paste("Red line: RR estimate | Blue band: 95% CI | Green line: Reference guideline")
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "gray40", size = 11),
      plot.caption = ggplot2::element_text(hjust = 0.5, size = 9, color = "gray40"),
      axis.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", size = 11),
      strip.background = ggplot2::element_rect(fill = "gray90", color = NA)
    ) +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 6))
  
  # Save plot if requested
  if (save_plot) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    safe_refname <- tolower(gsub("\\s+", "_", ref_name))
    filename <- paste0("exposure_response_by_region_", safe_refname, "_ref", ref_pm25, ".png")
    
    # Calculate number of regions for plot dimensions
    n_regions <- length(unique(specific_results$region))
    n_rows <- ceiling(n_regions / 2)
    
    ggplot2::ggsave(
      file.path(output_dir, filename),
      plot = exp_plot,
      width = 12,
      height = 4 * n_rows,
      dpi = 300,
      bg = "white"
    )
  }
  
  return(exp_plot)
}

#' Air Pollution Power Calculation using Meta Results
#'
#' @description Produce a power statistic by region for PM2.5 attributable mortality
#' using meta-analysis results
#'
#' @param meta_results Meta-analysis results from air_pollution_meta_analysis
#' @param data_with_lags Lagged data frame
#' @param ref_pm25 Numeric. Reference PM2.5 value for attributable risk calculation
#' @param attr_thr Integer. Percentile at which to define the PM2.5 threshold for
#' calculating attributable risk. Defaults to 95.
#'
#' @returns A list containing power information by region
#'
#' @keywords internal
air_pollution_power_list <- function(
    meta_results,
    data_with_lags,
    ref_pm25 = 15,
    attr_thr = 95) {
  
  power_list <- list()
  alpha <- 0.05
  
  # Extract region results from meta_results
  region_results <- meta_results$region_results
  
  for (i in 1:nrow(region_results)) {
    region <- region_results$region[i]
    model_results <- region_results$model_results[[i]]
    
    # Get region data
    region_data <- data_with_lags[data_with_lags$region == region, ]
    
    # Calculate threshold PM2.5
    thresh_pm25 <- round(quantile(region_data$pm25, attr_thr / 100, na.rm = TRUE), 1)
    
    # Get cumulative coefficient from meta results
    # Find the cumulative lag row (e.g., "0-14" or "pm25_lag0_14")
    cum_lag_var <- paste0("pm25_lag0_", max(as.numeric(gsub(".*_", "", names(data_with_lags)[grep("pm25_lag0_", names(data_with_lags))]))))
    
    # Extract coefficient and SE for cumulative lag
    if (!is.null(model_results$coef_table)) {
      coef_table <- model_results$coef_table
      cum_row <- coef_table[grepl(cum_lag_var, coef_table$pm25_variable), ]
      
      if (nrow(cum_row) > 0) {
        # Create power dataframe for PM2.5 values above threshold
        pm25_above <- unique(region_data$pm25[region_data$pm25 >= thresh_pm25])
        pm25_above <- sort(pm25_above)
        
        if (length(pm25_above) > 0) {
          power_df <- data.frame(
            region = region,
            pm25 = pm25_above,
            cen = ref_pm25,
            log_rr = cum_row$coef * (pm25_above - ref_pm25),
            se = cum_row$se * abs(pm25_above - ref_pm25),
            z_alpha = stats::qnorm(1 - alpha / 2)
          )
          
          power_df <- power_df %>%
            dplyr::mutate(power = stats::pnorm(
              .data$log_rr / .data$se - .data$z_alpha
            ) + (1 - stats::pnorm(.data$log_rr / .data$se + .data$z_alpha))) %>%
            dplyr::select(-z_alpha) %>%
            dplyr::mutate(
              log_rr = round(.data$log_rr, 3),
              se = round(.data$se, 3),
              power_pct = round(.data$power * 100, 1)
            )
          
          power_list[[region]] <- power_df
        }
      }
    }
  }
  
  # Add national power using meta-analysis results
  meta_coefs <- meta_results$meta_results
  cum_row_meta <- meta_coefs[grepl("0-", meta_coefs$lag), ]
  
  if (nrow(cum_row_meta) > 0) {
    # Aggregate national PM2.5
    national_pm25 <- unlist(lapply(unique(data_with_lags$region), 
                                   function(r) data_with_lags$pm25[data_with_lags$region == r]))
    thresh_national <- round(quantile(national_pm25, attr_thr / 100, na.rm = TRUE), 1)
    
    pm25_above_national <- unique(national_pm25[national_pm25 >= thresh_national])
    pm25_above_national <- sort(pm25_above_national)
    
    if (length(pm25_above_national) > 0) {
      power_df_national <- data.frame(
        region = "National",
        pm25 = pm25_above_national,
        cen = ref_pm25,
        log_rr = cum_row_meta$coef * (pm25_above_national - ref_pm25),
        se = cum_row_meta$se * abs(pm25_above_national - ref_pm25),
        z_alpha = stats::qnorm(1 - alpha / 2)
      )
      
      power_df_national <- power_df_national %>%
        dplyr::mutate(power = stats::pnorm(
          .data$log_rr / .data$se - .data$z_alpha
        ) + (1 - stats::pnorm(.data$log_rr / .data$se + .data$z_alpha))) %>%
        dplyr::select(-z_alpha) %>%
        dplyr::mutate(
          log_rr = round(.data$log_rr, 3),
          se = round(.data$se, 3),
          power_pct = round(.data$power * 100, 1)
        )
      
      power_list[["National"]] <- power_df_national
    }
  }
  
  return(power_list)
}

#' Plot Power vs PM2.5 Concentration
#'
#' @description Plots the power statistic for each reference PM2.5 at and above
#' the attributable risk threshold for each region.
#'
#' @param power_list A list containing power information by region.
#' @param output_dir Character. Directory to save plot. Defaults to NULL.
#' @param save_plot Logical. Whether to save the plot. Defaults to FALSE.
#' @param ref_name Character. Reference standard name for plot title.
#'
#' @returns Invisible list of plot information
#'
#' @keywords internal
plot_air_pollution_power_simple <- function(
    power_list,
    output_dir = NULL,
    save_plot = FALSE,
    ref_name = "WHO") {
  
  if (save_plot && is.null(output_dir)) {
    stop("Output directory must be specified if save_plot==TRUE")
  }
  
  # Combine all power data
  all_power <- do.call(rbind, power_list)
  
  # Create plots for each region
  regions <- unique(all_power$region)
  n_regions <- length(regions)
  
  # Calculate grid dimensions
  grid_dims <- calculate_air_pollution_grid_dims(n_regions)
  
  plots <- list()
  
  for (region in regions) {
    region_power <- all_power[all_power$region == region, ]
    
    p <- ggplot2::ggplot(region_power, ggplot2::aes(x = pm25, y = power_pct)) +
      ggplot2::geom_line(color = "#296991", linewidth = 1.5) +
      ggplot2::geom_hline(yintercept = 80, linetype = "dashed", color = "black") +
      ggplot2::scale_y_continuous(
        limits = c(0, 100),
        breaks = seq(0, 100, 20)
      ) +
      ggplot2::labs(
        title = region,
        x = "PM2.5 (µg/m³)",
        y = "Power (%)"
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
        panel.grid.minor = ggplot2::element_blank()
      )
    
    plots[[region]] <- p
  }
  
  # Combine plots
  combined_plot <- patchwork::wrap_plots(
    plots,
    ncol = grid_dims$ncol,
    nrow = grid_dims$nrow
  ) +
    patchwork::plot_annotation(
      title = paste("Power vs PM2.5 Concentration -", ref_name, "Standard"),
      subtitle = "Dashed line shows 80% power threshold",
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5, color = "gray40")
      )
    )
  
  # Save if requested
  if (save_plot) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    safe_refname <- tolower(gsub("\\s+", "_", ref_name))
    output_file <- file.path(output_dir, 
                             paste0("power_vs_pm25_", safe_refname, ".png"))
    
    ggplot2::ggsave(
      filename = output_file,
      plot = combined_plot,
      width = 4 * grid_dims$ncol,
      height = 3.5 * grid_dims$nrow,
      dpi = 300,
      bg = "white"
    )
  }
  
  return(invisible(list(
    plots = plots,
    combined_plot = combined_plot,
    power_data = all_power
  )))
}

#===============================================================================
#' Comprehensive Air Pollution Analysis Pipeline
#'
#' @description Master function that runs the complete air pollution analysis
#' including data loading, modeling, plotting, and power analysis
#'
#' @param data_path Path to CSV data file
#' @param date_col Name of date column
#' @param region_col Name of region column
#' @param pm25_col Name of PM2.5 column
#' @param deaths_col Name of deaths column
#' @param humidity_col Name of humidity column
#' @param precipitation_col Name of precipitation column
#' @param tmax_col Name of maximum temperature column
#' @param wind_speed_col Name of wind speed column
#' @param Categorical_Others Optional vector of categorical variable names
#' @param Continuous_Others Optional vector of continuous variable names
#' @param max_lag Integer. Maximum lag days (default: 14)
#' @param family Character. GAM family (default: "quasipoisson")
#' @param reference_standards List of reference standards, each with 'value' and 'name'
#' @param output_dir Directory to save outputs
#' @param save_outputs Logical. Whether to save outputs
#' @param run_descriptive Logical. Whether to run descriptive statistics
#' @param run_power Logical. Whether to run power analysis
#' @param moving_average_window Integer. Window for moving average in descriptive stats
#'
#' @return List containing all analysis results
#' @export
do_air_pollution_analysis <- function(
    data_path,
    date_col = "date",
    region_col = "region",
    pm25_col = "pm25",
    deaths_col = "deaths",
    humidity_col = "humidity",
    precipitation_col = "precipitation",
    tmax_col = "tmax",
    wind_speed_col = "wind_speed",
    Categorical_Others = NULL,
    Continuous_Others = c("population"),
    max_lag = 14,
    family = "quasipoisson",
    reference_standards = list(
      list(value = 15, name = "WHO")
    ),
    output_dir = "air_pollution_results",
    save_outputs = TRUE,
    run_descriptive = TRUE,
    run_power = TRUE,
    moving_average_window = 3L
) {
  
  # Create output directory
  if (save_outputs && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    message("Created output directory: ", output_dir)
  }
  
  results <- list()
  
  # LOAD AND PREPARE DATA
  data <- load_air_pollution_data(
    data_path = data_path,
    date_col = date_col,
    region_col = region_col,
    pm25_col = pm25_col,
    deaths_col = deaths_col,
    humidity_col = humidity_col,
    precipitation_col = precipitation_col,
    tmax_col = tmax_col,
    wind_speed_col = wind_speed_col,
    Categorical_Others = Categorical_Others,
    Continuous_Others = Continuous_Others
  )
  
  results$data_raw <- data
  
  # CREATE LAGS
  data_with_lags <- create_air_pollution_lags(data, max_lag = max_lag)
  
  results$data_with_lags <- data_with_lags
  
  # DESCRIPTIVE STATISTICS
  if (run_descriptive) {
    env_labels <- c(
      "pm25" = "PM2.5 (µg/m³)",
      "tmax" = "Max Temperature (°C)",
      "precipitation" = "Precipitation (mm)",
      "humidity" = "Humidity (%)",
      "wind_speed" = "Wind Speed (m/s)"
    )
    
    # Add tmean if it exists
    if ("tmean" %in% names(data)) {
      env_labels <- c(env_labels, "tmean" = "Mean Temperature (°C)")
    }
    
    air_pollution_descriptive_stats(
      data = data,
      env_lables = env_labels,
      save_outputs = save_outputs,
      output_dir = output_dir,
      moving_average_window = moving_average_window
    )
  }
  
  # FIT MODELS AND META-ANALYSIS
  
  meta_results <- air_pollution_meta_analysis(
    data_with_lags = data_with_lags,
    max_lag = max_lag,
    family = family
  )
  
  results$meta_results <- meta_results
  
  # CALCULATE ATTRIBUTABLE BURDEN FOR EACH REFERENCE
  results$analysis_results <- list()
  
  for (ref_standard in reference_standards) {
    ref_pm25 <- ref_standard$value
    ref_name <- ref_standard$name
    
    # Daily analysis
    analysis_daily <- analyze_air_pollution_daily(
      data_with_lags = data_with_lags,
      meta_results = meta_results,
      ref_pm25 = ref_pm25,
      ref_name = ref_name,
      max_lag = max_lag
    )
    
    results$analysis_results[[ref_name]] <- analysis_daily
    
    # GENERATE ALL PLOTS
    results$plots <- list()
    
    analysis_res <- results$analysis_results[[ref_name]]
    
    # By region plots
    results$plots[[ref_name]]$ar_by_region <- plot_air_pollution_ar_by_region(
      analysis_results = analysis_res,
      max_lag = max_lag,
      output_dir = output_dir,
      save_plot = save_outputs
    )
    
    results$plots[[ref_name]]$an_by_region <- plot_air_pollution_an_by_region(
      analysis_results = analysis_res,
      max_lag = max_lag,
      output_dir = output_dir,
      save_plot = save_outputs
    )
    
    # By year plots
    results$plots[[ref_name]]$ar_by_year <- plot_air_pollution_ar_by_year(
      analysis_results = analysis_res,
      max_lag = max_lag,
      by_region = FALSE,
      output_dir = output_dir,
      save_plot = save_outputs
    )
    
    results$plots[[ref_name]]$an_by_year <- plot_air_pollution_an_by_year(
      analysis_results = analysis_res,
      max_lag = max_lag,
      by_region = FALSE,
      output_dir = output_dir,
      save_plot = save_outputs
    )
    
    # Forest plots
    results$plots[[ref_name]]$forest_by_region <- plot_air_pollution_forest_by_region(
      analysis_results = analysis_res,
      pm25_var = "pm25",
      output_dir = output_dir,
      save_plot = save_outputs
    )
    
    results$plots[[ref_name]]$forest_by_lag <- plot_air_pollution_forest_by_lag(
      analysis_results = analysis_res,
      max_lag = max_lag,
      output_dir = output_dir,
      save_plot = save_outputs
    )
    
    # Monthly histograms for AN and AR
    results$plots[[ref_name]]$monthly_histograms <- plot_air_pollution_monthly_histograms(
      analysis_results = analysis_res,
      pm25_var = "pm25",
      output_dir = output_dir,
      save_plot = save_outputs
    )
    
    # Exposure-response plot
    results$plots[[ref_name]]$exposure_response <- plot_air_pollution_exposure_response(
      analysis_results = analysis_res,
      pm25_var = "pm25",
      ref_pm25 = ref_pm25,
      output_dir = output_dir,
      save_plot = save_outputs
    )
    
    results$plots[[ref_name]]$aggregate_by_region <- aggregate_air_pollution_by_region (
      analysis_results = analysis_res,
      pm25_var = "pm25",
      max_lag = 14)
    
    results$plots[[ref_name]]$aggregate_by_year <- aggregate_air_pollution_by_year (
      analysis_results = analysis_res,
      pm25_var = "pm25",
      max_lag = 14,
      by_region = FALSE)
    
    results$plots[[ref_name]]$aggregate_by_month <- aggregate_air_pollution_by_month (
      analysis_results = analysis_res,
      pm25_var = "pm25",
      max_lag = 14,
      by_region = FALSE)
    
    # EXPOSURE-RESPONSE AND POWER ANALYSIS
    
    if (run_power) {
      results$power_results <- list()
      # Calculate power using meta results
      power_list <- air_pollution_power_list(
        meta_results = meta_results,
        data_with_lags = data_with_lags,
        ref_pm25 = ref_pm25,
        attr_thr = 95
      )
      
      results$power_results[[ref_name]] <- power_list
      
      # Plot power
      power_plot <- plot_air_pollution_power_simple(
        power_list = power_list,
        output_dir = output_dir,
        save_plot = save_outputs,
        ref_name = ref_name
      )
      
      results$plots[[ref_name]]$power_plot <- power_plot
      
    }
  }
  
  # Final message
  if (save_outputs) {
    message("\n  All outputs saved to: ", output_dir)
  }
  
  
  # Set class for print method
  class(results) <- c("air_pollution_analysis", "list")
  
  return(invisible(results))
}


# Load required libraries
library(tidyverse)
library(splines)
library(mixmeta)
library(metafor)
library(lubridate)
library(gridExtra)
library(mgcv)
library(tools)
library(patchwork)
library(data.table)
library(zoo)
library(corrplot)
library(RColorBrewer)


# #===============================================================================
# # EXAMPLE USAGE
# #===============================================================================
# 
# # Run complete analysis
# results <- do_air_pollution_analysis(
#   # Data specification
#   data_path = "data.csv",
#   date_col = "date",
#   region_col = "province",
#   pm25_col = "pm25",
#   deaths_col = "deaths",
#   humidity_col = "humidity",
#   precipitation_col = "precipitation",
#   tmax_col = "tmax",
#   wind_speed_col = "wind_speed",
#   Continuous_Others = c("tmean", "population"),
#   
#   # Analysis parameters
#   max_lag = 14,
#   family = "quasipoisson",
#   
#   # Reference standards to analyze
#   reference_standards = list(
#     list(value = 15, name = "WHO")
#   ),
#   
#   # Output settings
#   output_dir = "air_pollution_results",
#   save_outputs = TRUE,
#   
#   # Optional analyses
#   run_descriptive = TRUE,
#   run_power = TRUE,
#   moving_average_window = 7
# )
# 
# # Access specific results
# print(results)  # Summary
# names(results)  # All components
# 
# # Access specific components
# results$data_with_lags                    # Lagged data
# results$meta_results                      # Meta-analysis
# results$analysis_results$WHO              # WHO standard results
# results$plots$WHO$ar_by_region            # WHO AR by region plot
# results$plots$WHO$aggregate_by_region            # WHO aggregate by region plot
# results$plots$WHO$aggregate_by_year            # WHO aggregate by year plot
# results$plots$WHO$aggregate_by_month            # WHO aggregate by month plot
# results$power_results$WHO                 # WHO power analysis
# results$plots$WHO$power_plot
# 
# # Additional plots
# who_regional <- aggregate_air_pollution_by_region(
#   results$analysis_results$WHO,
#   max_lag = 14
# )
# 
# who_annual <- aggregate_air_pollution_by_year(
#   results$analysis_results$WHO,
#   max_lag = 14,
#   by_region = TRUE
# )
# 
# # Save aggregated data
# output_dir = "air_pollution_results"
# write.csv(who_regional, paste0(output_dir, "/who_regional_summary.csv"), row.names = FALSE)
# write.csv(who_annual, paste0(output_dir, "/who_annual_summary.csv"), row.names = FALSE)
# 
