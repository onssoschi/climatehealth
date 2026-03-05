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
#' @param population_col Character. Name of population column in the dataframe.
#' This is REQUIRED for calculating Attributable Rate (AR). Defaults to "population".
#' @param humidity_col Character. Name of humidity column in the dataframe. Defaults
#' to "humidity".
#' @param precipitation_col Character. Name of precipitation column in the dataframe.
#' Defaults to "precipitation".
#' @param tmax_col Character. Name of maximum temperature column in the dataframe.
#' Defaults to "tmax".
#' @param wind_speed_col Character. Name of wind speed column in the dataframe.
#' Defaults to "wind_speed".
#' @param categorical_others Optional. Character vector of additional categorical
#' variables (e.g., "sex", "age_group"). Defaults to NULL.
#' @param continuous_others Optional. Character vector of additional continuous
#' variables (e.g., "tmean"). Defaults to NULL.
#'
#' @return Dataframe with formatted and renamed with standardized column names.
#'
#' @keywords internal
load_air_pollution_data <- function(data_path,
                                    date_col = "date",
                                    region_col = "region",
                                    pm25_col = "pm25",
                                    deaths_col = "deaths",
                                    population_col = "population",
                                    humidity_col = "humidity",
                                    precipitation_col = "precipitation",
                                    tmax_col = "tmax",
                                    wind_speed_col = "wind_speed",
                                    categorical_others= NULL,
                                    continuous_others= NULL
) {

  if (!file.exists(data_path)) {
    stop("Data file not found at: ", data_path)
  }

  # Standardize columns by removing spaces
  categorical_others <- if(is.null(categorical_others)) NULL else gsub(" ", "_", categorical_others)
  continuous_others <- if(is.null(continuous_others)) NULL else gsub(" ", "_", continuous_others)
  others <- c(categorical_others, continuous_others) # all additional variables
  data0 <- if(is.character(data_path)) read.csv(data_path) else data_path

  # Define REQUIRED columns
  required_cols <- c(date_col, region_col, pm25_col, deaths_col, population_col,
                     humidity_col, precipitation_col, tmax_col, wind_speed_col)

  Col_NA <- setdiff(c(required_cols, others), names(data0))

  if(length(Col_NA) > 0) {
    stop(paste0("Variables not found: ", paste(Col_NA, collapse = ", "),
                ". Please check your dataset."))
  }

  # Rename main variables using tidy evaluation
  data <- data0 %>%
    rename(
      date         = !!sym(date_col),
      region       = !!sym(region_col),
      pm25         = !!sym(pm25_col),
      deaths       = !!sym(deaths_col),
      population   = !!sym(population_col),
      humidity     = !!sym(humidity_col),
      precipitation= !!sym(precipitation_col),
      tmax         = !!sym(tmax_col),
      wind_speed   = !!sym(wind_speed_col)
    ) %>%
    select(
      date, region, pm25, deaths, population, humidity, precipitation,
      tmax, wind_speed, all_of(others)
    )

  # Enhanced date parsing function
  universal_date <- function(x) {
    # Try to parse with English locale first
    parsed <- tryCatch({
      .with_english_locale({
        lubridate::parse_date_time(
          x,
          orders = c("ymd", "dmy", "mdy", "Ymd", "dmY", "mdY",
                     "Y/m/d", "d/m/Y", "m/d/Y", "Y-m-d H:M:S", "d/m/Y H:M", "m/d/Y H:M",
                     "dbY", "dby", "bdY", "bYd")  # Added formats for textual month names
        )
      })
    }, error = function(e) {
      # Fallback to system locale
      lubridate::parse_date_time(
        x,
        orders = c("ymd", "dmy", "mdy", "Ymd", "dmY", "mdY",
                   "Y/m/d", "d/m/Y", "m/d/Y", "Y-m-d H:M:S", "d/m/Y H:M", "m/d/Y H:M",
                   "dbY", "dby", "bdY", "bYd")
      )
    })

    return(as.Date(parsed))
  }

  # Process data with consistent English day names
  data <- .with_english_locale({
    data %>%
      arrange(region, date) %>%
      group_by(region) %>%
      dplyr::mutate(
        date = universal_date(date),
        year = lubridate::year(date),
        month = lubridate::month(date),
        day = lubridate::day(date),
        # Use English day names regardless of system locale
        dow = .english_dow_names(lubridate::wday(date), short = TRUE),
        time = dplyr::row_number()
      ) %>%
      dplyr::arrange(date)
  })

  # Convert to data.table for efficient aggregation
  data <- data.table::as.data.table(data)

  # Define grouping variables
  group_vars <- c("date", "region", "dow", categorical_others)

  # Define variables to aggregate
  agg_vars <- setdiff(names(data), c("deaths", "population", group_vars))

  # Perform aggregation
  data <- data[, c(
    list(
      deaths = sum(deaths, na.rm = TRUE),
      population = sum(population, na.rm = TRUE)
    ),
    lapply(.SD, function(x) round(mean(x, na.rm = TRUE), 2))
  ), by = group_vars, .SDcols = agg_vars]

  # Sort and select columns
  data <- data %>% arrange(region, date)

  return(data)
}

#' Create lagged values for PM2.5 variable and average lag column.
#'
#' @description Creates new variables in a dataframe for lags and means over lag
#' periods.
#'
#' @param data Dataframe from  load_air_pollution_data() containing a daily
#'             time series of health and environmental data.
#' @param max_lag Integer. The maximum lag days for outdoor PM2.5. Defaults to 14.
#'
#' @return Dataframe with added columns for lagged PM2.5 concentration.
#'
#' @import dplyr
#'
#' @keywords internal
create_air_pollution_lags <- function( data, max_lag = 14L ) {
  # Add guidance about max_lag for short-term impact assessment
  if (max_lag > 14) {
    message("NOTE: For short-term impact assessment, max_lag > 14 days is not recommended.")
    message("Typical lag periods for short-term PM2.5 health effects are \u2264 14 days.")
  }

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

#' Descriptive statistics
#'
#' @description Generates summary statistics for climate, environmental and health data
#'
#' @param data Dataframe containing a daily time series of climate, environmental
#' and health data
#' @param env_labels Named vector. Labels for environmental variables with units.
#' @param save_outputs Logical. Whether to save outputs. Defaults to FALSE.
#' @param output_dir Character. Directory to save descriptive statistics.
#' Defaults to NULL.
#' @param moving_average_window Numeric. Window size for moving average calculations.
#' Defaults to 3 (3-day moving average).
#' @param plot_corr_matrix Logical. Whether to plot correlation matrix. Defaults to FALSE.
#' @param correlation_method Character. Correlation method. One of 'pearson', 'spearman', 'kendall'.
#' @param plot_dist Logical. Whether to plot distribution histograms. Defaults to FALSE.
#' @param plot_na_counts Logical. Whether to plot NA counts. Defaults to FALSE.
#' @param plot_scatter Logical. Whether to plot scatter plots. Defaults to FALSE.
#' @param plot_box Logical. Whether to plot box plots. Defaults to FALSE.
#' @param plot_seasonal Logical. Whether to plot seasonal trends. Defaults to FALSE.
#' @param plot_regional Logical. Whether to plot regional trends. Defaults to FALSE.
#' @param plot_total Logical. Whether to plot total health outcomes per year. Defaults to FALSE.
#' @param detect_outliers Logical. Whether to detect outliers. Defaults to FALSE.
#' @param calculate_rate Logical. Whether to calculate rate per 100k people.. Defaults to FALSE.
#'
#' @return Invisibly returns the national data with moving averages
#'
#' @keywords internal
air_pollution_descriptive_stats <- function(data,
                                            env_labels = c(
                                              "pm25" = "PM2.5 (\u00B5g/m\u00B3)",
                                              "tmax" = "Max Temperature (\u00B0C)",
                                              "precipitation" = "Precipitation (mm)",
                                              "humidity" = "Humidity (%)",
                                              "wind_speed" = "Wind Speed (m/s)"),
                                            save_outputs = FALSE,
                                            output_dir = NULL,
                                            moving_average_window = 3L,
                                            plot_corr_matrix = FALSE,
                                            correlation_method = "pearson",
                                            plot_dist = FALSE,
                                            plot_na_counts = FALSE,
                                            plot_scatter = FALSE,
                                            plot_box = FALSE,
                                            plot_seasonal = FALSE,
                                            plot_regional = FALSE,
                                            plot_total = FALSE,
                                            detect_outliers = FALSE,
                                            calculate_rate = FALSE) {

  # Validate moving_average_window parameter
  if (!is.numeric(moving_average_window) || moving_average_window < 1) {
    stop("moving_average_window must be a positive integer")
  }

  if (moving_average_window < 3) {
    warning("Window < 3 provides minimal smoothing. Consider using window >= 3 for better results.")
  }
  moving_average_window <- as.integer(moving_average_window)

  # Validate data structure
  if (!("data.frame" %in% class(data))) stop("data must be a data.frame or tibble")
  if (!all(c("date", "deaths", "region") %in% names(data))) {
    stop("data must contain at least the columns: date, deaths, region")
  }

  # Determine which environmental variables to use based on env_labels and data columns
  env_vars <- intersect(names(env_labels), names(data))
  if (length(env_vars) == 0) {
    warning("No environmental variables from env_labels were found in the data. Continuing with deaths only.")
  }

  # Create units mapping
  units <- c(deaths = "Number of Deaths")
  if (length(env_vars) > 0) {
    units <- c(units, env_labels[env_vars])
  }

  # Check save_outputs / output_dir
  if (save_outputs && is.null(output_dir)) {
    stop("An output directory must be passed if save_outputs==TRUE.")
  }

  # Create output directory if needed
  if (save_outputs) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }
  }

  # Create national dataset: deaths sum, env_vars mean
  data_n <- data %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(
      deaths = sum(deaths, na.rm = TRUE),
      dplyr::across(all_of(env_vars), ~ mean(.x, na.rm = TRUE)),
      .groups = "drop"
    )

  # Prepare data for analysis
  data_for_stats <- data %>%
    dplyr::mutate(date = as.Date(date))

  # Define columns for analysis
  columns <- c("deaths", env_vars)

  # Create output subdirectory for descriptive stats
  if (save_outputs) {
    desc_stats_dir <- file.path(output_dir)
    if (!dir.exists(desc_stats_dir)) {
      dir.create(desc_stats_dir, recursive = TRUE, showWarnings = FALSE)
    }
  } else {
    # Use a temporary directory
    desc_stats_dir <- tempfile("air_pollution_desc_stats_")
    dir.create(desc_stats_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Run the generic descriptive statistics module
  tryCatch({
    # Use the package wrapper for internal module integration
    result_paths <- run_descriptive_stats(
      data = data_for_stats,
      output_path = desc_stats_dir,
      aggregation_column = "region",
      population_col = "population",
      dependent_col = "deaths",
      independent_cols = env_vars,
      units = units,
      plot_corr_matrix = plot_corr_matrix,
      plot_dist = plot_dist,
      plot_ma = TRUE,
      plot_na_counts = plot_na_counts,
      plot_scatter = plot_scatter,
      plot_box = plot_box,
      plot_seasonal = plot_seasonal,
      plot_regional = plot_regional,
      plot_total = plot_total,
      correlation_method = correlation_method,
      ma_days = moving_average_window,
      ma_sides = 1,
      timeseries_col = "date",
      detect_outliers = detect_outliers,
      calculate_rate = calculate_rate,
      create_base_dir = TRUE
    )

  }, error = function(e) {
    warning("Error running generic descriptive statistics module: ", e$message)
    message("Falling back to basic summary statistics...")

    # Fallback: basic summary statistics (silent mode)
    try({
      basic_summary <- create_column_summaries(data_for_stats,independent_cols = columns)
      basic_na_summary <- create_na_summary(data_for_stats,independent_cols = columns)
      corr_matrix <- create_correlation_matrix(data_for_stats, independent_cols = columns,
                                               correlation_method = correlation_method)

      # Save fallback results if requested
      if (save_outputs) {
        write.csv(basic_summary, file.path(desc_stats_dir, "fallback_column_summary.csv"), row.names = TRUE)
        write.csv(basic_na_summary, file.path(desc_stats_dir, "fallback_na_summary.csv"), row.names = FALSE)
        write.csv(corr_matrix, file.path(desc_stats_dir, "fallback_correlation_matrix.csv"), row.names = TRUE)

        # Create basic correlation plot
        try({
          plot_correlation_matrix(
            matrix_ = corr_matrix,
            title = "Correlation Matrix (Fallback Mode)",
            output_path = file.path(desc_stats_dir, "fallback_correlation_matrix.png")
          )
        }, silent = TRUE)
      }
    }, silent = TRUE)
  })

  # Compute national data with moving averages for return value
  national_df_ma <- data_n %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(
      dplyr::across(all_of(c("deaths", env_vars)),
                    ~ zoo::rollmean(.x, k = moving_average_window, fill = NA, align = "center"),
                    .names = "{.col}_ma{moving_average_window}")
    )

  invisible(national_df_ma)
}

#' Fit GAM model
#'
#' @description Fit a generalized additive model (mgcv::gam) including pm25 and its lagged
#' variables (pm25_lag1, ..., pm25_lagN)
#'
#' @param data_with_lags data.frame or tibble containing the outcome, confounders
#'   and pm25 lag variables.
#' @param max_lag integer. Maximum lag to include. Defaults to 14.
#' @param df_seasonal integer. Degrees of freedom for seasonal spline. Default 6.
#' @param family character or family object passed to mgcv::gam. Default "quasipoisson".
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
                                  df_seasonal = 6L,
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
  lag_formula <- paste(present_lag_vars, collapse = " + ")

  # Check if tmean is available in the data
  tmean_term <- if ("tmean" %in% names(data_with_lags) &&
                    !all(is.na(data_with_lags$tmean))) {
    " + s(tmean, k = 3)"
  } else {
    ""
  }

  GAM_formula <- as.formula(
    paste0(
      "deaths ~ ", lag_formula,
      " + s(time, k = ", df_seasonal * yr, ")",
      " + s(tmax, k = 3)",
      " + s(humidity, k = 3)",
      " + s(precipitation, k = 3)",
      " + s(wind_speed, k = 3)",
      tmean_term,
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
#' @param df_seasonal Integer. Degrees of freedom for seasonal spline. Default 6.
#' @param family Character string indicating the distribution family used in the GAM.
#'
#' @return Dataframe with lag-specific results including for regional and national
#'
#' @keywords internal
air_pollution_meta_analysis <- function(data_with_lags,
                                        max_lag = 14L,
                                        df_seasonal = 6L,
                                        family = "quasipoisson"
) {

  # Fit distributed-lag model per region
  region_results <- data_with_lags %>%
    dplyr::group_by(region) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      model_results = purrr::map2(data, region, ~ fit_air_pollution_gam(.x, max_lag, df_seasonal, family))
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
#' @param meta_results Dataset. Results from meta analysis.
#' @param ref_pm25 Numeric. PM2.5 reference value. Defaults to 15.
#' @param ref_name Character. Reference body name. Defaults to "WHO".
#' @param max_lag Integer. Maximum lag days. Defaults to 14.
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
    if (!is.null(x$coef_table)) return(x$coef_table)
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

  # Normalize columns in region_coefs
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
        rr <- exp(coef * pmax(prov_data[[var]] - ref_pm25, 0))
        rr.lb <- exp(ci.lb * pmax(prov_data[[var]] - ref_pm25, 0))
        rr.ub <- exp(ci.ub * pmax(prov_data[[var]] - ref_pm25, 0))

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
        var_name = var,
        coef = coef,
        se = se,
        rr = rr, rr.lb = rr.lb, rr.ub = rr.ub,
        af = af, af.lb = af.lb, af.ub = af.ub,
        an = an, an.lb = an.lb, an.ub = an.ub,
        ar = ar, ar.lb = ar.lb, ar.ub = ar.ub,
        tot_deaths = deaths_prov,
        pop = population_prov,
        pm25_values = prov_data[[var]],
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
      rr <- exp(coef * pmax(data_national[[var]] - ref_pm25, 0))
      rr.lb <- exp(ci.lb * pmax(data_national[[var]] - ref_pm25, 0))
      rr.ub <- exp(ci.ub * pmax(data_national[[var]] - ref_pm25, 0))

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
      var_name = var,
      coef = coef,
      se = se,
      rr = rr, rr.lb = rr.lb, rr.ub = rr.ub,
      af = af, af.lb = af.lb, af.ub = af.ub,
      an = an, an.lb = an.lb, an.ub = an.ub,
      ar = ar, ar.lb = ar.lb, ar.ub = ar.ub,
      tot_deaths = deaths_national,
      pop = population_national,
      pm25_values = data_national[[var]],
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
#' @keywords internal
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

#' Plot forest plot for PM2.5 effects by region
#'
#' @param analysis_results Processed results with RR/AF/AN/AR with lag variables
#' @param max_lag Integer. The maximum lag days for outdoor PM2.5. Defaults to 14.
#' @param include_national Logical. Whether to include national results. Default TRUE.
#' @param output_dir Character. Directory to save plot. Defaults to NULL.
#' @param save_plot Logical. Whether to save the plot. Defaults to FALSE.
#'
#' @return ggplot object
#'
#' @keywords internal
plot_air_pollution_forest_by_region <- function(analysis_results,
                                                max_lag = 14L,
                                                include_national = TRUE,
                                                output_dir = NULL,
                                                save_plot = FALSE) {
  if (is.null(output_dir) && save_plot) {
    stop("output_dir must be specified when save_plot == TRUE.")
  }

  # Filter data based on include_national
  if (!include_national) {
    analysis_results <- analysis_results %>% dplyr::filter(region != "National")
  }

  # select and summarise by region
  pm25 <- var_name <- paste0("pm25_lag0_", max_lag)
  specific_results <- analysis_results %>%
    dplyr::filter(var_name == pm25) %>%
    dplyr::group_by(region, ref_name, ref_pm25, var_name) %>%
    dplyr::summarise(
      rr = mean(rr, na.rm = TRUE),
      rr.lb = mean(rr.lb, na.rm = TRUE),
      rr.ub = mean(rr.ub, na.rm = TRUE),
      .groups = "drop"
    )

  if (nrow(specific_results) == 0) {
    stop("No results for the selected var_name.")
  }

  # reference metadata (first non-NA)
  ref_name <- na.omit(unique(specific_results$ref_name))[1]
  ref_pm25 <- na.omit(unique(specific_results$ref_pm25))[1]

  # mark national row so it can be highlighted
  specific_results <- specific_results %>%
    dplyr::mutate(is_national = (.data$region == "National"))

  # Create ordered factor for region
  if (include_national) {
    regions_only <- specific_results %>% filter(region != "National")
    region_order <- c(unique(regions_only$region), "National")
  } else {
    region_order <- unique(specific_results$region)
  }

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
      title = sprintf('PM2.5 effects by region - Ref: "%s" = %s', ref_name, ref_pm25)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(hjust = 0.5),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )

  if (save_plot) {
    filename <- sprintf("forest_plot_by_region_%s_%s_%s", var_name, ref_name, ref_pm25)
    save_air_pollution_plot(
      plot_object = forest_plot,
      output_dir = output_dir,
      filename = filename
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
                                             max_lag = 14L,
                                             output_dir = NULL,
                                             save_plot = FALSE) {
  if (is.null(output_dir) && save_plot) {
    stop("output_dir must be specified when save_plot == TRUE.")
  }

  # select and summarise by region
  specific_results <- analysis_results %>%
    dplyr::group_by(var_name, ref_name, ref_pm25) %>%
    dplyr::summarise(
      rr = mean(rr, na.rm = TRUE),
      rr.lb = mean(rr.lb, na.rm = TRUE),
      rr.ub = mean(rr.ub, na.rm = TRUE),
      .groups = "drop"
    )

  if (nrow(specific_results) == 0) {
    stop("No results for the selected var_name.")
  }

  # reference metadata (first non-NA)
  ref_name <- na.omit(unique(specific_results$ref_name))[1]
  ref_pm25 <- na.omit(unique(specific_results$ref_pm25))[1]

  # mark national row so it can be highlighted
  specific_results <- specific_results %>%
    dplyr::mutate(is_cumulative = (.data$var_name == paste0("pm25_lag0_", max_lag)))

  # Create ordered factor for var_name
  lag_order <- c("pm25", paste0("pm25_lag", 1:max_lag), paste0("pm25_lag0_", max_lag))

  specific_results <- specific_results %>%
    dplyr::mutate(var_name = factor(.data$var_name, levels = lag_order))

  # global plotting limits
  global_max_rr <- max(specific_results$rr.ub, na.rm = TRUE)
  global_min_rr <- min(specific_results$rr.lb, na.rm = TRUE)
  max_ylim <- global_max_rr * 1.01
  min_ylim <- min(global_min_rr, 1) * 0.99

  forest_plot <- ggplot2::ggplot(specific_results, ggplot2::aes(x = .data$var_name, y = .data$rr)) +
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
      title = sprintf('PM2.5 effects by region - Ref: "%s" = %s', ref_name, ref_pm25)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(hjust = 0.5),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
  var_name_n <- na.omit(unique(specific_results$var_name))

  if (save_plot) {
    filename <- sprintf("forest_plot_by_lag_%s_%s", ref_name, ref_pm25)
    save_air_pollution_plot(
      plot_object = forest_plot,
      output_dir = output_dir,
      filename = filename
    )
  }

  return(forest_plot)
}

#' Aggregate air pollution results by region
#'
#' @description Aggregates daily analysis results to regional summaries
#'
#' @param analysis_results Results from analyze_air_pollution_daily
#' @param max_lag Integer. Maximum lag used in analysis. Defaults to 14.
#'
#' @return Dataframe with regional aggregates
#'
#' @keywords internal
aggregate_air_pollution_by_region <- function(analysis_results,
                                              max_lag = 14L) {

  # Filter to cumulative PM2.5 variable
  var_name <- paste0("pm25_lag0_", max_lag)
  results_filtered <- analysis_results %>%
    dplyr::filter(var_name == !!var_name)

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

#' Aggregate air pollution results by year
#'
#' @description Aggregates daily analysis results to annual summaries
#'
#' @param analysis_results Results from analyze_air_pollution_daily
#' @param max_lag Integer. Maximum lag used in analysis. Defaults to 14.
#' @param include_national Logical. Whether to include national results. Default TRUE.
#'
#' @return Dataframe with annual aggregates
#'
#' @keywords internal
aggregate_air_pollution_by_year <- function(analysis_results,
                                            max_lag = 14L,
                                            include_national = TRUE) {

  # Filter based on include_national
  if (!include_national) {
    analysis_results <- analysis_results %>% dplyr::filter(region != "National")
  }

  # Add year column
  var_name <- paste0("pm25_lag0_", max_lag)
  results_with_year <- analysis_results %>%
    dplyr::filter(var_name == !!var_name) %>%
    dplyr::mutate(year = lubridate::year(date))

  # Define grouping variables
  group_vars <- c("year", "ref_name", "ref_pm25", "region")

  # Aggregate by region
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

#' Aggregate air pollution results by month
#'
#' @description Aggregates daily analysis results to monthly summaries
#'
#' @param analysis_results Results from analyze_air_pollution_daily
#' @param max_lag Integer. Maximum lag used in analysis. Defaults to 14.
#' @param include_national Logical. Whether to include national results. Default TRUE.
#'
#' @return Dataframe with monthly aggregates
#'
#' @keywords internal
aggregate_air_pollution_by_month <- function(analysis_results,
                                             max_lag = 14L,
                                             include_national = TRUE) {

  # Filter based on include_national
  if (!include_national) {
    analysis_results <- analysis_results %>% dplyr::filter(region != "National")
  }

  # Add year and month columns
  var_name <- paste0("pm25_lag0_", max_lag)
  results_with_time <- analysis_results %>%
    dplyr::filter(var_name == !!var_name) %>%
    dplyr::mutate(
      year = lubridate::year(date),
      month = lubridate::month(date)
    )

  # Define grouping variables
  group_vars <- c("year", "month", "ref_name", "ref_pm25", "region")

  # Aggregate by region
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

#' Combined Monthly Time Series Plots of AN and AR
#'
#' @description Creates both Attributable Number (AN) and Attributable Rate (AR)
#' monthly time series plots in a single function call.
#'
#' @param analysis_results Results from analyze_air_pollution_daily
#' @param max_lag Integer. Maximum lag used in analysis. Defaults to 14.
#' @param include_national Logical. Whether to include national results. Default TRUE.
#' @param output_dir Character. Directory to save plot
#' @param save_plot Logical. Whether to save the plot
#'
#' @return List with two ggplot objects: an_plot and ar_plot
#'
#' @keywords internal
plot_air_pollution_an_ar_monthly <- function(analysis_results,
                                             max_lag = 14L,
                                             include_national = TRUE,
                                             output_dir = NULL,
                                             save_plot = FALSE) {

  if (save_plot && is.null(output_dir)) {
    stop("output_dir must be specified when save_plot == TRUE")
  }

  # Filter based on include_national
  if (!include_national) {
    analysis_results <- analysis_results %>% dplyr::filter(region != "National")
  }

  # Filter to cumulative PM2.5 variable
  var_name <- paste0("pm25_lag0_", max_lag)
  results_filtered <- analysis_results %>%
    dplyr::filter(var_name == !!var_name)

  # Get reference info
  ref_name <- unique(results_filtered$ref_name)[1]
  ref_pm25 <- unique(results_filtered$ref_pm25)[1]

  # Aggregate by month
  results_filtered <- results_filtered %>%
    dplyr::mutate(
      year = lubridate::year(date),
      month = lubridate::month(date),
      year_month = as.Date(paste(year, month, "01", sep = "-"))
    )

  # Define grouping variables
  group_vars <- c("year_month", "ref_name", "ref_pm25", "var_name", "region")

  # Aggregate to monthly level for both AN and AR
  monthly_data <- results_filtered %>%
    dplyr::group_by(across(all_of(group_vars))) %>%
    dplyr::summarise(
      an_monthly = sum(an, na.rm = TRUE),
      an_lower = sum(an.lb, na.rm = TRUE),
      an_upper = sum(an.ub, na.rm = TRUE),
      ar_monthly = sum(ar, na.rm = TRUE),
      ar_lower = sum(ar.lb, na.rm = TRUE),
      ar_upper = sum(ar.ub, na.rm = TRUE),
      n_days = n(),
      .groups = 'drop'
    ) %>%
    dplyr::arrange(year_month)

  # Create custom month-year labels with English month abbreviations
  monthly_data <- .with_english_locale({
    monthly_data %>%
      dplyr::mutate(
        month_abbr = .english_month_names(lubridate::month(year_month), short = TRUE),
        year_num = lubridate::year(year_month),
        month_label = paste(month_abbr, year_num),
        month_label = factor(month_label, levels = unique(month_label))
      ) %>%
      dplyr::arrange(year_month)
  })

  # Determine number of columns for facet_wrap
  n_regions <- length(unique(monthly_data$region))
  grid_dims <- calculate_air_pollution_grid_dims(n_regions)
  n_cols <- grid_dims$ncol

  # Calculate global y-axis limits for consistent scaling
  # For AN plot: use maximum of an_upper across all regions
  an_global_max <- max(monthly_data$an_upper, na.rm = TRUE) * 1.05
  an_global_min <- min(monthly_data$an_lower, na.rm = TRUE) * 0.95
  if (an_global_min > 0) an_global_min <- 0

  # For AR plot: use maximum of ar_upper across all regions
  ar_global_max <- max(monthly_data$ar_upper, na.rm = TRUE) * 1.05
  ar_global_min <- min(monthly_data$ar_lower, na.rm = TRUE) * 0.95
  if (ar_global_min > 0) ar_global_min <- 0

  # Create AN facetted plot with consistent y-axis
  an_plot <- ggplot2::ggplot(
    monthly_data,
    ggplot2::aes(x = year_month, y = an_monthly)
  ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = an_lower, ymax = an_upper),
      alpha = 0.2,
      fill = "#F00001"
    ) +
    ggplot2::geom_line(color = "darkblue", linewidth = 1) +
    ggplot2::facet_wrap(~ region, ncol = n_cols) +
    ggplot2::scale_x_date(
      date_breaks = "6 months",
      date_labels = "%b %Y"
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::comma,
      limits = c(an_global_min, an_global_max)
    ) +
    ggplot2::labs(
      title = sprintf(
        "Monthly Attributable Number (AN) by Region\n(%s Standard: %s \u00B5g/m\u00B3)",
        ref_name, ref_pm25
      ),
      subtitle = "Shaded area shows 95% confidence interval",
      x = "Month",
      y = "Attributable Number of Deaths"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 16),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12, color = "gray40"),
      axis.title = ggplot2::element_text(face = "bold"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 9),
      plot.caption = ggplot2::element_text(hjust = 0.5, size = 10, color = "gray40"),
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", size = 11),
      strip.background = ggplot2::element_rect(fill = "gray90", color = "gray80")
    )

  # Create AR facetted plot with consistent y-axis
  ar_plot <- ggplot2::ggplot(
    monthly_data,
    ggplot2::aes(x = year_month, y = ar_monthly)
  ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = ar_lower, ymax = ar_upper),
      alpha = 0.2,
      fill = "#F00001"
    ) +
    ggplot2::geom_line(color = "darkblue", linewidth = 1) +
    ggplot2::facet_wrap(~ region, ncol = n_cols) +
    ggplot2::scale_x_date(
      date_breaks = "6 months",
      date_labels = "%b %Y"
    ) +
    ggplot2::scale_y_continuous(
      limits = c(ar_global_min, ar_global_max)
    ) +
    ggplot2::labs(
      title = sprintf(
        "Monthly Attributable Rate (AR) by Region\n(%s Standard: %s \u00B5g/m\u00B3)",
        ref_name, ref_pm25
      ),
      subtitle = "Shaded area shows 95% confidence interval",
      x = "Month",
      y = "Attributable Rate (per 100K population)"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 16),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12, color = "gray40"),
      axis.title = ggplot2::element_text(face = "bold"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 9),
      plot.caption = ggplot2::element_text(hjust = 0.5, size = 10, color = "gray40"),
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", size = 11),
      strip.background = ggplot2::element_rect(fill = "gray90", color = "gray80")
    )

  # Save plots if requested
  if (save_plot) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }

    safe_refname <- tolower(gsub("\\s+", "_", ref_name))

    # Calculate appropriate dimensions
    n_rows <- grid_dims$nrow
    plot_height <- max(6, 4 * n_rows)
    plot_width <- 14

    # Save AN plot
    an_filename <- paste0("air_pollution_an_monthly_by_region_", safe_refname, "_ref", ref_pm25, ".png")

    ggplot2::ggsave(
      file.path(output_dir, an_filename),
      plot = an_plot,
      width = plot_width,
      height = plot_height,
      dpi = 300,
      bg = "white"
    )

    # Save AR plot
    ar_filename <- paste0("air_pollution_ar_monthly_by_region_", safe_refname, "_ref", ref_pm25, ".png")

    ggplot2::ggsave(
      file.path(output_dir, ar_filename),
      plot = ar_plot,
      width = plot_width,
      height = plot_height,
      dpi = 300,
      bg = "white"
    )
  }

  # Return list of plots
  return(list(an_plot = an_plot, ar_plot = ar_plot))
}

#' Plot the AN and AR by year
#'
#' @description Creates both Attributable Number (AN) and Attributable Rate (AR)
#' plots aggregated by year in a single function call.
#'
#' @param analysis_results Results from analyze_air_pollution_daily
#' @param max_lag Integer. Maximum lag. Defaults to 14.
#' @param include_national Logical. Whether to include national results. Default TRUE.
#' @param output_dir Character. Directory to save plot
#' @param save_plot Logical. Whether to save
#'
#' @return List with two ggplot objects: an_plot and ar_plot
#'
#' @keywords internal
plot_air_pollution_an_ar_by_year <- function(analysis_results,
                                             max_lag = 14L,
                                             include_national = TRUE,
                                             output_dir = NULL,
                                             save_plot = FALSE) {

  if (save_plot && is.null(output_dir)) {
    stop("output_dir must be specified when save_plot == TRUE")
  }

  # Filter based on include_national
  if (!include_national) {
    analysis_results <- analysis_results %>% dplyr::filter(region != "National")
  }

  # Aggregate data
  annual_data <- aggregate_air_pollution_by_year(
    analysis_results = analysis_results,
    max_lag = max_lag,
    include_national = include_national
  )

  # Get reference info
  ref_name <- unique(annual_data$ref_name)[1]
  ref_pm25 <- unique(annual_data$ref_pm25)[1]

  # Determine number of columns for facet_wrap
  n_regions <- length(unique(annual_data$region))
  grid_dims <- calculate_air_pollution_grid_dims(n_regions)
  n_cols <- grid_dims$ncol

  # Calculate year breaks for x-axis
  year_range <- range(annual_data$year, na.rm = TRUE)
  year_breaks <- seq(floor(year_range[1]), ceiling(year_range[2]), by = 1)

  # Calculate global y-axis limits for consistent scaling
  # For AR plot: use maximum of ar_upper across all regions
  ar_global_max <- max(annual_data$ar_upper, na.rm = TRUE) * 1.05
  ar_global_min <- min(annual_data$ar_lower, na.rm = TRUE) * 0.95
  if (ar_global_min > 0) ar_global_min <- 0

  # For AN plot: use maximum of an_upper across all regions
  an_global_max <- max(annual_data$an_upper, na.rm = TRUE) * 1.05
  an_global_min <- min(annual_data$an_lower, na.rm = TRUE) * 0.95
  if (an_global_min > 0) an_global_min <- 0

  # Create AR facetted plot with consistent y-axis
  ar_plot <- ggplot2::ggplot(
    annual_data,
    ggplot2::aes(x = year, y = ar_per_100k)
  ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = ar_lower, ymax = ar_upper),
      alpha = 0.2,
      fill = "#F00001"
    ) +
    ggplot2::geom_line(color = "darkblue", linewidth = 1) +
    ggplot2::facet_wrap(~ region, ncol = n_cols) +
    ggplot2::scale_x_continuous(
      breaks = year_breaks,
      labels = function(x) format(x, big.mark = "", scientific = FALSE)
    ) +
    ggplot2::scale_y_continuous(
      limits = c(ar_global_min, ar_global_max)
    ) +
    ggplot2::labs(
      title = sprintf(
        "Annual Attributable Rate (AR) by Region\n(%s Standard: %s \u00B5g/m\u00B3)",
        ref_name, ref_pm25
      ),
      subtitle = "Shaded area shows 95% confidence interval",
      x = "Year",
      y = "Attributable Rate (per 100K population)"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 16),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12, color = "gray40"),
      axis.title = ggplot2::element_text(face = "bold"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 9),
      plot.caption = ggplot2::element_text(hjust = 0.5, size = 10, color = "gray40"),
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", size = 11),
      strip.background = ggplot2::element_rect(fill = "gray90", color = "gray80")
    )

  # Create AN facetted plot with consistent y-axis
  an_plot <- ggplot2::ggplot(
    annual_data,
    ggplot2::aes(x = year, y = an)
  ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = an_lower, ymax = an_upper),
      alpha = 0.2,
      fill = "#F00001"
    ) +
    ggplot2::geom_line(color = "darkblue", linewidth = 1) +
    ggplot2::facet_wrap(~ region, ncol = n_cols) +
    ggplot2::scale_x_continuous(
      breaks = year_breaks,
      labels = function(x) format(x, big.mark = "", scientific = FALSE)
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::comma,
      limits = c(an_global_min, an_global_max)
    ) +
    ggplot2::labs(
      title = sprintf(
        "Annual Attributable Number (AN) by Region\n(%s Standard: %s \u00B5g/m\u00B3)",
        ref_name, ref_pm25
      ),
      subtitle = "Shaded area shows 95% confidence interval",
      x = "Year",
      y = "Attributable Number of Deaths"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 16),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12, color = "gray40"),
      axis.title = ggplot2::element_text(face = "bold"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 9),
      plot.caption = ggplot2::element_text(hjust = 0.5, size = 10, color = "gray40"),
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", size = 11),
      strip.background = ggplot2::element_rect(fill = "gray90", color = "gray80")
    )

  if (save_plot) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }

    safe_refname <- tolower(gsub("\\s+", "_", ref_name))

    # Calculate appropriate dimensions
    n_rows <- grid_dims$nrow
    plot_height <- max(6, 4 * n_rows)
    plot_width <- 14

    ggplot2::ggsave(
      file.path(output_dir, paste0("air_pollution_ar_by_year_by_region_", safe_refname, "_ref", ref_pm25, ".png")),
      plot = ar_plot,
      width = plot_width,
      height = plot_height,
      dpi = 300,
      bg = "white"
    )

    ggplot2::ggsave(
      file.path(output_dir, paste0("air_pollution_an_by_year_by_region_", safe_refname, "_ref", ref_pm25, ".png")),
      plot = an_plot,
      width = plot_width,
      height = plot_height,
      dpi = 300,
      bg = "white"
    )
  }

  return(list(ar_plot = ar_plot, an_plot = an_plot))
}

#' Plot histograms for AN and AR by month
#'
#' @description Creates histogram plots for Attributable Number (AN) and
#' Attributable Rate (AR) aggregated by month with connecting lines
#'
#' @param analysis_results Processed results with RR/AF/AN/AR with lag variables
#' @param include_national Logical. Whether to include national results. Default TRUE.
#' @param output_dir Character. Directory to save plots.
#' @param save_plot Logical. Whether to save the plots.
#'
#' @return List with ggplot objects
#'
#' @keywords internal
plot_air_pollution_monthly_histograms <- function(analysis_results,
                                                  max_lag = 14L,
                                                  include_national = TRUE,
                                                  output_dir = NULL,
                                                  save_plot = FALSE) {

  if (save_plot && is.null(output_dir)) {
    stop("output_dir must be specified when save_plot == TRUE")
  }

  # Filter for cumulative PM2.5 variable
  var_name <- paste0("pm25_lag0_", max_lag)
  specific_results <- analysis_results %>%
    dplyr::filter(var_name == !!var_name)

  # Filter based on include_national
  if (!include_national) {
    specific_results <- specific_results %>% dplyr::filter(region != "National")
  }

  # Get reference info
  ref_name <- na.omit(unique(specific_results$ref_name))[1]
  ref_pm25 <- na.omit(unique(specific_results$ref_pm25))[1]

  # Aggregate by month and region using English month names
  monthly_agg <- .with_english_locale({
    specific_results %>%
      dplyr::mutate(
        month_num = lubridate::month(date),
        month = .english_month_names(month_num)  # Use English month names
      ) %>%
      dplyr::group_by(region, month) %>%
      dplyr::summarise(
        an_total = sum(an, na.rm = TRUE),
        ar_aggr = sum(ar, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        month = factor(month, levels = .english_month_names())
      )
  })

  # Determine number of columns for facet_wrap
  n_regions <- length(unique(monthly_agg$region))
  grid_dims <- calculate_air_pollution_grid_dims(n_regions)
  n_cols <- grid_dims$ncol

  # Calculate global y-axis limits for consistent scaling
  an_global_max <- max(monthly_agg$an_total, na.rm = TRUE) * 1.05
  ar_global_max <- max(monthly_agg$ar_aggr, na.rm = TRUE) * 1.05

  # Create fill color column based on region
  if (include_national) {
    # Create fill color column
    monthly_agg <- monthly_agg %>%
      dplyr::mutate(fill_color = ifelse(region == "National", "National", "Regional"))
  }

  # Plot AN by month with consistent y-axis
  if (include_national) {
    an_plot <- ggplot2::ggplot(monthly_agg, ggplot2::aes(x = month, y = an_total, fill = fill_color)) +
      ggplot2::geom_bar(stat = "identity", alpha = 0.8) +
      # Add line connecting top centers of bars
      ggplot2::geom_line(ggplot2::aes(group = region, color = fill_color),
                         linewidth = 0.8, na.rm = TRUE) +
      # Add points at top centers of bars
      ggplot2::geom_point(ggplot2::aes(group = region, color = fill_color),
                          size = 2, na.rm = TRUE) +
      ggplot2::scale_fill_manual(values = c("National" = "#F00001", "Regional" = "#2E86AB")) +
      ggplot2::scale_color_manual(values = c("National" = "#F00001", "Regional" = "#2E86AB")) +
      ggplot2::labs(
        title = paste("Monthly Attributable Number (AN) by Region -", ref_name, "Standard"),
        subtitle = paste("Reference PM2.5:", ref_pm25, "\u00B5g/m\u00B3"),
        x = "Month",
        y = "Total Attributable Number"
      ) +
      ggplot2::guides(fill = "none", color = "none")
  } else {
    an_plot <- ggplot2::ggplot(monthly_agg, ggplot2::aes(x = month, y = an_total, fill = region)) +
      ggplot2::geom_bar(stat = "identity", alpha = 0.8) +
      # Add line connecting top centers of bars
      ggplot2::geom_line(ggplot2::aes(group = region, color = region),
                         linewidth = 0.8, na.rm = TRUE) +
      # Add points at top centers of bars
      ggplot2::geom_point(ggplot2::aes(group = region, color = region),
                          size = 2, na.rm = TRUE) +
      ggplot2::scale_fill_brewer(palette = "Set3") +
      ggplot2::scale_color_brewer(palette = "Set3") +
      ggplot2::labs(
        title = paste("Monthly Attributable Number (AN) by Region -", ref_name, "Standard"),
        subtitle = paste("Reference PM2.5:", ref_pm25, "\u00B5g/m\u00B3"),
        x = "Month",
        y = "Total Attributable Number"
      ) +
      ggplot2::guides(fill = "none", color = "none")
  }

  # Add common theme elements for AN plot
  an_plot <- an_plot +
    ggplot2::facet_wrap(~ region, ncol = n_cols) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 16),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "gray40", size = 11),
      plot.caption = ggplot2::element_text(hjust = 0.5, size = 9, color = "gray40"),
      axis.title = ggplot2::element_text(face = "bold"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 9),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", size = 11),
      strip.background = ggplot2::element_rect(fill = "gray90", color = "gray80")
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::comma,
      limits = c(0, an_global_max)
    )

  # Plot AR by month with consistent y-axis
  if (include_national) {
    ar_plot <- ggplot2::ggplot(monthly_agg, ggplot2::aes(x = month, y = ar_aggr, fill = fill_color)) +
      ggplot2::geom_bar(stat = "identity", alpha = 0.8) +
      # Add line connecting top centers of bars
      ggplot2::geom_line(ggplot2::aes(group = region, color = fill_color),
                         linewidth = 0.8, na.rm = TRUE) +
      # Add points at top centers of bars
      ggplot2::geom_point(ggplot2::aes(group = region, color = fill_color),
                          size = 2, na.rm = TRUE) +
      ggplot2::scale_fill_manual(values = c("National" = "#F00001", "Regional" = "#2E86AB")) +
      ggplot2::scale_color_manual(values = c("National" = "#F00001", "Regional" = "#2E86AB")) +
      ggplot2::labs(
        title = paste("Monthly Attributable Rate (AR) by Region -", ref_name, "Standard"),
        subtitle = paste("Reference PM2.5:", ref_pm25, "\u00B5g/m\u00B3"),
        x = "Month",
        y = "Aggregated AR (per 100,000 population)"
      ) +
      ggplot2::guides(fill = "none", color = "none")
  } else {
    ar_plot <- ggplot2::ggplot(monthly_agg, ggplot2::aes(x = month, y = ar_aggr, fill = region)) +
      ggplot2::geom_bar(stat = "identity", alpha = 0.8) +
      # Add line connecting top centers of bars
      ggplot2::geom_line(ggplot2::aes(group = region, color = region),
                         linewidth = 0.8, na.rm = TRUE) +
      # Add points at top centers of bars
      ggplot2::geom_point(ggplot2::aes(group = region, color = region),
                          size = 2, na.rm = TRUE) +
      ggplot2::scale_fill_brewer(palette = "Set3") +
      ggplot2::scale_color_brewer(palette = "Set3") +
      ggplot2::labs(
        title = paste("Monthly Attributable Rate (AR) by Region -", ref_name, "Standard"),
        subtitle = paste("Reference PM2.5:", ref_pm25, "\u00B5g/m\u00B3"),
        x = "Month",
        y = "Aggregated AR (per 100,000 population)"
      ) +
      ggplot2::guides(fill = "none", color = "none")
  }

  # Add common theme elements for AR plot
  ar_plot <- ar_plot +
    ggplot2::facet_wrap(~ region, ncol = n_cols) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 16),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "gray40", size = 11),
      plot.caption = ggplot2::element_text(hjust = 0.5, size = 9, color = "gray40"),
      axis.title = ggplot2::element_text(face = "bold"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 9),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", size = 11),
      strip.background = ggplot2::element_rect(fill = "gray90", color = "gray80")
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, ar_global_max)
    )

  # Save plots if requested
  if (save_plot) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }

    safe_refname <- tolower(gsub("\\s+", "_", ref_name))

    # Calculate appropriate dimensions
    n_rows <- grid_dims$nrow
    plot_height <- max(6, 4 * n_rows)
    plot_width <- 14

    ggplot2::ggsave(
      file.path(output_dir, paste0("an_monthly_histogram_", safe_refname, "_ref", ref_pm25, ".png")),
      plot = an_plot,
      width = plot_width,
      height = plot_height,
      dpi = 300,
      bg = "white"
    )

    ggplot2::ggsave(
      file.path(output_dir, paste0("ar_monthly_histogram_", safe_refname, "_ref", ref_pm25, ".png")),
      plot = ar_plot,
      width = plot_width,
      height = plot_height,
      dpi = 300,
      bg = "white"
    )
  }

  return(list(an_plot = an_plot, ar_plot = ar_plot))
}

#' Combined AN and AR plots by region
#'
#' @description Creates both Attributable Number (AN) and Attributable Rate (AR)
#' bar charts by region in a single function call.
#'
#' @param analysis_results Results from analyze_air_pollution_daily
#' @param max_lag Integer. Maximum lag. Defaults to 14.
#' @param include_national Logical. Whether to include national results. Default TRUE.
#' @param output_dir Character. Directory to save plot
#' @param save_plot Logical. Whether to save
#'
#' @return List with two ggplot objects: an_plot and ar_plot
#'
#' @keywords internal
plot_air_pollution_an_ar_by_region <- function(analysis_results,
                                               max_lag = 14L,
                                               include_national = TRUE,
                                               output_dir = NULL,
                                               save_plot = FALSE) {

  if (save_plot && is.null(output_dir)) {
    stop("output_dir must be specified when save_plot == TRUE")
  }

  # Aggregate data by region
  var_name <- paste0("pm25_lag0_", max_lag)
  regional_data <- aggregate_air_pollution_by_region(
    analysis_results,
    max_lag
  )

  # Filter based on include_national
  if (!include_national) {
    regional_data <- regional_data %>% dplyr::filter(region != "National")
  }

  # Get reference info
  ref_name <- unique(regional_data$ref_name)[1]
  ref_pm25 <- unique(regional_data$ref_pm25)[1]

  # Separate national from regional if needed
  national_data <- regional_data %>% filter(region == "National")
  regional_data_filtered <- regional_data %>% filter(region != "National")

  # Create AR bar plot
  ar_plot <- ggplot2::ggplot(
    regional_data_filtered,
    ggplot2::aes(x = factor(region, levels = sort(unique(region))),
                 y = ar_per_100k,
                 fill = region)
  ) +
    ggplot2::geom_bar(stat = "identity", alpha = 0.8) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = ar_lower, ymax = ar_upper),
      width = 0.2,
      color = "black"
    ) +
    ggplot2::labs(
      title = sprintf(
        "Attributable Rate (AR) by Region\n(%s Standard: %s \u00B5g/m\u00B3)",
        ref_name, ref_pm25
      ),
      x = "Region",
      y = "Attributable Rate (per 100K population)",
      caption = "Error bars show 95% confidence interval"
    ) +
    ggplot2::scale_fill_brewer(palette = "Set3") +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      axis.title = ggplot2::element_text(face = "bold"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 11),
      plot.caption = ggplot2::element_text(hjust = 0.5, color = "gray40")
    )

  # Add national bar if include_national is TRUE
  if (include_national && nrow(national_data) > 0) {
    # Combine regional and national data with different fill colors
    combined_data <- regional_data %>%
      dplyr::mutate(fill_color = ifelse(region == "National", "red", "blue"))

    ar_plot <- ggplot2::ggplot(
      combined_data,
      ggplot2::aes(x = factor(region, levels = sort(unique(region))),
                   y = ar_per_100k,
                   fill = fill_color)
    ) +
      ggplot2::geom_bar(stat = "identity", alpha = 0.8) +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = ar_lower, ymax = ar_upper),
        width = 0.2,
        color = "black"
      ) +
      ggplot2::scale_fill_manual(values = c("blue" = "#2E86AB", "red" = "#F00001")) +
      ggplot2::labs(
        title = sprintf(
          "Attributable Rate (AR) by Region\n(%s Standard: %s \u00B5g/m\u00B3)",
          ref_name, ref_pm25
        ),
        x = "Region",
        y = "Attributable Rate (per 100K population)",
        caption = "Error bars show 95% confidence interval"
      ) +
      ggplot2::theme_minimal(base_size = 14) +
      ggplot2::theme(
        legend.position = "none",
        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
        axis.title = ggplot2::element_text(face = "bold"),
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 11),
        plot.caption = ggplot2::element_text(hjust = 0.5, color = "gray40")
      )
  }

  # Create AN bar plot
  an_plot <- ggplot2::ggplot(
    regional_data_filtered,
    ggplot2::aes(x = factor(region, levels = sort(unique(region))),
                 y = an,
                 fill = region)
  ) +
    ggplot2::geom_bar(stat = "identity", alpha = 0.8) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = an_lower, ymax = an_upper),
      width = 0.2,
      color = "black"
    ) +
    ggplot2::labs(
      title = sprintf(
        "Attributable Number (AN) by Region\n(%s Standard: %s \u00B5g/m\u00B3)",
        ref_name, ref_pm25
      ),
      x = "Region",
      y = "Attributable Number of Deaths",
      caption = "Error bars show 95% confidence interval"
    ) +
    ggplot2::scale_fill_brewer(palette = "Set3") +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      axis.title = ggplot2::element_text(face = "bold"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 11),
      plot.caption = ggplot2::element_text(hjust = 0.5, color = "gray40")
    )

  # Add national bar if include_national is TRUE for AN plot
  if (include_national && nrow(national_data) > 0) {
    # Combine regional and national data with different fill colors
    combined_data <- regional_data %>%
      dplyr::mutate(fill_color = ifelse(region == "National", "red", "blue"))

    an_plot <- ggplot2::ggplot(
      combined_data,
      ggplot2::aes(x = factor(region, levels = sort(unique(region))),
                   y = an,
                   fill = fill_color)
    ) +
      ggplot2::geom_bar(stat = "identity", alpha = 0.8) +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = an_lower, ymax = an_upper),
        width = 0.2,
        color = "black"
      ) +
      ggplot2::scale_fill_manual(values = c("blue" = "#2E86AB", "red" = "#F00001")) +
      ggplot2::scale_y_continuous(labels = scales::comma) +
      ggplot2::labs(
        title = sprintf(
          "Attributable Number (AN) by Region\n(%s Standard: %s \u00B5g/m\u00B3)",
          ref_name, ref_pm25
        ),
        x = "Region",
        y = "Attributable Number of Deaths",
        caption = "Error bars show 95% confidence interval"
      ) +
      ggplot2::theme_minimal(base_size = 14) +
      ggplot2::theme(
        legend.position = "none",
        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
        axis.title = ggplot2::element_text(face = "bold"),
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 11),
        plot.caption = ggplot2::element_text(hjust = 0.5, color = "gray40")
      )
  }

  if (save_plot) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }

    safe_refname <- tolower(gsub("\\s+", "_", ref_name))

    ggplot2::ggsave(
      file.path(output_dir, paste0("air_pollution_ar_by_region_", safe_refname, "_ref", ref_pm25, ".png")),
      plot = ar_plot,
      width = 10,
      height = 7,
      dpi = 300,
      bg = "white"
    )

    ggplot2::ggsave(
      file.path(output_dir, paste0("air_pollution_an_by_region_", safe_refname, "_ref", ref_pm25, ".png")),
      plot = an_plot,
      width = 10,
      height = 7,
      dpi = 300,
      bg = "white"
    )
  }

  return(list(ar_plot = ar_plot, an_plot = an_plot))
}

#' Plot exposure-response relationship with confidence intervals by region
#'
#' @description Creates faceted exposure-response plots showing RR with confidence
#' intervals across PM2.5 concentrations for each region
#'
#' @param analysis_results Processed results with RR/AF/AN/AR with lag variables
#' @param include_national Logical. Whether to include national results. Default TRUE.
#' @param ref_pm25 Numeric. Reference PM2.5 value to highlight.
#' @param output_dir Character. Directory to save plot.
#' @param save_plot Logical. Whether to save the plot.
#'
#' @return ggplot object
#'
#' @keywords internal
plot_air_pollution_exposure_response <- function(analysis_results,
                                                 max_lag = 14L,
                                                 include_national = TRUE,
                                                 ref_pm25 = 15,
                                                 output_dir = NULL,
                                                 save_plot = FALSE) {

  if (save_plot && is.null(output_dir)) {
    stop("output_dir must be specified when save_plot == TRUE")
  }

  # Use provided ref_pm25 or extract from analysis_results
  if (is.null(ref_pm25)) {
    ref_pm25 <- na.omit(unique(analysis_results$ref_pm25))[1]
  }

  # Filter for cumulative PM2.5 variable
  var_name <- paste0("pm25_lag0_", max_lag)
  specific_results <- analysis_results %>%
    dplyr::filter(var_name == !!var_name)

  # Filter based on include_national
  if (!include_national) {
    specific_results <- specific_results %>% dplyr::filter(region != "National")
  }

  specific_results <- specific_results %>%
    dplyr::select(date, region, pm25_values, rr, rr.lb, rr.ub, ref_name, ref_pm25) %>%
    dplyr::distinct()

  if (nrow(specific_results) == 0) {
    stop("No data available for exposure-response plot by region")
  }

  # Get reference name
  ref_name <- na.omit(unique(specific_results$ref_name))[1]

  # Calculate common x-axis limits for all regions
  pm25_range <- range(specific_results$pm25_values, na.rm = TRUE)
  x_buffer <- diff(pm25_range) * 0.05
  x_limits <- c(pm25_range[1] - x_buffer, pm25_range[2] + x_buffer)

  # Calculate common y-axis limits for all regions
  # Use maximum of rr.ub and minimum of rr.lb across all regions
  rr_global_max <- max(specific_results$rr.ub, na.rm = TRUE) * 1.05
  rr_global_min <- min(specific_results$rr.lb, na.rm = TRUE) * 0.95
  # Ensure y-axis includes 1 for reference
  rr_global_min <- min(rr_global_min, 0.95)

  # Determine number of columns for facet_wrap
  n_regions <- length(unique(specific_results$region))
  grid_dims <- calculate_air_pollution_grid_dims(n_regions)
  n_cols <- grid_dims$ncol

  # Create faceted exposure-response plot with common axes
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
    ggplot2::facet_wrap(~ region, ncol = n_cols) +
    ggplot2::coord_cartesian(
      xlim = x_limits,
      ylim = c(rr_global_min, rr_global_max)
    ) +
    ggplot2::labs(
      title = paste("Exposure-Response Relationship by Region: PM2.5 vs Relative Risk -", ref_name, "Standard"),
      subtitle = paste("Reference guideline:", ref_pm25, "\u00B5g/m\u00B3"),
      x = "PM2.5 Concentration (\u00B5g/m\u00B3)",
      y = "Relative Risk (RR)",
      caption = paste("Red line: RR estimate | Blue band: 95% CI | Green line: Reference guideline")
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "gray40", size = 11),
      plot.caption = ggplot2::element_text(hjust = 0.5, size = 9, color = "gray40"),
      axis.title = ggplot2::element_text(face = "bold"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 9),
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", size = 11),
      strip.background = ggplot2::element_rect(fill = "gray90", color = "gray80")
    ) +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 6))

  # Save plot if requested
  if (save_plot) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }

    safe_refname <- tolower(gsub("\\s+", "_", ref_name))
    filename <- paste0("exposure_response_by_region_", safe_refname, "_ref", ref_pm25, ".png")

    # Calculate appropriate dimensions
    n_rows <- grid_dims$nrow
    plot_height <- max(6, 4 * n_rows)
    plot_width <- 14

    ggplot2::ggsave(
      file.path(output_dir, filename),
      plot = exp_plot,
      width = plot_width,
      height = plot_height,
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
#' @param include_national Logical. Whether to include national level calculations.
#' Defaults to TRUE.
#'
#' @returns A list containing power information by region
#'
#' @keywords internal
air_pollution_power_list <- function(
    meta_results,
    data_with_lags,
    ref_pm25 = 15,
    attr_thr = 95,
    include_national = TRUE) {

  power_list <- list()
  alpha <- 1 - attr_thr / 100

  # Extract region results from meta_results
  region_results <- meta_results$region_results

  for (i in 1:nrow(region_results)) {
    region <- region_results$region[i]
    model_results <- region_results$model_results[[i]]

    # Get region data
    region_data <- data_with_lags[data_with_lags$region == region, ]

    # Calculate threshold PM2.5 as percentile of EXCESS above reference
    excess_pm25 <- region_data$pm25 - ref_pm25
    excess_pm25 <- excess_pm25[excess_pm25 > 0]  # Only positive excesses

    if (length(excess_pm25) > 0) {
      thresh_excess <- quantile(excess_pm25, attr_thr / 100, na.rm = TRUE)
      thresh_pm25 <- ref_pm25 + thresh_excess
    } else {
      thresh_pm25 <- ref_pm25  # No excesses, use reference as threshold
    }

    # Get cumulative coefficient from meta results
    # Find the cumulative lag row
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

  # Add national power using meta-analysis results only if include_national = TRUE
  if (include_national) {
    meta_coefs <- meta_results$meta_results
    cum_row_meta <- meta_coefs[grepl("0-", meta_coefs$lag), ]

    if (nrow(cum_row_meta) > 0) {

      # Aggregate national PM2.5
      national_pm25 <- unlist(lapply(unique(data_with_lags$region),
                                     function(r) data_with_lags$pm25[data_with_lags$region == r]))
      thresh_national <- round(quantile(national_pm25, attr_thr / 100, na.rm = TRUE), 1)
      # Create power dataframe for PM2.5 values above threshold
      national_pm25_above <- unique(national_pm25[national_pm25 >= thresh_national])
      national_pm25_above <- sort(national_pm25_above)

      if (length(national_pm25_above) > 0) {
        power_df_national <- data.frame(
          region = "National",
          pm25 = national_pm25_above,
          cen = ref_pm25,
          log_rr = cum_row$coef * (national_pm25_above - ref_pm25),
          se = cum_row$se * abs(national_pm25_above - ref_pm25),
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
#' @param include_national Logical. Whether to include national level in the plot.
#' Defaults to TRUE.
#'
#' @returns Invisible list of plot information
#'
#' @keywords internal
plot_air_pollution_power <- function(
    power_list,
    output_dir = NULL,
    save_plot = FALSE,
    ref_name = "WHO",
    include_national = TRUE) {

  if (save_plot && is.null(output_dir)) {
    stop("Output directory must be specified if save_plot==TRUE")
  }

  # Filter out National if include_national = FALSE
  if (!include_national) {
    power_list <- power_list[names(power_list) != "National"]
  }

  # Check if power_list is not empty after filtering
  if (length(power_list) == 0) {
    stop("No data available in power_list after filtering")
  }

  # Combine all power data
  all_power <- do.call(rbind, power_list)

  # Get regions
  regions <- unique(all_power$region)
  n_regions <- length(regions)

  # Determine number of columns for facet_wrap
  grid_dims <- calculate_air_pollution_grid_dims(n_regions)
  n_cols <- grid_dims$ncol

  # Create facetted power plot
  power_plot <- ggplot2::ggplot(all_power, ggplot2::aes(x = pm25, y = power_pct)) +
    ggplot2::geom_line(color = "black", linewidth = 1.5) +
    ggplot2::geom_hline(yintercept = 80, linetype = "dashed", color = "#2ECC71", linewidth = 0.3) +
    ggplot2::geom_hline(yintercept = 50, linetype = "dashed", color = "#F39C12", linewidth = 0.3, alpha = 0.7) +
    ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = 80, ymax = 100,
                      alpha = 0.1, fill = "#2ECC71") +
    ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = 50, ymax = 80,
                      alpha = 0.1, fill = "#F39C12") +
    ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0, ymax = 50,
                      alpha = 0.1, fill = "#E74C3C") +
    ggplot2::facet_wrap(~ region, ncol = n_cols) +
    ggplot2::scale_y_continuous(
      limits = c(0, 100),
      breaks = seq(0, 100, 20)
    ) +
    ggplot2::labs(
      title = paste("Power vs PM2.5 Concentration by Region -", ref_name, "Standard"),
      subtitle = paste(
        "<span style='color:#2ECC71;'><b>Green zone:</b> \u226580% (Adequate statistical power range to detect effect)</span><br>",
        "<span style='color:#F39C12;'><b>Yellow zone:</b> 50-79% (Moderate power - effect may be detectable but with less certainty)</span><br>",
        "<span style='color:#E74C3C;'><b>Red zone:</b> <50% (Low power - study may be underpowered to detect the effect)</span>",
        sep = ""
      ),
      x = "PM2.5 Concentration (\u00B5g/m\u00B3)",
      y = "Statistical Power (%)"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 16),
      plot.subtitle = ggtext::element_markdown(hjust = 0.5, size = 11, color = "gray40", lineheight = 1.3),
      axis.title = ggplot2::element_text(face = "bold"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 9),
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", size = 11),
      strip.background = ggplot2::element_rect(fill = "gray90", color = "gray80")
    )

  # Save if requested
  if (save_plot) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }

    safe_refname <- tolower(gsub("\\s+", "_", ref_name))
    national_suffix <- ifelse(include_national, "with_national", "regional_only")
    output_file <- file.path(output_dir,
                             paste0("power_vs_pm25_", safe_refname, "_", national_suffix, ".png"))

    # Calculate appropriate dimensions
    n_rows <- grid_dims$nrow
    plot_height <- max(6, 4 * n_rows) + 1  # Extra space for subtitle
    plot_width <- 14

    ggplot2::ggsave(
      filename = output_file,
      plot = power_plot,
      width = plot_width,
      height = plot_height,
      dpi = 300,
      bg = "white"
    )
  }

  return(invisible(list(
    power_plot = power_plot,
    power_data = all_power,
    include_national = include_national
  )))
}

#' Comprehensive Air Pollution Analysis Pipeline
#'
#' @description Master function that runs the complete air pollution analysis
#' including data loading, preprocessing (including lags), modeling, plotting,
#' attribution calculations vs reference standards, power analysis and
#' descriptive statistics
#'
#' @param data_path Character. Path to CSV data file
#' @param date_col Character. Name of date column
#' @param region_col Character. Name of region column
#' @param pm25_col Character. Name of PM2.5 column
#' @param deaths_col Character. Name of deaths column
#' @param population_col Character. Name of the population column.
#' @param humidity_col Character. Name of humidity column
#' @param precipitation_col Character. Name of precipitation column
#' @param tmax_col Character. Name of temperature column
#' @param wind_speed_col Character. Name of wind speed column
#' @param categorical_others Optional character vector. Names of additional
#' categorical variables.
#' @param continuous_others Optional character vector. Names of additional
#' continuous variables (e.g., "tmean")
#' @param max_lag Integer. Maximum lag days. Defaults to 14.
#' @param df_seasonal Integer. Degrees of freedom for seasonal spline. Default 6.
#' @param family Character. GAM family (default: "quasipoisson")
#' @param reference_standards List of reference standards, each with 'value' and 'name'
#' @param output_dir Directory to save outputs
#' @param save_outputs Logical. Whether to save outputs
#' @param run_descriptive Logical. Whether to run descriptive statistics
#' @param run_power Logical. Whether to run power analysis
#' @param moving_average_window Integer. Window for moving average in descriptive stats
#' @param include_national Logical. Whether to include national results in plots. Default TRUE.
#' @param years_filter Optional numeric vector of years to include (e.g., c(2020, 2021, 2022)).
#'  It is recommended to filter for at least 3 consecutive years for a minimum considerable time series
#' @param regions_filter Optional character vector of regions to include
#' @param attr_thr Numeric (0-100). Percentile threshold used in power
#' analysis to evaluate attribution detectability. Default 95.
#'
#' @param plot_corr_matrix Logical. Plot correlation matrix. Default TRUE.
#' @param correlation_method Character. Correlation method for corr matrix
#' (e.g.,"pearson", "spearman"). Default "pearson".
#' @param plot_dist Logical. Plot distributions (hist/density) for key variables.
#' Default TRUE.
#' @param plot_na_counts Logical. Plot missingness/NA counts. Default TRUE.
#' @param plot_scatter Logical. Plot scatter plots for key pairs. Default TRUE.
#' @param plot_box Logical. Plot boxplots by region/season where applicable.
#' Default TRUE.
#' @param plot_seasonal Logical. Plot seasonal summaries. Default TRUE.
#' @param plot_regional Logical. Plot regional summaries. Default TRUE.
#' @param plot_total Logical. Plot overall totals where relevant. Default TRUE.
#' @param detect_outliers Logical. Flag potential outliers in descriptive workflow.
#' Default TRUE.
#' @param calculate_rate Logical. Whether to calculate rate variables during
#' descriptive stats (e.g., deaths per population). Default FALSE
#'
#' @examples
#' \dontrun{
#' results <- do_air_pollution_analysis(
#'  data_path = "data.csv",
#'   date_col = "date",
#'   region_col = "province",
#'   pm25_col = "pm25",
#'   deaths_col = "deaths",
#'   population_col = "population",
#'   humidity_col = "humidity",
#'   precipitation_col = "precipitation",
#'   tmax_col = "tmax",
#'   wind_speed_col = "wind_speed",
#'   continuous_others = NULL,
#'   max_lag = 14L,
#'   df_seasonal = 6,
#'   family = "quasipoisson",
#'   reference_standards = list(list(value = 15, name = "WHO")),
#'   years_filter = NULL,
#'   regions_filter = NULL,
#'   include_national = TRUE,
#'   output_dir = "air_pollution_results2",
#'   save_outputs = TRUE,
#'   run_descriptive = TRUE,
#'   run_power = TRUE,
#'   moving_average_window = 7L,
#'   attr_thr = 95,
#'   plot_corr_matrix = TRUE,
#'   correlation_method = "pearson",
#'   plot_dist = TRUE,
#'   plot_na_counts = TRUE,
#'   plot_scatter = TRUE,
#'   plot_box = TRUE,
#'   plot_seasonal = TRUE,
#'   plot_regional = TRUE,
#'   plot_total = TRUE,
#'   detect_outliers = TRUE,
#'   calculate_rate = FALSE
#' )
#' }
#'
#' @return List containing:
#' \describe{
#'   \item{data}{Processed data with lag variables}
#'   \item{meta_analysis}{Meta-analysis results with AF/AN calculations}
#'   \item{lag_analysis}{Lag-specific analysis results}
#'   \item{distributed_lag_analysis}{Distributed lag model results (if requested)}
#'   \item{plots}{List of generated plots (forest, lags, distributed lags)}
#'   \item{power_list}{A list containing power information by area}
#'   \item{exposure_response_plots}{Exposure-response plots for each reference
#'   standard (if requested)}
#'   \item{reference_specific_af_an}{AF/AN calculations specific to each
#'   reference standard (if requested)}
#'   \item{descriptive_stats}{Summary statistics of key variables}
#'
#' }
#'
#' @export
air_pollution_do_analysis <- function(
    # Data specification
  data_path,
  date_col = "date",
  region_col = "region",
  pm25_col = "pm25",
  deaths_col = "deaths",
  population_col = "population",
  humidity_col = "humidity",
  precipitation_col = "precipitation",
  tmax_col = "tmax",
  wind_speed_col = "wind_speed",
  categorical_others = NULL,
  continuous_others = NULL,

  # Analysis parameters
  max_lag = 14L,
  df_seasonal = 6,
  family = "quasipoisson",

  # Reference standards to analyze
  reference_standards = list(
    list(value = 15, name = "WHO")
  ),

  # Output settings
  output_dir = "air_pollution_results",
  save_outputs = TRUE,

  # Optional analyses
  run_descriptive = TRUE,
  run_power = TRUE,
  moving_average_window = 3L,

  # Filter parameters
  include_national = TRUE,
  years_filter = NULL,
  regions_filter = NULL,
  attr_thr = 95,

  # Descriptive statistics settings
  plot_corr_matrix = TRUE,
  correlation_method = "pearson",
  plot_dist = TRUE,
  plot_na_counts = TRUE,
  plot_scatter = TRUE,
  plot_box = TRUE,
  plot_seasonal = TRUE,
  plot_regional = TRUE,
  plot_total = TRUE,
  detect_outliers = TRUE,
  calculate_rate = FALSE
) {

  # AUTO-SET ENGLISH LOCALE FOR ENTIRE ANALYSIS
  original_locale <- Sys.getlocale("LC_TIME")
  english_locales <- c("English", "en_US.UTF-8", "en_GB.UTF-8", "C")

  locale_set <- FALSE
  for (loc in english_locales) {
    try_locale <- tryCatch({
      Sys.setlocale("LC_TIME", loc)
      TRUE
    }, error = function(e) FALSE, warning = function(w) FALSE)

    if (try_locale) {
      locale_set <- TRUE
      break
    }
  }

  if (!locale_set) {
    warning("Could not set English locale. Date month/day names may vary by system language.")
  }

  # Restore original locale on exit
  on.exit({
    if (locale_set) {
      Sys.setlocale("LC_TIME", original_locale)
    }
  })

  # Create output directory
  if (save_outputs && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  results <- list()

  # LOAD AND PREPARE DATA
  data <- load_air_pollution_data(
    data_path = data_path,
    date_col = date_col,
    region_col = region_col,
    pm25_col = pm25_col,
    deaths_col = deaths_col,
    population_col = population_col,
    humidity_col = humidity_col,
    precipitation_col = precipitation_col,
    tmax_col = tmax_col,
    wind_speed_col = wind_speed_col,
    categorical_others = categorical_others,
    continuous_others = continuous_others
  )

  # Apply filters if specified
  if (!is.null(years_filter)) {
    data <- data %>% dplyr::filter(year %in% years_filter)
  }

  if (!is.null(regions_filter)) {
    data <- data %>% dplyr::filter(region %in% regions_filter)
  }

  results$data_raw <- data

  # CREATE LAGS
  data_with_lags <- create_air_pollution_lags(data, max_lag = max_lag)

  results$data_with_lags <- data_with_lags

  # DESCRIPTIVE STATISTICS
  if (run_descriptive) {
    env_labels <- c(
      "pm25" = "PM2.5 (\u00B5g/m\u00B3)",
      "tmax" = "Max Temperature (\u00B0C)",
      "precipitation" = "Precipitation (mm)",
      "humidity" = "Humidity (%)",
      "wind_speed" = "Wind Speed (m/s)"
    )

    if ("tmean" %in% names(data)) {
      env_labels <- c(env_labels, "tmean" = "Mean Temperature (\u00B0C)")
    }

    air_pollution_descriptive_stats(
      data = data,
      env_labels = env_labels,
      save_outputs = save_outputs,
      output_dir = output_dir,
      moving_average_window = moving_average_window,
      plot_corr_matrix = plot_corr_matrix,
      correlation_method = correlation_method,
      plot_dist = plot_dist,
      plot_na_counts = plot_na_counts,
      plot_scatter = plot_scatter,
      plot_box = plot_box,
      plot_seasonal = plot_seasonal,
      plot_regional = plot_regional,
      plot_total = plot_total,
      detect_outliers = detect_outliers,
      calculate_rate = calculate_rate
    )
  }

  # FIT MODELS AND META-ANALYSIS
  meta_results <- air_pollution_meta_analysis(
    data_with_lags = data_with_lags,
    max_lag = max_lag,
    df_seasonal = df_seasonal,
    family = family
  )

  results$meta_results <- meta_results

  # CALCULATE ATTRIBUTABLE BURDEN FOR EACH REFERENCE
  results$analysis_results <- list()

  for (ref_standard in reference_standards) {
    ref_pm25 <- ref_standard$value
    ref_name <- ref_standard$name

    analysis_daily <- analyze_air_pollution_daily(
      data_with_lags = data_with_lags,
      meta_results = meta_results,
      ref_pm25 = ref_pm25,
      ref_name = ref_name,
      max_lag = max_lag
    )

    results$analysis_results[[ref_name]] <- analysis_daily

    results$plots <- list()
    analysis_res <- results$analysis_results[[ref_name]]

    # PLOTS
    results$plots[[ref_name]]$an_ar_monthly <- plot_air_pollution_an_ar_monthly(
      analysis_results = analysis_res,
      max_lag = max_lag,
      include_national = include_national,
      output_dir = output_dir,
      save_plot = save_outputs
    )

    results$plots[[ref_name]]$an_ar_by_year <- plot_air_pollution_an_ar_by_year(
      analysis_results = analysis_res,
      max_lag = max_lag,
      include_national = include_national,
      output_dir = output_dir,
      save_plot = save_outputs
    )

    results$plots[[ref_name]]$forest_by_region <- plot_air_pollution_forest_by_region(
      analysis_results = analysis_res,
      max_lag = max_lag,
      include_national = include_national,
      output_dir = output_dir,
      save_plot = save_outputs
    )

    results$plots[[ref_name]]$forest_by_lag <- plot_air_pollution_forest_by_lag(
      analysis_results = analysis_res,
      max_lag = max_lag,
      output_dir = output_dir,
      save_plot = save_outputs
    )

    results$plots[[ref_name]]$monthly_histograms <- plot_air_pollution_monthly_histograms(
      analysis_results = analysis_res,
      max_lag = max_lag,
      include_national = include_national,
      output_dir = output_dir,
      save_plot = save_outputs
    )

    results$plots[[ref_name]]$exposure_response <- plot_air_pollution_exposure_response(
      analysis_results = analysis_res,
      max_lag = max_lag,
      include_national = include_national,
      ref_pm25 = ref_pm25,
      output_dir = output_dir,
      save_plot = save_outputs
    )

    results$plots[[ref_name]]$an_ar_by_region <- plot_air_pollution_an_ar_by_region(
      analysis_results = analysis_res,
      max_lag = max_lag,
      include_national = include_national,
      output_dir = output_dir,
      save_plot = save_outputs
    )

    results$plots[[ref_name]]$aggregate_by_region <- aggregate_air_pollution_by_region(
      analysis_results = analysis_res,
      max_lag = max_lag)

    results$plots[[ref_name]]$aggregate_by_year <- aggregate_air_pollution_by_year(
      analysis_results = analysis_res,
      max_lag = max_lag,
      include_national = include_national)

    results$plots[[ref_name]]$aggregate_by_month <- aggregate_air_pollution_by_month(
      analysis_results = analysis_res,
      max_lag = max_lag,
      include_national = include_national)

    # POWER ANALYSIS
    if (run_power) {
      results$power_results <- list()
      power_list <- air_pollution_power_list(
        meta_results = meta_results,
        data_with_lags = data_with_lags,
        ref_pm25 = ref_pm25,
        attr_thr = attr_thr,
        include_national = include_national
      )

      results$power_results[[ref_name]] <- power_list

      power_plot <- plot_air_pollution_power(
        power_list = power_list,
        output_dir = output_dir,
        save_plot = save_outputs,
        ref_name = ref_name,
        include_national = include_national
      )

      results$plots[[ref_name]]$power_plot <- power_plot
    }
  }

  if (save_outputs) {
    message("\n  All outputs saved to: ", output_dir)
  }

  class(results) <- c("air_pollution_analysis", "list")

  return(invisible(results))
}
