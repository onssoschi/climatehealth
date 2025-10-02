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
#' to "province".
#' @param pm25_col Character. Name of PM2.5 column in the dataframe. Defaults to
#' "pm25".
#' @param deaths_col Character. Name of all-cause mortality column in the dataframe.
#' Defaults to "deaths"
#' @param humidity_col Character. Name of humidity column in the dataframe. Defaults
#' to "humidity".
#' @param precipitation_col Character. Name of precipitation column in the dataframe.
#' Defaults to "precipitation".
#' @param tmax_col Character. Name of temperature column in the dataframe.
#' Defaults to "tmax".
#' @param population_col Character. Name of population column in the dataframe.
#' Defaults to NULL.
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
                                    region_col = "province",
                                    pm25_col = "pm25",
                                    deaths_col = "deaths",
                                    humidity_col = "humidity",
                                    precipitation_col = "precipitation",
                                    tmax_col = "tmax",
                                    population_col = NULL,
                                    age_col = NULL,
                                    sex_col = NULL,
                                    urbanisation_col = NULL) {

  data <- if(is.character(data_path)) read.csv(data_path) else data_path

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

  if(!is.null(population_col) && population_col %in% names(data)) {
    data <- data %>% rename(population = !!rlang::sym(population_col))
  } else if(!("population" %in% names(data))) {
    data <- data %>% dplyr::mutate(population = 1)
  }

  if(!is.null(age_col) && age_col %in% names(data)) {
    data <- data %>% rename(age = !!rlang::sym(age_col))
  } else if(!("age" %in% names(data))) {
    data <- data %>% dplyr::mutate(age = "all")
  }

  if(!is.null(sex_col) && sex_col %in% names(data)) {
    data <- data %>% rename(sex = !!rlang::sym(sex_col))
  } else if(!("sex" %in% names(data))) {
    data <- data %>% dplyr::mutate(sex = "both")
  }

  if(!is.null(urbanisation_col) && urbanisation_col %in% names(data)) {
    data <- data %>% rename(urbanisation = !!rlang::sym(urbanisation_col))
  } else if(!("urbanisation" %in% names(data))) {
    data <- data %>% dplyr::mutate(urbanisation = "mixed")
  }

  date_function <- lubridate::ymd
  if (grepl("^\\d{2}/\\d{2}/\\d{4}$", data[["date"]][1])) {
    date_function <- lubridate::dmy
  }

  data <- data %>%
    dplyr::mutate(
      date = date_function(date),
      year = lubridate::year(date),
      month = lubridate::month(date),
      day = lubridate::day(date),
      dow = as.character(lubridate::wday(date, label = TRUE)),
      time = dplyr::row_number()
    ) %>%
    dplyr::arrange(date)

  essential_cols <- c("date", "region", "pm25", "deaths", "humidity",
                      "precipitation", "tmax", "population", "age", "sex",
                      "urbanisation", "year", "month", "day", "dow", "time")

  data <- data %>% dplyr::select(all_of(essential_cols))

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
#' @import dplyr
#'
#' @export
create_air_pollution_lags <- function(
  data,
  max_lag = 2
) {

  data_with_lags <- data %>%
    dplyr::group_by(region) %>%
    dplyr::arrange(date)

  for (i in 1:max_lag) {
    lag_name <- paste0("pm25_lag", as.character(i))
    data_with_lags <- data_with_lags %>%
      dplyr::mutate(!!lag_name := lag(pm25, i))
  }

  lag_vars <- paste0("pm25_lag", 1:max_lag)
  all_lag_vars <- c("pm25", lag_vars)
  avg_lag_name <- paste0("pm25_lag0_", max_lag)
  data_with_lags <- data_with_lags %>%
    dplyr::mutate(!!avg_lag_name := rowMeans(across(all_of(all_lag_vars)), na.rm = FALSE))

  data_with_lags <- data_with_lags %>%
    filter(dplyr::if_all(all_of(all_lag_vars), ~!is.na(.))) %>%
    dplyr::ungroup()

  return(data_with_lags)
}


#' Show descriptive statistics
#'
#' @description Generates summary statistics for climate, environmental and health data.
#'
#' @param data Dataframe containing a daily time series of climate, environmental
#' and health data
#' @param variables Character or character vector with variable(s) to produce
#' summary statistics for. Must include at least 1 variable.
#' @param bin_width Integer. Width of each bin in a histogram of the outcome
#' variable. Defaults to 1.
#' @param output_dir Character. Directory to save descriptive statistics.
#' Defaults to NULL.
#' @param save_outputs Logical. Whether to save outputs. Defaults to FALSE.
#'
#' @export
air_pollution_descriptive_stats <- function(data,
                                            variables,
                                            bin_width = 1,
                                            output_dir = NULL,
                                            save_outputs = FALSE) {
  # Check input params
  if (save_outputs && is.null(output_dir)) {
    stop("An output directory must be passed if save_outputs==T.")
  }

  # Create output dir
  if (save_outputs) {
    output_dir <- file.path(output_dir, "air_pollution_descriptive_stats")
    if (!file.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }
  }

  # Create PNG device if save requested
  if (save_outputs) {
    png(file.path(output_dir, "mortality_histogram.png"), width = 800, height = 600)
  }

  # Display histogram (or draw on PDF)
  graphics::hist(data$deaths,
                 breaks = seq(0, max(data$deaths) + bin_width,
                              by = bin_width),
                 main = "All cause mortality",
                 xlab = "Mortality", col = "blue")

  # Save PNG device if save requested
  if (save_outputs) dev.off()

  # Store summary statistics
  summary_list <- list()

  for (i in seq_along(variables)) {
    variable_name <- variables[i]
    summary_stats <- summary(data[[variable_name]])
    summary_list[[variable_name]] <- summary_stats
  }

  # Save summary statistics if requested
  if (save_outputs) {
    capture.output({
      for (var in names(summary_list)) {
        cat("\n", var, ":\n", sep = "")
        print(summary_list[[var]])
      }
    }, file = file.path(output_dir, "descriptive_statistics.txt"))
  }

  # Return desc stats summary (no hist)
  return(summary_list)
}


#' Create a scatterplot
#'
#' @description Produces a ggplot2 scatterplot of two variables x versus y.
#'
#' @param data Dataframe containing a daily time series of climate, environmental
#' and health data
#' @param xvar Character. x variable
#' @param yvar Character. y variable
#' @param output_dir Character. Directory to save plot. Defaults NULL.
#' @param save_plot Logical. Whether to save the plot. Defaults FALSE.
#'
#' @export
plot_air_pollution_variables <- function(data,
                                         xvar,
                                         yvar,
                                         output_dir = NULL,
                                         save_plot = FALSE) {
  # Check input params
  if (save_plot && is.null(output_dir)) {
    stop("An output directory must be passed if save_outputs==T.")
  }

  # Plot Scatter
  p <- ggplot2::ggplot(data = data, ggplot2::aes(x = .data[[xvar]], y = .data[[yvar]])) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth() +
    ggplot2::theme_bw()

  print(p)

  if (save_plot) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    fname <- paste0(xvar, "_vs_", yvar, ".png")
    output_path <- file.path(output_dir, fname)
    ggplot2::ggsave(output_path, p, width = 8, height = 6, dpi = 150)
  }
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
#' @export
fit_air_pollution_gam <- function(data,
                                  var_name = "pm25",
                                  family = "quasipoisson") {
  # Data Validation
  if(nrow(data) < 500) {
    warning(
      "Insufficient data: only ",
      nrow(data),
      " observations of a recommended 500 minimum."
    )
    return(NULL)
  }

  # Parameter Validation
  if (!(var_name %in% names(data))) stop(paste0(var_name, " not in dataset."))

  # Create Model Formula
  GAM_formula <- as.formula(
    paste(
      "deaths ~ ", var_name,
      "+ ns(time, df = 7) + ns(tmax, df = 3) + ns(humidity, df = 3)",
      "+ ns(precipitation, df = 3) + dow + offset(log(population))"
    )
  )

  # Fit Model
  tryCatch({
    mgcv::gam(GAM_formula, data = data, family = family)
  }, error = function(e) {
    warning("Model fitting failed: ", e$message)
    return(NULL)
  })
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
#' @export
extract_air_pollution_coef <- function(model,
                                       var_name = "pm25") {

  if(is.null(model)) {
    return(list(coef = NA, se = NA))
  }

  tryCatch({
    res <- summary(model)
    coef_val <- res$p.coeff[var_name]
    se_val <- res$se[var_name]

    return(list(
      coef = as.numeric(coef_val),
      se = as.numeric(se_val)
    ))

  }, error = function(e) {
    warning("Failed to extract coefficient for '", var_name, "': ", e$message)
    return(list(coef = NA, se = NA))
  })
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
#' @export
air_pollution_meta_analysis <- function(data,
                                        var_name = "pm25",
                                        family = "quasipoisson") {
  # Stage 0: Fit Regional Models
  region_results <- data %>%
    dplyr::group_by(.data$region) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      n_obs = tidytable::map_dbl(.data$data, nrow),
      total_deaths = tidytable::map_dbl(.data$data, ~sum(.x$deaths, na.rm = TRUE)),
      model = purrr::map2(.data$data, .data$region, ~{
        fit_air_pollution_gam(.x, var_name, family)
      })
    ) %>%
    dplyr::ungroup()

  # Stage 1: Extract Coefficients and Calculate AN/AF
  region_results <- region_results %>%
    dplyr::mutate(
      coef_results = purrr::map(.data$model, ~extract_air_pollution_coef(.x, var_name)),
      coef_pm25 = tidytable::map_dbl(.data$coef_results, ~.x$coef),
      se_pm25 = tidytable::map_dbl(.data$coef_results, ~.x$se),

      rr_10ug = exp(.data$coef_pm25 * 10),
      ci_lower = exp((.data$coef_pm25 - 1.96 * .data$se_pm25) * 10),
      ci_upper = exp((.data$coef_pm25 + 1.96 * .data$se_pm25) * 10),

      af_10ug = (.data$rr_10ug - 1) / .data$rr_10ug,
      an_10ug = .data$total_deaths * .data$af_10ug
    ) %>%
    dplyr::filter(!is.na(.data$coef_pm25) & !is.na(.data$se_pm25)) %>%
    dplyr::select(-.data$coef_results)

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
#' @param data Input data
#' @param max_lag Integer. Maximum lag days. Defaults to 2
#' @param family Character string indicating the distribution family used in the GAM.
#' Must be one of the four options "quasipoisson", "nb", "ziP", and "poisson".
#' "quasipoisson" (Appropriate for count data with overdispersion),
#' "nb" (negativebinomial which is suitable for highly dispersed count data),
#' "ziP" (zero-inflated which is suitable for count data with excess zeros (sparse data)),
#' "poisson" (Suitable for count data without overdispersion (mean = variance)).
#' Defaults to "quasipoisson"
#'
#' @return Dataframe with lag-specific results including AF and AN.
#'
#' @export
analyze_air_pollution_lags <- function(data,
                                       max_lag = 2,
                                       family = "quasipoisson") {

  lag_vars <- c("pm25", paste0("pm25_lag", 1:max_lag), paste0("pm25_lag0_", max_lag))
  lag_labels <- c("Lag 0", paste0("Lag ", 1:max_lag), paste0("Lag 0-", max_lag))

  tidytable::map2_dfr(lag_vars, lag_labels, ~{
    result <- tryCatch({
      air_pollution_meta_analysis(data, .x, family)
    }, error = function(e) {
      warning("Failed to analyze ", .y, ": ", e$message)
      return(NULL)
    })

    if(is.null(result)) {
      return(data.frame(
        lag = .y, rr = NA, ci_lower = NA, ci_upper = NA,
        af = NA, an = NA, tau2 = NA, I2 = NA
      ))
    }

    data.frame(
      lag = .y,
      rr = result$overall_rr,
      ci_lower = result$overall_ci[1],
      ci_upper = result$overall_ci[2],
      af = result$overall_af,
      an = result$overall_an,
      tau2 = result$heterogeneity$tau2,
      I2 = result$heterogeneity$I2,
      p_value = result$meta_result$pval
    )
  })
}


#' Function to plot and save air pollution plots.
#'
#' @description Function for saving all plot types with consistent parameters.
#'
#' @param plot_object Character. ggplot object or grid.dplyr::arrange object to save.
#' @param output_dir Character. Directory to save plots.
#' @param filename Character. Filename without extension.
#' @param width_per_panel Integer. Width per panel (for multi-panel plots).
#' Defaults to 4.
#' @param height_per_panel Integer. Height per panel (for multi-panel plots).
#' Defaults to 4.
#' @param grid_dims Grid dimensions (list with nrow, ncol) - optional
#' for single plots. Defaults to NULL.
#' @param dpi Integer. Resolution for output. Defaults to 150.
#' @param single_plot_width Width for single plots (when grid_dims is NULL).
#' Defaults to 10.
#' @param single_plot_height Height for single plots (when grid_dims is NULL).
#' Defaults to 8.
#'
#' @export
save_air_pollution_plot <- function(plot_object,
                                    output_dir,
                                    filename,
                                    width_per_panel = 4,
                                    height_per_panel = 4,
                                    grid_dims = NULL,
                                    dpi = 150,
                                    single_plot_width = 10,
                                    single_plot_height = 8) {
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
  }
  else {
    output_path <- file.path(output_dir, filename)
  }
  ggplot2::ggsave(output_path, plot_object,
         width = fig_width, height = fig_height, dpi = dpi)
}


#' Create forest plot with AF and AN
#'
#' @param meta_results Meta-analysis results with AF/AN.
#' @param title Character. Plot title. Defaults to "PM2.5 Effects by Region"
#' @param output_dir Character. Directory to save plot. Defaults to NULL.
#' @param save_plot Logical. Whether to save the plot. Defaults to FALSE.
#'
#' @return ggplot object
#'
#' @export
plot_air_pollution_forest <- function(meta_results,
                                      title = "PM2.5 Effects by Region",
                                      output_dir = NULL,
                                      save_plot = FALSE) {
  if (is.null(output_dir) && save_plot) {
    stop("Output directory must be specified if save_plot == TRUE.")
  }

  region_data <- meta_results$region_results %>%
    dplyr::select(.data$region, .data$rr_10ug, .data$ci_lower, .data$ci_upper,
                  .data$af_10ug, .data$an_10ug) %>%
    dplyr::mutate(
      type = "Region",
      label = sprintf("RR: %.3f", .data$rr_10ug)
    )

  overall_data <- data.frame(
    region = "Overall",
    rr_10ug = meta_results$overall_rr,
    ci_lower = meta_results$overall_ci[1],
    ci_upper = meta_results$overall_ci[2],
    af_10ug = meta_results$overall_af,
    an_10ug = meta_results$overall_an,
    type = "Overall",
    label = sprintf("RR: %.3f", meta_results$overall_rr)
  )

  plot_data <- dplyr::bind_rows(region_data, overall_data) %>%
    dplyr::mutate(
      region = factor(.data$region, levels = rev(c(region_data$region, "Overall"))),
      is_overall = .data$type == "Overall"
    )

  forest_plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$rr_10ug, y = .data$region)) +
    ggplot2::geom_errorbarh(
      ggplot2::aes(xmin = .data$ci_lower, xmax = .data$ci_upper,
                   color = .data$is_overall, linewidth = .data$is_overall),
      height = 0.3
    ) +
    ggplot2::geom_point(ggplot2::aes(color = .data$is_overall, size = .data$is_overall)) +
    ggplot2::geom_text(ggplot2::aes(label = .data$label, x = .data$ci_upper),
                       hjust = -0.1, size = 3) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
    ggplot2::scale_color_manual(values = c("FALSE" = "blue", "TRUE" = "red")) +
    ggplot2::scale_size_manual(values = c("FALSE" = 2, "TRUE" = 3)) +
    ggplot2::scale_linewidth_manual(values = c("FALSE" = 0.8, "TRUE" = 1.2)) +
    ggplot2::xlim(c(min(plot_data$ci_lower) * 0.95, max(plot_data$ci_upper) * 1.05)) +
    ggplot2::labs(
      x = "Health Relative Risk", y = "Region",
      title = title,
      subtitle = sprintf("I\u00b2 = %.1f%%; \u03c4\u00b2 = %.4f",
                         meta_results$heterogeneity$I2,
                         meta_results$heterogeneity$tau2)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(hjust = 0.5)
    )

  if (save_plot) {
    save_air_pollution_plot(
      plot_object = forest_plot,
      output_dir = output_dir,
      filename = "forest_plot",
      single_plot_width = 10,
      single_plot_height = 8
    )
  }

  return(forest_plot)
}


#' Plot lag comparison with AF and AN
#'
#' @param lag_results Lag analysis results with AF/AN.
#' @param max_lag Integer. Maximum lag days. Defaults to 2.
#' @param output_dir Character. Directory to save plot. Defaults to NULL.
#' @param save_plot Logical. Whether to save the plot. Defaults to FALSE.
#'
#' @return ggplot object
#'
#' @export
plot_air_pollution_lags <- function(lag_results,
                                    max_lag = 2,
                                    output_dir = NULL,
                                    save_plot = FALSE) {
  # Param Validation
  if (is.null(output_dir) && save_plot == TRUE) {
    stop("Output directory must be specified if save_plot==T.")
  }

  lag_results_clean <- lag_results %>% filter(!is.na(.data$rr))

  if(nrow(lag_results_clean) == 0) {
    stop("No successful lag analyses to plot")
  }

  lag_labels <- c("Lag 0", paste0("Lag ", 1:max_lag), paste0("Lag 0-", max_lag))
  lag_results_clean$lag <- factor(lag_results_clean$lag,
                                  levels = lag_labels)

  lag_results_clean <- lag_results_clean %>%
    dplyr::mutate(label = "")

  lag_plot <- ggplot2::ggplot(lag_results_clean, ggplot2::aes(x = lag, y = rr)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$ci_lower, ymax = .data$ci_upper),
                  width = 0.2, color = "darkblue", linewidth = 0.8) +
    ggplot2::geom_point(size = 3, color = "blue") +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
    ggplot2::labs(
      x = "Lag Structure", y = "Health Relative Risk",
      title = "RR by Lag Structure"
    ) +
    ggplot2::ylim(c(0.98,
           max(lag_results_clean$ci_upper) * 1.05)) +
    ggplot2::theme_minimal() + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  # Use unified save_plot function
  if (save_plot) {
    save_air_pollution_plot(
      plot_object = lag_plot,
      output_dir = output_dir,
      filename = "lag_results",
      single_plot_width = 8,
      single_plot_height = 6
    )
  }

  return(lag_plot)
}


#' Calculate Optimal Grid Dimensions for Multiple Plot Display
#'
#' @description
#' Determines the optimal grid layout (rows x columns) for displaying multiple plots
#' in a single figure. The function prioritizes aesthetically pleasing arrangements
#' that are wider than tall, minimizing empty cells while maintaining visual balance.
#'
#' @param n_plots Integer. Number of plots to display. Must be positive.
#'
#' @return
#' A named list with two components:
#' - nrow: Number of rows in the grid
#' - ncol: Number of columns in the grid
#'
#' @note
#' For plot counts exceeding 400, the function will return a 20x20 grid.
#' Consider whether such large grids are appropriate for your visualization needs.
#'
#' @export
calculate_air_pollution_grid_dims <- function(n_plots) {
  if (!is.numeric(n_plots) || length(n_plots) != 1) {
    stop("n_plots must be a single numeric value")
  }

  if (n_plots <= 0) {
    warning("n_plots should be positive. Returning 1x1 grid.")
    return(list(nrow = 1, ncol = 1))
  }

  if (n_plots == 1) {
    return(list(nrow = 1, ncol = 1))
  }

  optimal_grids <- list(
    c(2, 2), c(2, 3), c(3, 3), c(3, 4), c(4, 4), c(4, 5), c(5, 5), c(5, 6),
    c(6, 6), c(6, 7), c(7, 7), c(7, 8), c(8, 8), c(8,9),  c(9,9),  c(9,10),
    c(10,10), c(10,11), c(11,12), c(12,12), c(12,13), c(13,14), c(14,15),
    c(15,15), c(15,16), c(16,16), c(16,17), c(17,17), c(17,18), c(18,18),
    c(18,19), c(19,19), c(19,20), c(20,20)
  )

  for (grid in optimal_grids) {
    if (grid[1] * grid[2] >= n_plots) {
      return(list(nrow = grid[1], ncol = grid[2]))
    }
  }

  warning("Number of plots exceeds 400. Returning maximum 20x20 grid.")
  return(list(nrow = 20, ncol = 20))
}


#' Calculate AF and AN for a specific PM2.5 reference value
#'
#' @param data A region-specific dataset.
#' @param reference Integer. PM2.5 reference value. Defaults to 15
#' (WHO guideline).
#' @param var_name Character. PM2.5 variable name. Defaults to "pm25".
#' @param family Character. Distribution family for GAM. Defaults to
#' "quasipoisson".
#'
#' @return List with AF and AN
#'
#' @export
calculate_air_pollution_af_an <- function(data,
                                          reference = 15,
                                          var_name = "pm25",
                                          family = "quasipoisson") {
  model <- fit_air_pollution_gam(data, var_name, family)
  coef_extract <- extract_air_pollution_coef(model, var_name)
  beta <- coef_extract$coef

  pm_values <- data[[var_name]]
  deaths <- data$deaths

  above_ref <- pm_values > reference & !is.na(pm_values) & !is.na(deaths)

  if (!any(above_ref)) {
    return(list(af = 0, an = 0))
  }

  rr_t <- exp(beta * (pm_values[above_ref] - reference))
  af_t <- (rr_t - 1) / rr_t
  deaths_t <- deaths[above_ref]

  af_overall <- sum(deaths_t * af_t) / sum(deaths, na.rm = TRUE)
  an_overall <- af_overall * sum(deaths, na.rm = TRUE)

  return(list(af = af_overall, an = an_overall))
}


#' Analyze region-specific distributed lag effects with AF/AN for a chosen
#' PM2.5 reference
#'
#' @param data Input data with lag variables.
#' @param reference Numeric. PM2.5 reference value (e.g., 15, 25, 50, ...).
#' Defaults to 15.
#' @param max_lag Integer. Maximum lag days. Defaults to 2
#' @param family Character. Distribution family for GAM. Defaults to
#' "quasipoisson".
#'
#' @return List with region-specific and meta-analysis results including AF/AN
#'
#' @export
analyze_air_pollution_dlm <- function(data,
                                      reference = 15,
                                      max_lag = 2,
                                      family = "quasipoisson") {
  regions <- unique(data$region)
  all_vars <- c("pm25", paste0("pm25_lag", 1:max_lag), paste0("pm25_lag0_", max_lag))
  all_labels <- c("0", paste0("", 1:max_lag), paste0("0-", max_lag))
  region_results_list <- list()

  for (prov in regions) {
    prov_data <- data %>% dplyr::filter(.data$region == prov)
    total_deaths_prov <- sum(prov_data$deaths, na.rm = TRUE)
    if (nrow(prov_data) < 500) {
      message("Skipping region ", prov, " due to insufficient data")
      next
    }
    prov_results <- data.frame(
      lag_group = all_labels,
      RR = NA,
      LB = NA,
      UB = NA,
      AF = NA,
      AN = NA,
      region = prov,
      stringsAsFactors = FALSE
    )
    for (i in seq_along(all_vars)) {
      var_name <- all_vars[i]
      model <- tryCatch({
        fit_air_pollution_gam(prov_data, var_name, family)
      }, error = function(e) NULL)
      if (!is.null(model)) {
        coef_result <- extract_air_pollution_coef(model, var_name)
        if (!is.na(coef_result$coef) && !is.na(coef_result$se)) {

          beta <- coef_result$coef
          beta_se <- coef_result$se
          beta_lower <- beta - 1.96 * beta_se
          beta_upper <- beta + 1.96 * beta_se
          pm_var <- prov_data[[var_name]]
          deaths_var <- prov_data$deaths
          above_ref <- pm_var > reference & !is.na(pm_var) & !is.na(deaths_var)

          if (any(above_ref)) {
            rr_t <- exp(beta * (pm_var[above_ref] - reference))
            rr_t_lower <- exp(beta_lower * (pm_var[above_ref] - reference))
            rr_t_upper <- exp(beta_upper * (pm_var[above_ref] - reference))
            af_t <- (rr_t - 1) / rr_t
            deaths_t <- deaths_var[above_ref]

            rr_overall <- sum(deaths_t * rr_t) / sum(deaths_t)
            lb_overall <- sum(deaths_t * rr_t_lower) / sum(deaths_t)
            ub_overall <- sum(deaths_t * rr_t_upper) / sum(deaths_t)
            af_overall <- sum(deaths_t * af_t) / sum(deaths_var, na.rm = TRUE)
            an_overall <- af_overall * sum(deaths_var, na.rm = TRUE)

          } else {
            rr_overall <- 0
            lb_overall <- 0
            ub_overall <- 0
            af_overall <- 0
            an_overall <- 0
          }
          prov_results[i, c("RR", "LB", "UB", "AF", "AN")] <-
            list(rr_overall, lb_overall, ub_overall, af_overall, an_overall)
        }
      }
    }
    region_results_list[[prov]] <- prov_results
  }

  if (length(region_results_list) > 0) {
    region_dlm_results <- do.call(rbind, region_results_list)
  } else {
    stop("No regions with sufficient data for distributed lag analysis")
  }

  total_deaths_all <- sum(data$deaths, na.rm = TRUE)
  meta_dlm_results <- data.frame(
    lag_group = all_labels,
    RR = NA,
    LB = NA,
    UB = NA,
    AF = NA,
    AN = NA,
    I2 = NA,
    Q_p = NA
  )
  for (lg in all_labels) {
    lag_data <- region_dlm_results %>%
      dplyr::filter(.data$lag_group == lg) %>%
      dplyr::filter(!is.na(.data$RR))
    if (nrow(lag_data) < 2) next
    lag_data$yi <- log(lag_data$RR)
    lag_data$sei <- (log(lag_data$UB) - log(lag_data$LB)) / (2 * 1.96)
    meta_res <- tryCatch({
      metafor::rma(yi = yi, sei = sei, data = lag_data, method = "REML")
    }, error = function(e) NULL)
    if (!is.null(meta_res)) {
      pooled_rr <- exp(meta_res$b)
      pooled_af <- mean(lag_data$AF, na.rm = TRUE)
      pooled_an <- mean(lag_data$AN, na.rm = TRUE)
      meta_dlm_results[meta_dlm_results$lag_group == lg, "RR"] <- pooled_rr
      meta_dlm_results[meta_dlm_results$lag_group == lg, "LB"] <- exp(meta_res$ci.lb)
      meta_dlm_results[meta_dlm_results$lag_group == lg, "UB"] <- exp(meta_res$ci.ub)
      meta_dlm_results[meta_dlm_results$lag_group == lg, "AF"] <- pooled_af
      meta_dlm_results[meta_dlm_results$lag_group == lg, "AN"] <- pooled_an
      meta_dlm_results[meta_dlm_results$lag_group == lg, "I2"] <- meta_res$I2
      meta_dlm_results[meta_dlm_results$lag_group == lg, "Q_p"] <- meta_res$QEp
    }
  }
  return(list(
    region_results = region_dlm_results,
    meta_results = meta_dlm_results
  ))
}


#' Create distributed lag plots for countries (national) and regions
#'
#' @description Create distributed lag plots for countries (national) and regions.
#'
#' @param dlm_results Results from climatehealth::analyze_air_pollution_dlm.
#' @param output_dir String. Directory to save plots. Defaults to NULL.
#' @param max_lag Integer. Maximum lag days. Defaults to to 2.
#' @param width_per_panel Integer. Width per panel for saving. Defaults to 4.
#' @param height_per_panel Integer. Height per panel for saving. Defaults to 4.
#'
#' @return List of ggplot objects.
#'
#' @export
plot_air_pollution_dlm <- function(dlm_results,
                                   output_dir = NULL,
                                   max_lag = 2,
                                   width_per_panel = 4,
                                   height_per_panel = 4) {

  region_results <- dlm_results$region_results
  meta_results <- dlm_results$meta_results
  all_labels <- c("0", paste0("", 1:max_lag), paste0("0-", max_lag))

  plots_list <- list()

  # Create meta plot
  meta_plot_data <- meta_results %>%
    filter(!is.na(.data$RR)) %>%
    dplyr::mutate(lag_group = factor(.data$lag_group, levels = all_labels))

  meta_plot <- ggplot2::ggplot(meta_plot_data, ggplot2::aes(x = .data$lag_group, y = .data$RR)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$LB, ymax = .data$UB), width = 0.2, color = "darkred") +
    ggplot2::geom_point(size = 2, color = "red") +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "red", size = 1) +
    ggplot2::labs(x = "Lag (days)", y = "RR", title = "Countrywide") +
    ggplot2::ylim(0.98, max(meta_plot_data$UB) * 1.05) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 10),
      axis.title = ggplot2::element_text(face = "bold", size = 8),
      axis.text = ggplot2::element_text(size = 8),
      panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 1)
    )

  plots_list[["Countrywide"]] <- meta_plot

  # Create regional plots
  regions <- unique(region_results$region)

  for (prov in regions) {
    prov_data <- region_results %>%
      filter(.data$region == prov) %>%
      filter(!is.na(.data$RR)) %>%
      dplyr::mutate(lag_group = factor(lag_group, levels = all_labels))

    if (nrow(prov_data) == 0) next

    prov_plot <- ggplot2::ggplot(prov_data, ggplot2::aes(x = .data$lag_group, y = .data$RR)) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$LB, ymax = .data$UB), width = 0.2, color = "darkblue") +
      ggplot2::geom_point(size = 2, color = "blue") +
      ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "red", size = 1) +
      ggplot2::labs(x = "Lag (days)", y = "RR", title = prov) +
      ggplot2::ylim(0.98, max(prov_data$UB) * 1.05) +
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

  # Use unified save_plot function
  if (!is.null(output_dir)) {
    save_air_pollution_plot(
      plot_object = combined_plot,
      output_dir = output_dir,
      filename = "distributed_lag_plots",
      width_per_panel = width_per_panel,
      height_per_panel = height_per_panel,
      grid_dims = grid_dims
    )
  }

  return(list(
    individual_plots = plots_list,
    combined_plot = combined_plot,
    grid_dimensions = grid_dims
  ))
}


#' Create adaptive exposure-response plots with optimal grid layout
#'
#' @param data_with_lags Data frame containing pre-processed data with exposure lags.
#' Must include columns: region, date, deaths, population, and exposure variable.
#' @param meta_results List. Meta-analysis results from previous analysis.
#' Should include overall relative risk estimate (overall_rr).
#' @param reference_pm25 Numeric value specifying the reference PM2.5
#' concentration for risk calculations and plot centering, expressed in \eqn{\mu g/m^3}.
#' @param reference_name Character string describing the reference scenario
#' (e.g., "WHO", "National"). Defaults to "Reference".
#' @param tlag Integer. Maximum lag days for distributed lag models.
#' Defaults to 2.
#' @param vardf Integer. Degrees of freedom for exposure-response functions. Defaults to 3.
#' @param dfseas Integer. Seasonal degrees of freedom per year. Defaults to 4.
#' @param max_ylim Maximum Y-axis limit for relative risk. Defaults to 2.0.
#' @param output_file Character. Full path for output PNG file. If NULL, output path will
#' default to 'air_pollution_results/(reference_name)_exposure_response_plots.png'.
#' Default to NULL.
#' @param plot_width_per_panel Width (inches) per panel in output. Defaults to 4.
#' @param plot_height_per_panel Height (inches) per panel in output. Defaults to 3.5.
#' @param res Integer. Resolution (DPI) for output PNG. Defaults to 150.
#' @param include_af_an Logical. Whether to include AF/AN calculations in plots.
#' Defaults to TRUE.
#' @param var_name Character. Name of PM2.5 variable in dataset. Defaults to "pm25".
#' @param max_lag Integer. Maximum lag days. Defaults to 2.
#' @param family Character. Distribution family for GAM. Defaults to "quasipoisson".
#'
#' @return List containing predictions, meta model, and AF/AN values
#'
#' @export
create_air_pollution_exposure_plots <- function(data_with_lags,
                                                meta_results,
                                                reference_pm25,
                                                reference_name = "Reference",
                                                tlag = 2,
                                                vardf = 3,
                                                dfseas = 4,
                                                max_ylim = 2.0,
                                                output_file = NULL,
                                                plot_width_per_panel = 4,
                                                plot_height_per_panel = 3.5,
                                                res = 150,
                                                include_af_an = TRUE,
                                                var_name = "pm25",
                                                max_lag = 2,
                                                family = "quasipoisson") {

  # Validate essential columns
  required_cols <- c(
    "region", "date", "year", "month", "day", "deaths", "population", "pm25",
    "tmax", "humidity"
  )
  missing_cols <- required_cols[!(required_cols %in% colnames(data_with_lags))]
  if (length(missing_cols)>0) {
    stop(paste0(
      "Missing required columns in 'data_with_lags': ",
      paste(missing_cols, collapse = " ,"))
    )
  }

  if (is.null(output_file)) {
    output_file <- paste0("air_pollution_results/", tolower(reference_name),
                          "_exposure_response_plots.png")
  }

  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

  data_aggreg <- data_with_lags %>%
    dplyr::filter(!is.na(.data$date)) %>%
    dplyr::group_by(.data$region, .data$date, .data$year, .data$month, .data$day) %>%
    dplyr::summarise(
      deaths = sum(.data$deaths, na.rm = TRUE),
      pm25 = mean(.data$pm25, na.rm = TRUE),
      tmax = mean(.data$tmax, na.rm = TRUE),
      humidity = mean(.data$humidity, na.rm = TRUE),
      precipitation = mean(.data$precipitation, na.rm = TRUE),
      population = sum(.data$population, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(.data$region, .data$date) %>%
    dplyr::group_by(.data$region) %>%
    dplyr::mutate(days = dplyr::row_number()) %>%
    dplyr::ungroup()

  plist <- data_aggreg %>% split(., .$region)
  prov <- names(plist)

  varfun <- "bs"
  degree <- 1
  yr <- length(unique(data_aggreg$year))
  knots_values <- c(0.25, 0.5, 0.75)

  # Fit GAM models
  coef_matrix <- NULL
  vcov_list <- vector("list", length(prov))
  names(vcov_list) <- prov
  region_deaths <- numeric(length(prov))
  names(region_deaths) <- prov

  region_af_an <- list()

  for (j in 1:length(prov)) {
    dat <- plist[[prov[j]]]
    var1 <- dat$pm25
    region_deaths[j] <- sum(dat$deaths, na.rm = TRUE)

    if (nrow(dat) < 100) {
      next
    }

    argvar <- list(fun = varfun, degree = degree,
                   knots = quantile(var1, knots_values, na.rm = TRUE))
    arglag <- list(fun = "ns", df = min(2, tlag))

    cb <- tryCatch({
      dlnm::crossbasis(var1, lag = tlag, argvar = argvar, arglag = arglag)
    }, error = function(e) {
      warning("Error creating cross-basis for ", prov[j], ": ", e$message)
      return(NULL)
    })

    if (is.null(cb)) next

    fmla_prov <- as.formula(paste(
      "deaths ~ offset(log(population)) + cb +",
      "ns(days, df =", dfseas * yr, ") + factor(year)"
    ))

    mod <- tryCatch({
      mgcv::gam(fmla_prov, data = dat, family = quasipoisson)
    }, error = function(e) {
      warning("Error fitting model for ", prov[j], ": ", e$message)
      return(NULL)
    })

    if (is.null(mod)) next

    red <- tryCatch({
      dlnm::crossreduce(cb, mod, cen = reference_pm25)
    }, error = function(e) {
      warning("Error reducing cross-basis for ", prov[j], ": ", e$message)
      return(NULL)
    })

    if (is.null(red)) next

    if (is.null(coef_matrix)) {
      actual_df <- length(coef(red))
      coef_matrix <- matrix(data = NA, nrow = length(prov), ncol = actual_df,
                            dimnames = list(prov))
    }

    coef_matrix[j, ] <- coef(red)
    vcov_list[[j]] <- vcov(red)

    prov_data <- data_with_lags %>% filter(region == prov[j])

    simple_model <- tryCatch({
      fit_air_pollution_gam(prov_data, var_name, family)
    }, error = function(e) NULL)

    if (!is.null(simple_model)) {
      coef_info <- extract_air_pollution_coef(simple_model, var_name)
      if (!is.na(coef_info$coef)) {
        af_an_ref <- calculate_air_pollution_af_an(prov_data, reference_pm25,
                                                   var_name, family)
        region_af_an[[prov[j]]] <- af_an_ref
      }
    }

    rm(dat, var1, argvar, arglag, cb, mod, red, fmla_prov)
  }

  valid_rows <- which(!apply(is.na(coef_matrix), 1, all))
  if (length(valid_rows) < 2) {
    stop("Insufficient regions with successful model fits for meta-analysis")
  }

  meta_model <- tryCatch({
    mixmeta::mixmeta(coef_matrix[valid_rows, ] ~ 1, S = vcov_list[valid_rows],
            control = list(showiter = FALSE))
  }, error = function(e) {
    stop("Error in meta-analysis: ", e$message)
  })

  blup_results <- mixmeta::blup(meta_model, vcov = TRUE)

  t1 <- unlist(lapply(plist[prov[valid_rows]], function(x) x$pm25))
  argvar_national <- list(x = t1, fun = varfun, degree = degree,
                          knots = quantile(t1, knots_values, na.rm = TRUE))
  bvar_national <- do.call(dlnm::onebasis, argvar_national)

  pred_national <- dlnm::crosspred(bvar_national, coef = meta_model$coefficients,
                             vcov = meta_model$vcov, model.link = "log",
                             by = 0.1, cen = reference_pm25)

  national_af_an <- list(af = 0, an = 0)
  if (!is.null(meta_results)) {
    overall_beta <- log(meta_results$overall_rr) / 10
    all_data <- data_with_lags
    national_af_an <- calculate_air_pollution_af_an(all_data, reference_pm25,
                                                    var_name, family)
  }

  n_regions <- length(valid_rows)
  n_plots <- n_regions + 1
  grid_dims <- calculate_air_pollution_grid_dims(n_plots)

  # Create Plots
  fig_width <- plot_width_per_panel * grid_dims$ncol
  fig_height <- plot_height_per_panel * grid_dims$nrow

  png(output_file, width = fig_width * res, height = fig_height * res,
      res = res, bg = "white")

  par(mfrow = c(grid_dims$nrow, grid_dims$ncol), mar = c(3, 3, 3, 3),
      oma = c(4, 4, 3, 1), cex.main = 1.1, cex.lab = 0.9, cex.axis = 0.8,
      mgp = c(2, 0.7, 0), tcl = -0.3)

  predictions <- list()
  plot_count <- 0

  plot_count <- plot_count + 1
  y_seq <- seq(0, max_ylim, by = 0.5)

  plot(pred_national, ylab = "", ylim = c(0.0, max_ylim), xlab = "", xaxt = "n",
       yaxt = "n", main = "Nationwide", lwd = 2, col = "red",
       ci.arg = list(col = rgb(0, 0, 1, 0.2), density = NULL))

  abline(h = 1, col = "black", lwd = 1, lty = 1)
  axis(2, at = y_seq, labels = format(y_seq, nsmall = 1), cex.axis = 0.8,
       las = 1, tck = -0.02)
  axis(1, at = seq(floor(min(t1)), ceiling(max(t1)), by = 20), cex.axis = 0.8,
       tck = -0.02)

  breaks <- c(min(t1) - 1, seq(pred_national$predvar[1],
                               pred_national$predvar[length(pred_national$predvar)],
                               length = 30), max(t1) + 1)
  hist_data <- hist(t1, breaks = breaks, plot = FALSE)
  hist_data$density <- hist_data$density / max(hist_data$density) * 0.7
  prop <- max(hist_data$density) / max(hist_data$counts)
  counts <- pretty(hist_data$count, 3)

  hist_scaling_factor <- max_ylim / 1.8
  plot(hist_data, ylim = c(0, max(hist_data$density) * hist_scaling_factor),
       axes = FALSE, ann = FALSE,
       col = rgb(0.8, 0.8, 0.8, 0.5),
       border = rgb(0.7, 0.7, 0.7, 0.8),
       breaks = breaks, freq = FALSE, add = TRUE)

  axis(4, at = counts * prop, labels = counts, cex.axis = 0.8, las = 1,
       tck = -0.02)
  abline(v = reference_pm25, col = "red", lty = 2, lwd = 2)
  abline(v = quantile(t1, c(0.05, 0.95)), col = grey(0.5), lty = 3, lwd = 1.5)

  predictions[["Nationwide"]] <- pred_national

  valid_regions <- prov[valid_rows]

  for (i in 1:length(valid_regions)) {
    plot_count <- plot_count + 1
    region_name <- valid_regions[i]
    dat <- plist[[region_name]]
    var1 <- dat$pm25

    argvar <- list(x = var1, fun = varfun, degree = degree,
                   knots = quantile(var1, knots_values, na.rm = TRUE))
    bvar <- do.call(dlnm::onebasis, argvar)

    valid_idx <- which(prov[valid_rows] == region_name)
    cp <- dlnm::crosspred(bvar, coef = blup_results[[valid_idx]]$blup,
                    vcov = blup_results[[valid_idx]]$vcov,
                    model.link = "log", by = 0.1, cen = reference_pm25)

    plot(cp, "overall", ylim = c(0.0, max_ylim), lwd = 2, col = "red",
         ci.arg = list(col = rgb(0, 0, 1, 0.2), density = NULL),
         xaxt = "n", yaxt = "n", xlab = "", ylab = "",
         main = region_name)

    abline(h = 1, col = "black", lwd = 1, lty = 1)
    axis(2, at = y_seq, labels = format(y_seq, nsmall = 1), cex.axis = 0.8,
         las = 1, tck = -0.02)
    axis(1, at = seq(floor(min(var1)), ceiling(max(var1)), by = 20),
         cex.axis = 0.8, tck = -0.02)

    breaks_prov <- c(min(var1) - 1, seq(cp$predvar[1],
                                        cp$predvar[length(cp$predvar)],
                                        length = 30), max(var1) + 1)
    hist_prov <- hist(var1, breaks = breaks_prov, plot = FALSE)
    hist_prov$density <- hist_prov$density / max(hist_prov$density) * 0.7
    prop_prov <- max(hist_prov$density) / max(hist_prov$counts)
    counts_prov <- pretty(hist_prov$count, 3)

    plot(hist_prov, ylim = c(0, max(hist_prov$density) * hist_scaling_factor),
         axes = FALSE, ann = FALSE,
         col = rgb(0.8, 0.8, 0.8, 0.5),
         border = rgb(0.7, 0.7, 0.7, 0.8),
         breaks = breaks_prov, freq = FALSE, add = TRUE)

    axis(4, at = counts_prov * prop_prov, labels = counts_prov, cex.axis = 0.8,
         las = 1, tck = -0.02)
    abline(v = reference_pm25, col = "red", lty = 2, lwd = 2)
    abline(v = quantile(var1, c(0.05, 0.95)), col = grey(0.5), lty = 3, lwd = 1.5)

    predictions[[region_name]] <- cp
    rm(argvar, bvar, cp, var1)
  }

  if (plot_count < grid_dims$nrow * grid_dims$ncol) {
    remaining_panels <- (grid_dims$nrow * grid_dims$ncol) - plot_count
    for (i in 1:remaining_panels) {
      plot.new()
    }
  }

  mtext(text = "Daily PM2.5 (\u03bc\u0067\u002f\u006d\u00b3)", side = 1, line = 2.5, outer = TRUE,
        cex = 1.1)
  mtext(text = "Relative risk", side = 2, line = 2.5, outer = TRUE, cex = 1.1)
  mtext(text = paste0(reference_name, " (PM2.5 reference: ",
                      reference_pm25, " \u03bc\u0067\u002f\u006d\u00b3)"),
        side = 3, line = 1, outer = TRUE, cex = 1.2, font = 2)

  if (plot_count < grid_dims$nrow * grid_dims$ncol) {
    legend_row <- ceiling((plot_count + 1) / grid_dims$ncol)
    legend_col <- ((plot_count) %% grid_dims$ncol) + 1

    par(mfg = c(legend_row, legend_col))
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")

    legend_items <- c("RR curve", "95% CI",
                      paste0("Reference (", reference_pm25, " \u03bc\u0067\u002f\u006d\u00b3)"),
                      "5th/95th percentile")

    legend("center",
           legend = legend_items,
           col = c("red", rgb(0, 0, 1, 0.2), "red", grey(0.5)),
           lty = c(1, NA, 2, 3),
           pch = c(NA, 15, NA, NA),
           pt.bg = c(NA, rgb(0, 0, 1, 0.2), NA, NA),
           pt.cex = c(NA, 2, NA, NA),
           cex = 0.9,
           bg = "white",
           box.lty = 1)
  }

  dev.off()

  invisible(list(
    predictions = predictions,
    meta_model = meta_model,
    reference_pm25 = reference_pm25,
    reference_name = reference_name,
    grid_dimensions = grid_dims,
    region_af_an = region_af_an,
    national_af_an = national_af_an
  ))
}


#' PM2.5 Mortality Analysis with Enhanced Features (Air Pollution)
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
#' @param population_col Character. Name of population column
#' @param max_lag Integer. Maximum lag days. Defaults to 2.
#' @param var_name Character. Variable name for labeling. Defaults to "pm25".
#' @param family Character. Distribution family for GAM model. Defaults to "quasipoisson".
#' @param reference Numeric. Reference value for AF/AN calculations. Defaults to 15.
#' @param output_dir Character. Output directory for results. Defaults to NULL.
#' @param save_outputs Logical. Whether to save plots and results. Defaults to FALSE.
#' @param include_distributed_lags Logical. Whether to include distributed lag .
#' analysis. Defaults to TRUE.
#' @param create_plots Logical. Whether to create exposure-response plots.
#' Defaults to TRUE.
#' @param reference_standards List of reference thresholds used in exposure-response plots.
#' Defaults include WHO (\eqn{15~\mu g/m^3}) and Rwanda (\eqn{50~\mu g/m^3}) air quality standards.
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
                                        population_col,
                                        max_lag = 2,
                                        var_name = "pm25",
                                        family = "quasipoisson",
                                        reference = 15,
                                        output_dir = NULL,
                                        save_outputs = FALSE,
                                        include_distributed_lags = TRUE,
                                        create_plots = TRUE,
                                        reference_standards = list(
                                          list(value = 15, name = "WHO"),
                                          list(value = 50, name = "Rwanda")
                                        )) {
  data <- load_air_pollution_data(data_path = data_path,
                                  date_col = date_col,
                                  region_col = region_col,
                                  pm25_col = pm25_col,
                                  deaths_col = deaths_col,
                                  humidity_col = humidity_col,
                                  precipitation_col = precipitation_col,
                                  tmax_col = tmax_col,
                                  population_col = population_col)

  data_with_lags <- create_air_pollution_lags(data, max_lag)

  # Display descriptive statistics
  key_variables <- c(deaths_col, pm25_col, tmax_col, humidity_col, population_col)
  desc_stats <- air_pollution_descriptive_stats(
    data_with_lags,
    variables = key_variables,
    output_dir = output_dir,
    save_outputs = save_outputs
  )

  # Create scatterplots of key relationships
  if (save_outputs || create_plots) {
    plot_air_pollution_variables(data_with_lags, "pm25", "deaths", output_dir, save_outputs)
    plot_air_pollution_variables(data_with_lags, "tmax", "deaths", output_dir, save_outputs)
    plot_air_pollution_variables(data_with_lags, "humidity", "deaths", output_dir, save_outputs)
    plot_air_pollution_variables(data_with_lags, "precipitation", "deaths", output_dir, save_outputs)
  }

  meta_results <- air_pollution_meta_analysis(data_with_lags, var_name, family)

  lag_results <- analyze_air_pollution_lags(data_with_lags, max_lag, family)

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

  dlm_results <- NULL
  dlm_plots <- NULL

  if (include_distributed_lags) {
    dlm_results <- analyze_air_pollution_dlm(data_with_lags,
                                             reference, max_lag, family)
    dlm_plots <- plot_air_pollution_dlm(dlm_results, output_dir, max_lag)
  }

  exposure_response_plots <- NULL
  reference_specific_af_an <- NULL

  if (create_plots && !is.null(meta_results)) {

    exposure_response_plots <- list()
    reference_specific_af_an <- list()

    for (ref_std in reference_standards) {
      ref_af_an <- analyze_air_pollution_dlm(
        data_with_lags, reference = ref_std$value, max_lag, family
      )

      reference_specific_af_an[[ref_std$name]] <- ref_af_an

      meta_summary <- ref_af_an$meta_results %>%
        filter(lag_group == "0") %>%
        dplyr::select(AF, AN)

      if (nrow(meta_summary) > 0) {
        cat("  Overall AF for", ref_std$name, "reference:",
            round(meta_summary$AF * 100, 2), "%\n")
        cat("  Overall AN for", ref_std$name, "reference:",
            round(meta_summary$AN, 0), "deaths\n")
      }

      plot_results <- create_air_pollution_exposure_plots(
        data_with_lags = data_with_lags,
        meta_results = meta_results,
        reference_pm25 = ref_std$value,
        reference_name = ref_std$name,
        output_file = file.path(
          ifelse(is.null(output_dir), "results", output_dir),
          paste0(tolower(ref_std$name), "_exposure_response_plots.png")
        ),
        include_af_an = TRUE,
        var_name = var_name,
        max_lag = max_lag,
        family = family
      )

      exposure_response_plots[[ref_std$name]] <- plot_results
    }
  }

  cat("\n=== COMPREHENSIVE RESULTS SUMMARY ===\n")
  cat(
    "Overall RR (10 \u03bc\u0067\u002f\u006d\u00b3):",
    round(meta_results$overall_rr, 3), "\n"
  )
  cat("95% CI:", paste(round(meta_results$overall_ci, 3), collapse = "-"), "\n")
  cat("Overall AF:", round(meta_results$overall_af * 100, 2), "%\n")
  cat("Overall AN:", round(meta_results$overall_an, 0), "deaths\n")
  cat("Heterogeneity I\u00b2:", round(meta_results$heterogeneity$I2, 1), "%\n")

  if (!is.null(reference_specific_af_an)) {
    cat("\n--- Reference-specific AF/AN Summary ---\n")
    for (ref_name in names(reference_specific_af_an)) {
      ref_data <- reference_specific_af_an[[ref_name]]$meta_results %>%
        filter(lag_group == "0")
      if (nrow(ref_data) > 0) {
        cat(ref_name, "standard: AF =", round(ref_data$AF * 100, 2), "%, AN =",
            round(ref_data$AN, 0), "deaths\n")
      }
    }
  }

  if (save_outputs && !is.null(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

    # Save cleaned region results
    region_results_clean <- meta_results$region_results %>%
      dplyr::select(-data, -model)
    write.csv(region_results_clean, file.path(output_dir, "region_results.csv"),
              row.names = FALSE)
    write.csv(lag_results, file.path(output_dir, "lag_results.csv"), row.names = FALSE)

    if (!is.null(dlm_results)) {
      write.csv(dlm_results$region_results, file.path(output_dir,
                                                      "region_dlm_results.csv"),
                row.names = FALSE)
      write.csv(dlm_results$meta_results, file.path(output_dir, "meta_dlm_results.csv"),
                row.names = FALSE)
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

  return(list(
    data = data_with_lags,
    meta_analysis = meta_results,
    lag_analysis = lag_results,
    distributed_lag_analysis = dlm_results,
    plots = list(
      forest = forest_plot,
      lags = lag_plot,
      distributed_lags = dlm_plots
    ),
    exposure_response_plots = exposure_response_plots,
    reference_specific_af_an = reference_specific_af_an,
    descriptive_stats = desc_stats
  ))
}
