#-------------------------------------------------------------------------------
#' @title R-code for I1: All-cause mortality attributable to Outdoor PM2.5.
#-------------------------------------------------------------------------------

# # Load required libraries
# library(tidyverse), library(dlnm), library(splines), library(metafor), library(lubridate)
# library(gridExtra), library(mgcv), library(tools), library(patchwork)

#' 
#' @description This script contains functions for analyzing the relationship between PM2.5 and all cause mortality using GAM models.
#' 
#' @param data_path Path to data file
#' @param date_col Character. Name of date column
#' @param region_col Character. Name of region column  
#' @param pm25_col Character. Name of PM2.5 column
#' @param deaths_col Character. Name of deaths column
#' @param humidity_col Character. Name of humidity column
#' @param precipitation_col Character. Name of precipitation column
#' @param tmax_col Character. Name of temperature column
#' @param population_col Character. Name of population column
#' @param max_lag Integer. Maximum lag days. Default to 2
#' @param output_dir String. Output directory for results. The possible options. 
#' Default to NULL
#' @param save_outputs Boolean. Whether to save plots and results. Default to FALSE
#' @param include_distributed_lags Boolean. Whether to include distributed 
#' lag analysis. Default to TRUE
#'
#' @return List with all analysis results including AF and AN

pm25_analysis <- function(data_path, 
                          date_col,
                          region_col,
                          pm25_col,
                          deaths_col,
                          humidity_col,
                          precipitation_col,
                          tmax_col,
                          population_col,
                          max_lag = 2,
                          output_dir = NULL,
                          save_outputs = FALSE,
                          include_distributed_lags = TRUE) {
  
  cat("=== PM2.5 MORTALITY ANALYSIS ===\n")
  
  # 1. Load and process data
  data <- load_pollution_data(data_path = data_path,
                              date_col = date_col,
                              region_col = region_col,
                              pm25_col = pm25_col,
                              deaths_col = deaths_col,
                              humidity_col = humidity_col,
                              precipitation_col = precipitation_col,
                              tmax_col = tmax_col,
                              population_col = population_col
                              )
  
  data_with_lags <- create_lags(data, max_lag)
  
  # 2. Run meta-analysis with AF/AN calculations
  meta_results <- meta_analysis_with_af(data_with_lags)
  
  # 3. Analyze lags with AF/AN
  lag_results <- analyze_lags_with_af(data_with_lags)
  
  # 4. Create plots
  forest_plot <- plot_forest_with_af(meta_results)
  lag_plot <- plot_lags_with_af(lag_results)
  
  # 5. Distributed lag analysis
  dlm_results <- NULL
  dlm_plots <- NULL
  
  if (include_distributed_lags) {
    cat("\n=== DISTRIBUTED LAG ANALYSIS ===\n")
    dlm_results <- analyze_region_distributed_lags_with_af(data_with_lags)
    dlm_plots <- plot_distributed_lags_adaptive(dlm_results, output_dir)
  }
  
  # 6. Print summary
  cat("\n=== RESULTS SUMMARY ===\n")
  cat("Overall RR (10 μg/m³):", round(meta_results$overall_rr, 3), "\n")
  cat("95% CI:", paste(round(meta_results$overall_ci, 3), collapse = "-"), "\n")
  cat("Overall AF:", round(meta_results$overall_af * 100, 2), "%\n")
  cat("Overall AN:", round(meta_results$overall_an, 0), "deaths\n")
  cat("Heterogeneity I²:", round(meta_results$heterogeneity$I2, 1), "%\n")
  
  # 7. Save outputs
  if(save_outputs && !is.null(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    
    # Save plots
    ggsave(file.path(output_dir, "forest_plot.pdf"), forest_plot, width = 10, height = 8)
    ggsave(file.path(output_dir, "lag_results.pdf"), lag_plot, width = 8, height = 6)
    
    # Save results with AF/AN
    region_results_clean <- meta_results$region_results %>%
      select(-data, -model)
    write.csv(region_results_clean, file.path(output_dir, "region_results.csv"), row.names = FALSE)
    write.csv(lag_results, file.path(output_dir, "lag_results.csv"), row.names = FALSE)
    
    # Save distributed lag results if available
    if (!is.null(dlm_results)) {
      write.csv(dlm_results$region_results, file.path(output_dir, "region_dlm_results.csv"), row.names = FALSE)
      write.csv(dlm_results$meta_results, file.path(output_dir, "meta_dlm_results.csv"), row.names = FALSE)
    }
    
    cat("Results saved to:", output_dir, "\n")
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
    )
  ))
}
#
# 1. DATA PROCESSING ----
#


#' Load and standardize air pollution data
#'
#' @param data_path Path to CSV file or dataframe
#' @param date_col Name of date column
#' @param region_col Name of region column  
#' @param pm25_col Name of PM2.5 column
#' @param deaths_col Name of deaths column
#' @param humidity_col Name of humidity column
#' @param precipitation_col Name of precipitation column
#' @param tmax_col Name of temperature column
#' @param population_col Name of population column
#' @param age_col Name of age column
#' @param sex_col Name of sex column
#' @param urbanisation_col Name of urbanisation column
#'
#' @return Formatted dataframe with standardized column names
load_pollution_data <- function(data_path,
                                date_col = "date",
                                region_col = "region", 
                                pm25_col = "pm25",
                                deaths_col = "deaths",
                                humidity_col = "humidity",
                                precipitation_col = "precipitation",
                                tmax_col = "tmax",
                                population_col = NULL, 
                                age_col = NULL,
                                sex_col = NULL,
                                urbanisation_col = NULL) {
  
  # Load data
  data <- if(is.character(data_path)) read.csv(data_path) else data_path
  
  # Rename columns to standard names (same logic as original)
  if(date_col != "date" && date_col %in% names(data)) {
    data <- data %>% rename(date = !!sym(date_col))
  }
  if(region_col != "region" && region_col %in% names(data)) {
    data <- data %>% rename(region = !!sym(region_col))
  }
  if(pm25_col != "pm25" && pm25_col %in% names(data)) {
    data <- data %>% rename(pm25 = !!sym(pm25_col))
  }
  if(deaths_col != "deaths" && deaths_col %in% names(data)) {
    data <- data %>% rename(deaths = !!sym(deaths_col))
  }
  if(humidity_col != "humidity" && humidity_col %in% names(data)) {
    data <- data %>% rename(humidity = !!sym(humidity_col))
  }
  if(precipitation_col != "precipitation" && precipitation_col %in% names(data)) {
    data <- data %>% rename(precipitation = !!sym(precipitation_col))
  }
  if(tmax_col != "tmax" && tmax_col %in% names(data)) {
    data <- data %>% rename(tmax = !!sym(tmax_col))
  }
  
  # Handle optional columns
  if(!is.null(population_col) && population_col %in% names(data)) {
    data <- data %>% rename(population = !!sym(population_col))
  } else if(!"population" %in% names(data)) {
    data <- data %>% mutate(population = 1)
  }
  
  if(!is.null(age_col) && age_col %in% names(data)) {
    data <- data %>% rename(age = !!sym(age_col))
  } else if(!"age" %in% names(data)) {
    data <- data %>% mutate(age = "all")
  }
  
  if(!is.null(sex_col) && sex_col %in% names(data)) {
    data <- data %>% rename(sex = !!sym(sex_col))
  } else if(!"sex" %in% names(data)) {
    data <- data %>% mutate(sex = "both")
  }
  
  if(!is.null(urbanisation_col) && urbanisation_col %in% names(data)) {
    data <- data %>% rename(urbanisation = !!sym(urbanisation_col))
  } else if(!"urbanisation" %in% names(data)) {
    data <- data %>% mutate(urbanisation = "mixed")
  }
  
  # Date format identification (same as original)
  date_function <- lubridate::ymd
  if (grepl("^\\d{2}/\\d{2}/\\d{4}$", data[["date"]][1])) {
    date_function <- lubridate::dmy
  }
  
  # Process dates and create time variables
  data <- data %>%
    mutate(
      date = date_function(date),
      year = lubridate::year(date),
      month = lubridate::month(date),
      day = lubridate::day(date),
      dow = as.character(lubridate::wday(date, label = TRUE)),
      time = row_number()
    ) %>%
    arrange(date)
  
  # Keep only essential columns
  essential_cols <- c("date", "region", "pm25", "deaths", "humidity", 
                      "precipitation", "tmax", "population", "age", "sex", 
                      "urbanisation", "year", "month", "day", "dow", "time")
  
  data <- data %>% select(any_of(essential_cols))
  
  cat("Data loaded: ", length(unique(data$region)), " regions, ",
      nrow(data), " observations, ", paste(range(data$date), collapse = " to "), "\n")
  
  return(data)
}

#' Create lagged PM2.5 variables
#' @param data Input dataframe
#' @param max_lag Maximum lag days (default: 2)
#' @return Dataframe with lag variables
create_lags <- function(data, max_lag = 2) {
  
  data_with_lags <- data %>%
    group_by(region) %>%
    arrange(date) %>%
    mutate(
      pm25_lag1 = lag(pm25, 1),
      pm25_lag2 = lag(pm25, 2),
      pm25_lag0_2 = rowMeans(cbind(pm25, pm25_lag1, pm25_lag2), na.rm = FALSE)
    ) %>%
    filter(!is.na(pm25_lag2)) %>%
    ungroup()
  
  cat("Lag variables created. Observations with complete lags:", nrow(data_with_lags), "\n")
  return(data_with_lags)
}

# 2. MODELING ----

#' Fit GAM model for single region
#' @param data Region-specific data
#' @param pm25_var PM2.5 variable to use
#' @return GAM model object or NULL if failed
fit_region_model <- function(data, pm25_var = "pm25") {
  
  if(nrow(data) < 100) {
    warning("Insufficient data: only ", nrow(data), " observations")
    return(NULL)
  }
  
  formula_str <- paste(
    "deaths ~", pm25_var, 
    "+ ns(time, df = 7) + ns(tmax, df = 3) + ns(humidity, df = 3)",
    "+ ns(precipitation, df = 3) + dow + offset(log(population))"
  )
  
  tryCatch({
    gam(as.formula(formula_str), data = data, family = quasipoisson)
  }, error = function(e) {
    warning("Model fitting failed: ", e$message)
    return(NULL)
  })
}

# 3. META-ANALYSIS ----

#' Extract coefficient and standard error safely from GAM model
#' @param model GAM model object
#' @param var_name Variable name to extract
#' @return List with coefficient and standard error
extract_coef_se <- function(model, var_name) {
  if(is.null(model)) {
    return(list(coef = NA, se = NA, p_value = NA))
  }
  
  tryCatch({
    # Get coefficient
    coef_val <- coef(model)[var_name]
    if(is.na(coef_val) || is.null(coef_val)) {
      return(list(coef = NA, se = NA, p_value = NA))
    }
    
    # Get standard error
    vcov_matrix <- vcov(model)
    if(var_name %in% rownames(vcov_matrix)) {
      se_val <- sqrt(vcov_matrix[var_name, var_name])
    } else {
      return(list(coef = NA, se = NA, p_value = NA))
    }
    
    # Get p-value
    summary_table <- summary(model)$p.table
    if(var_name %in% rownames(summary_table)) {
      p_val <- summary_table[var_name, "Pr(>|t|)"]
    } else {
      p_val <- NA
    }
    
    return(list(coef = as.numeric(coef_val), se = as.numeric(se_val), p_value = as.numeric(p_val)))
    
  }, error = function(e) {
    warning("Failed to extract coefficient for ", var_name, ": ", e$message)
    return(list(coef = NA, se = NA, p_value = NA))
  })
}

#' Extract coefficient, standard error, AF and AN from GAM model
#' @param model GAM model object
#' @param var_name Variable name to extract
#' @param total_deaths Total number of deaths for AN calculation
#' @return List with coefficient, standard error, AF, AN, and p-value
extract_coef_se_af <- function(model, var_name, total_deaths) {
  if(is.null(model)) {
    return(list(coef = NA, se = NA, af = NA, an = NA, p_value = NA))
  }
  
  tryCatch({
    # Get coefficient
    coef_val <- coef(model)[var_name]
    if(is.na(coef_val) || is.null(coef_val)) {
      return(list(coef = NA, se = NA, af = NA, an = NA, p_value = NA))
    }
    
    # Get standard error
    vcov_matrix <- vcov(model)
    if(var_name %in% rownames(vcov_matrix)) {
      se_val <- sqrt(vcov_matrix[var_name, var_name])
    } else {
      return(list(coef = NA, se = NA, af = NA, an = NA, p_value = NA))
    }
    
    # Get p-value
    summary_table <- summary(model)$p.table
    if(var_name %in% rownames(summary_table)) {
      p_val <- summary_table[var_name, "Pr(>|t|)"]
    } else {
      p_val <- NA
    }
    
    # Calculate AF and AN
    # AF = (RR - 1) / RR where RR = exp(beta)
    rr_unit <- exp(coef_val)  # RR for 1 unit increase
    af <- (rr_unit - 1) / rr_unit
    an <- total_deaths * af
    
    return(list(
      coef = as.numeric(coef_val), 
      se = as.numeric(se_val), 
      af = as.numeric(af),
      an = as.numeric(an),
      p_value = as.numeric(p_val)
    ))
    
  }, error = function(e) {
    warning("Failed to extract coefficient for ", var_name, ": ", e$message)
    return(list(coef = NA, se = NA, af = NA, an = NA, p_value = NA))
  })
}

#' Conduct two-stage meta-analysis with AF and AN calculations
#' @param data Input data with multiple regions
#' @param pm25_var PM2.5 variable to analyze
#' @return List with meta-analysis results including AF and AN
meta_analysis_with_af <- function(data, pm25_var = "pm25") {
  
  cat("Stage 1: Fitting regional models...\n")
  
  # Stage 1: Fit models by region
  region_results <- data %>%
    group_by(region) %>%
    nest() %>%
    mutate(
      n_obs = map_dbl(data, nrow),
      total_deaths = map_dbl(data, ~sum(.x$deaths, na.rm = TRUE)),
      model = map2(data, region, ~{
        cat("  Fitting:", .y, "\n")
        fit_region_model(.x, pm25_var)
      })
    ) %>%
    ungroup()
  
  # Extract coefficients with AF/AN
  cat("Extracting coefficients and calculating AF/AN...\n")
  region_results <- region_results %>%
    mutate(
      coef_results = map2(model, total_deaths, ~extract_coef_se_af(.x, pm25_var, .y)),
      coef_pm25 = map_dbl(coef_results, ~.x$coef),
      se_pm25 = map_dbl(coef_results, ~.x$se),
      af = map_dbl(coef_results, ~.x$af),
      an = map_dbl(coef_results, ~.x$an),
      p_value = map_dbl(coef_results, ~.x$p_value),
      
      # Calculate effect estimates for 10 μg/m³ increase
      rr_10ug = exp(coef_pm25 * 10),
      ci_lower = exp((coef_pm25 - 1.96 * se_pm25) * 10),
      ci_upper = exp((coef_pm25 + 1.96 * se_pm25) * 10),
      
      # AF and AN for 10 μg/m³ increase
      af_10ug = (rr_10ug - 1) / rr_10ug,
      an_10ug = total_deaths * af_10ug
    ) %>%
    filter(!is.na(coef_pm25) & !is.na(se_pm25)) %>%
    select(-coef_results)
  
  if(nrow(region_results) < 2) {
    stop("Need at least 2 regions with successful model fits for meta-analysis")
  }
  
  cat("Stage 2: Meta-analysis of", nrow(region_results), "regions...\n")
  
  # Stage 2: Random-effects meta-analysis
  meta_result <- rma(
    yi = coef_pm25 * 10,
    sei = se_pm25 * 10,
    data = region_results,
    method = "REML",
    slab = region
  )
  
  # Calculate overall AF and AN
  overall_rr <- exp(meta_result$beta)
  overall_af <- (overall_rr - 1) / overall_rr
  overall_an <- sum(region_results$total_deaths) * overall_af
  
  return(list(
    region_results = region_results,
    meta_result = meta_result,
    overall_rr = overall_rr,
    overall_ci = exp(c(meta_result$ci.lb, meta_result$ci.ub)),
    overall_af = overall_af,
    overall_an = overall_an,
    heterogeneity = list(
      tau2 = meta_result$tau2,
      I2 = meta_result$I2,
      Q = meta_result$QE,
      Q_pval = meta_result$QEp
    )
  ))
}

#' Analyze multiple lag structures with AF and AN
#' @param data Input data
#' @return Dataframe with lag-specific results including AF and AN
analyze_lags_with_af <- function(data) {
  
  lag_vars <- c("pm25", "pm25_lag1", "pm25_lag2", "pm25_lag0_2")
  lag_labels <- c("Lag 0", "Lag 1", "Lag 2", "Lag 0-2")
  
  map2_dfr(lag_vars, lag_labels, ~{
    cat("Analyzing", .y, "...\n")
    result <- tryCatch({
      meta_analysis_with_af(data, .x)
    }, error = function(e) {
      warning("Failed to analyze ", .y, ": ", e$message)
      return(NULL)
    })
    
    if(is.null(result)) {
      return(data.frame(
        lag = .y, rr = NA, ci_lower = NA, ci_upper = NA,
        af = NA, an = NA, tau2 = NA, I2 = NA, p_value = NA
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

#' Create forest plot with AF and AN
#' @param meta_results Meta-analysis results with AF/AN
#' @param title Plot title
#' @return ggplot object
plot_forest_with_af <- function(meta_results, title = "PM2.5 Effects by Region") {
  
  # Prepare data
  region_data <- meta_results$region_results %>%
    select(region, rr_10ug, ci_lower, ci_upper, af_10ug, an_10ug) %>%
    mutate(type = "Region",
           label = sprintf("RR: %.3f (AF: %.1f%%, AN: %.0f)", 
                           rr_10ug, af_10ug * 100, an_10ug))
  
  overall_data <- data.frame(
    region = "Overall",
    rr_10ug = meta_results$overall_rr,
    ci_lower = meta_results$overall_ci[1],
    ci_upper = meta_results$overall_ci[2],
    af_10ug = meta_results$overall_af,
    an_10ug = meta_results$overall_an,
    type = "Overall",
    label = sprintf("RR: %.3f (AF: %.1f%%, AN: %.0f)", 
                    meta_results$overall_rr, 
                    meta_results$overall_af * 100, 
                    meta_results$overall_an)
  )
  
  plot_data <- bind_rows(region_data, overall_data) %>%
    mutate(
      region = factor(region, levels = rev(c(region_data$region, "Overall"))),
      is_overall = type == "Overall"
    )
  suppressWarnings(print(
  ggplot(plot_data, aes(x = rr_10ug, y = region)) +
    geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper, 
                       color = is_overall, linewidth = is_overall), height = 0.3) +
    geom_point(aes(color = is_overall, size = is_overall)) +
    geom_text(aes(label = label, x = ci_upper), hjust = -0.1, size = 3) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
    scale_color_manual(values = c("FALSE" = "blue", "TRUE" = "red")) +
    scale_size_manual(values = c("FALSE" = 2, "TRUE" = 3)) +
    scale_linewidth_manual(values = c("FALSE" = 0.8, "TRUE" = 1.2)) +
    xlim(c(min(plot_data$ci_lower) * 0.95, max(plot_data$ci_upper) * 1.5)) +
    labs(
      x = "Health Relative Risk", y = "Region",
      title = title,
      subtitle = sprintf("I² = %.1f%%; τ² = %.4f", 
                         meta_results$heterogeneity$I2, 
                         meta_results$heterogeneity$tau2)
    ) +
    theme_minimal() +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
  ))
}

#' Plot lag comparison with AF and AN
#' @param lag_results Lag analysis results with AF/AN
#' @return ggplot object
plot_lags_with_af <- function(lag_results) {
  
  lag_results_clean <- lag_results %>% filter(!is.na(rr))
  
  if(nrow(lag_results_clean) == 0) {
    stop("No successful lag analyses to plot")
  }
  
  lag_results_clean$lag <- factor(lag_results_clean$lag, 
                                  levels = c("Lag 0", "Lag 1", "Lag 2", "Lag 0-2"))
  
  # Create labels with AF and AN
  lag_results_clean <- lag_results_clean %>%
    mutate(label = sprintf("AF: %.1f%%\nAN: %.0f", af * 100, an))
  
  ggplot(lag_results_clean, aes(x = lag, y = rr)) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                  width = 0.2, color = "darkblue", linewidth = 0.8) +
    geom_point(size = 3, color = "blue") +
    geom_text(aes(label = label), vjust = -2, size = 3) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
    labs(
      x = "Lag Structure", y = "Health Relative Risk",
      title = "RR by Lag Structure"
    ) +
    ylim(c(min(lag_results_clean$ci_lower) * 0.95, 
           max(lag_results_clean$ci_upper) * 1.15)) +
    theme_minimal() + theme(plot.title = element_text(hjust = 0.5))
}

#' Calculate Optimal Grid Dimensions for Multiple Plot Display
#' 
#' @description 
#' Determines the optimal grid layout (rows × columns) for displaying multiple plots
#' in a single figure. The function prioritizes aesthetically pleasing arrangements
#' that are wider than tall, minimizing empty cells while maintaining visual balance.
#' 
#' @param n_plots Integer. Number of plots to display. Must be positive.
#' 
#' @return 
#' A named list with two components:
#' 
#' @details 
#' The function uses predefined optimal grid configurations for common plot counts
#' to ensure visually balanced layouts. The algorithm follows these principles:
#' 
#' \itemize{
#'   \item Prefers wider layouts (more columns than rows) for better visual balance
#'   \item Minimizes the number of empty cells in the grid
#'   \item Uses established aesthetic ratios for common scientific figure layouts
#' }
#' 
#' @note 
#' For plot counts exceeding 400, the function will return a 20×20 grid.
#' Consider whether such large grids are appropriate for your visualization needs.
#' 
#' 
#' @export
calculate_grid_dims <- function(n_plots) {
  # Validate input
  if (!is.numeric(n_plots) || length(n_plots) != 1) {
    stop("n_plots must be a single numeric value")
  }
  
  # Handle edge cases
  if (n_plots <= 0) {
    warning("n_plots should be positive. Returning 1x1 grid.")
    return(list(nrow = 1, ncol = 1))
  }
  
  if (n_plots == 1) {
    return(list(nrow = 1, ncol = 1))
  }
  
  # Define optimal grid configurations for common cases
  # These configurations are aesthetically pleasing and minimize empty cells
  # Format: c(rows, columns) - covers up to rows*columns plots
  optimal_grids <- list(
    c(2, 2), c(2, 3), c(3, 3), c(3, 4), c(4, 4), c(4, 5), c(5, 5), c(5, 6), 
    c(6, 6), c(6, 7), c(7, 7), c(7, 8), c(8, 8), c(8,9),  c(9,9),  c(9,10),
    c(10,10), c(10,11), c(11,12), c(12,12), c(12,13), c(13,14), c(14,15),
    c(15,15), c(15,16), c(16,16), c(16,17), c(17,17), c(17,18), c(18,18),
    c(18,19), c(19,19), c(19,20), c(20,20)
  )
  
  # Find the appropriate grid configuration
  for (grid in optimal_grids) {
    if (grid[1] * grid[2] >= n_plots) {
      return(list(nrow = grid[1], ncol = grid[2]))
    }
  }
  
  # For very large numbers (>400), return 20x20
  # This is a practical limit for visualization
  warning("Number of plots exceeds 400. Returning maximum 20x20 grid.")
  return(list(nrow = 20, ncol = 20))
}

#' Calculate AF and AN for a specific PM2.5 reference value
#' @param data region-specific data
#' @param beta Coefficient from model (log RR per beta_unit)
#' @param reference PM2.5 reference value
#' @param beta_unit Unit for which beta is reported (default: 10 µg/m³)
#' @param pm25_var PM2.5 variable name
#' @return List with AF and AN
#' 
#' 
calculate_af_an_for_reference <- function(data, beta, reference, beta_unit = 10, pm25_var = "pm25") {
  pm_values <- data[[pm25_var]]
  deaths <- data$deaths
  
  # Filter for values above reference
  above_ref <- pm_values > reference & !is.na(pm_values) & !is.na(deaths)
  
  if (!any(above_ref)) {
    return(list(af = 0, an = 0))
  }
  
  # Calculate RR for each observation above reference
  
  suppressWarnings({
  rr_t <- exp(beta * (pm_values[above_ref] - reference) / beta_unit)})
  af_t <- (rr_t - 1) / rr_t
  deaths_t <- deaths[above_ref]
  
  # Overall AF and AN
  
  af_overall <- sum(deaths_t * af_t) / sum(deaths, na.rm = TRUE)
  an_overall <- af_overall * sum(deaths, na.rm = TRUE)
  
  return(list(af = af_overall, an = an_overall))
}

#' Analyze region-specific distributed lag effects with AF/AN for a chosen PM2.5 reference, properly handling beta units
#'
#' @param data Input data with lag variables
#' @param reference Numeric. PM2.5 reference value (e.g., 15, 25, 50, ...)
#' @param beta_unit Numeric. The unit for which beta is reported (default: 10)
#' @return List with region-specific and meta-analysis results including AF/AN (named 'AF' and 'AN') for the chosen reference
#' @export

analyze_region_distributed_lags_with_af <- function(data, reference = 15, beta_unit = 10) {
  regions <- unique(data$region)
  lag_vars <- c("pm25", "pm25_lag1", "pm25_lag2")
  lag_labels <- c("0", "1", "2")
  cum_vars <- "pm25_lag0_2" 
  cum_labels <- "0-2"
  all_vars <- c(lag_vars, cum_vars)
  all_labels <- c(lag_labels, cum_labels)
  region_results_list <- list()
  
  for (prov in regions) {
    cat("Analyzing region:", prov, "\n")
    prov_data <- data %>% dplyr::filter(region == prov)
    total_deaths_prov <- sum(prov_data$deaths, na.rm = TRUE)
    if (nrow(prov_data) < 100) {
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
      p_value = NA,
      region = prov,
      stringsAsFactors = FALSE
    )
    for (i in seq_along(all_vars)) {
      var_name <- all_vars[i]
      model <- tryCatch({
        fit_region_model(prov_data, var_name)
      }, error = function(e) NULL)
      if (!is.null(model)) {
        coef_result <- extract_coef_se_af(model, var_name, total_deaths_prov)
        if (!is.na(coef_result$coef) && !is.na(coef_result$se)) {
          # RR and CI for beta_unit increase
          rr <- exp(coef_result$coef * beta_unit)
          lb <- exp((coef_result$coef - 1.96 * coef_result$se) * beta_unit)
          ub <- exp((coef_result$coef + 1.96 * coef_result$se) * beta_unit)
          
          # Calculate AF/AN for arbitrary reference (accounting for beta_unit!)
          
          beta <- coef_result$coef # log RR per beta_unit µg/m³
          pm_var <- prov_data[[var_name]]
          deaths_var <- prov_data$deaths
          above_ref <- pm_var > reference & !is.na(pm_var) & !is.na(deaths_var)
          if (any(above_ref)) {
            # The proper scaling:
            rr_t <- exp(beta * (pm_var[above_ref] - reference) / beta_unit)
            af_t <- (rr_t - 1) / rr_t
            deaths_t <- deaths_var[above_ref]
            af_overall <- sum(deaths_t * af_t) / sum(deaths_var, na.rm = TRUE)
            an_overall <- af_overall * sum(deaths_var, na.rm = TRUE)
          } else {
            af_overall <- 0
            an_overall <- 0
          }
          prov_results[i, c("RR", "LB", "UB", "AF", "AN", "p_value")] <-
            list(rr, lb, ub, af_overall, an_overall, coef_result$p_value)
        }
      }
    }
    region_results_list[[prov]] <- prov_results
  }
  
  # Combine all region results
  
  if (length(region_results_list) > 0) {
    region_dlm_results <- do.call(rbind, region_results_list)
  } else {
    stop("No regions with sufficient data for distributed lag analysis")
  }
  
  # Meta-analysis
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
      dplyr::filter(lag_group == lg) %>%
      dplyr::filter(!is.na(RR))
    if (nrow(lag_data) < 2) next
    lag_data$yi <- log(lag_data$RR)
    lag_data$sei <- (log(lag_data$UB) - log(lag_data$LB)) / (2 * 1.96)
    meta_res <- tryCatch({
      rma(yi = yi, sei = sei, data = lag_data, method = "REML")
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




#' @description Create distributed lag plots for national and regions
#' 
#' @param dlm_results Results from analyze_region_distributed_lags_with_af
#' @param output_dir String. Directory to save plots. Default to NULL
#' 
#' @return List of ggplot objects
#' 
plot_distributed_lags_adaptive <- function(dlm_results, output_dir = NULL) {
  
  region_results <- dlm_results$region_results
  meta_results <- dlm_results$meta_results
  
  # Create a list of plots
  plots_list <- list()
  
  # Meta-analysis plot (Countrywide)
  meta_plot_data <- meta_results %>%
    filter(!is.na(RR)) %>%
    mutate(lag_group = factor(lag_group, levels = c("0", "1", "2", "0-2")))
  
  meta_plot <- ggplot(meta_plot_data, aes(x = lag_group, y = RR)) +
    geom_errorbar(aes(ymin = LB, ymax = UB), width = 0.2, color = "darkred") +
    geom_point(size = 2, color = "red") +
    geom_hline(yintercept = 1, linetype = "dashed", color = "red", size = 1) +
    geom_text(aes(label = sprintf("AF: %.1f%%\nAN: %.0f", AF * 100, AN)), 
              vjust = -1.5, size = 2.5) +
    labs(x = "Lag (days)", y = "RR",
         title = "Countrywide") +
    ylim(0.95, 1.35) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 10),
      axis.title = element_text(face = "bold", size = 8),
      axis.text = element_text(size = 8),
      panel.border = element_rect(colour = "black", fill = NA, size = 1)
    )
  
  plots_list[["Countrywide"]] <- meta_plot
  
  # region-specific plots
  
  regions <- unique(region_results$region)
  
  for (prov in regions) {
    prov_data <- region_results %>% 
      filter(region == prov) %>%
      filter(!is.na(RR)) %>%
      mutate(lag_group = factor(lag_group, levels = c("0", "1", "2", "0-2")))
    
    if (nrow(prov_data) == 0) next
    
    prov_plot <- ggplot(prov_data, aes(x = lag_group, y = RR)) +
      geom_errorbar(aes(ymin = LB, ymax = UB), width = 0.2, color = "darkblue") +
      geom_point(size = 2, color = "blue") +
      geom_hline(yintercept = 1, linetype = "dashed", color = "red", size = 1) +
      geom_text(aes(label = sprintf("AF: %.1f%%\nAN: %.0f", AF * 100, AN)), 
                vjust = -1.5, size = 2.5) +
      labs(x = "Lag (days)", y = "RR",
           title = prov) +
      ylim(0.95, 1.35) +
      theme_bw() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 10),
        axis.title = element_text(face = "bold", size = 8),
        axis.text = element_text(size = 8),
        panel.border = element_rect(colour = "black", fill = NA, size = 1)
      )
    
    plots_list[[prov]] <- prov_plot
  }
  
  # Calculate optimal grid dimensions
  n_plots <- length(plots_list)
  grid_dims <- calculate_grid_dims(n_plots)
  
  cat("Creating", grid_dims$nrow, "x", grid_dims$ncol, "grid for", n_plots, "plots\n")
  
  # Create combined plot with adaptive grid
  combined_plot <- gridExtra::grid.arrange(
    grobs = plots_list,
    ncol = grid_dims$ncol, 
    nrow = grid_dims$nrow
  )
  
  # Save plot if output directory specified
  if (!is.null(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    
    # Adjust figure size based on grid dimensions
    fig_width <- 4 * grid_dims$ncol
    fig_height <- 4 * grid_dims$nrow
    
    ggsave(file.path(output_dir, "distributed_lag_plots.png"), 
           combined_plot, width = fig_width, height = fig_height, dpi = 150)
    cat("Distributed lag plots saved to:", file.path(output_dir, "distributed_lag_plots.png"), "\n")
    cat("Grid dimensions:", grid_dims$nrow, "rows x", grid_dims$ncol, "columns\n")
  }
  
  return(list(
    individual_plots = plots_list,
    combined_plot = combined_plot,
    grid_dimensions = grid_dims
  ))
}

#' Create adaptive exposure-response plots with optimal grid layout
#' Updated to show reference-specific AF/AN values
#'
#' @param data_with_lags Data frame containing pre-processed data with exposure lags. 
#'        Must include columns: region, date, deaths, population, and exposure variable.
#' @param meta_results List containing meta-analysis results from previous analysis. 
#'        Should include overall relative risk estimate (`overall_rr`).
#' @param reference_pm25 Numeric value specifying the reference PM2.5 concentration (μg/m³) 
#'        for risk calculations and plot centering.
#' @param reference_name Character string describing the reference scenario 
#'        (e.g., "WHO", "National"). Default: "Reference".
#' @param tlag Integer specifying maximum lag days for distributed lag models. Default: 2.
#' @param vardf Degrees of freedom for exposure-response functions. Default: 3.
#' @param dfseas Seasonal degrees of freedom per year. Default: 4.
#' @param max_ylim Maximum Y-axis limit for relative risk. Default: 2.0.
#' @param output_file Full path for output PNG file. Default: figures/{reference_name}_exposure_response_plots.png
#' @param plot_width_per_panel Width (inches) per panel in output. Default: 4.
#' @param plot_height_per_panel Height (inches) per panel in output. Default: 3.5.
#' @param res Resolution (DPI) for output PNG. Default: 150.
#' @param include_af_an Logical indicating whether to include AF/AN calculations in plots. Default: TRUE.
#' @param pm25_var Name of PM2.5 variable in dataset. Default: "pm25".
#' @param beta_unit Unit change for beta coefficient interpretation. Default: 10 (per 10 μg/m³).
#'
create_exposure_response_plots_adaptive <- function(data_with_lags,
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
                                                    pm25_var = "pm25",
                                                    beta_unit = 10) {
  
  # Input validation
  if (!require(mixmeta, quietly = TRUE)) {
    stop("Package 'mixmeta' is required for meta-analysis")
  }
  
  if (is.null(output_file)) {
    output_file <- paste0("figures/", tolower(reference_name), "_exposure_response_plots.png")
  }
  
  cat("Creating exposure-response plots with", reference_name, "reference (", reference_pm25, "μg/m³)...\n")
  
  # Create output directory if it doesn't exist
  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
  
  # Prepare aggregated data by region and date
  data_aggreg <- data_with_lags %>%
    filter(!is.na(date)) %>%
    group_by(region, date, year, month, day) %>%
    summarise(
      deaths = sum(deaths, na.rm = TRUE),
      pm25 = mean(pm25, na.rm = TRUE),
      tmax = mean(tmax, na.rm = TRUE),
      humidity = mean(humidity, na.rm = TRUE),
      precipitation = mean(precipitation, na.rm = TRUE),
      population = sum(population, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    arrange(region, date) %>%
    group_by(region) %>%
    mutate(days = row_number()) %>%
    ungroup()
  
  # Split data by region
  plist <- data_aggreg %>% split(., .$region)
  prov <- names(plist)
  
  # Set GAM parameters
  varfun <- "bs"
  degree <- 1
  yr <- length(unique(data_aggreg$year))
  knots_values <- c(0.25, 0.5, 0.75)
  
  # Stage 1: Fit GAM models for each region
  cat("Fitting GAM models for", length(prov), "regions...\n")
  
  # Initialize storage
  coef_matrix <- NULL
  vcov_list <- vector("list", length(prov))
  names(vcov_list) <- prov
  region_deaths <- numeric(length(prov))
  names(region_deaths) <- prov
  
  # Storage for reference-specific AF/AN
  region_af_an <- list()
  
  # Fit models for each region
  for (j in 1:length(prov)) {
    dat <- plist[[prov[j]]]
    var1 <- dat$pm25
    region_deaths[j] <- sum(dat$deaths, na.rm = TRUE)
    
    if (nrow(dat) < 100) {
      cat("  Skipping", prov[j], "- insufficient data\n")
      next
    }
    
    cat("  Fitting model for", prov[j], "\n")
    
    # Create cross-basis
    argvar <- list(fun = varfun, degree = degree, knots = quantile(var1, knots_values, na.rm = TRUE))
    arglag <- list(fun = "ns", df = min(2, tlag))
    
    cb <- tryCatch({
      crossbasis(var1, lag = tlag, argvar = argvar, arglag = arglag)
    }, error = function(e) {
      warning("Error creating cross-basis for ", prov[j], ": ", e$message)
      return(NULL)
    })
    
    if (is.null(cb)) next
    
    # Fit model
    fmla_prov <- as.formula(paste(
      "deaths ~ offset(log(population)) + cb +",
      "ns(days, df =", dfseas * yr, ") + factor(year)"
    ))
    
    mod <- tryCatch({
      gam(fmla_prov, data = dat, family = quasipoisson)
    }, error = function(e) {
      warning("Error fitting model for ", prov[j], ": ", e$message)
      return(NULL)
    })
    
    if (is.null(mod)) next
    
    # Reduce cross-basis
    red <- tryCatch({
      crossreduce(cb, mod, cen = reference_pm25)
    }, error = function(e) {
      warning("Error reducing cross-basis for ", prov[j], ": ", e$message)
      return(NULL)
    })
    
    if (is.null(red)) next
    
    # Initialize coefficient matrix on first successful fit
    if (is.null(coef_matrix)) {
      actual_df <- length(coef(red))
      coef_matrix <- matrix(data = NA, nrow = length(prov), ncol = actual_df, 
                            dimnames = list(prov))
    }
    
    # Store results
    coef_matrix[j, ] <- coef(red)
    vcov_list[[j]] <- vcov(red)
    
    # Calculate reference-specific AF/AN for this region
    # Get the coefficient for pm25_var from the original data
    prov_data <- data_with_lags %>% filter(region == prov[j])
    
    # Fit a simple model to get the linear coefficient
    simple_model <- tryCatch({
      fit_region_model(prov_data, pm25_var)
    }, error = function(e) NULL)
    
    if (!is.null(simple_model)) {
      coef_info <- extract_coef_se(simple_model, pm25_var)
      if (!is.na(coef_info$coef)) {
        af_an_ref <- calculate_af_an_for_reference(
          prov_data, 
          coef_info$coef, 
          reference_pm25, 
          beta_unit, 
          pm25_var
        )
        region_af_an[[prov[j]]] <- af_an_ref
      }
    }
    
    rm(dat, var1, argvar, arglag, cb, mod, red, fmla_prov)
  }
  
  # Check if we have enough regions for meta-analysis
  valid_rows <- which(!apply(is.na(coef_matrix), 1, all))
  if (length(valid_rows) < 2) {
    stop("Insufficient regions with successful model fits for meta-analysis")
  }
  
  # Stage 2: Random-effects meta-analysis
  cat("Conducting meta-analysis across", length(valid_rows), "regions...\n")
  
  meta_model <- tryCatch({
    mixmeta(coef_matrix[valid_rows, ] ~ 1, S = vcov_list[valid_rows], 
            control = list(showiter = FALSE))
  }, error = function(e) {
    stop("Error in meta-analysis: ", e$message)
  })
  
  # Best linear unbiased predictions
  blup_results <- blup(meta_model, vcov = TRUE)
  
  # Create nationwide prediction
  t1 <- unlist(lapply(plist[prov[valid_rows]], function(x) x$pm25))
  argvar_national <- list(x = t1, fun = varfun, degree = degree, 
                          knots = quantile(t1, knots_values, na.rm = TRUE))
  bvar_national <- do.call(onebasis, argvar_national)
  
  pred_national <- crosspred(bvar_national, coef = meta_model$coefficients, 
                             vcov = meta_model$vcov, model.link = "log", 
                             by = 0.1, cen = reference_pm25)
  
  # Calculate nationwide AF/AN for the reference
  # Use the overall coefficient from meta_results
  national_af_an <- list(af = 0, an = 0)
  if (!is.null(meta_results)) {
    # Calculate overall AF/AN for reference
    overall_beta <- log(meta_results$overall_rr) / beta_unit
    all_data <- data_with_lags
    national_af_an <- calculate_af_an_for_reference(
      all_data, 
      overall_beta, 
      reference_pm25, 
      beta_unit, 
      pm25_var
    )
  }
  
  # Calculate grid dimensions
  n_regions <- length(valid_rows)
  n_plots <- n_regions + 1  # regions + nationwide
  grid_dims <- calculate_grid_dims(n_plots)
  
  cat("Creating", grid_dims$nrow, "x", grid_dims$ncol, "grid for", n_plots, "plots\n")
  
  # Calculate figure dimensions
  fig_width <- plot_width_per_panel * grid_dims$ncol
  fig_height <- plot_height_per_panel * grid_dims$nrow
  
  # Create the plot
  png(output_file, width = fig_width * res, height = fig_height * res, res = res, bg = "white")
  
  # Set up grid layout
  par(mfrow = c(grid_dims$nrow, grid_dims$ncol), 
      mar = c(3, 3, 3, 3),
      oma = c(4, 4, 3, 1),
      cex.main = 1.1,
      cex.lab = 0.9,
      cex.axis = 0.8,
      mgp = c(2, 0.7, 0),
      tcl = -0.3)
  
  # Store predictions
  predictions <- list()
  plot_count <- 0
  
  # Plot 1: Nationwide
  plot_count <- plot_count + 1
  y_seq <- seq(0, max_ylim, by = 0.5)
  
  plot(pred_national, ylab = "", ylim = c(0.0, max_ylim), 
       xlab = "", xaxt = "n", yaxt = "n",
       main = "Nationwide", 
       lwd = 2, col = "red",
       ci.arg = list(col = rgb(0, 0, 1, 0.2), density = NULL))
  
  abline(h = 1, col = "black", lwd = 1, lty = 1)
  axis(2, at = y_seq, labels = format(y_seq, nsmall = 1), cex.axis = 0.8, las = 1, tck = -0.02)
  axis(1, at = seq(floor(min(t1)), ceiling(max(t1)), by = 20), cex.axis = 0.8, tck = -0.02)
  
  # Add histogram
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
  
  axis(4, at = counts * prop, labels = counts, cex.axis = 0.8, las = 1, tck = -0.02)
  abline(v = reference_pm25, col = "red", lty = 2, lwd = 2)
  abline(v = quantile(t1, c(0.05, 0.95)), col = grey(0.5), lty = 3, lwd = 1.5)
  
  # Add reference-specific AF/AN for nationwide
  if (include_af_an) {
    mtext(sprintf("AF: %.1f%%, AN: %.0f", 
                  national_af_an$af * 100, 
                  national_af_an$an), 
          side = 3, line = -1.5, cex = 0.8)
  }
  
  predictions[["Nationwide"]] <- pred_national
  
  # Plot regional results
  valid_regions <- prov[valid_rows]
  
  for (i in 1:length(valid_regions)) {
    plot_count <- plot_count + 1
    region_name <- valid_regions[i]
    dat <- plist[[region_name]]
    var1 <- dat$pm25
    
    # Create prediction for this region
    argvar <- list(x = var1, fun = varfun, degree = degree, 
                   knots = quantile(var1, knots_values, na.rm = TRUE))
    bvar <- do.call(onebasis, argvar)
    
    # Use BLUP results
    valid_idx <- which(prov[valid_rows] == region_name)
    cp <- crosspred(bvar, coef = blup_results[[valid_idx]]$blup, 
                    vcov = blup_results[[valid_idx]]$vcov,
                    model.link = "log", by = 0.1, cen = reference_pm25)
    
    # Plot regional curve
    plot(cp, "overall", ylim = c(0.0, max_ylim), lwd = 2, col = "red",
         ci.arg = list(col = rgb(0, 0, 1, 0.2), density = NULL),
         xaxt = "n", yaxt = "n", xlab = "", ylab = "",
         main = region_name)
    
    abline(h = 1, col = "black", lwd = 1, lty = 1)
    axis(2, at = y_seq, labels = format(y_seq, nsmall = 1), cex.axis = 0.8, las = 1, tck = -0.02)
    axis(1, at = seq(floor(min(var1)), ceiling(max(var1)), by = 20), cex.axis = 0.8, tck = -0.02)
    
    # Add histogram
    breaks_prov <- c(min(var1) - 1, seq(cp$predvar[1], cp$predvar[length(cp$predvar)], 
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
    
    axis(4, at = counts_prov * prop_prov, labels = counts_prov, cex.axis = 0.8, las = 1, tck = -0.02)
    abline(v = reference_pm25, col = "red", lty = 2, lwd = 2)
    abline(v = quantile(var1, c(0.05, 0.95)), col = grey(0.5), lty = 3, lwd = 1.5)
    
    # Add reference-specific AF/AN for this region
    if (include_af_an && region_name %in% names(region_af_an)) {
      af_an_prov <- region_af_an[[region_name]]
      mtext(sprintf("AF: %.1f%%, AN: %.0f", 
                    af_an_prov$af * 100, 
                    af_an_prov$an), 
            side = 3, line = -1.5, cex = 0.8)
    }
    
    predictions[[region_name]] <- cp
    rm(argvar, bvar, cp, var1)
  }
  
  # Fill empty panels if grid is not completely filled
  if (plot_count < grid_dims$nrow * grid_dims$ncol) {
    remaining_panels <- (grid_dims$nrow * grid_dims$ncol) - plot_count
    for (i in 1:remaining_panels) {
      plot.new()
    }
  }
  
  # Add common labels and title
  mtext(text = "Daily PM2.5 (μg/m³)", side = 1, line = 2.5, outer = TRUE, cex = 1.1)
  mtext(text = "Relative risk", side = 2, line = 2.5, outer = TRUE, cex = 1.1)
  mtext(text = paste0(reference_name, " (PM2.5 reference: ", reference_pm25, " μg/m³)"), 
        side = 3, line = 1, outer = TRUE, cex = 1.2, font = 2)
  
  # Add legend in the last panel if there's space
  if (plot_count < grid_dims$nrow * grid_dims$ncol) {
    # Calculate position for legend in the grid
    legend_row <- ceiling((plot_count + 1) / grid_dims$ncol)
    legend_col <- ((plot_count) %% grid_dims$ncol) + 1
    
    # Set up coordinates for legend panel
    par(mfg = c(legend_row, legend_col))
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    
    legend_items <- c("RR curve", "95% CI", 
                      paste0("Reference (", reference_pm25, " μg/m³)"), 
                      "5th/95th percentile")
    if (include_af_an) {
      legend_items <- c(legend_items, 
                        "AF: Attributable Fraction", 
                        "AN: Attributable Number",
                        "(Values specific to reference level)")
    }
    
    legend("center", 
           legend = legend_items,
           col = c("red", rgb(0, 0, 1, 0.2), "red", grey(0.5), "black", "black", "black"), 
           lty = c(1, NA, 2, 3, NA, NA, NA),
           pch = c(NA, 15, NA, NA, NA, NA, NA),
           pt.bg = c(NA, rgb(0, 0, 1, 0.2), NA, NA, NA, NA, NA),
           pt.cex = c(NA, 2, NA, NA, NA, NA, NA),
           cex = 0.9,
           bg = "white",
           box.lty = 1)
  }
  
  # Close the device
  dev.off()
  
  cat("Exposure-response plots saved to:", output_file, "\n")
  cat("Grid dimensions:", grid_dims$nrow, "rows x", grid_dims$ncol, "columns\n")
  
  # Return predictions and AF/AN values
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

#' Enhanced PM2.5 analysis
#' 
#' @description Extended version of pm25_analysis that includes AF/AN calculations
#' specific to each reference standard
#' 
#' @param ... All parameters from pm25_analysis function
#' @param create_plots Logical. Whether to create exposure-response plots. Default TRUE.
#' @param reference_standards List of reference standards to plot.
#' 
#' @export
pm25_analysis_enhanced <- function(..., 
                                   create_plots = TRUE,
                                   reference_standards = list(
                                     list(value = 15, name = "WHO"),
                                     list(value = 50, name = "Rwanda")
                                   )) {
  
  # Run the analysis with AF/AN
  results <- pm25_analysis(...)
  
  # Create exposure-response plots
  if (create_plots && !is.null(results$meta_analysis)) {
    
    cat("\n=== CREATING ADAPTIVE EXPOSURE-RESPONSE PLOTS ===\n")
    
    results$exposure_response_plots <- list()
    results$reference_specific_af_an <- list()
    
    for (ref_std in reference_standards) {
      
      cat("\nCreating plots for", ref_std$name, "reference standard (", ref_std$value, "μg/m³)...\n")
      
      # Calculate reference-specific AF/AN using the distributed lag function
      ref_af_an <- analyze_region_distributed_lags_with_af(
        results$data, 
        reference = ref_std$value,
        beta_unit = 10
      )
      
      # Store reference-specific results
      results$reference_specific_af_an[[ref_std$name]] <- ref_af_an
      
      # Print summary
      meta_summary <- ref_af_an$meta_results %>% 
        filter(lag_group == "0") %>%
        select(AF, AN)
      
      if (nrow(meta_summary) > 0) {
        cat("  Overall AF for", ref_std$name, "reference:", 
            round(meta_summary$AF * 100, 2), "%\n")
        cat("  Overall AN for", ref_std$name, "reference:", 
            round(meta_summary$AN, 0), "deaths\n")
      }
      
      plot_results <- create_exposure_response_plots_adaptive(
        data_with_lags = results$data,
        meta_results = results$meta_analysis,
        reference_pm25 = ref_std$value,
        reference_name = ref_std$name,
        output_file = file.path(
          ifelse(is.null(list(...)$output_dir), "figures", list(...)$output_dir),
          paste0(tolower(ref_std$name), "_exposure_response_plots.png")
        ),
        include_af_an = TRUE,
        pm25_var = ifelse(is.null(list(...)$pm25_var), "pm25", list(...)$pm25_var)
      )
      
      results$exposure_response_plots[[ref_std$name]] <- plot_results
    }
  }
  
  return(results)
}

#' Create a summary table of AF/AN across different reference standards
#' @param results Results from pm25_analysis_enhanced
#' @param output_file Path to save the summary table
#' @return Data frame with AF/AN for each reference standard
create_reference_summary_table <- function(results, output_file = NULL) {
  
  if (is.null(results$reference_specific_af_an)) {
    stop("No reference-specific AF/AN results found. Run pm25_analysis_enhanced first.")
  }
  
  summary_list <- list()
  
  for (ref_name in names(results$reference_specific_af_an)) {
    ref_data <- results$reference_specific_af_an[[ref_name]]
    
    # Get lag 0 results (current day effect)
    meta_lag0 <- ref_data$meta_results %>% 
      filter(lag_group == "0")
    
    if (nrow(meta_lag0) > 0) {
      summary_list[[ref_name]] <- data.frame(
        Reference = ref_name,
        AF_percent = round(meta_lag0$AF * 100, 2),
        AN_deaths = round(meta_lag0$AN, 0),
        stringsAsFactors = FALSE
      )
    }
  }
  
  summary_table <- do.call(rbind, summary_list)
  
  if (!is.null(output_file)) {
    write.csv(summary_table, output_file, row.names = FALSE)
    cat("\nReference summary table saved to:", output_file, "\n")
  }
  
  print(summary_table)
  return(summary_table)
}

