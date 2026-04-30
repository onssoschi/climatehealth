#' Read in and format data - Mental Health
#'
#' @description Reads in a CSV file for a daily time series of health and climate
#' data, renames columns and creates stratum for case-crossover analysis.
#'
#' @param data_path Path to a csv file containing a daily time series of data
#' for a particular health outcome and climate variables, which may be
#' disaggregated by region.
#' @param date_col Character. Name of the column in the dataframe that contains
#' the date.
#' @param region_col Character. Name of the column in the dataframe that contains
#' the region names. Defaults to NULL.
#' @param temperature_col Character. Name of the column in the dataframe that
#' contains the temperature column.
#' @param health_outcome_col Character. Name of the column in the dataframe that
#' contains the health outcome count column (e.g. number of deaths, hospital
#' admissions).
#' @param population_col Character. Name of the column in the dataframe that
#' contains the population estimate coloumn.
#'
#' @returns A list of dataframes with formatted and renamed columns.
#'
#' @keywords internal
mh_read_and_format_data <- function(
    data_path,
    date_col,
    region_col = NULL,
    temperature_col,
    health_outcome_col,
    population_col) {
  # make sure data_path is a csv if a path is passed
  if (is.character(data_path)) {
    check_file_extension(data_path, ".csv", "data_path")
  }
  # read data
  df <- read_input_data(data_path)
  # process region col
  if (is.null(region_col)) {
    df <- df %>%
      dplyr::mutate(region = "aggregated")
  }
  # subset needed cols
  needed_cols <- c(
    date_col,
    region_col,
    temperature_col,
    health_outcome_col,
    population_col
  )
  standard_cols <- c(
    "date", "region", "tmean", "health_outcome", "population"
  )
  for (i in seq_along(standard_cols)) {
    std_col <- standard_cols[i]
    need_col <- needed_cols[i]
    if (!identical(std_col, need_col) && std_col %in% names(df)) {
      df[[std_col]] <- NULL
    }
  }
  df <- df %>%
    dplyr::rename(
      date = all_of(date_col),
      region = all_of(region_col),
      temp = all_of(temperature_col),
      suicides = all_of(health_outcome_col),
      population = all_of(population_col)
    ) %>%
    dplyr::mutate(
      date = as.Date(date, tryFormats = c("%d/%m/%Y", "%Y-%m-%d")),
      year = as.factor(lubridate::year(date)),
      month = as.factor(lubridate::month(date)),
      dow = as.factor(lubridate::wday(date, label = TRUE)),
      region = as.factor(.data$region),
      stratum = as.factor(.data$region:.data$year:.data$month:.data$dow),
      ind = tapply(.data$suicides, .data$stratum, sum)[.data$stratum]
    )
  df_list <- aggregate_by_column(df, "region")

  return(df_list)
}

#' Create cross-basis matrix
#'
#' @description Creates a cross-basis matrix for each region
#'
#' @param df_list A list of dataframes containing daily timeseries data for a health outcome
#' and climate variables which may be disaggregated by a particular region.
#' @param var_fun Character. Exposure function for argvar
#' (see dlnm::crossbasis). Defaults to 'bs'.
#' @param var_degree Integer. Degree of the piecewise polynomial for argvar
#' (see dlnm::crossbasis). Defaults to 2 (quadratic).
#' @param var_per Vector. Internal knot positions for argvar
#' (see dlnm::crossbasis). Defaults to c(25,50,75).
#' @param lag_fun Character. Exposure function for arglag
#' (see dlnm::crossbasis). Defaults to 'strata'.
#' @param lag_breaks Integer. Internal cut-off point defining the strata for arglag
#' (see dlnm::crossbasis). Defaults to 1.
#' @param lag_days Integer. Maximum lag. Defaults to 2.
#' (see dlnm::crossbasis).
#'
#' @returns A list of cross-basis matrices by region
#'
#' @keywords internal
mh_create_crossbasis <- function(
    df_list,
    var_fun = "bs",
    var_degree = 2,
    var_per = c(25, 50, 75),
    lag_fun = "strata",
    lag_breaks = 1,
    lag_days = 2) {
  cb_list <- list()
  # loop through regions and create cross basis matrices
  for (reg in names(df_list)) {
    region_data <- df_list[[reg]]
    argvar <- list(
      fun = var_fun,
      knots = quantile(region_data$temp, var_per / 100, na.rm = TRUE),
      degree = var_degree
    )
    arglag <- list(fun = lag_fun, breaks = lag_breaks)
    cb <- dlnm::crossbasis(
      region_data$temp,
      lag = lag_days, argvar = argvar, arglag = arglag
    )
    cb_list[[reg]] <- cb
  }
  return(cb_list)
}


#' Produce check results of model combinations
#'
#' @description Runs every combination of model based on user selected additional
#' independent variables and returns model diagnostic checks for each.
#'
#' @param df_list A list of dataframes containing daily timeseries data for a health outcome
#' and climate variables which may be disaggregated by a particular region.
#' @param cb_list List of cross_basis matrices from create_crossbasis function.
#' @param independent_cols Additional independent variables to test in model validation
#' as confounders.
#'
#' @returns
#'  \itemize{
#'   \item `qaic_results` A dataframe of QAIC and dispersion metrics for each model
#'   combination.
#'   \item `residuals_list` A list. Residuals for each model combination.
#'   }
#'
#' @keywords internal
mh_model_combo_res <- function(
    df_list,
    cb_list,
    independent_cols = NULL) {
  qaic_results <- list()
  residuals_list <- list()
  # define var lists for model validation
  if (!is.null(independent_cols)) {
    control_vars <- c(independent_cols)
    transformed_vars <- unlist(lapply(
      independent_cols, function(v) {
        paste0(v, "_ns")
      }
    ))
  } else {
    (transformed_vars <- NULL)
  }

  if (!is.null(independent_cols)) {
    all_combos <- unlist(lapply(0:length(transformed_vars), function(i) {
      combn(transformed_vars, i, simplify = FALSE)
    }), recursive = FALSE)
  } else {
    all_combos <- list(character(0))
  }
  for (reg in names(df_list)) {
    formula_list <- list()
    region_data <- df_list[[reg]]
    cb <- cb_list[[reg]]
    if (!is.null(independent_cols)) {
      for (v in control_vars) {
        ns_matrix <- splines::ns(region_data[[v]], df = 3)
        assign(paste0(v, "_ns"), ns_matrix)
      }
    }
    for (vars in all_combos) {
      # Build the full formula string
      formula_str <- paste(
        "suicides ~ cb", if (length(vars) > 0) paste("+", paste(vars, collapse = " + ")) else ""
      )
      model <- gnm::gnm(as.formula(formula_str),
                        eliminate = region_data$stratum, family = quasipoisson(), data = region_data,
                        na.action = "na.exclude", subset = region_data$ind > 0
      )
      # Get model values
      if (!is.null(independent_cols)) {
        disp <- summary(model)$dispersion
        loglik <- sum(dpois(model$y, model$fitted.values, log = TRUE))
        k <- length(coef(model))
        qaic <- -2 * loglik / disp + 2 * k
        qaic_results[[length(qaic_results) + 1]] <- data.frame(
          region = reg,
          formula = formula_str,
          disp = disp,
          qaic = qaic
        )
      }
      residuals_df <- data.frame(
        region = reg,
        formula = formula_str,
        fitted = fitted(model),
        residuals = residuals(model, type = "deviance")
      )
      formula_list[[formula_str]] <- residuals_df
    }
    residuals_list[[reg]] <- formula_list
  }
  if (!is.null(independent_cols)) {
    # Combine results into a single data frame
    qaic_results <- do.call(rbind, qaic_results)
    # Sort by region and QAIC
    qaic_results <- qaic_results[order(qaic_results$region, qaic_results$formula), ]
  } else {
    qaic_results <- NULL
  }
  return(list(qaic_results, residuals_list))
}

#' Model Validation Assessment
#'
#' @description Produces results on QAIC for each model combination, variance inflation
#' factor for each independent variable, and plots for residuals to assess the models
#'
#' @param df_list A list of dataframes containing daily timeseries data for a health outcome
#' and climate variables which may be disaggregated by a particular region.
#' @param cb_list List of cross_basis matrices from create_crossbasis function.
#' @param independent_cols Additional independent variables to test in model validation
#' as confounders.
#' @param save_fig Boolean. Whether to save the plot as an output. Defaults to
#' FALSE.
#' @param save_csv Boolean. Whether to save the results as a CSV. Defaults to
#' FALSE.
#' @param output_folder_path Path to folder where plots should be saved.
#' Defaults to NULL.
#'
#' @returns
#'   \itemize{
#'   \item `qaic_results` A dataframe of QAIC and dispersion metrics for each model combination and geography.
#'   \item `qaic_summary` A dataframe with the mean QAIC and dispersion metrics for each model combination.
#'   \item `vif_results` A dataframe. Variance inflation factors for each independent variables by region.
#'   \item `vif_summary` A dataframe with the mean variance inflation factors for each independent variable.
#'   }
#'
#' @keywords internal
mh_model_validation <- function(
    df_list,
    cb_list,
    independent_cols = NULL,
    save_fig = FALSE,
    save_csv = FALSE,
    output_folder_path = NULL,
    seed = NULL
) {
  cols <- get_accessible_palette()

  model_combo <- mh_model_combo_res(
    df_list = df_list,
    cb_list = cb_list,
    independent_cols = independent_cols
  )
  qaic_results <- model_combo[[1]]
  residuals_list <- model_combo[[2]]

  # Output directory for validation artifacts
  dir_path <- file.path(output_folder_path, "model_validation")

  # Save QAIC results if applicable
  if (save_csv == TRUE) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)

    if (!is.null(qaic_results)) {
      write.csv(
        qaic_results,
        file = file.path(dir_path, "qaic_results.csv"),
        row.names = FALSE
      )
    }
  }

  # Calculate VIF if independent_cols is not NULL
  if (!is.null(independent_cols)) {
    vif_list <- dlnm_vif(
      df_list = df_list,
      independent_cols = independent_cols
    )

    # Preserve region id if possible
    vif_results <- dplyr::bind_rows(vif_list, .id = "Region")

    if (save_csv == TRUE) {
      write.csv(
        vif_results,
        file = file.path(dir_path, "vif_results.csv"),
        row.names = FALSE
      )
    }
  } else {
    vif_results <- NULL
  }

  # Summaries (only meaningful when there are multiple regions and QAIC exists)
  if (length(df_list) > 1 && !is.null(independent_cols) && !is.null(qaic_results)) {
    qaic_summary <- qaic_results %>%
      dplyr::group_by(.data$formula) %>%
      dplyr::summarise(
        mean_disp = mean(.data$disp, na.rm = TRUE),
        mean_qaic = mean(.data$qaic, na.rm = TRUE),
        .groups = "drop"
      )

    if (save_csv == TRUE) {
      write.csv(
        qaic_summary,
        file = file.path(dir_path, "qaic_summary.csv"),
        row.names = FALSE
      )
    }

    if (!is.null(vif_results)) {
      # If dlnm_vif returns variable_combo use it; otherwise summarise by variable only
      if ("variable_combo" %in% names(vif_results)) {
        vif_summary <- vif_results %>%
          dplyr::group_by(.data$variable_combo, .data$variable) %>%
          dplyr::summarise(mean_vif = mean(.data$vif, na.rm = TRUE), .groups = "drop")
      } else {
        vif_summary <- vif_results %>%
          dplyr::group_by(.data$variable) %>%
          dplyr::summarise(mean_vif = mean(.data$vif, na.rm = TRUE), .groups = "drop")
      }

      if (save_csv == TRUE) {
        write.csv(
          vif_summary,
          file = file.path(dir_path, "vif_summary.csv"),
          row.names = FALSE
        )
      }
    } else {
      vif_summary <- NULL
    }
  } else {
    qaic_summary <- NULL
    vif_summary <- NULL
  }

  # Prepare region labels for saved outputs
  if (save_fig == TRUE) {
    short_labels <- sapply(as.character(names(df_list)), function(x) {
      x_clean <- gsub(" ", "", x)
      if (nchar(x_clean) > 10) substr(x_clean, 1, 10) else x_clean
    })
    named_label_list <- as.list(short_labels)
    names(named_label_list) <- names(df_list)
  }

  # Determine whether to sample residuals for heavy plots
  if ((save_fig == TRUE) &&
      (nrow(do.call(rbind, do.call(rbind, residuals_list))) > 100000)) {
    sample_check <- TRUE
  } else {
    sample_check <- FALSE
  }

  # Plotting loop by region
  for (reg in names(df_list)) {
    region_data <- df_list[[reg]]
    formula_list <- residuals_list[[reg]]

    if (save_fig == TRUE) {
      named_label <- named_label_list[[reg]]
      reg_folder <- gsub(pattern = " ", replacement = "_", x = reg)

      output_folder_main <- file.path(output_folder_path, "model_validation", reg_folder)
      dir.create(output_folder_main, recursive = TRUE, showWarnings = FALSE)
    } else {
      old_par <- graphics::par(no.readonly = TRUE)
      on.exit(graphics::par(old_par), add = TRUE)
      graphics::par(mfrow = c(1, 1), mar = c(5.1, 4.1, 4.1, 2.1), oma = c(0, 0, 0, 0))
    }

    # 1) Residuals vs Date
    if (save_fig == TRUE) {
      output_path <- file.path(output_folder_main, paste0(named_label, "_residuals_timeseries.pdf"))
      open_diag_pdf(output_path, length(formula_list))
    }

    for (i in names(formula_list)) {
      plot(
        x = region_data$date[region_data$ind > 0],
        y = formula_list[[i]]$residuals,
        ylim = c(-5, 10),
        pch = 19,
        cex = 0.25,
        col = cols$deep_water,
        main = unique(formula_list[[i]]$formula),
        ylab = "Deviance residuals",
        xlab = "Date"
      )
      abline(h = 0, lty = 2, lwd = 2, col = cols$text)
    }

    if (save_fig == TRUE) {
      close_diag_pdf(
        title = paste0("Deviance residuals by date, ", reg),
        subtitle = "Each panel shows one model specification.",
        alt_text = paste(
          "Alt text: Multi-panel scatter plot of deviance residuals against date for", reg, ".",
          "Each panel corresponds to a model specification.",
          "A dashed horizontal line marks zero residual."
        )
      )
    }

    # Sampling for fitted and QQ plots
    if (sample_check == TRUE) {
      all_residuals <- do.call(rbind, formula_list)

      if (!is.null(seed)) {
        set.seed(seed)
      }

      sampled_residuals <- all_residuals %>%
        dplyr::group_by(.data$formula) %>%
        dplyr::sample_frac(0.2) %>%
        dplyr::ungroup()

      new_res_list <- split(sampled_residuals, sampled_residuals$formula)
      sample_title <- " (20% sample)"
    } else {
      new_res_list <- formula_list
      sample_title <- ""
    }

    # 2) Residuals vs Fitted
    if (save_fig == TRUE) {
      output_path <- file.path(output_folder_main, paste0(named_label, "_residuals_fitted.pdf"))
      open_diag_pdf(output_path, length(new_res_list))
    }

    for (i in names(new_res_list)) {
      plot(
        x = jitter(new_res_list[[i]]$fitted, amount = 0.5),
        y = jitter(new_res_list[[i]]$residuals, amount = 0.5),
        pch = 19,
        cex = 0.25,
        col = cols$deep_water,
        main = unique(new_res_list[[i]]$formula),
        ylab = "Deviance residuals",
        xlab = "Fitted values"
      )
      abline(h = 0, lty = 2, lwd = 2, col = cols$text)
    }

    if (save_fig == TRUE) {
      close_diag_pdf(
        title = paste0("Deviance residuals by fitted values, ", reg, sample_title),
        subtitle = "Each panel shows one model specification.",
        alt_text = paste(
          "Alt text: Multi-panel scatter plot of deviance residuals against fitted values for", reg, sample_title, ".",
          "Each panel corresponds to a model specification.",
          "A dashed horizontal line marks zero residual."
        )
      )
    }

    # 3) QQ Plot
    if (save_fig == TRUE) {
      output_path <- file.path(output_folder_main, paste0(named_label, "_qq_plot.pdf"))
      open_diag_pdf(output_path, length(new_res_list))
    }

    for (i in names(new_res_list)) {
      qqnorm(
        new_res_list[[i]]$residuals,
        pch = 19,
        cex = 0.25,
        col = cols$deep_water,
        main = unique(new_res_list[[i]]$formula)
      )
      qqline(new_res_list[[i]]$residuals, lwd = 2, col = cols$text)
    }

    if (save_fig == TRUE) {
      close_diag_pdf(
        title = paste0("Normal Q-Q plot of residuals, ", reg, sample_title),
        subtitle = "Each panel shows one model specification.",
        alt_text = paste(
          "Alt text: Multi-panel Q-Q plot of residuals for", reg, sample_title, ".",
          "Each panel compares residual quantiles to a normal reference line."
        )
      )
    }

    # 4) ACF and PACF plots with ACF on the left and PACF on the right
    if (save_fig == TRUE) {
      output_path <- file.path(output_folder_main, paste0(named_label, "_residuals_acf_pacf.pdf"))

      # Two columns so ACF and PACF sit side by side for each model
      open_accessible_pdf(
        file = output_path,
        n_plots = length(formula_list) * 2,
        max_cols = 2,
        panel_width = 6.2,
        panel_height = 5.0,
        mar = c(5.2, 4.8, 3.2, 3.8),
        oma = c(6.2, 0.6, 7.2, 0.6)
      )
    }

    for (i in names(formula_list)) {
      residuals_clean <- stats::na.omit(formula_list[[i]]$residuals)
      spec_label <- unique(formula_list[[i]]$formula)

      stats::acf(
        residuals_clean,
        main = paste0("ACF: ", spec_label),
        col = cols$olive_green
      )

      stats::pacf(
        residuals_clean,
        main = paste0("PACF: ", spec_label),
        col = cols$olive_green
      )
    }

    if (save_fig == TRUE) {
      run_accessible_pdf_plot(
        title = paste0("Residual autocorrelation diagnostics, ", reg),
        subtitle = "Each row shows one model specification. Left panel is ACF and right panel is PACF.",
        line_title = 4.7,
        line_subtitle = 3.0
      )

      add_accessible_alt_text(
        alt_text = paste(
          "Alt text: Multi-panel figure showing residual autocorrelation diagnostics for", reg, ".",
          "Each row corresponds to one model specification.",
          "The left panel shows the autocorrelation function (ACF) of residuals across lags,",
          "and the right panel shows the partial autocorrelation function (PACF)."
        ),
        width = 150
      )

      dev.off()
    }
  }

  return(list(qaic_results, qaic_summary, vif_results, vif_summary))
}


#' Quasi-Poisson Case-Crossover model with DLNM
#'
#' @description Fits a quasi-Poisson case-crossover with a distributed lag
#' non-linear model
#'
#' @param df_list A list of dataframes containing daily timeseries data for a health outcome
#' and climate variables which may be disaggregated by a particular region.
#' @param control_cols A list of confounders to include in the final model adjustment.
#' Defaults to NULL if none.
#' @param cb_list List of cross_basis matrices from create_crossbasis function.
#'
#' @returns List containing models by region
#'
#' @keywords internal
mh_casecrossover_dlnm <- function(
    df_list,
    control_cols = NULL,
    cb_list) {
  model_list <- list()
  spline_list <- c()

  if (!is.null(control_cols)) {
    # normalize type
    if (is.character(control_cols)) {
      control_cols <- c(control_cols)
      transformed_vars <- unlist(lapply(control_cols, function(v) {
        paste0(v, "_bs")
      }))
    }

    # type check column names
    for (col in control_cols) {
      if (!is.character(col)) {
        stop(
          paste0(
            "'control_cols' expected a vector of strings or a string. Got",
            typeof(col)
          )
        )
      }
    }
    for (v in control_cols) {
      spline_var <- paste0("splines::ns(", v, ", df = 3)")
      spline_list[v] <- spline_var
    }
  }

  formula <- as.formula(paste(
    "suicides ~ cb",
    if (length(spline_list) > 0) {
      paste("+", paste(spline_list, collapse = " + "))
    } else {
      ""
    }
  ))

  for (reg in names(df_list)) {
    region_data <- df_list[[reg]]
    cb <- cb_list[[reg]]

    model <- gnm::gnm(formula,
                      eliminate = region_data$stratum, family = quasipoisson(), data = region_data,
                      na.action = "na.exclude", subset = region_data$ind > 0
    )
    model_list[[reg]] <- model
  }

  return(model_list)
}

#' Run regional predictions from model
#'
#' @description Use model to run regional predictions
#'
#' @param df_list A list of dataframes containing daily timeseries data for a health outcome
#' and climate variables which may be disaggregated by a particular region.
#' @param var_fun Character. Exposure function for argvar
#' (see dlnm::crossbasis). Defaults to 'bs'.
#' @param var_per Vector. Internal knot positions for argvar
#' (see dlnm::crossbasis). Defaults to c(25,50,75).
#' @param var_degree Integer. Degree of the piecewise polynomial for argvar
#' (see dlnm::crossbasis). Defaults to 2 (quadratic).
#' @param minpercreg Vector. Percentile of maximum suicide temperature for each region.
#' @param blup A list. BLUP (best linear unbiased predictions) from the
#' meta-analysis model for each region.
#' @param coef_ A matrix of coefficients for the reduced model.
#' @param vcov_ A list. Covariance matrices for each region for the reduced model.
#' @param meta_analysis Boolean. Whether to perform a meta-analysis.
#'
#' @returns A list containing predictions by region
#'
#' @keywords internal
mh_predict_reg <- function(
    df_list,
    var_fun = "bs",
    var_per = c(25, 50, 75),
    var_degree = 2,
    minpercreg,
    blup,
    coef_,
    vcov_,
    meta_analysis = FALSE) {
  if (meta_analysis == TRUE) {
    coef_list <- lapply(blup, function(x) x$blup)
    vcov_list <- lapply(blup, function(x) x$vcov)
  } else {
    coef_list <- split(coef_, rownames(coef_))
    vcov_list <- vcov_
  }

  pred_list <- list()

  for (reg in names(df_list)) {
    region_data <- df_list[[reg]]

    argvar <- list(
      x = region_data$temp,
      fun = var_fun,
      knots = quantile(region_data$temp, var_per / 100, na.rm = TRUE),
      degree = var_degree
    )

    bvar <- do.call(dlnm::onebasis, argvar)

    cen <- quantile(region_data$temp, minpercreg[reg] / 100, na.rm = TRUE)

    pred <- dlnm::crosspred(bvar,
                            coef = coef_list[[reg]],
                            vcov = vcov_list[[reg]],
                            model.link = "log",
                            by = 0.1,
                            cen = cen,
                            from = min(round(region_data$temp, 1), na.rm = TRUE),
                            to = max(round(region_data$temp, 1), na.rm = TRUE)
    )

    pred_list[[reg]] <- pred
  }

  return(pred_list)
}


#' Process data for national analysis
#'
#' @description Aggregate to national data and run crossbasis
#'
#' @param df_list A list of dataframes containing daily timeseries data for a health outcome
#' and climate variables which may be disaggregated by a particular region.
#' @param pop_list List of population totals by year and region.
#' @param var_fun Character. Exposure function for argvar
#' (see dlnm::crossbasis). Defaults to 'bs'.
#' @param var_per Vector. Internal knot positions for argvar
#' (see dlnm::crossbasis). Defaults to c(25,50,75).
#' @param var_degree Integer. Degree of the piecewise polynomial for argvar
#' (see dlnm::crossbasis). Defaults to 2 (quadratic).
#' @param lag_fun Character. Exposure function for arglag
#' (see dlnm::crossbasis). Defaults to 'strata'.
#' @param lag_breaks Integer. Internal cut-off point defining the strata for arglag
#' (see dlnm::crossbasis). Defaults to 1.
#' @param lag_days Integer. Maximum lag. Defaults to 2.
#' (see dlnm::crossbasis).
#' @param country Character. Name of country for national level estimates.
#' @param cb_list A list of cross-basis matrices by region.
#' @param mm A model object. A multivariate meta-analysis model.
#' @param minpercreg Vector. Percentile of maximum suicide temperature for each region.
#'
#' @returns
#' \itemize{
#'   \item `df_list` List. A list of data frames for each region and nation.
#'   \item `cb_list` List. A list of cross-basis matrices by region and nation.
#'   \item `minpercreg` Vector. Percentile of minimum suicide temperature for each region and nation.
#'   \item `mmpredall` List. A list of national coefficients and covariance matrices.
#'   }
#'
#' @keywords internal
mh_add_national_data <- function(
    df_list,
    pop_list,
    var_fun = "bs",
    var_per = c(25, 50, 75),
    var_degree = 2,
    lag_fun = "strata",
    lag_breaks = 1,
    lag_days = 2,
    country = "National",
    cb_list,
    mm,
    minpercreg) {
  # Aggregate national level data
  national_data <- as.data.frame(do.call(rbind, df_list))

  nat_pop <- pop_list[[country]] %>%
    dplyr::rename(nat_population = all_of("population"))

  national_data <- national_data %>%
    dplyr::left_join(nat_pop, by = "year") %>%
    dplyr::mutate(
      weight = .data$population / .data$nat_population,
      weighted_temp = .data$temp * .data$weight
    ) %>%
    dplyr::group_by(.data$date) %>%
    dplyr::summarise(
      temp = round(sum(.data$weighted_temp, na.rm = TRUE), 2),
      suicides = sum(.data$suicides, na.rm = TRUE),
      population = unique(.data$nat_population)
    ) %>%
    dplyr::mutate(
      year = as.factor(lubridate::year(.data$date)),
      month = as.factor(lubridate::month(.data$date)),
      region = country
    )

  df_list[[country]] <- as.data.frame(national_data)

  # Create cross basis for national data
  argvar <- list(
    fun = var_fun,
    knots = quantile(national_data$temp, var_per / 100, na.rm = TRUE),
    degree = var_degree
  )
  arglag <- list(fun = lag_fun, breaks = lag_breaks)

  cb_list[[country]] <- dlnm::crossbasis(national_data$temp, lag = lag_days, argvar = argvar, arglag = arglag)

  # Add national min and max suicide temperatures
  predvar <- quantile(national_data$temp, 1:99 / 100, na.rm = TRUE)

  argvar <- list(
    x = predvar,
    fun = var_fun,
    knots = quantile(national_data$temp, var_per / 100, na.rm = TRUE),
    degree = var_degree,
    Boundary.knots = range(national_data$temp, na.rm = TRUE)
  )

  bvar <- do.call(dlnm::onebasis, argvar)

  datanew <- data.frame(
    temp_avg = mean(national_data$temp),
    temp_range = diff(range(national_data$temp, na.rm = TRUE))
  )

  mmpredall <- predict(mm, datanew, vcov = TRUE, format = "list")

  minpercnat <- (1:50)[which.min((bvar %*% mmpredall$fit)[1:50, ])]

  minpercreg[[country]] <- minpercnat

  return(list(df_list, cb_list, minpercreg, mmpredall))
}

#' Plot power
#'
#' @description Plots the power statistic for each reference temperature at and above
#' the attributable risk threshold for each area.
#'
#' @param power_list A list containing power information by area.
#' @param save_fig Boolean. Whether to save the plot as an output. Defaults to
#' FALSE.
#' @param output_folder_path Path to folder where plots should be saved.
#' Defaults to NULL.
#' @param country Character. Name of country for national level estimates.
#' Defaults to 'National'
#'
#' @returns Plots of power by temperature for the attributable threshold and above
#' for each area.
#'
#' @keywords internal
mh_plot_power <- function(
    power_list,
    save_fig = FALSE,
    output_folder_path = NULL,
    country = "National"
) {
  cols <- get_accessible_palette()

  if (save_fig == TRUE) {
    dir.create(
      file.path(output_folder_path, "model_validation"),
      recursive = TRUE,
      showWarnings = FALSE
    )

    open_accessible_pdf(
      file = file.path(output_folder_path, "model_validation", "power_vs_temperature.pdf"),
      n_plots = length(power_list),
      max_cols = 2,
      panel_width = 6.8,
      panel_height = 6.2,
      mar = c(6.8, 5, 3.2, 4.2),
      oma = c(7.5, 0.6, 9, 0.6)
    )
  } else {
    old_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old_par), add = TRUE)
  }

  for (reg in names(power_list)) {
    df <- power_list[[reg]]
    df <- df[order(df$temperature), ]

    plot(
      x = df$temperature,
      y = df$power,
      type = "l",
      xlab = "Temperature",
      ylab = "Power (%)",
      main = reg,
      col = cols$deep_water,
      ylim = c(0, 100),
      lwd = 2
    )

    abline(
      h = 80,
      col = cols$text,
      lty = 2,
      lwd = 1.5
    )
  }

  if (save_fig == TRUE) {
    main_title <- paste0("Power vs temperature by area, ", country)
    sub_title <- paste(
      "Each panel shows the power statistic across temperature values.",
      "The dashed horizontal line marks the 80% power reference threshold."
    )

    run_accessible_pdf_plot(
      title = main_title,
      subtitle = sub_title,
      line_title = 4.9,
      line_subtitle = 3.2
    )

    add_figure_legend(
      legend = c("Power curve", "80% reference threshold"),
      col = c(cols$deep_water, cols$text),
      lty = c(1, 2),
      lwd = c(2, 1.5),
      pch = c(NA, NA),
      pt.cex = c(1.2, 1.2),
      cex = 1.05,
      seg.len = 2,
      inset = 0.03,
      text.col = cols$text,
      vpad = 0.028,
      bty = "o"
    )

    add_accessible_alt_text(
      alt_text = paste(
        "Alt text: Multi-panel line chart showing statistical power against temperature by area.",
        "Each panel contains a line showing power percentage across temperatures.",
        "A dashed horizontal line marks the 80 percent reference threshold."
      )
    )

    dev.off()
  }
}


#' Produce cumulative relative risk results of analysis
#'
#' @description Produces cumulative relative risk and confidence intervals
#' from analysis.
#'
#' @param pred_list A list containing predictions from the model by region.
#' @param df_list A list of dataframes containing daily timeseries data for a health outcome
#' and climate variables which may be disaggregated by a particular region.
#' @param attr_thr Integer. Percentile at which to define the temperature threshold for
#' calculating attributable risk. Defaults to 97.5.
#' @param minpercreg Vector. Percentile of minimum suicide temperature for each area.
#'
#' @returns Dataframe containing cumulative relative risk and confidence
#' intervals from analysis.
#'
#' @keywords internal
mh_rr_results <- function(
    pred_list,
    df_list,
    attr_thr = 97.5,
    minpercreg) {
  rr_results <- bind_rows(lapply(names(pred_list), function(region_name) {
    reg_pred <- pred_list[[region_name]]
    region_temp <- df_list[[region_name]]$temp

    min_st <- quantile(region_temp, minpercreg[region_name] / 100, na.rm = TRUE)
    attr_thr_temp <- quantile(region_temp, attr_thr / 100, na.rm = TRUE)

    temp_rounded <- round(region_temp, 1)
    temp_freq_table <- table(temp_rounded)

    pred_temp_rounded <- round(reg_pred$predvar, 1)
    temp_freq <- as.numeric(temp_freq_table[as.character(pred_temp_rounded)])
    temp_freq[is.na(temp_freq)] <- 0 # Replace NAs with 0 for bins not present

    n <- length(reg_pred$predvar)
    df <- data.frame(
      Area = rep(region_name, n),
      MinST = rep(round(min_st, 1), n),
      Attr_Threshold_Temp = rep(round(attr_thr_temp, 1), n),
      Temperature = round(reg_pred$predvar, 1),
      Temp_Frequency = temp_freq,
      RR = round(reg_pred$allRRfit, 2),
      RR_lower_CI = round(reg_pred$allRRlow, 2),
      RR_upper_CI = round(reg_pred$allRRhigh, 2)
    )

    return(df)
  }))

  rownames(rr_results) <- NULL

  return(rr_results)
}


#' Plot results of relative risk analysis - Mental Health
#'
#' @description Plots cumulative lag exposure-response function with histogram of
#' temperature distribution for each region
#'
#' @param df_list A list of dataframes containing daily timeseries data for a
#' health outcome and climate variables which may be disaggregated by a
#' particular region.
#' @param pred_list A list containing predictions from the model by region.
#' @param attr_thr Integer. Percentile at which to define the temperature
#' threshold for calculating attributable risk. Defaults to 97.5.
#' @param minpercreg Vector. Percentile of minimum suicide temperature for each
#' area.
#' @param country Character. Name of country for national level estimates.
#' Defaults to 'National'.
#' @param save_fig Boolean. Whether to save the plot as an output. Defaults to
#' FALSE.
#' @param output_folder_path Path to folder where plots should be saved.
#' Defaults to NULL.
#'
#' @returns Plots of cumulative lag exposure-response function with histogram of
#' temperature distribution for each region
#'
#' @keywords internal
mh_plot_rr <- function(
    df_list,
    pred_list,
    attr_thr = 97.5,
    minpercreg,
    country = "National",
    save_fig = FALSE,
    output_folder_path = NULL
) {
  cols <- get_accessible_palette()

  if (!save_fig) {
    old_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old_par), add = TRUE)
  }

  xlim <- c(
    min(sapply(pred_list, function(x) min(x$predvar, na.rm = TRUE))),
    max(sapply(pred_list, function(x) max(x$predvar, na.rm = TRUE)))
  )

  ylim <- c(
    min(c(min(sapply(pred_list, function(x) min(x$allRRfit, na.rm = TRUE))) - 0.5, 0.4)),
    max(c(max(sapply(pred_list, function(x) max(x$allRRfit, na.rm = TRUE))) + 0.5, 2.1))
  )

  hist_max <- max(unlist(lapply(df_list, function(x) {
    temp_range <- range(x$temp, na.rm = TRUE)
    breaks <- seq(floor(temp_range[1]), ceiling(temp_range[2]), by = 1)
    hist(x$temp, breaks = breaks, plot = FALSE)$counts
  })), na.rm = TRUE)

  if (save_fig == TRUE) {
    open_accessible_pdf(
      file = file.path(output_folder_path, "suicides_rr_plot.pdf"),
      n_plots = length(pred_list),
      max_cols = 2,
      panel_width = 7.8,
      panel_height = 5.4,
      mar = c(5.4, 5.4, 3.8, 4.4),
      oma = c(7.5, 0.6, 9, 0.6)
    )
  }

  for (reg in names(pred_list)) {
    region_pred <- pred_list[[reg]]
    region_temp <- df_list[[reg]]$temp

    graphics::par(mar = c(5, 5, 4, 5) + 0.1)

    # Base RR curve with crosspred plot
    plot(
      region_pred,
      "overall",
      xlab = expression(paste("Temperature (", degree, "C)")),
      ylab = "Relative risk",
      ylim = ylim,
      xlim = xlim,
      main = reg,
      col = cols$deep_water
    )

    # Attributable risk threshold line
    vline_pos_thr_x <- quantile(region_temp, attr_thr / 100, na.rm = TRUE)
    vline_pos_thr_y <- max(region_pred$allRRfit, na.rm = TRUE) + 0.3
    vline_lab_thr <- paste0(
      "Attributable risk threshold\n",
      round(vline_pos_thr_x, 1), intToUtf8(176), "C (p", attr_thr, ")"
    )

    graphics::abline(v = vline_pos_thr_x, col = cols$text, lty = 2, lwd = 1.5)
    graphics::text(
      x = vline_pos_thr_x,
      y = vline_pos_thr_y,
      labels = vline_lab_thr,
      pos = 2,
      col = cols$text,
      cex = 0.8,
      font = 2
    )

    # Minimum suicide temperature (centering percentile) line
    vline_pos_min_x <- quantile(region_temp, minpercreg[reg] / 100, na.rm = TRUE)
    min_rr <- min(region_pred$allRRfit, na.rm = TRUE)

    if (dplyr::between(min_rr, 0.90, 1.1)) {
      vline_pos_min_y <- min_rr - 0.2
    } else {
      vline_pos_min_y <- min_rr - 0.1
    }

    vline_lab_min <- paste0(
      "Minimum ST\n",
      round(vline_pos_min_x, 1), intToUtf8(176), "C (p", round(minpercreg[reg], 2), ")"
    )

    graphics::abline(v = vline_pos_min_x, col = cols$text, lty = 3, lwd = 1.5)
    graphics::text(
      x = vline_pos_min_x,
      y = vline_pos_min_y,
      labels = vline_lab_min,
      pos = 4,
      col = cols$text,
      cex = 0.8,
      font = 2
    )

    # Histogram overlay (scaled to sit at bottom of RR plot)
    reg_temp_range <- range(region_temp, na.rm = TRUE)

    hist_data <- hist(
      region_temp,
      breaks = seq(floor(reg_temp_range[1]), ceiling(reg_temp_range[2]), by = 1),
      plot = FALSE
    )

    hist_scale <- 0.4 / hist_max
    scaled_counts <- hist_data$counts * hist_scale

    for (i in seq_along(hist_data$counts)) {
      graphics::rect(
        xleft = hist_data$breaks[i],
        xright = hist_data$breaks[i + 1],
        ybottom = ylim[1],
        ytop = ylim[1] + scaled_counts[i],
        col = cols$dusky_rose,
        border = "white"
      )
    }

    # Right axis for histogram frequency
    axis_labels <- pretty(c(0, hist_max), n = 2)
    axis_scaled <- c(ylim[1], ylim[1] + (axis_labels[-1] * hist_scale))

    add_right_axis_label(
      at = axis_scaled,
      labels = axis_labels,
      ylim = ylim,
      side_label = "Frequency (days)",
      axis_col = cols$text,
      cex_axis = 0.85,
      cex_label = 0.75,
      line = 2.9
    )
  }

  if (save_fig == TRUE) {
    year_range <- paste0(
      "(",
      min(sapply(df_list, function(x) min(lubridate::year(x$date), na.rm = TRUE))),
      "-",
      max(sapply(df_list, function(x) max(lubridate::year(x$date), na.rm = TRUE))),
      ")"
    )

    main_title <- paste0(
      "Relative risk of suicide by mean temperature and area, ",
      country, " ", year_range
    )

    sub_title <- paste(
      "Each panel shows a relative risk curve with vertical reference lines for",
      "the attributable risk threshold and the minimum suicide temperature, plus a histogram of observed temperatures."
    )

    run_accessible_pdf_plot(
      title = main_title,
      subtitle = sub_title,
      line_title = 4.9,
      line_subtitle = 3.2
    )

    # Legend nudged down a bit using vpad
    add_figure_legend(
      legend = c(
        "Relative risk curve",
        "Attributable risk threshold",
        "Minimum suicide temperature",
        "Histogram (temperature frequency)"
      ),
      col = c(cols$deep_water, cols$text, cols$text, cols$dusky_rose),
      lty = c(1, 2, 3, NA),
      lwd = c(2.5, 1.5, 1.5, NA),
      pch = c(NA, NA, NA, 15),
      pt.cex = c(1.2, 1.2, 1.2, 1.7),
      cex = 1.03,
      seg.len = 2,
      text.col = cols$text,
      vpad = 0.033,
      bty = "o"
    )

    add_accessible_alt_text(
      alt_text = paste(
        "Alt text: Multi-panel figure showing relative risk of suicide across temperature for each area.",
        "Each panel contains a relative risk curve and two vertical reference lines marking the attributable risk threshold",
        "and the minimum suicide temperature. A histogram at the bottom of each panel shows the frequency of observed temperatures.",
        "A right-hand axis reports histogram frequency in days."
      ),
      width = 200
    )

    grDevices::dev.off()
  }
}


#' Estimate attributable numbers
#'
#' @description Estimate attributable numbers for each region and confidence
#' intervals using Monte Carlo simulations.
#'
#' @param df_list A list of dataframes containing daily timeseries data for a health outcome
#' and climate variables which may be disaggregated by a particular region.
#' @param cb_list A list of cross-basis matrices by region.
#' @param pred_list A list containing predictions from the model by region.
#' @param minpercreg Vector. Percentile of maximum suicide temperature for each region.
#' @param attr_thr Integer. Percentile at which to define the temperature threshold for
#' calculating attributable risk. Defaults to 97.5.
#'
#' @returns A list containing attributable numbers per region
#'
#' @keywords internal
mh_attr <- function(
    df_list,
    cb_list,
    pred_list,
    minpercreg,
    attr_thr = 97.5) {
  attr_list <- list()

  for (reg in names(df_list)) {
    region_data <- df_list[[reg]]
    cb <- cb_list[[reg]]
    pred <- pred_list[[reg]]
    minperc <- minpercreg[reg]

    cen <- quantile(region_data$temp, minperc / 100, na.rm = TRUE)
    min_range <- quantile(region_data$temp, attr_thr / 100, na.rm = TRUE)
    max_range <- max(region_data$temp, na.rm = TRUE)

    attr <- an_attrdl(
      x = region_data$temp,
      basis = cb,
      cases = region_data$suicides,
      coef = pred$coefficients,
      vcov = pred$vcov,
      dir = "forw",
      cen = cen,
      range = c(min_range, max_range),
      tot = FALSE,
      nsim = 1000
    )
    results <- region_data %>%
      select(all_of(c("region", "date", "temp", "year", "month", "suicides", "population"))) %>%
      mutate(
        threshold_temp = round(min_range, 1),
        af = attr[[1]],
        af_lower_ci = attr[[2]],
        af_upper_ci = attr[[3]],
        an = attr[[4]],
        an_lower_ci = attr[[5]],
        an_upper_ci = attr[[6]],
        ar = (attr[[4]] / .data$population) * 100000,
        ar_lower_ci = (attr[[5]] / .data$population) * 100000,
        ar_upper_ci = (attr[[6]] / .data$population) * 100000
      )
    attr_list[[reg]] <- list(
      results = results,
      ansim_mat = attr[[7]]
    )
  }
  return(attr_list)
}


#' Create attributable estimates tables
#'
#' @description Aggregate tables of attributable numbers, rates and fractions
#' for total, yearly and monthly by region and nation
#'
#' @param attr_list A list containing attributable numbers per region.
#' @param country Character. Name of country for national level estimates.
#' Defaults to 'National'.
#' @param meta_analysis Boolean. Whether to perform a meta-analysis. Defaults
#' to FALSE.
#'
#' @returns
#' \itemize{
#'   \item `res_attr_tot` Dataframe. Total attributable fractions, numbers and
#'   rates for each area over the whole time series.
#'   \item `attr_yr_list` List. Dataframes containing yearly estimates of
#'   attributable fractions, numbers and rates by area.
#'   \item `attr_mth_list` List. Dataframes containing total attributable
#'   fractions, numbers and rates by calendar month and area.
#'   }
#'
#' @keywords internal
mh_attr_tables <- function(
    attr_list,
    country = "National",
    meta_analysis = FALSE) {
  attr_res <- do.call(rbind, lapply(attr_list, `[[`, "results")) %>%
    mutate(year = as.numeric(as.character(.data$year)))

  ansim_mats <- lapply(attr_list, `[[`, "ansim_mat")
  ansim_all <- do.call(rbind, ansim_mats)

  attr_res$sim_index <- seq_len(nrow(attr_res))

  res_list <- list()

  groupings <- list(
    monthly = rlang::quos(.data$month, .data$region),
    yearly  = rlang::quos(.data$year, .data$region),
    overall = rlang::quos(.data$region)
  )

  for (grp_name in names(groupings)) {
    # Group rows
    grouped <- attr_res %>%
      group_by(!!!groupings[[grp_name]]) %>%
      summarise(
        population = round(mean(.data$population, na.rm = TRUE), 0),
        temp = round(mean(.data$temp, na.rm = TRUE), 1),
        threshold_temp = mean(.data$threshold_temp, na.rm = TRUE),
        suicides = sum(.data$suicides, na.rm = TRUE),
        an = sum(.data$an, na.rm = TRUE),
        sim_rows = list(.data$sim_index)
      )

    # Compute CI from simulation matrix
    grouped <- grouped %>%
      rowwise() %>%
      mutate(
        sim_sum = list(colSums(ansim_all[unlist(.data$sim_rows), , drop = FALSE], na.rm = TRUE)),
        an_lower_ci = quantile(unlist(.data$sim_sum), probs = 0.025, na.rm = TRUE),
        an_upper_ci = quantile(unlist(.data$sim_sum), probs = 0.975, na.rm = TRUE)
      ) %>%
      ungroup() %>%
      mutate(
        af = .data$an / .data$suicides * 100,
        af_lower_ci = .data$an_lower_ci / .data$suicides * 100,
        af_upper_ci = .data$an_upper_ci / .data$suicides * 100,
        ar = .data$an / .data$population * 100000,
        ar_lower_ci = .data$an_lower_ci / .data$population * 100000,
        ar_upper_ci = .data$an_upper_ci / .data$population * 100000
      ) %>%
      select(-all_of("sim_rows"), -all_of("sim_sum"))

    res_list[[grp_name]] <- grouped
  }
  if (meta_analysis == TRUE) {
    region_order <- c(sort(setdiff(names(attr_list), country)), country)
  } else {
    region_order <- sort(names(attr_list))
  }

  res_attr_tot <- res_list[["overall"]]

  attr_yr_list <- aggregate_by_column(res_list[["yearly"]], "region")
  attr_yr_list <- attr_yr_list[region_order]

  attr_mth_list <- res_list[["monthly"]] %>%
    dplyr::mutate(month = month.name[.data$month]) %>%
    aggregate_by_column("region")
  attr_mth_list <- attr_mth_list[region_order]

  return(list(res_attr_tot, attr_yr_list, attr_mth_list))
}


#' Plot total attributable fractions and rates
#'
#' @description Plot total attributable fractions and rates over the whole time series by area.
#'
#' @param df_list A list of dataframes containing daily timeseries data for a health outcome
#' and climate variables which may be disaggregated by a particular region.
#' @param res_attr_tot Matrix containing total attributable fractions, numbers and rates for each
#' area over the whole time series.
#' @param save_fig Boolean. Whether to save the plot as an output. Defaults to
#' FALSE.
#' @param output_folder_path Path to folder where plots should be saved.
#' Defaults to NULL.
#' @param country Character. Name of country for national level estimates.
#' Defaults to 'National'.
#'
#' @returns Plots of total attributable fractions and rates by area
#'
#' @keywords internal
mh_plot_attr_totals <- function(
    df_list,
    res_attr_tot,
    save_fig = FALSE,
    output_folder_path = NULL,
    country = "National"
) {
  cols <- get_accessible_palette()

  if (!save_fig) {
    old_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old_par), add = TRUE)
    graphics::par(mfrow = c(2, 1))
  }

  num_regions <- nrow(res_attr_tot)

  # Shorten labels to keep the y-axis readable
  short_labels <- sapply(as.character(res_attr_tot$region), function(x) {
    if (nchar(x) > 28) paste0(substr(x, 1, 28), "...") else x
  })
  names(short_labels) <- NULL

  year_range <- paste0(
    "(",
    min(sapply(df_list, function(x) min(lubridate::year(x$date), na.rm = TRUE))),
    "-",
    max(sapply(df_list, function(x) max(lubridate::year(x$date), na.rm = TRUE))),
    ")"
  )

  # CI range warnings (overall)
  af_ci_range <- c(min(res_attr_tot$af_lower_ci, na.rm = TRUE),
                   max(res_attr_tot$af_upper_ci, na.rm = TRUE))
  ar_ci_range <- c(min(res_attr_tot$ar_lower_ci, na.rm = TRUE),
                   max(res_attr_tot$ar_upper_ci, na.rm = TRUE))

  af_warning <- sprintf("Warning: AF CI's range from %.2f%% to %.2f%%",
                        af_ci_range[1], af_ci_range[2])
  ar_warning <- sprintf("Warning: AR CI's range from %.2f to %.2f per 100,000",
                        ar_ci_range[1], ar_ci_range[2])
  ovr_warning <- "(Please refer to the associated data table for more information on the uncertainty around each estimate)"

  if (save_fig == TRUE) {
    # Make the plot taller when there are many regions
    panel_height <- max(7.5, 0.45 * num_regions)

    open_accessible_pdf(
      file = file.path(output_folder_path, "suicides_total_attr_plot.pdf"),
      n_plots = 2,
      max_cols = 1,
      panel_width = 13,
      panel_height = panel_height,
      mar = c(7.5, 12, 5, 2),
      oma = c(7.5, 1, 4.8, 1)
    )
  } else {
    graphics::par(mar = c(7.5, 12, 2.5, 2))
  }

  # Panel 1: AF (horizontal bar)
  sorted_indices_af <- order(res_attr_tot$af, decreasing = FALSE)
  res_af_tot <- res_attr_tot[sorted_indices_af, ]
  short_labs_af <- short_labels[sorted_indices_af]

  bar_col_af <- rep(cols$deep_water, length(short_labs_af))
  nat_ind_af <- which(res_af_tot$region == country)
  if (length(nat_ind_af) > 0) bar_col_af[nat_ind_af] <- cols$olive_green

  graphics::barplot(
    names.arg = short_labs_af,
    height = res_af_tot$af,
    xlab = "Attributable fraction (AF, %)",
    main = paste0("Attributable fraction of suicides by area, ", country, " ", year_range),
    col = bar_col_af,
    las = 1,
    horiz = TRUE,
    xlim = c(0, max(res_af_tot$af, na.rm = TRUE) * 1.15),
    cex.main = 1.25
  )

  graphics::mtext(af_warning, side = 1, line = 4.2, cex = 1, col = "red", font = 3)
  graphics::mtext(ovr_warning, side = 1, line = 5.35, cex = 1, col = "red", font = 3)

  # Panel 2: AR (horizontal bar)
  if (save_fig == TRUE) {
    graphics::par(mar = c(7.5, 12, 5, 2))
  }

  sorted_indices_ar <- order(res_attr_tot$ar, decreasing = FALSE)
  res_ar_tot <- res_attr_tot[sorted_indices_ar, ]
  short_labs_ar <- short_labels[sorted_indices_ar]

  bar_col_ar <- rep(cols$dusky_rose, length(short_labs_ar))
  nat_ind_ar <- which(res_ar_tot$region == country)
  if (length(nat_ind_ar) > 0) bar_col_ar[nat_ind_ar] <- cols$olive_green

  graphics::barplot(
    names.arg = short_labs_ar,
    height = res_ar_tot$ar,
    xlab = "Attributable rate (AR, per 100,000 population)",
    main = paste0("Attributable rate of suicides by area, ", country, " ", year_range),
    col = bar_col_ar,
    las = 1,
    horiz = TRUE,
    xlim = c(0, max(res_ar_tot$ar, na.rm = TRUE) * 1.15),
    cex.main = 1.25
  )

  graphics::mtext(ar_warning, side = 1, line = 4.2, cex = 1, col = "red", font = 3)
  graphics::mtext(ovr_warning, side = 1, line = 5.35, cex = 1, col = "red", font = 3)

  # Figure title, legend, alt text
  if (save_fig == TRUE) {
    main_title <- paste0("Suicides attributable to extreme heat by area, ", country, " ", year_range)
    sub_title <- paste(
      "Top panel shows attributable fraction (AF) and bottom panel shows attributable rate (AR).",
      "The highlighted bar represents", country, "."
    )

    run_accessible_pdf_plot(
      title = main_title,
      subtitle = sub_title,
      line_title = 2.2,
      line_subtitle = 1.15
    )

    # Legend moved down a bit using vpad
    add_figure_legend(
      legend = c("Area estimate", paste0("Highlighted: ", country)),
      col = c(cols$deep_water, cols$olive_green),
      lty = c(NA, NA),
      lwd = c(NA, NA),
      pch = c(15, 15),
      pt.cex = 1.6,
      cex = 1.02,
      seg.len = 2,
      text.col = cols$text,
      vpad = 0.055,
      bty = "o"
    )

    add_accessible_alt_text(
      alt_text = paste(
        "Alt text: Two-panel horizontal bar chart of suicides attributable to extreme heat by area.",
        "The top panel shows attributable fraction in percent and the bottom panel shows attributable rate per 100,000 population.",
        "Areas are listed on the y-axis.",
        "The", country, "bar is highlighted in green.",
        "Warning text below each panel summarises the overall confidence interval range."
      ),
      width = 170
    )

    grDevices::dev.off()
  }
}

#' Plot attributable fractions by year
#'
#' @description Plot attributable fractions by year and area with confidence intervals
#'
#' @param attr_yr_list A list of matrices containing yearly estimates of attributable
#' fractions, numbers and rates by area
#' @param save_fig Boolean. Whether to save the plot as an output. Defaults to
#' FALSE.
#' @param output_folder_path Path to folder where plots should be saved.
#' Defaults to NULL.
#' @param country Character. Name of country for national level estimates.
#' Defaults to 'National'
#'
#' @returns Plots of yearly attributable fractions per area
#'
#' @keywords internal
mh_plot_af_yearly <- function(
    attr_yr_list,
    save_fig = FALSE,
    output_folder_path = NULL,
    country = "National"
) {
  cols <- get_accessible_palette()

  if (!save_fig) {
    old_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old_par), add = TRUE)
  }

  if (save_fig == TRUE) {
    open_accessible_pdf(
      file = file.path(output_folder_path, "suicides_af_timeseries.pdf"),
      n_plots = length(attr_yr_list),
      max_cols = 2,
      panel_width = 6.8,
      panel_height = 6.6,
      mar = c(7.2, 5, 3.2, 4.2),
      oma = c(7.5, 0.6, 9, 0.6)
    )
  }

  year_min <- min(sapply(attr_yr_list, function(x) min(x$year, na.rm = TRUE)))
  year_max <- max(sapply(attr_yr_list, function(x) max(x$year, na.rm = TRUE)))

  y_min <- min(sapply(attr_yr_list, function(x) min(x$af, na.rm = TRUE)))
  y_max <- max(sapply(attr_yr_list, function(x) max(x$af, na.rm = TRUE)))

  ylim <- c(min(c(0, y_min)), y_max) * 1.5

  for (reg in names(attr_yr_list)) {
    region_af <- as.data.frame(attr_yr_list[[reg]])
    region_af <- region_af[order(region_af$year), ]

    graphics::par(mar = c(7.2, 5, 3.4, 4.2))

    plot(
      x = region_af$year,
      y = region_af$af,
      type = "l",
      xlim = c(year_min, year_max),
      ylim = ylim,
      xlab = "Year",
      ylab = "Attributable fraction (AF, %)",
      main = reg,
      cex.main = 1.2,
      col = cols$deep_water,
      lwd = 2
    )

    # Shaded confidence interval
    graphics::polygon(
      x = c(region_af$year, rev(region_af$year)),
      y = c(region_af$af_upper_ci, rev(region_af$af_lower_ci)),
      col = grDevices::adjustcolor(cols$deep_water, alpha.f = 0.2),
      border = NA
    )

    # Zero reference line
    graphics::abline(h = 0, col = cols$text, lty = 2, lwd = 1.2)

    # CI-out-of-bounds warning per panel (only when saving)
    if (save_fig == TRUE) {
      af_ci_range <- c(
        min(region_af$af_lower_ci, na.rm = TRUE),
        max(region_af$af_upper_ci, na.rm = TRUE)
      )

      if (af_ci_range[1] < ylim[1] || af_ci_range[2] > ylim[2]) {
        ci_warning <- sprintf(
          paste(
            "Warning: CI's are outside the bounds of this chart.",
            "CI's range from %.2f%% to %.2f%%"
          ),
          af_ci_range[1], af_ci_range[2]
        )
        ovr_warning <- "(Please refer to the associated data table for more information on the uncertainty around each estimate)"

        graphics::mtext(ci_warning, side = 1, line = 4.6, cex = 0.72, col = "red",
                        adj = 0, font = 3)
        graphics::mtext(ovr_warning, side = 1, line = 5.9, cex = 0.72, col = "red",
                        adj = 0, font = 3)
      }
    }
  }

  if (save_fig == TRUE) {
    year_range <- paste0("(", year_min, "-", year_max, ")")

    main_title <- paste0(
      "Yearly attributable fraction of suicide by area, ",
      country, " ", year_range
    )

    sub_title <- paste(
      "Each panel shows yearly attributable fraction with a shaded 95% confidence interval.",
      "Dashed horizontal line marks zero attributable fraction."
    )

    run_accessible_pdf_plot(
      title = main_title,
      subtitle = sub_title,
      line_title = 4.9,
      line_subtitle = 3.2
    )

    # Figure level legend
    add_figure_legend(
      legend = c("Attributable fraction", "95% CI", "Zero reference line"),
      col = c(cols$deep_water, grDevices::adjustcolor(cols$deep_water, alpha.f = 0.2), cols$text),
      lty = c(1, NA, 2),
      lwd = c(2, NA, 1.2),
      pch = c(NA, 15, NA),
      pt.cex = c(1.2, 1.8, 1.2),
      cex = 1.05,
      seg.len = 2,
      text.col = cols$text,
      vpad = 0.029,
      bty = "o"
    )

    add_accessible_alt_text(
      alt_text = paste(
        "Alt text: Multi-panel line chart showing yearly attributable fraction of suicide by area.",
        "Each panel contains a line for attributable fraction and a shaded band for the 95% confidence interval.",
        "A dashed horizontal line marks zero attributable fraction."
      ),
      width = 170
    )

    grDevices::dev.off()
  }
}


#' Plot attributable rates by year
#'
#' @description Plot attributable rates by year and area with confidence intervals
#'
#' @param attr_yr_list A list of matrices containing yearly estimates of attributable
#' fractions, numbers and rates by area
#' @param save_fig Boolean. Whether to save the plot as an output. Defaults to
#' FALSE.
#' @param output_folder_path Path to folder where plots should be saved.
#' Defaults to NULL.
#' @param country Character. Name of country for national level estimates.
#' Defaults to 'National'.
#'
#' @returns Plots of yearly attributable rates per area
#'
#' @keywords internal
mh_plot_ar_yearly <- function(
    attr_yr_list,
    save_fig = FALSE,
    output_folder_path = NULL,
    country = "National") {
  if (save_fig == TRUE) {
    grid <- c(min(length(attr_yr_list), 3), ceiling(length(attr_yr_list) / 3))
    output_path <- file.path(output_folder_path, "suicides_ar_timeseries.pdf")
    pdf(output_path, width = max(10, grid[1] * 5.5), height = max(7, grid[2] * 4.5))

    par(mfrow = c(grid[2], grid[1]), oma = c(0, 0, 4, 0), mar = c(8, 4, 5, 4))
  }

  year_min <- min(sapply(attr_yr_list, function(x) min(x$year, na.rm = TRUE)))
  year_max <- max(sapply(attr_yr_list, function(x) max(x$year, na.rm = TRUE)))

  y_min <- min(sapply(attr_yr_list, function(x) min(x$ar, na.rm = TRUE)))
  y_max <- max(sapply(attr_yr_list, function(x) max(x$ar, na.rm = TRUE)))

  ylim <- c(min(c(0, y_min)), y_max) * 1.5

  for (reg in names(attr_yr_list)) {
    region_ar <- as.data.frame(attr_yr_list[[reg]])

    plot(
      x = region_ar$year,
      y = region_ar$ar,
      type = "l",
      xlim = c(year_min, year_max),
      ylim = ylim,
      xlab = "Year",
      ylab = "AR (per 100,000 population)",
      main = reg,
      col = "#C75E70"
    )

    # Ensure data is sorted by Year
    region_ar <- region_ar[order(region_ar$year), ]

    # Create x and y coordinates for the polygon
    x_poly <- c(region_ar$year, rev(region_ar$year))
    y_poly <- c(region_ar$ar_upper_ci, rev(region_ar$ar_lower_ci))

    # Draw shaded confidence interval
    polygon(
      x = x_poly,
      y = y_poly,
      col = adjustcolor("#C75E70", alpha.f = 0.2),
      border = NA
    )

    abline(
      h = 0,
      col = "black",
      lty = 2
    )

    legend("topright",
           inset = c(0, -0.1),
           legend = "95% CI",
           col = adjustcolor("#C75E70", alpha.f = 0.2),
           pch = 15,
           pt.cex = 2,
           bty = "n",
           xpd = TRUE,
           horiz = TRUE,
           cex = 0.9
    )

    if (save_fig == TRUE) {
      ar_ci_range <- c(min(region_ar$ar_lower_ci), max(region_ar$ar_upper_ci))

      if (ar_ci_range[1] < ylim[1] || ar_ci_range[2] > ylim[2]) {
        ci_warning <- sprintf("Warning: CI's are outside the bounds of this chart. CI's range from %.2f to %.2f per 100,000", ar_ci_range[1], ar_ci_range[2])
        ovr_warning <- "(Please refer to the associated data table for more information on the uncertainty around each estimate)"

        mtext(ci_warning, side = 1, line = 5, cex = 0.6, col = "red", font = 3)
        mtext(ovr_warning, side = 1, line = 6, cex = 0.6, col = "red", font = 3)
      }
    }
  }

  if (save_fig == TRUE) {
    year_range <- paste0("(", year_min, " - ", year_max, ")")
    title <- paste0("Yearly Attributable Rate of Suicide by Area, ", country, " ", year_range)

    mtext(title, outer = TRUE, cex = 1.5, line = 1, font = 2)

    dev.off()
  }
}


#' Plot attributable fractions by calendar month
#'
#' @description Plot attributable fractions grouped over the whole time series by
#' calendar month to explore seasonality.
#'
#' @param attr_mth_list A list of data frames containing total attributable
#' fractions, numbers and rates by calendar month and area.
#' @param df_list A list of dataframes containing daily timeseries data for a
#' health outcome and climate variables which may be disaggregated by a
#' particular region.
#' @param country Character. Name of country for national level estimates.
#' Defaults to 'National'.
#' @param attr_thr Integer. Percentile at which to define the temperature
#' threshold for calculating attributable risk. Defaults to 97.5.
#' @param save_fig Boolean. Whether to save the plot as an output. Defaults to
#' FALSE.
#' @param output_folder_path Path to folder where plots should be saved.
#' Defaults to NULL.
#'
#' @returns Plots of attributable fractions by calendar month per area
#'
#' @keywords internal
mh_plot_af_monthly <- function(
    attr_mth_list,
    df_list,
    country = "National",
    attr_thr = 97.5,
    save_fig = FALSE,
    output_folder_path = NULL) {
  if (save_fig == TRUE) {
    grid <- c(min(length(attr_mth_list), 3), ceiling(length(attr_mth_list) / 3))
    output_path <- file.path(output_folder_path, "suicides_af_month_plot.pdf")
    pdf(output_path, width = max(10, grid[1] * 4.5), height = max(8, grid[2] * 4.5))

    par(mfrow = c(grid[2], grid[1]), mar = c(5, 5, 5, 5), oma = c(4, 0, 4, 0))
  }

  ylim_max <- max(sapply(attr_mth_list, function(x) max(x$af, na.rm = TRUE)))

  ylim2_min <- min(sapply(attr_mth_list, function(x) min(x$temp, na.rm = TRUE)))
  ylim2_max <- max(sapply(attr_mth_list, function(x) max(x$temp, na.rm = TRUE)))

  scale_factor <- (1 / ylim2_max) * ylim_max

  temp_ticks <- pretty(c(min(0, ylim2_min), ylim2_max))

  ylim <- c(min(0, temp_ticks[1] * scale_factor), max(temp_ticks[length(temp_ticks)] * scale_factor, ylim_max))

  for (reg in names(attr_mth_list)) {
    region_af <- attr_mth_list[[reg]]
    region_temp <- df_list[[reg]]$temp

    temp_scaled <- region_af$temp * scale_factor

    bar_pos <- barplot(
      names.arg = substr(region_af$month, 1, 1),
      height = region_af$af,
      ylim = ylim,
      xlab = "Month",
      ylab = "AF (%)",
      main = reg,
      col = "#296991"
    )

    lines(
      x = bar_pos,
      y = temp_scaled,
      type = "o",
      col = "#0a2e4d",
      pch = 16
    )

    # Add secondary axis on the right

    axis(
      side = 4,
      at = temp_ticks * scale_factor,
      labels = temp_ticks,
      col.axis = "black",
      col = "black"
    )

    mtext("Mean Temp (\u00b0C)", side = 4, line = 3, col = "black", cex = 0.7)

    abline(
      h = 0,
      col = "black",
      lty = 1
    )

    attr_thr_tmp <- round(quantile(region_temp, attr_thr / 100, na.rm = TRUE), 2)
    af_leg_lab <- paste0("AF (%) - from Attr. Risk Treshold, ", attr_thr_tmp, "\u00b0C (", attr_thr, "p)")

    legend("topleft",
           inset = c(0, -0.05),
           legend = c(af_leg_lab, "Mean Temp (\u00b0C)"),
           fill = c("#296991", NA),
           border = NA,
           lty = c(NA, 1),
           pch = c(NA, 16),
           col = c("#296991", "#0a2e4d"),
           bty = "n",
           cex = 0.9,
           horiz = FALSE,
           xpd = TRUE
    )
  }

  if (save_fig == TRUE) {
    year_range <- paste0(
      "(",
      min(sapply(df_list, function(x) min(lubridate::year(x$date), na.rm = TRUE))),
      "-",
      max(sapply(df_list, function(x) max(lubridate::year(x$date), na.rm = TRUE))),
      ")"
    )

    title <- paste0("Attributable Fraction of Suicide by Calendar Month and Area, ", country, " ", year_range)

    mtext(title, outer = TRUE, cex = 1.5, line = 1, font = 2)

    af_ci_min <- min(sapply(attr_mth_list, function(x) min(x$af_lower_ci, na.rm = TRUE)))
    af_ci_max <- max(sapply(attr_mth_list, function(x) max(x$af_upper_ci, na.rm = TRUE)))
    af_ci_range <- c(af_ci_min, af_ci_max)

    ci_warning <- sprintf("Warning: CI's range from %.2f%% to %.2f%%", af_ci_range[1], af_ci_range[2])
    ovr_warning <- "(Please refer to the associated data table for more information on the uncertainty around each estimate)"

    mtext(ci_warning, outer = TRUE, side = 1, line = 1, cex = 0.8, col = "red", font = 3)
    mtext(ovr_warning, outer = TRUE, side = 1, line = 2, cex = 0.8, col = "red", font = 3)

    dev.off()
  }
}


#' Plot attributable rates by calendar month
#'
#' @description Plot attributable rates grouped over the whole time series by
#' calendar month to explore seasonality.
#'
#' @param attr_mth_list A list of data frames containing total attributable
#' fractions, numbers and rates by calendar month and area.
#' @param df_list A list of dataframes containing daily timeseries data for a health outcome
#' and climate variables which may be disaggregated by a particular region.
#' @param country Character. Name of country for national level estimates.
#' Defaults to 'National'.
#' @param attr_thr Integer. Percentile at which to define the temperature threshold for
#' calculating attributable risk. Defaults to 97.5.
#' @param save_fig Boolean. Whether to save the plot as an output. Defaults to
#' FALSE.
#' @param output_folder_path Path to folder where plots should be saved.
#' Defaults to NULL.
#'
#' @returns Plots of attributable rates by calendar month per area
#'
#' @keywords internal
mh_plot_ar_monthly <- function(
    attr_mth_list,
    df_list,
    country = "National",
    attr_thr = 97.5,
    save_fig = FALSE,
    output_folder_path = NULL) {
  if (save_fig == TRUE) {
    grid <- c(min(length(attr_mth_list), 3), ceiling(length(attr_mth_list) / 3))
    output_path <- file.path(output_folder_path, "suicides_ar_month_plot.pdf")
    pdf(output_path, width = max(10, grid[1] * 4.5), height = max(8, grid[2] * 4.5))

    par(mfrow = c(grid[2], grid[1]), mar = c(5, 5, 5, 5), oma = c(4, 0, 4, 0))
  }

  ylim_max <- max(sapply(attr_mth_list, function(x) max(x$ar, na.rm = TRUE)))

  ylim2_min <- min(sapply(attr_mth_list, function(x) min(x$temp, na.rm = TRUE)))
  ylim2_max <- max(sapply(attr_mth_list, function(x) max(x$temp, na.rm = TRUE)))

  scale_factor <- (1 / ylim2_max) * ylim_max

  temp_ticks <- pretty(c(min(0, ylim2_min), ylim2_max))

  ylim <- c(min(0, temp_ticks[1] * scale_factor), max(temp_ticks[length(temp_ticks)] * scale_factor, ylim_max))

  for (reg in names(attr_mth_list)) {
    region_ar <- attr_mth_list[[reg]]
    region_temp <- df_list[[reg]]$temp

    temp_scaled <- region_ar$temp * scale_factor

    bar_pos <- barplot(
      names.arg = substr(region_ar$month, 1, 1),
      height = region_ar$ar,
      ylim = ylim,
      xlab = "Month",
      ylab = "AR (per 100,000 population)",
      main = reg,
      col = "#c75e70"
    )

    lines(
      x = bar_pos,
      y = temp_scaled,
      type = "o",
      col = "#0a2e4d",
      pch = 16
    )

    axis(
      side = 4,
      at = temp_ticks * scale_factor,
      labels = temp_ticks,
      col.axis = "black",
      col = "black"
    )

    mtext("Mean Temp (\u00b0C)", side = 4, line = 3, col = "black", cex = 0.7)

    abline(
      h = 0,
      col = "black",
      lty = 1
    )

    attr_thr_tmp <- round(quantile(region_temp, attr_thr / 100, na.rm = TRUE), 2)
    ar_leg_lab <- paste0("AR - from Attr. Risk Threshold, ", attr_thr_tmp, "\u00b0C (", attr_thr, "p)")

    legend("topleft",
           inset = c(0, -0.05),
           legend = c(ar_leg_lab, "Mean Temp (\u00b0C)"),
           fill = c("#c75e70", NA),
           border = NA,
           lty = c(NA, 1),
           pch = c(NA, 16),
           col = c("#c75e70", "#0a2e4d"),
           bty = "n",
           cex = 0.9,
           horiz = FALSE,
           xpd = TRUE
    )
  }

  if (save_fig == TRUE) {
    year_range <- paste0(
      "(",
      min(sapply(df_list, function(x) min(lubridate::year(x$date), na.rm = TRUE))),
      "-",
      max(sapply(df_list, function(x) max(lubridate::year(x$date), na.rm = TRUE))),
      ")"
    )

    title <- paste0("Attributable Rate of Suicide by Calendar Month and Area, ", country, " ", year_range)

    mtext(title, outer = TRUE, cex = 1.5, line = 1, font = 2)

    ar_ci_min <- min(sapply(attr_mth_list, function(x) min(x$ar_lower_ci, na.rm = TRUE)))
    ar_ci_max <- max(sapply(attr_mth_list, function(x) max(x$ar_upper_ci, na.rm = TRUE)))
    ar_ci_range <- c(ar_ci_min, ar_ci_max)

    ci_warning <- sprintf("Warning: CI's range from %.2f to %.2f per 100,000 population", ar_ci_range[1], ar_ci_range[2])
    ovr_warning <- "(Please refer to the associated data table for more information on the uncertainty around each estimate)"

    mtext(ci_warning, outer = TRUE, side = 1, line = 1, cex = 0.8, col = "red", font = 3)
    mtext(ovr_warning, outer = TRUE, side = 1, line = 2, cex = 0.8, col = "red", font = 3)
  }

  dev.off()
}


#' Save results of analysis - Mental Health
#'
#' @description Saves a CSV file of cumulative relative risk and
#' confidence intervals.
#'
#' @param rr_results Dataframe containing cumulative relative risk and confidence
#' intervals from analysis.
#' @param res_attr_tot Matrix containing total attributable fractions, numbers and rates for each
#' area over the whole time series.
#' @param attr_yr_list A list of matrices containing yearly estimates of attributable
#' fractions, numbers and rates by area
#' @param attr_mth_list A list of data frames containing total attributable
#' fractions, numbers and rates by calendar month and area.
#' @param power_list A list containing power information by area.
#' @param output_folder_path Path to folder where results should be saved.
#' Defaults to NULL.
#'
#' @keywords internal
mh_save_results <- function(
    rr_results,
    res_attr_tot,
    attr_yr_list,
    attr_mth_list,
    power_list,
    output_folder_path = NULL) {
  if (!is.null(output_folder_path)) {
    check_file_exists(file.path(output_folder_path))

    write.csv(rr_results, file = file.path(
      output_folder_path, "suicides_rr_results.csv"
    ), row.names = FALSE)

    write.csv(res_attr_tot, file = file.path(
      output_folder_path, "suicides_attr_tot_results.csv"
    ), row.names = FALSE)

    res_attr_yr <- do.call(rbind, attr_yr_list) %>%
      select("region", everything())

    write.csv(res_attr_yr, file = file.path(
      output_folder_path, "suicides_attr_yr_results.csv"
    ), row.names = FALSE)

    res_attr_mth <- do.call(rbind, attr_mth_list) %>%
      select("region", everything())

    write.csv(res_attr_mth, file = file.path(
      output_folder_path, "suicides_attr_mth_results.csv"
    ), row.names = FALSE)

    res_power <- do.call(rbind, power_list)

    write.csv(res_power, file = file.path(
      output_folder_path, "model_validation", "suicides_power_results.csv"
    ), row.names = FALSE)

    # TODO also in wildfire functions, generalise and put in files.utils
  } else {
    stop("Output path not specified")
  }
}


#' Full analysis pipeline for the suicides and extreme heat indicator
#'
#' @description Runs the full pipeline to analyse the impact of extreme heat
#' on suicides using a time-stratified case-crossover approach with distributed
#' lag non-linear model. This function generates relative risk of the
#' suicide-temperature association as well as attributable numbers, rates and
#' fractions of suicides to a specified temperature threshold. Model validation
#' statistics are also provided.
#'
#' @param data_path Path to a csv file containing a daily time series of data
#' for a particular health outcome and climate variables, which may be
#' disaggregated by region.
#' @param date_col Character. Name of the column in the dataframe that contains
#' the date.
#' @param region_col Character. Name of the column in the dataframe that contains
#' the region names. Defaults to NULL.
#' @param temperature_col Character. Name of the column in the dataframe that
#' contains the temperature column.
#' @param health_outcome_col Character. Name of the column in the dataframe that
#' contains the health outcome count column (e.g. number of deaths, hospital
#' admissions).
#' @param population_col Character. Name of the column in the dataframe that
#' contains the population estimate coloumn.
#' @param country Character. Name of country for national level estimates.
#' @param meta_analysis Boolean. Whether to perform a meta-analysis.
#' @param var_fun Character. Exposure function for argvar
#' (see dlnm::crossbasis). Defaults to 'bs'.
#' @param var_degree Integer. Degree of the piecewise polynomial for argvar
#' (see dlnm:crossbasis). Defaults to 2 (quadratic).
#' @param var_per Vector. Internal knot positions for argvar
#' (see dlnm::crossbasis). Defaults to c(25,50,75).
#' @param lag_fun Character. Exposure function for arglag
#' (see dlnm::crossbasis). Defaults to 'strata'.
#' @param lag_breaks Integer. Internal cut-off point defining the strata for arglag
#' (see dlnm:crossbasis). Defaults to 1.
#' @param lag_days Integer. Maximum lag. Defaults to 2.
#' (see dlnm:crossbasis).
#' @param independent_cols Additional independent variables to test in model validation
#' @param control_cols A list of confounders to include in the final model adjustment.
#' Defaults to NULL if none.
#' @param cenper Integer. Value for the percentile in calculating the centering
#' value 0-100. Defaults to 50.
#' @param attr_thr Integer. Percentile at which to define the temperature threshold for
#' calculating attributable risk.
#' @param save_fig Boolean. Whether to save the plot as an output. Defaults to
#' FALSE.
#' @param save_csv Boolean. Whether to save the results as a CSV. Defaults to
#' FALSE.
#' @param output_folder_path Path to folder where plots and/or CSV should be
#' saved. Defaults to NULL.
#' @param seed Optional integer random seed used when sampling residuals for
#' model validation plots. Defaults to NULL.
#'
#' @details
#' This analysis pipeline requires a daily time series of temperature and suicide
#' deaths with population values as a minimum. This is then processed using a
#' conditional Poisson case-crossover analysis with distributed lag non-linear
#' model and optional meta-analysis. Meta-analysis is recommended if the input
#' data is disaggregated by area.
#'
#' The model parameters have default values, which are recommended to keep as based
#' on existing studies. However, if desired these can be adjusted for sensitivity
#' analysis.
#'
#' Model validation testing is provided as a standard output from the pipeline so
#' a user can assess the quality of the model. If a user has additional independent
#' variables these can be specified as `independent_cols` and assessed within
#' different model combinations in the outputs of this testing. These can be added
#' in the final model via `control_cols`.
#'
#' For attributable deaths the default is to use extreme heat as a threshold,
#' defined as the 97.5th percentile of temperature over the corresponding time
#' period for each geography. This can be adjusted if desired, following review of
#' the relative risk association between temperature and suicides, using `attr_thr`.
#'
#' Further details on the input data requirements, methodology, quality information
#' and guidance on interpreting outputs can be found in the accompanying published
#' \doi{10.5281/zenodo.14050224}.
#'
#' @references
#' \enumerate{
#'  \item Pearce M, Watkins E, Glickman M, Lewis B, Ingole V. Standards for Official Statistics on
#'  Climate-Health Interactions (SOSCHI): Suicides attributed to extreme heat: methodology.
#'  Zenodo; 2024. Available from: \doi{10.5281/zenodo.14050224}
#'  \item Gasparrini A, Guo Y, Hashizume M, Lavigne E, Zanobetti A, Schwartz J, et al. Mortality
#'  risk attributable to high and low ambient temperature: a multicountry observational study.
#'  Lancet. 2015 Jul;386(9991):369-75. Available from: \url{https://linkinghub.elsevier.com/retrieve/pii/S0140673614621140}
#'  \item Kim Y, Kim H, Gasparrini A, Armstrong B, Honda Y, Chung Y, et al. Suicide and Ambient
#'  Temperature: A Multi-Country Multi-City Study. Environ Health Perspect. 2019 Nov;127(11):1-10.
#'  Available from: \url{https://pubmed.ncbi.nlm.nih.gov/31769300/}
#'  \item Gasparrini A, Armstrong B. Reducing and meta-analysing estimates from distributed lag
#'  non-linear models. BMC Med Res Methodol. 2013 Jan 9;13:1. Available from: \doi{10.1186/1471-2288-13-1}
#' 	\item Gasparrini A, Armstrong B, Kenward MG. Multivariate meta-analysis for non-linear and
#' 	other multi-parameter associations. Stat Med. 2012 Dec 20;31(29):3821-39.
#' 	Available from: \doi{10.1002/sim.5471}
#' 	\item Sera F, Armstrong B, Blangiardo M, Gasparrini A. An extended mixed-effects framework
#' 	for meta-analysis. Stat Med. 2019 Dec 20;38(29):5429-44. Available from: \doi{10.1002/sim.8362}
#' 	\item Gasparrini A, Leone M. Attributable risk from distributed lag models.
#' 	BMC Med Res Methodol. 2014 Dec 23;14(1):55.
#' 	Available from: \url{https://link.springer.com/article/10.1186/1471-2288-14-55}
#' 	}
#'
#' @returns
#' \itemize{
#'   \item `qaic_results` A dataframe of QAIC and dispersion metrics for each model
#'   combination and geography.
#'   \item `qaic_summary` A dataframe with the mean QAIC and dispersion metrics for
#'   each model combination.
#'   \item `vif_results` A dataframe. Variance inflation factors for each independent
#'   variables by region.
#'   \item `vif_summary` A dataframe with the mean variance inflation factors for
#'   each independent variable.
#'   \item `meta_test_res` A dataframe of results from statistical tests on the meta model.
#'   \item `power_list` A list containing power information by area.
#'   \item `rr_results` Dataframe containing cumulative relative risk and confidence
#'   intervals from analysis.
#'   \item `res_attr_tot` Dataframe. Total attributable fractions, numbers and
#'   rates for each area over the whole time series.
#'   \item `attr_yr_list` List. Dataframes containing yearly estimates of
#'   attributable fractions, numbers and rates by area.
#'   \item `attr_mth_list` List. Dataframes containing total attributable
#'   fractions, numbers and rates by calendar month and area.
#'   }
#'
#' @examples
#' \donttest{
#' example_data <- data.frame(
#'   date = seq.Date(as.Date("2020-01-01"), by = "day", length.out = 365),
#'   region = "Example Region",
#'   tmean = stats::runif(365, 5, 30),
#'   suicides = stats::rpois(365, lambda = 2),
#'   pop = 250000
#' )
#' example_path <- tempfile(fileext = ".csv")
#' utils::write.csv(example_data, example_path, row.names = FALSE)
#'
#' suicides_heat_do_analysis(
#'   data_path = example_path,
#'   date_col = "date",
#'   region_col = "region",
#'   temperature_col = "tmean",
#'   health_outcome_col = "suicides",
#'   population_col = "pop",
#'   country = "Example Region",
#'   meta_analysis = FALSE,
#'   var_fun = "bs",
#'   var_degree = 2,
#'   var_per = c(25, 50, 75),
#'   lag_fun = "strata",
#'   lag_breaks = 1,
#'   lag_days = 2,
#'   independent_cols = NULL,
#'   control_cols = NULL,
#'   cenper = 50,
#'   attr_thr = 97.5,
#'   save_fig = FALSE,
#'   save_csv = FALSE,
#'   output_folder_path = tempdir()
#' )
#' }
#'
#' @export
suicides_heat_do_analysis <- function(
    data_path,
    date_col,
    region_col = NULL,
    temperature_col,
    health_outcome_col,
    population_col,
    country = "National",
    meta_analysis = FALSE,
    var_fun = "bs",
    var_degree = 2,
    var_per = c(25, 50, 75),
    lag_fun = "strata",
    lag_breaks = 1,
    lag_days = 2,
    independent_cols = NULL,
    control_cols = NULL,
    cenper = 50,
    attr_thr = 97.5,
    save_fig = FALSE,
    save_csv = FALSE,
    output_folder_path = NULL,
    seed = NULL) {
  # Setup additional output DIR
  if (!is.null(output_folder_path)) {
    # Check output dir exists
    check_file_exists(output_folder_path, TRUE)
    new_fpath <- file.path(
      output_folder_path,
      paste0("suicides_analysis_", format(Sys.time(), "%d_%m_%Y_%H_%M"))
    )
    if (!is.null(new_fpath)) {
      (
        dir.create(new_fpath)
      )
    }
    output_folder_path <- new_fpath
  }
  # read and normalise input dataset
  df_list <- mh_read_and_format_data(
    data_path = data_path,
    date_col = date_col,
    region_col = region_col,
    temperature_col = temperature_col,
    health_outcome_col = health_outcome_col,
    population_col = population_col
  )
  # create list of population totals
  pop_list <- dlnm_pop_totals(
    df_list = df_list,
    country = country,
    meta_analysis = meta_analysis
  )
  # generate a list of cross-basis matrices
  cb_list <- mh_create_crossbasis(
    df_list = df_list,
    var_fun = var_fun,
    var_degree = var_degree,
    var_per = var_per,
    lag_fun = lag_fun,
    lag_breaks = lag_breaks,
    lag_days = lag_days
  )
  # calculate qaic and vif for model validation
  model_val <- mh_model_validation(
    df_list = df_list,
    cb_list = cb_list,
    independent_cols = independent_cols,
    save_fig = save_fig,
    save_csv = save_csv,
    output_folder_path = output_folder_path,
    seed = seed
  )
  qaic_results <- model_val[[1]]
  qaic_summary <- model_val[[2]]
  vif_results <- model_val[[3]]
  vif_summary <- model_val[[4]]
  # create list of DLNM models
  model_list <- mh_casecrossover_dlnm(
    df_list = df_list,
    control_cols = control_cols,
    cb_list = cb_list
  )
  # calculate values for reduced model
  reduced <- dlnm_reduce_cumulative(
    df_list = df_list,
    var_per = var_per,
    var_degree = var_degree,
    cenper = cenper,
    cb_list = cb_list,
    model_list = model_list
  )
  coef_ <- reduced[[1]]
  vcov_ <- reduced[[2]]
  # conditionally carry out meta-analysis
  if (meta_analysis == TRUE) {
    meta <- dlnm_meta_analysis(
      df_list = df_list,
      coef_ = coef_,
      vcov_ = vcov_,
      save_csv = save_csv,
      output_folder_path = output_folder_path
    )
    mm <- meta[[1]]
    blup <- meta[[2]]
    meta_test_res <- meta[[3]]
  } else {
    # assign NULL placeholders
    blup <- NULL
    meta_test_res <- NULL
  }

  # get vector of minimum suicide temperatures (percentile)
  minpercreg <- dlnm_min_mortality_temp(
    df_list = df_list,
    var_fun = var_fun,
    var_per = var_per,
    var_degree = var_degree,
    blup = blup,
    coef_ = coef_,
    meta_analysis = meta_analysis,
    outcome_type = "suicide"
  )
  # get model predictions by region
  pred_list <- mh_predict_reg(
    df_list = df_list,
    var_fun = var_fun,
    var_per = var_per,
    var_degree = var_degree,
    minpercreg = minpercreg,
    blup = blup,
    coef_ = coef_,
    vcov_ = vcov_,
    meta_analysis = meta_analysis
  )
  # aggregate data to national level and generate crossbasis
  if (meta_analysis == TRUE) {
    nat_data <- mh_add_national_data(
      df_list = df_list,
      pop_list = pop_list,
      var_fun = var_fun,
      var_per = var_per,
      var_degree = var_degree,
      lag_fun = lag_fun,
      lag_breaks = lag_breaks,
      lag_days = lag_days,
      country = country,
      cb_list = cb_list,
      mm = mm,
      minpercreg = minpercreg
    )
    df_list <- nat_data[[1]]
    cb_list <- nat_data[[2]]
    minpercreg <- nat_data[[3]]
    mmpredall <- nat_data[[4]]
    # get predictions
    pred_list <- dlnm_predict_nat(
      df_list = df_list,
      var_fun = var_fun,
      var_per = var_per,
      var_degree = var_degree,
      minpercreg = minpercreg,
      mmpredall = mmpredall,
      pred_list = pred_list,
      country = country
    )
  }
  # carry out power calculation to assess model
  power_list <- dlnm_power_list(
    df_list = df_list,
    pred_list = pred_list,
    minperc = minpercreg,
    attr_thr_high = attr_thr,
    compute_low = FALSE
  )
  # plot power calculation results
  if (save_fig == TRUE) {
    mh_plot_power(
      power_list = power_list,
      save_fig = save_fig,
      output_folder_path = output_folder_path,
      country = country
    )
  }
  # obtain relative risk values
  rr_results <- mh_rr_results(
    pred_list = pred_list,
    df_list = df_list,
    attr_thr = attr_thr,
    minpercreg = minpercreg
  )
  # plot relative risk values
  if (save_fig == TRUE) {
    mh_plot_rr(
      df_list = df_list,
      pred_list = pred_list,
      attr_thr = attr_thr,
      minpercreg = minpercreg,
      country = country,
      save_fig = save_fig,
      output_folder_path = output_folder_path
    )
  }
  # calculate attributable numbers per region
  attr_list <- mh_attr(
    df_list = df_list,
    cb_list = cb_list,
    pred_list = pred_list,
    minpercreg = minpercreg,
    attr_thr = attr_thr
  )
  # create a table containing attributable estimates
  attr <- mh_attr_tables(
    attr_list = attr_list,
    country = country,
    meta_analysis = meta_analysis
  )
  res_attr_tot <- attr[[1]]
  attr_yr_list <- attr[[2]]
  attr_mth_list <- attr[[3]]
  if (save_fig == TRUE) {
    # Plot attributable numbers
    mh_plot_attr_totals(
      df_list = df_list,
      res_attr_tot = res_attr_tot,
      save_fig = save_fig,
      output_folder_path = output_folder_path,
      country = country
    )
    # Plot yearly attributable fraction values
    mh_plot_af_yearly(
      attr_yr_list = attr_yr_list,
      save_fig = save_fig,
      output_folder_path = output_folder_path,
      country = country
    )
    # plot year attributable rates
    mh_plot_ar_yearly(
      attr_yr_list = attr_yr_list,
      save_fig = save_fig,
      output_folder_path = output_folder_path,
      country = country
    )
    # plot monthly attributable fractions
    mh_plot_af_monthly(
      attr_mth_list = attr_mth_list,
      df_list = df_list,
      country = country,
      attr_thr = attr_thr,
      save_fig = save_fig,
      output_folder_path = output_folder_path
    )
    # plot monthly attributable rates
    mh_plot_ar_monthly(
      attr_mth_list = attr_mth_list,
      df_list = df_list,
      country = country,
      attr_thr = attr_thr,
      save_fig = save_fig,
      output_folder_path = output_folder_path
    )
  }
  # conditionally save csv files containing raw data
  if (save_csv == TRUE) {
    mh_save_results(
      rr_results = rr_results,
      res_attr_tot = res_attr_tot,
      attr_yr_list = attr_yr_list,
      attr_mth_list = attr_mth_list,
      power_list = power_list,
      output_folder_path = output_folder_path
    )
  }
  # return structured data
  return(list(
    qaic_results = qaic_results,
    qaic_summary = qaic_summary,
    vif_results = vif_results,
    vif_summary = vif_summary,
    meta_test_res = meta_test_res,
    power_list = power_list,
    rr_results = rr_results,
    res_attr_tot = res_attr_tot,
    attr_yr_list = attr_yr_list,
    attr_mth_list = attr_mth_list
  ))
}
