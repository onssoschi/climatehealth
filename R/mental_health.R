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

  if (is.null(region_col)) {
    df <- df %>%
      dplyr::mutate(region = "aggregated")
  }

  df <- df %>%
    dplyr::rename(
      date = date_col,
      region = region_col,
      temp = temperature_col,
      suicides = health_outcome_col,
      population = population_col
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


#' Create population totals
#'
#' @description Creates a list of population totals by year and region for use
#' in the attributable rate calculations.
#'
#' @param df_list A list of dataframes containing daily timeseries data for a health outcome
#' and climate variables which may be disaggregated by a particular region.
#' @param country Character. Name of country for national level estimates.
#' @param meta_analysis Boolean. Whether to perform a meta-analysis.
#'
#' @returns List of population totals by year and region
#'
#' @keywords internal
mh_pop_totals <- function(
    df_list,
    country = "National",
    meta_analysis = FALSE) {
  # create list of population aggregates
  pop_list <- lapply(
    df_list, function(x) aggregate(population ~ year, data = x, mean)
  )
  if (meta_analysis == TRUE) {
    tot_pop <- do.call(rbind, pop_list)
    tot_pop <- aggregate(population ~ year, data = tot_pop, sum)
    pop_list[[country]] <- tot_pop
  }
  return(pop_list)
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
      knots = quantile(region_data$temp, var_per / 100, na.rm = T),
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

  all_combos <- unlist(lapply(0:length(transformed_vars), function(i) {
    combn(transformed_vars, i, simplify = FALSE)
  }), recursive = FALSE)

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
  # Combine results into a single data frame
  qaic_results <- do.call(rbind, qaic_results)
  # Sort by region and QAIC
  qaic_results <- qaic_results[order(qaic_results$region, qaic_results$formula), ]
  return(list(qaic_results, residuals_list))
}


#' Produce variance inflation factor
#'
#' @description Produces variance inflation factor for the independent variables.
#'
#' @param df_list A list of dataframes containing daily timeseries data for a health outcome
#' and climate variables which may be disaggregated by a particular region.
#' @param independent_cols Additional independent variables to test in model validation.
#'
#' @returns A list. Variance inflation factors for each independent variables by region.
#'
#' @keywords internal
mh_vif <- function(
    df_list,
    independent_cols = NULL) {
  # get all combinations
  all_combos <- unlist(lapply(1:length(independent_cols), function(i) {
    combn(independent_cols, i, simplify = FALSE)
  }), recursive = FALSE)
  # calculate VIF for each region
  vif_list <- list()
  for (reg in names(df_list)) {
    region_data <- df_list[[reg]]
    reg_vif <- list()
    for (combo in all_combos) {
      # define model formula and calculate VIF
      formula_str <- paste(paste("suicides ~ temp"), paste(combo, collapse = " + "), sep = " + ")
      vif_model <- glm(as.formula(formula_str), data = region_data, family = quasipoisson())
      vif_values <- car::vif(vif_model)
      var_combo <- paste0("temp_", paste(combo, collapse = "_"))
      vif_df <- data.frame(
        region = reg,
        variable_combo = var_combo,
        variable = names(vif_values),
        vif = as.numeric(vif_values),
        stringsAsFactors = FALSE
      )
      reg_vif[[var_combo]] <- vif_df
    }
    vif_list[[reg]] <- do.call(rbind, reg_vif)
  }
  return(vif_list)
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
    output_folder_path = NULL) {
  c(qaic_results, residuals_list) %<-% mh_model_combo_res(
    df_list = df_list,
    cb_list = cb_list,
    independent_cols = independent_cols
  )
  if (save_csv == TRUE) {
    dir_path <- file.path(output_folder_path, "model_validation")
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
    write.csv(
      qaic_results,
      file = file.path(dir_path, "qaic_results.csv"),
      row.names = FALSE
    )
  }
  # calculate VIF if independent_cols is not NULL
  if (!is.null(independent_cols)) {
    vif_list <- mh_vif(
      df_list = df_list,
      independent_cols = independent_cols
    )
    vif_results <- do.call(rbind, vif_list)
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

  if (length(df_list) > 1) {
    # calculate QAIC summary
    qaic_summary <- qaic_results %>%
      group_by(formula) %>%
      summarise(mean_disp = mean(.data$disp), mean_qaic = mean(.data$qaic))
    if (save_csv == TRUE) {
      write.csv(
        qaic_summary,
        file = file.path(dir_path, "qaic_summary.csv"),
        row.names = FALSE
      )
    }
    # conditionall calculate VIF summary
    if (!is.null(vif_results)) {
      vif_summary <- vif_results %>%
        dplyr::group_by(.data$variable_combo, .data$variable) %>%
        dplyr::summarise(mean_vif = mean(.data$vif, na.rm = TRUE))
      if (save_csv == TRUE) {
        write.csv(
          vif_summary,
          file = file.path(dir_path, "vif_summary.csv"),
          row.names = FALSE
        )
      }
    }
  } else {
    qaic_summary <- vif_summary <- NULL
  }
  if (save_fig == TRUE) {
    # Shorten the labels to a fixed length
    short_labels <- sapply(as.character(names(df_list)), function(x) {
      x_clean <- gsub(" ", "", x) # remove all spaces
      if (nchar(x_clean) > 10) {
        substr(x_clean, 1, 10) # truncate to first 10 characters
      } else {
        x_clean
      }
    })
    # Assign names to the list
    named_label_list <- as.list(short_labels)
    names(named_label_list) <- names(df_list)
  }
  if (nrow(do.call(rbind, do.call(rbind, residuals_list))) > 100000) {
    sample_check <- TRUE
  } else {
    sample_check <- FALSE
  }
  for (reg in names(df_list)) {
    region_data <- df_list[[reg]]
    formula_list <- residuals_list[[reg]]
    named_label <- named_label_list[[reg]]

    if (save_fig == TRUE) {
      reg_folder <- gsub(pattern = " ", replacement = "_", x = reg)

      output_folder_main <- file.path(output_folder_path, "model_validation", reg_folder)
      dir.create(output_folder_main, recursive = TRUE, showWarnings = FALSE)

      grid <- c(min(length(formula_list), 3), ceiling(length(formula_list) / 3))
      output_path <- paste0(output_folder_main, "/", named_label, "_residuals_timeseries.pdf")
      pdf(output_path, width = grid[1] * 5.5, height = grid[2] * 4.5)

      par(mfrow = c(grid[2], grid[1]), oma = c(0, 0, 4, 0))
    }

    for (i in names(formula_list)) {
      plot(
        x = region_data$date[region_data$ind > 0],
        y = formula_list[[i]]$residuals,
        ylim = c(-5, 10),
        pch = 19,
        cex = 0.2,
        col = "#0A2E4D",
        main = unique(formula_list[[i]]$formula),
        ylab = "Deviance residuals",
        xlab = "Date"
      )

      abline(h = 0, lty = 2, lwd = 2)

      if (save_fig == TRUE) {
        title <- paste0("Deviance Residuals by Date: ", reg)
        mtext(title, outer = TRUE, cex = 1.5, line = 1, font = 2)
      }
    }

    dev.off()

    if (sample_check == TRUE) {
      all_residuals <- do.call(rbind, formula_list)

      set.seed(123) # for reproducibility
      sampled_residuals <- all_residuals %>%
        group_by(.data$formula) %>%
        sample_frac(0.2) %>%
        ungroup()

      new_res_list <- split(sampled_residuals, sampled_residuals$formula)

      sample_title <- " (20% sample)"
    } else {
      new_res_list <- formula_list
      sample_title <- ""
    }

    if (save_fig == TRUE) {
      grid <- c(min(length(formula_list), 3), ceiling(length(new_res_list) / 3))
      output_path <- paste0(output_folder_main, "/", named_label, "_residuals_fitted.pdf")
      pdf(output_path, width = grid[1] * 5.5, height = grid[2] * 4.5)

      par(mfrow = c(grid[2], grid[1]), oma = c(0, 0, 4, 0))
    }

    for (i in names(new_res_list)) {
      plot(
        x = jitter(new_res_list[[i]]$fitted, amount = 0.5),
        y = jitter(new_res_list[[i]]$residuals, amount = 0.5),
        pch = 19,
        cex = 0.2,
        col = "#0A2E4D",
        main = unique(new_res_list[[i]]$formula),
        ylab = "Deviance residuals",
        xlab = "Fitted values"
      )

      abline(h = 0, lty = 2, lwd = 2)

      if (save_fig == TRUE) {
        title <- paste0("Deviance Residuals by Fitted Values: ", reg, sample_title)
        mtext(title, outer = TRUE, cex = 1.5, line = 1, font = 2)
      }
    }

    dev.off()

    if (save_fig == TRUE) {
      grid <- c(min(length(formula_list), 3), ceiling(length(new_res_list) / 3))
      output_path <- paste0(output_folder_main, "/", named_label, "_qq_plot.pdf")
      pdf(output_path, width = grid[1] * 5.5, height = grid[2] * 4.5)

      par(mfrow = c(grid[2], grid[1]), oma = c(0, 0, 4, 0))
    }

    for (i in names(new_res_list)) {
      qqnorm(new_res_list[[i]]$residuals,
        pch = 19,
        cex = 0.2,
        col = "#0A2E4D",
        main = unique(new_res_list[[i]]$formula)
      )

      qqline(new_res_list[[i]]$residuals, lwd = 2)

      if (save_fig == TRUE) {
        title <- paste0("Normal Q-Q Plot of Residuals: ", reg, sample_title)
        mtext(title, outer = TRUE, cex = 1.5, line = 1, font = 2)
      }
    }

    dev.off()

    if (save_fig == TRUE) {
      grid <- c(min(length(formula_list), 2), ceiling(length(formula_list) / 3) * 2)
      output_path <- file.path(output_folder_main, paste0(named_label, "_residuals_acf_pacf.pdf"))
      pdf(output_path, width = grid[1] * 5.5, height = grid[2] * 4.5)

      par(mfrow = c(grid[2], grid[1]), oma = c(0, 0, 5, 0))
    }

    for (i in names(formula_list)) {
      residuals_clean <- stats::na.omit(formula_list[[i]]$residuals)
      stats::acf(residuals_clean, main = paste0("ACF: ", unique(formula_list[[i]]$formula)), col = "#7A855C")
      stats::pacf(residuals_clean, main = paste0("PACF: ", unique(formula_list[[i]]$formula)), col = "#7A855C")
    }

    if (save_fig == TRUE) {
      title <- paste0("Autocorrelation and Partial Autocorrelation of Residuals:\n", reg)
      mtext(title, outer = TRUE, cex = 1.5, line = 0.5, font = 2)
    }

    dev.off()
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


#' Reduce to overall cumulative
#'
#' @description Reduce model to the overall cumulative association
#'
#' @param df_list A list of dataframes containing daily timeseries data for a health outcome
#' and climate variables which may be disaggregated by a particular region.
#' @param var_per Vector. Internal knot positions for argvar
#' (see dlnm::crossbasis). Defaults to c(25,50,75).
#' @param var_degree Integer. Degree of the piecewise polynomial for argvar
#' (see dlnm::crossbasis). Defaults to 2 (quadratic).
#' @param cenper Integer. Value for the percentile in calculating the centering
#' value 0-100. Defaults to 50.
#' @param cb_list List of cross_basis matrices from create_crossbasis function.
#' @param model_list List of models produced from case-crossover and DLNM
#' analysis.
#'
#' @returns
#'  \itemize{
#'   \item `coef_` A matrix of coefficients for the reduced model.
#'   \item `vcov_` A list. Covariance matrices for each region for the reduced model.
#'   }
#'
#' @keywords internal
mh_reduce_cumulative <- function(
    df_list,
    var_per = c(25, 50, 75),
    var_degree = 2,
    cenper = 50,
    cb_list,
    model_list) {
  coef_ <- matrix(
    data = NA,
    nrow = length(names(df_list)),
    ncol = length(var_per) + var_degree,
    dimnames = list(names(df_list))
  )
  vcov_ <- vector("list", length(names(df_list)))
  names(vcov_) <- names(df_list)

  for (reg in names(df_list)) {
    region_data <- df_list[[reg]]
    cb <- cb_list[[reg]]


    red <- dlnm::crossreduce(cb, model_list[[reg]], cen = quantile(region_data$temp, cenper / 100, na.rm = T))

    coef_[reg, ] <- coef(red)
    vcov_[[reg]] <- vcov(red)
  }

  return(list(coef_, vcov_))
}


#' Meta-analysis and BLUPs
#'
#' @description Run meta-analysis using temperature average and range as meta
#' predictors. Then create the best linear unbiased predictions (BLUPs).
#'
#' @param df_list A list of dataframes containing daily timeseries data for a health outcome
#' and climate variables which may be disaggregated by a particular region.
#' @param coef_ A matrix of coefficients for the reduced model.
#' @param vcov_ A list. Covariance matrices for each region for the reduced model.
#' @param save_csv Boolean. Whether to save the results as a CSV. Defaults to
#' FALSE.
#' @param output_folder_path Path to folder where results should be saved.
#' Defaults to NULL.
#'
#' @returns
#' \itemize{
#'   \item `mm` A model object. A multivariate meta-analysis model.
#'   \item `blup` A list. BLUP (best linear unbiased predictions) from the
#'   meta-analysis model for each region.
#'   \item `meta_test_res` A dataframe of results from statistical tests on the meta model.
#'   }
#'
#' @keywords internal
mh_meta_analysis <- function(
    df_list,
    coef_,
    vcov_,
    save_csv = FALSE,
    output_folder_path = NULL) {
  # Create temperature average and range as meta predictors
  temp_avg <- sapply(df_list, function(x) mean(x$temp, na.rm = TRUE))
  temp_range <- sapply(df_list, function(x) diff(range(x$temp, na.rm = TRUE)))
  # Meta-analysis
  mm <- mixmeta::mixmeta(
    formula = coef_ ~ temp_avg + temp_range,
    S = vcov_,
    data = as.data.frame(names(df_list)),
    method = "reml"
  )
  # BLUP
  blup <- mixmeta::blup(
    object = mm,
    vcov = TRUE
  )

  names(blup) <- names(df_list)
  # Wald test
  temp_avg_wald <- fwald(mm, "temp_avg")
  temp_range_wald <- fwald(mm, "temp_range")
  # Cochran Q-test
  qstat <- mixmeta::qtest(mm)
  # I2 statistic
  i2stat <- ((qstat$Q - qstat$df) / qstat$Q)[1] * 100
  meta_test_res <- data.frame(
    test = c(
      "Temp_avg Wald p-value",
      "Temp_range Wald p-value",
      "Cochrane Q test p-value",
      "I2 (%)",
      "AIC"
    ),
    result = round(c(
      temp_avg_wald,
      temp_range_wald,
      qstat[["pvalue"]][1],
      i2stat,
      summary(mm)$AIC
    ), 3)
  )

  if (save_csv == TRUE) {
    if (!is.null(output_folder_path)) {
      check_file_exists(file.path(output_folder_path))

      write.csv(meta_test_res, file = file.path(
        output_folder_path, "meta_model_stat_test_results.csv"
      ), row.names = FALSE)
    } else {
      stop("Output path not specified")
    }
  }
  return(list(mm, blup, meta_test_res))
}


#' Define minimum suicide temperature values
#'
#' @description Define the minimum suicide temperature values (between 1st and
#' 50th percentiles).
#'
#' @param df_list A list of dataframes containing daily timeseries data for a health outcome
#' and climate variables which may be disaggregated by a particular region.
#' @param var_fun Character. Exposure function for argvar
#' (see dlnm::crossbasis). Defaults to 'bs'.
#' @param var_per Vector. Internal knot positions for argvar
#' (see dlnm::crossbasis). Defaults to c(25,50,75).
#' @param var_degree Integer. Degree of the piecewise polynomial for argvar
#' (see dlnm::crossbasis). Defaults to 2 (quadratic).
#' @param blup A list. BLUP (best linear unbiased predictions) from the
#' meta-analysis model for each region.
#' @param coef_ A matrix of coefficients for the reduced model.
#' @param meta_analysis Boolean. Whether to perform a meta-analysis.
#'
#' @returns Vector. Percentile of minimum suicide temperature for each region.
#'
#' @keywords internal
mh_min_suicide_temp <- function(
    df_list,
    var_fun = "bs",
    var_per = c(25, 50, 75),
    var_degree = 2,
    blup,
    coef_,
    meta_analysis = FALSE) {
  if (meta_analysis == TRUE) {
    coef_list <- lapply(blup, function(x) x$blup)
  } else {
    coef_list <- split(coef_, rownames(coef_))
  }

  # Generate matrix for storing results

  minpercreg <- rep(NA, length(df_list))
  names(minpercreg) <- names(df_list)

  # Define min and max suicide values: exclude low and very hot temperature

  for (reg in names(df_list)) {
    region_data <- df_list[[reg]]

    predvar <- quantile(region_data$temp, 1:99 / 100, na.rm = TRUE)

    # Redefine the function using all arguments (boundary knots included)

    argvar <- list(
      x = predvar,
      fun = var_fun,
      knots = quantile(region_data$temp, var_per / 100, na.rm = TRUE),
      degree = var_degree,
      Boundary.knots = range(region_data$temp, na.rm = TRUE)
    )

    bvar <- do.call(dlnm::onebasis, argvar)

    minpercreg[reg] <- (1:50)[which.min((bvar %*% coef_list[[reg]])[1:50, ])]
  }

  return(minpercreg)
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
    dplyr::rename(nat_population = .data$population)

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


#' Run national predictions from meta analysis
#'
#' @description Use the meta analysis to create national level predictions
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
#' @param mmpredall List of national coefficients and covariance matrices for the crosspred.
#' @param pred_list A list containing predictions from the model by region.
#' @param country Character. Name of country for national level estimates.
#'
#' @returns A list containing predictions by region.
#'
#' @keywords internal
mh_predict_nat <- function(
    df_list,
    var_fun = "bs",
    var_per = c(25, 50, 75),
    var_degree = 2,
    minpercreg,
    mmpredall,
    pred_list,
    country = "National") {
  national_data <- df_list[[country]]

  argvar <- list(
    x = national_data$temp,
    fun = var_fun,
    knots = quantile(national_data$temp, var_per / 100, na.rm = TRUE),
    degree = var_degree
  )

  bvar <- do.call(dlnm::onebasis, argvar)

  cen <- quantile(national_data$temp, minpercreg[country] / 100, na.rm = T)

  pred_nat <- dlnm::crosspred(bvar,
    coef = mmpredall$fit,
    vcov = mmpredall$vcov,
    cen = cen,
    model.link = "log",
    by = 0.1,
    from = min(round(national_data$temp, 1), na.rm = TRUE),
    to = max(round(national_data$temp, 1), na.rm = TRUE)
  )

  pred_list[[country]] <- pred_nat

  return(pred_list)
}


#' Mental Health Power calculation
#'
#' @description Produce a power statistic by area for the attributable threshold
#' and above as a reference.
#'
#' @param df_list A list of dataframes containing daily timeseries data for a health outcome
#' and climate variables which may be disaggregated by a particular region.
#' @param pred_list A list containing predictions from the model by region.
#' @param minpercreg Vector. Percentile of maximum suicide temperature for each region.
#' @param attr_thr Integer. Percentile at which to define the temperature threshold for
#' calculating attributable risk. Defaults to 97.5.
#'
#' @returns A list containing power information by area.
#'
#' @keywords internal
mh_power_list <- function(
    df_list,
    pred_list,
    minpercreg,
    attr_thr = 97.5) {
  power_list <- list()
  alpha <- 0.05

  for (reg in names(df_list)) {
    region_data <- df_list[[reg]]
    pred <- pred_list[[reg]]
    min_st <- round(quantile(region_data$temp, minpercreg[reg] / 100, na.rm = TRUE), 1)

    thresh_temp <- round(quantile(region_data$temp, attr_thr / 100, na.rm = TRUE), 1)

    coef_effect_with_se <- data.frame(
      temperature = round(pred$predvar, 1),
      log_rr = pred$allfit,
      se = pred$allse
    )

    coef_effect_with_se <- coef_effect_with_se %>%
      dplyr::filter(.data$temperature >= thresh_temp)

    rownames(coef_effect_with_se) <- NULL

    power_df <- data.frame(
      region = reg,
      temperature = coef_effect_with_se$temperature,
      cen = min_st,
      log_rr = coef_effect_with_se$log_rr,
      se = coef_effect_with_se$se,
      z_alpha = stats::qnorm(1 - alpha / 2)
    )

    power_df <- power_df %>%
      mutate(power = stats::pnorm(
        .data$log_rr / .data$se - .data$z_alpha
      ) + (1 - stats::pnorm(.data$log_rr / .data$se + .data$z_alpha))) %>%
      select(all_of(c(-"z_alpha"))) %>%
      mutate(
        log_rr = round(.data$log_rr, 2),
        se = round(.data$se, 2),
        power = round(.data$power * 100, 1)
      )

    power_list[[reg]] <- power_df
  }
  return(power_list)
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
    country = "National") {
  if (save_fig == TRUE) {
    grid <- c(min(length(power_list), 3), ceiling(length(power_list) / 3))
    output_path <- file.path(output_folder_path, "model_validation", "power_vs_temperature.pdf")
    pdf(output_path, width = max(10, grid[1] * 5.5), height = max(7, grid[2] * 4.5))
    par(mfrow = c(grid[2], grid[1]), oma = c(0, 0, 4, 0), mar = c(8, 4, 5, 4))
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
      col = "#296991",
      ylim = c(0, 100),
      lwd = 2
    )

    abline(
      h = 80,
      col = "black",
      lty = 2
    )
  }

  if (save_fig == TRUE) {
    title <- paste0("Power vs Temperature by Area, ", country)
    mtext(title, outer = TRUE, cex = 1.5, line = 1, font = 2)
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

    df <- data.frame(
      Area = region_name,
      MinST = round(min_st, 1),
      Attr_Threshold_Temp = round(attr_thr_temp, 1),
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
    output_folder_path = NULL) {
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


  if (save_fig == T) {
    grid <- c(min(length(pred_list), 3), ceiling(length(pred_list) / 3))

    output_path <- file.path(output_folder_path, "suicides_rr_plot.pdf")
    pdf(output_path, width = max(10, grid[1] * 5.5), height = max(7, grid[2] * 4))

    layout_ids <- seq_len(grid[1] * grid[2])
    layout_matrix <- matrix(layout_ids, nrow = grid[2], ncol = grid[1], byrow = TRUE)

    layout(layout_matrix, heights = rep(1, grid[2]), widths = rep(1, grid[1]))

    par(oma = c(0, 1, 4, 1))
  }

  for (reg in names(pred_list)) {
    region_pred <- pred_list[[reg]]
    region_temp <- df_list[[reg]]$temp

    par(mar = c(5, 5, 4, 5) + 0.1)

    plot(region_pred,
      "overall",
      xlab = expression(paste("Temperature (", degree, "C)")),
      ylab = "RR",
      ylim = ylim,
      xlim = xlim,
      main = reg,
      col = "#296991"
    )

    vline_pos_max_x <- quantile(region_temp, attr_thr / 100, na.rm = TRUE)
    vline_pos_max_y <- max(region_pred$allRRfit, na.rm = TRUE) + 0.3
    vline_lab_max <- paste0("Attr. Risk Threshold\n", round(vline_pos_max_x, 2), intToUtf8(176), "C (p", attr_thr, ")")

    abline(v = vline_pos_max_x, col = "black", lty = 2)
    text(x = vline_pos_max_x, y = vline_pos_max_y, labels = vline_lab_max, pos = 2, col = "black", cex = 0.8)

    vline_pos_min_x <- quantile(region_temp, minpercreg[reg] / 100, na.rm = TRUE)
    min_rr <- min(region_pred$allRRfit, na.rm = TRUE)

    if (dplyr::between(min_rr, 0.90, 1.1)) {
      vline_pos_min_y <- min_rr - 0.2
    } else {
      vline_pos_min_y <- min_rr - 0.1
    }

    vline_lab_min <- paste0("Min ST\n", round(vline_pos_min_x, 2), intToUtf8(176), "C (p", round(minpercreg[reg], 2), ")")

    abline(v = vline_pos_min_x, col = "black", lty = 2)
    text(x = vline_pos_min_x, y = vline_pos_min_y, labels = vline_lab_min, pos = 4, col = "black", cex = 0.8)

    reg_temp_range <- range(region_temp, na.rm = TRUE)

    hist_data <- hist(region_temp,
      breaks = seq(floor(reg_temp_range[1]), ceiling(reg_temp_range[2]), by = 1),
      plot = FALSE
    )

    hist_scale <- (0.3) / hist_max
    scaled_counts <- hist_data$counts * hist_scale

    for (i in seq_along(hist_data$counts)) {
      rect(
        xleft = hist_data$breaks[i],
        xright = hist_data$breaks[i + 1],
        ybottom = ylim[1],
        ytop = ylim[1] + scaled_counts[i],
        col = "#C75E70",
        border = "white"
      )
    }

    axis_labels <- pretty(c(0, hist_max), n = 2)
    axis_scaled <- c(ylim[1], ylim[1] + (c(axis_labels[-1]) * hist_scale))
    axis(side = 4, at = axis_scaled, labels = axis_labels, las = 1)

    hist_midpoint <- ylim[1] + (max(scaled_counts, na.rm = TRUE) / 2)

    # Normalize midpoint to [0, 1] scale for adj
    adj_val <- (hist_midpoint - ylim[1]) / (ylim[2] - ylim[1])

    # Add axis title with dynamic vertical alignment
    mtext("Frequency", side = 4, line = 3, adj = adj_val, cex = 0.7)
  }

  if (save_fig == T) {
    year_range <- paste0(
      "(",
      min(sapply(df_list, function(x) min(lubridate::year(x$date), na.rm = TRUE))),
      "-",
      max(sapply(df_list, function(x) max(lubridate::year(x$date), na.rm = TRUE))),
      ")"
    )

    title <- paste0("Relative Risk of Suicide by Mean Temperature and Area, ", country, " ", year_range)

    mtext(title, outer = TRUE, cex = 1.5, line = 1, font = 2)

    dev.off()
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

    c(
      af, af_lower_ci, af_upper_ci, an, an_lower_ci, an_upper_ci, ansim_mat
    ) %<-% an_attrdl(
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
        af = af,
        af_lower_ci = af_lower_ci,
        af_upper_ci = af_upper_ci,
        an = an,
        an_lower_ci = an_lower_ci,
        an_upper_ci = an_upper_ci,
        ar = (an / .data$population) * 100000,
        ar_lower_ci = (an_lower_ci / population) * 100000,
        ar_upper_ci = (an_upper_ci / population) * 100000
      )
    attr_list[[reg]] <- list(
      results = results,
      ansim_mat = ansim_mat
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
      select(all_of(c(-"sim_rows", -"sim_sum")))

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
    country = "National") {
  if (save_fig == TRUE) {
    num_regions <- nrow(res_attr_tot)

    # Dynamically adjust height based on number of regions
    chart_height <- 6
    chart_width <- 0.3 * num_regions # adjust as needed
    total_width <- max(8, chart_width)

    output_path <- file.path(output_folder_path, "suicides_total_attr_plot.pdf")
    pdf(output_path, width = total_width, height = chart_height * 2)

    # Set up layout: 1 row for barplot and 1 row for table
    layout(matrix(c(1, 2), nrow = 2), heights = c(chart_height, chart_height))

    # Set up plotting area for the bar chart
    par(mar = c(10, 5, 4, 2), oma = c(1, 0, 0, 0))
  }

  # Shorten the labels to a fixed length
  short_labels <- sapply(as.character(res_attr_tot$region), function(x) {
    if (nchar(x) - 3 > 10) {
      paste0(substr(x, 1, 10), "...")
    } else {
      x
    }
  })

  names(short_labels) <- NULL

  year_range <- paste0(
    "(",
    min(sapply(df_list, function(x) min(lubridate::year(x$date), na.rm = TRUE))),
    "-",
    max(sapply(df_list, function(x) max(lubridate::year(x$date), na.rm = TRUE))),
    ")"
  )

  # Calculate CI ranges
  af_ci_range <- c(min(res_attr_tot$af_lower_ci), max(res_attr_tot$af_upper_ci))
  ar_ci_range <- c(min(res_attr_tot$ar_lower_ci), max(res_attr_tot$ar_upper_ci))

  # Format warning messages
  af_warning <- sprintf("Warning: AF CI's range from %.2f%% to %.2f%%", af_ci_range[1], af_ci_range[2])
  ar_warning <- sprintf("Warning: AR CI's range from %.2f to %.2f per 100,000", ar_ci_range[1], ar_ci_range[2])
  ovr_warning <- "(Please refer to the associated data table for more information on the uncertainty around each estimate)"

  # Sort by AF descending
  sorted_indices <- order(res_attr_tot$af, decreasing = TRUE)
  res_af_tot <- res_attr_tot[sorted_indices, ]
  short_labs_af <- short_labels[sorted_indices]

  # Define bar colors
  bar_col_af <- rep("#296991", length(short_labs_af))
  nat_ind_af <- which(res_af_tot$region == country)
  if (length(nat_ind_af) > 0) {
    bar_col_af[nat_ind_af] <- "#7a855c" # Highlight color
  }

  barplot(
    names.arg = short_labs_af,
    height = res_af_tot$af,
    ylab = "AF (%)",
    main = paste0("Attributable Fraction of Suicides by Area, ", country, " ", year_range),
    col = bar_col_af,
    las = 2,
    horiz = FALSE
  )

  mtext(af_warning, side = 1, line = 7, cex = 0.8, col = "red", font = 3)
  mtext(ovr_warning, side = 1, line = 8, cex = 0.8, col = "red", font = 3)

  if (save_fig == TRUE) {
    par(mar = c(10, 5, 4, 2))
  }

  # Sort by AF descending
  sorted_indices <- order(res_attr_tot$ar, decreasing = TRUE)
  res_ar_tot <- res_attr_tot[sorted_indices, ]
  short_labs_ar <- short_labels[sorted_indices]

  # Define bar colors
  bar_col_ar <- rep("#c75e70", length(short_labs_ar))
  nat_ind_ar <- which(res_ar_tot$region == country)
  if (length(nat_ind_ar) > 0) {
    bar_col_ar[nat_ind_ar] <- "#7a855c" # Highlight color
  }

  barplot(
    names.arg = short_labs_ar,
    height = res_ar_tot$ar,
    ylab = "AR (per 100,000 population)",
    main = paste0("Attributable Rate of Suicides by Area, ", country, " ", year_range),
    col = bar_col_ar,
    las = 2,
    horiz = FALSE
  )

  mtext(ar_warning, side = 1, line = 7, cex = 0.8, col = "red", font = 3)
  mtext(ovr_warning, side = 1, line = 8, cex = 0.8, col = "red", font = 3)

  if (save_fig == TRUE) {
    dev.off()
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
    country = "National") {
  if (save_fig == TRUE) {
    grid <- c(min(length(attr_yr_list), 3), ceiling(length(attr_yr_list) / 3))
    output_path <- file.path(output_folder_path, "suicides_af_timeseries.pdf")
    pdf(output_path, width = max(10, grid[1] * 5.5), height = max(7, grid[2] * 4.5))

    par(mfrow = c(grid[2], grid[1]), oma = c(0, 0, 4, 0), mar = c(8, 4, 5, 4))
  }

  year_min <- min(sapply(attr_yr_list, function(x) min(x$year, na.rm = TRUE)))
  year_max <- max(sapply(attr_yr_list, function(x) max(x$year, na.rm = TRUE)))

  y_min <- min(sapply(attr_yr_list, function(x) min(x$af, na.rm = TRUE)))
  y_max <- max(sapply(attr_yr_list, function(x) max(x$af, na.rm = TRUE)))

  ylim <- c(min(c(0, y_min)), y_max) * 1.5

  for (reg in names(attr_yr_list)) {
    region_af <- as.data.frame(attr_yr_list[[reg]])

    plot(
      x = region_af$year,
      y = region_af$af,
      type = "l",
      xlim = c(year_min, year_max),
      ylim = ylim,
      xlab = "Year",
      ylab = "AF (%)",
      main = reg,
      col = "#296991"
    )

    # Ensure data is sorted by Year
    region_af <- region_af[order(region_af$year), ]

    # Create x and y coordinates for the polygon
    x_poly <- c(region_af$year, rev(region_af$year))
    y_poly <- c(region_af$af_upper_ci, rev(region_af$af_lower_ci))

    # Draw shaded confidence interval
    polygon(
      x = x_poly,
      y = y_poly,
      col = adjustcolor("#296991", alpha.f = 0.2),
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
      col = adjustcolor("#296991", alpha.f = 0.2),
      pch = 15,
      pt.cex = 2,
      bty = "n",
      xpd = TRUE,
      horiz = TRUE,
      cex = 0.9
    )

    if (save_fig == TRUE) {
      af_ci_range <- c(min(region_af$af_lower_ci), max(region_af$af_upper_ci))

      if (af_ci_range[1] < ylim[1] || af_ci_range[2] > ylim[2]) {
        ci_warning <- sprintf("Warning: CI's are outside the bounds of this chart. CI's range from %.2f%% to %.2f%%", af_ci_range[1], af_ci_range[2])
        ovr_warning <- "(Please refer to the associated data table for more information on the uncertainty around each estimate)"

        mtext(ci_warning, side = 1, line = 5, cex = 0.6, col = "red", font = 3)
        mtext(ovr_warning, side = 1, line = 6, cex = 0.6, col = "red", font = 3)
      }
    }
  }

  if (save_fig == TRUE) {
    year_range <- paste0("(", year_min, " - ", year_max, ")")
    title <- paste0("Yearly Attributable Fraction of Suicide by Area, ", country, " ", year_range)

    mtext(title, outer = TRUE, cex = 1.5, line = 1, font = 2)

    dev.off()
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
  }

  dev.off()
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
#' \href{https://doi.org/10.5281/zenodo.14050224}{Zenodo documentation}.
#'
#' @references
#' \enumerate{
#'  \item Pearce M, Watkins E, Glickman M, Lewis B, Ingole V. Standards for Official Statistics on
#'  Climate-Health Interactions (SOSCHI): Suicides attributed to extreme heat: methodology.
#'  Zenodo; 2024. Available from: \url{https://doi.org/10.5281/zenodo.14050224}
#'  \item Gasparrini A, Guo Y, Hashizume M, Lavigne E, Zanobetti A, Schwartz J, et al. Mortality
#'  risk attributable to high and low ambient temperature: a multicountry observational study.
#'  Lancet. 2015 Jul;386(9991):369–75. Available from: \url{https://linkinghub.elsevier.com/retrieve/pii/S0140673614621140}
#'  \item Kim Y, Kim H, Gasparrini A, Armstrong B, Honda Y, Chung Y, et al. Suicide and Ambient
#'  Temperature: A Multi-Country Multi-City Study. Environ Health Perspect. 2019 Nov;127(11):1–10.
#'  Available from: \url{https://ehp.niehs.nih.gov/doi/10.1289/EHP4898}
#'  \item Gasparrini A, Armstrong B. Reducing and meta-analysing estimates from distributed lag
#'  non-linear models. BMC Med Res Methodol. 2013 Jan 9;13:1. Available from: \url{https://doi.org/10.1186/1471-2288-13-1}
#' 	\item Gasparrini A, Armstrong B, Kenward MG. Multivariate meta‐analysis for non‐linear and
#' 	other multi‐parameter associations. Stat Med. 2012 Dec 20;31(29):3821–39.
#' 	Available from: \url{https://doi.org/10.1002/sim.5471}
#' 	\item Sera F, Armstrong B, Blangiardo M, Gasparrini A. An extended mixed‐effects framework
#' 	for meta‐analysis. Stat Med. 2019 Dec 20;38(29):5429–44. Available from: \url{https://doi.org/10.1002/sim.8362}
#' 	\item Gasparrini A, Leone M. Attributable risk from distributed lag models.
#' 	BMC Med Res Methodol. 2014 Dec 23;14(1):55.
#' 	Available from: \url{https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/1471-2288-14-55}
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
#' \dontrun{
#' suicides_heat_do_analysis(
#'   data_path = "data/inputs/daily_suicides_climate_E_2001_2023.csv",
#'   date_col = "date",
#'   region_col = "region",
#'   temperature_col = "tmean",
#'   health_outcome_col = "suicides",
#'   population_col = "pop",
#'   country = "England",
#'   meta_analysis = TRUE,
#'   var_fun = "bs",
#'   var_degree = 2,
#'   var_per = c(25, 50, 75),
#'   lag_fun = "strata",
#'   lag_breaks = 1,
#'   lag_days = 2,
#'   independent_cols = c("rainfall", "humidity"),
#'   control_cols = NULL,
#'   cenper = 50,
#'   attr_thr = 97.5,
#'   save_fig = TRUE,
#'   save_csv = TRUE,
#'   output_folder_path = "data/outputs/england_analysis"
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
    output_folder_path = NULL) {
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
  pop_list <- mh_pop_totals(
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
  c(
    qaic_results, qaic_summary, vif_results, vif_summary
  ) %<-% mh_model_validation(
    df_list = df_list,
    cb_list = cb_list,
    independent_cols = independent_cols,
    save_fig = save_fig,
    save_csv = save_csv,
    output_folder_path = output_folder_path
  )
  # create list of DLNM models
  model_list <- mh_casecrossover_dlnm(
    df_list = df_list,
    control_cols = control_cols,
    cb_list = cb_list
  )
  # calculate values for reduced model
  c(coef_, vcov_) %<-% mh_reduce_cumulative(
    df_list = df_list,
    var_per = var_per,
    var_degree = var_degree,
    cenper = cenper,
    cb_list = cb_list,
    model_list = model_list
  )
  # conditionally carry out meta-analysis
  if (meta_analysis == TRUE) {
    c(mm, blup, meta_test_res) %<-% mh_meta_analysis(
      df_list = df_list,
      coef_ = coef_,
      vcov_ = vcov_,
      save_csv = save_csv,
      output_folder_path = output_folder_path
    )
  } else {
    # assign NULL placeholders
    blup <- NULL
    meta_test_res <- NULL
  }
  # get vector of minimum suicide temperatures (percentile)
  minpercreg <- mh_min_suicide_temp(
    df_list = df_list,
    var_fun = var_fun,
    var_per = var_per,
    var_degree = var_degree,
    blup = blup,
    coef_ = coef_,
    meta_analysis = meta_analysis
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
    c(df_list, cb_list, minpercreg, mmpredall) %<-% mh_add_national_data(
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
    # get predictions
    pred_list <- mh_predict_nat(
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
  power_list <- mh_power_list(
    df_list = df_list,
    pred_list = pred_list,
    minpercreg = minpercreg,
    attr_thr = attr_thr
  )
  # plot power calculation results
  mh_plot_power(
    power_list = power_list,
    save_fig = save_fig,
    output_folder_path = output_folder_path,
    country = country
  )
  # obtain relative risk values
  rr_results <- mh_rr_results(
    pred_list = pred_list,
    df_list = df_list,
    attr_thr = attr_thr,
    minpercreg = minpercreg
  )
  # plot relative risk values
  mh_plot_rr(
    df_list = df_list,
    pred_list = pred_list,
    attr_thr = attr_thr,
    minpercreg = minpercreg,
    country = country,
    save_fig = save_fig,
    output_folder_path = output_folder_path
  )
  # calculate attributable numbers per region
  attr_list <- mh_attr(
    df_list = df_list,
    cb_list = cb_list,
    pred_list = pred_list,
    minpercreg = minpercreg,
    attr_thr = attr_thr
  )
  # create a table containing attributable estimates
  c(res_attr_tot, attr_yr_list, attr_mth_list) %<-% mh_attr_tables(
    attr_list = attr_list,
    country = country,
    meta_analysis = meta_analysis
  )
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
