# Functions to generate analysis for the heat and cold indicator.


#' Read heat and cold indicator data
#'
#' @description Reads in data and geography names for analysis from a CSV file.
#'
#' @param input_csv_path Path to a CSV containing a
#' daily time series of death counts and temperature per geography.
#' @param dependent_col the column name of the health outcome
#' dependent variable of interest e.g,. deaths.
#' @param date_col Date column.
#' @param region_col The geography column over which the data
#' are spatially aggregated e.g., geognames.
#' @param temperature_col The temperature variable column e.g., tmean.
#' @param population_col The population estimate column e.g., pop.
#'
#' @return An alphabetically-ordered list of dataframes for each
#' geography comprising of dates, deaths, and temperatures.
#'
#' @keywords internal
hc_read_data <- function(input_csv_path,
                         dependent_col,
                         date_col,
                         region_col,
                         temperature_col,
                         population_col) {
  # Load the input dataset
  df <- read_input_data(input_csv_path)

  # Format the geography column. If geog is missing, then assume geographies are aggregated or only single input geography
  if (is.null(region_col)) {
    df <- df %>%
      dplyr::mutate(geog = "aggregated")
  }
  # subset needed cols
  needed_cols <- c(
    dependent_col,
    date_col,
    region_col,
    temperature_col,
    population_col
  )
  standard_cols <- c(
    "depdendent", "date", "region", "temp", "population"
  )
  for (i in seq_along(standard_cols)) {
    std_col <- standard_cols[i]
    need_col <- needed_cols[i]
    if (!identical(std_col, need_col) && std_col %in% names(df)) {
      df[[std_col]] <- NULL
    }
  }
  # Rename the columns
  df <- df %>%
    dplyr::rename(
      dependent = dependent_col,
      date = date_col,
      region = region_col,
      temp = temperature_col,
      population = population_col,
    ) %>%
    dplyr::mutate(
      date = as.Date(date, tryFormats = c("%d/%m/%Y", "%Y-%m-%d")),
      year = as.factor(lubridate::year(date)),
      month = as.factor(lubridate::month(date)),
      dow = as.factor(lubridate::wday(date, label = TRUE)),
      region = as.factor(.data$region)
    )

  # Reformat data and fill NaNs
  df <- reformat_data(df,
    reformat_date = TRUE,
    fill_na = c("dependent"),
    year_from_date = TRUE
  )
  # Split the data by region
  df_list <- aggregate_by_column(df, "region")
  return(df_list)
}

#' Create cross-basis matrix
#'
#' @description Creates a cross-basis matrix for each geography
#'
#' @param df_list A list of dataframes containing daily timeseries data for a health outcome
#' and climate variables which may be disaggregated by a particular geography.
#' @param var_fun Character. Exposure function for argvar
#' (see dlnm::crossbasis). Defaults to 'bs'.
#' @param var_degree Integer. Degree of the piecewise polynomial for argvar
#' (see dlnm::crossbasis). Defaults to 2 (quadratic).
#' @param var_per Vector. Internal knot positions for argvar
#' (see dlnm::crossbasis). Defaults to c(10,75,90).
#' @param lagn Integer. Number of days in the lag period. Defaults to 21.
#' (see dlnm::crossbasis).
#' @param lagnk Integer. Number of knots in lag function. Defaults to 3.
#' (see dlnm::logknots).
#' @param dfseas Integer. Degrees of freedom for seasonality.
#'
#' @returns A list of cross-basis matrices by geography
#'
#' @keywords internal
hc_create_crossbasis <- function(df_list,
                                 var_fun = "bs", # TODO What if natural spline (degree won't be needed as cubic) - if statement
                                 var_degree = 2,
                                 var_per = c(10, 75, 90),
                                 lagn = 21,
                                 lagnk = 3,
                                 dfseas = 8) {
  cb_list <- list()

  for (geog in names(df_list)) {
    geog_data <- df_list[[geog]]
    argvar <- list(
      fun = var_fun,
      knots = quantile(geog_data$temp,
        var_per / 100,
        na.rm = TRUE
      ),
      degree = var_degree
    )

    lagn <- as.numeric(lagn)
    lagnk <- as.numeric(lagnk)
    dfseas <- as.numeric(dfseas)
    arglag <- list(knots = dlnm::logknots(
      lagn,
      lagnk
    ))
    cb <- dlnm::crossbasis(geog_data$temp,
      lag = lagn,
      argvar = argvar,
      arglag = arglag
    )
    cb_list[[geog]] <- cb
  }

  return(cb_list)
}


#' Produce check results of model combinations
#'
#' @description Runs every combination of model based on user selected additional
#' independent variables and returns model diagnostic checks for each.
#'
#' @param df_list A list of dataframes containing daily timeseries data for a health outcome
#' and climate variables which may be disaggregated by a particular geography.
#' @param cb_list List of cross_basis matrices from create_crossbasis function.
#' @param independent_cols Additional independent variables to test in model validation
#' as confounders.
#'
#' @return
#'  \itemize{
#'   \item `qaic_results` A dataframe of QAIC and dispersion metrics for each model
#'   combination.
#'   \item `residuals_list` A list. Residuals for each model combination.
#'   }
#'
#' @keywords internal
hc_model_combo_res <- function(df_list,
                               cb_list,
                               independent_cols = NULL,
                               dfseas = 8) {
  qaic_results <- list()
  residuals_list <- list()

  if (!is.null(independent_cols)) {
    control_vars <- c(independent_cols)

    transformed_vars <- unlist(lapply(independent_cols, function(v) {
      paste0(v, "_ns")
    }))
  } else {
    (transformed_vars <- NULL)
  }

  # EW poss no need to apply ns to independents - check literature whether should be controlled with splines
  if (!is.null(transformed_vars)) {
    all_combos <- c(
      list(list()), # Create empty list to allow case where independent_cols = NULL
      unlist(lapply(1:length(transformed_vars), function(i) {
        combn(transformed_vars, i, simplify = FALSE)
      }), recursive = FALSE)
    )
  } else {
    all_combos <- list(list()) # Only base formula
  }

  for (geog in names(df_list)) {
    formula_list <- list()

    geog_data <- df_list[[geog]]
    cb <- cb_list[[geog]]

    if (!is.null(independent_cols)) {
      for (v in control_vars) {
        ns_matrix <- splines::ns(geog_data[[v]], df = 3)
        assign(paste0(v, "_ns"), ns_matrix)
      }
    }

    k <- length(unique(geog_data$year))
    base_independent_cols <- c(
      "cb",
      "dow",
      sprintf("splines::ns(as.numeric(date), df = %d)", k)
    )

    for (vars in all_combos) {
      # Build the full formula string
      base_formula <- paste("dependent ~", paste(base_independent_cols, collapse = " + "))
      formula_str <- paste(base_formula, if (length(vars) > 0) paste("+", paste(vars, collapse = " + ")) else "")


      model <- glm(as.formula(formula_str),
        geog_data,
        family = quasipoisson,
        na.action = "na.exclude"
      )

      disp <- summary(model)$dispersion
      loglik <- sum(dpois(model$y, model$fitted.values, log = TRUE))
      k <- length(coef(model))
      qaic <- -2 * loglik / disp + 2 * k

      qaic_results[[length(qaic_results) + 1]] <- data.frame(
        geography = geog,
        formula = formula_str,
        disp = disp,
        qaic = qaic
      )

      residuals_df <- data.frame(
        geography = geog,
        formula = formula_str,
        fitted = fitted(model),
        residuals = residuals(model, type = "deviance")
      )

      formula_list[[formula_str]] <- residuals_df
    }

    residuals_list[[geog]] <- formula_list
  }

  # Combine results into a single data frame
  qaic_results <- do.call(rbind, qaic_results)
  # Sort by geog and QAIC
  qaic_results <- qaic_results[order(qaic_results$geography, qaic_results$formula), ]


  return(list(qaic_results, residuals_list))
}

#' Run ADF test and produce PACF plots for each model combo
#'
#' @description Run augmented Dickey-Fuller test for stationarity of dependent variable
#'  and produce partial autocorrelation function plot of residuals for each model combo
#'
#' @param df_list A list of dataframes containing daily timeseries data for a health outcome
#' and climate variables which may be disaggregated by a particular geography.
#'
#' @return adf_result for each geography.
#'
#' @keywords internal
hc_adf <- function(df_list) {
  adf_list <- list()

  for (geog in names(df_list)) {
    geog_data <- df_list[[geog]]

    # Perform Augmented Dickey-Fuller Test - values can only be missing in first and last rows of series
    # null hypothesis = time series is non stationary
    adf_test_res <- tseries::adf.test(geog_data$dependent)
    # cat("ADF Test for", geog, ":\n")
    # print(adf_test_res)

    adf_df <- data.frame(
      adf_test_stat = as.numeric(adf_test_res$statistic),
      lag_order = as.numeric(adf_test_res$parameter),
      p_value = as.numeric(adf_test_res$p.value),
      stringsAsFactors = FALSE
    )

    adf_list[[geog]] <- adf_df
  }

  return(adf_list)
}


#' Model Validation Assessment
#'
#' @description Produces results on QAIC for each model combination, variance inflation
#' factor for each independent variable, ADF test for stationarity, and plots for residuals to assess the models
#'
#' @param df_list A list of dataframes containing daily timeseries data for a health outcome
#' and climate variables which may be disaggregated by a particular geography.
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
#' @return
#'   \itemize{
#'   \item `qaic_results` A dataframe of QAIC and dispersion metrics for each model combination and geography.
#'   \item `qaic_summary` A dataframe with the mean QAIC and dispersion metrics for each model combination.
#'   \item `vif_results` A dataframe of variance inflation factors for each independent variables by geography.
#'   \item `vif_summary` A dataframe with the mean variance inflation factors for each independent variable.
#'   \item `adf_results` A dataframe of ADF test results for each geography.
#'   }
#'
#' @keywords internal
hc_model_validation <- function(df_list,
                                cb_list,
                                independent_cols = NULL,
                                dfseas = 8,
                                save_fig = FALSE,
                                save_csv = FALSE,
                                output_folder_path = NULL) {
  # Create empty storage
  qaic_summary <- NULL
  vif_results <- NULL
  vif_summary <- NULL
  adf_results <- NULL

  # QAIC results to csv
  model_combo <- hc_model_combo_res(
    df_list = df_list,
    cb_list = cb_list,
    dfseas = dfseas,
    independent_cols = independent_cols
  )
  qaic_results <- model_combo[[1]]
  residuals_list <- model_combo[[2]]
  if (save_csv == TRUE) {
    dir.create(file.path(
      output_folder_path, "model_validation"
    ), recursive = TRUE, showWarnings = FALSE)

    write.csv(qaic_results, file = file.path(
      output_folder_path, "model_validation", "qaic_results.csv"
    ), row.names = FALSE)
  }

  # VIF results to csv by geog
  if (!is.null(independent_cols)) {
    vif_list <- dlnm_vif(
      df_list = df_list,
      independent_cols = independent_cols
    )

    vif_results <- dplyr::bind_rows(vif_list, .id = "Geography")

    if (save_csv == TRUE) {
      write.csv(vif_results, file = file.path(
        output_folder_path, "model_validation", "vif_results.csv"
      ), row.names = FALSE)
    }
  } else {
    vif_results <- NULL
  }

  # ADF test results to csv by geog
  adf_list <- hc_adf(df_list = df_list)

  adf_results <- dplyr::bind_rows(adf_list, .id = "Geography")

  if (save_csv == TRUE) {
    write.csv(adf_results, file = file.path(
      output_folder_path, "model_validation", "adf_results.csv"
    ), row.names = FALSE)
  }

  # Produce and write csv of mean QAIC and VIF results
  if (length(df_list) > 1) {
    qaic_summary <- qaic_results %>%
      group_by(formula) %>%
      summarise(
        mean_disp = mean(.data$disp),
        mean_qaic = mean(.data$qaic)
      )

    if (save_csv == TRUE) {
      write.csv(qaic_summary, file = file.path(
        output_folder_path, "model_validation", "qaic_summary.csv"
      ), row.names = FALSE)
    }

    if (!is.null(vif_results)) {
      vif_summary <- vif_results %>%
        dplyr::group_by(.data$variable) %>%
        dplyr::summarise(mean_vif = mean(.data$vif, na.rm = TRUE))

      if (save_csv == TRUE) {
        write.csv(vif_summary, file = file.path(
          output_folder_path, "model_validation", "vif_summary.csv"
        ), row.names = FALSE)
      }
    }
  } else {
    qaic_summary <- vif_summary <- NULL
  }


  # Model validation plots
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

  for (geog in names(df_list)) {
    geog_data <- df_list[[geog]]
    formula_list <- residuals_list[[geog]]
    named_label <- named_label_list[[geog]]

    if (save_fig == TRUE) {
      geog_folder <- gsub(pattern = " ", replacement = "_", x = geog)

      output_folder_main <- file.path(output_folder_path, "model_validation", geog_folder)
      dir.create(output_folder_main, recursive = TRUE, showWarnings = FALSE)

      grid <- c(min(length(formula_list), 3), ceiling(length(formula_list) / 3))
      output_path <- paste0(output_folder_main, "/", named_label, "_residuals_timeseries.pdf")
      pdf(output_path, width = grid[1] * 5.5, height = grid[2] * 4.5)

      par(mfrow = c(grid[2], grid[1]), oma = c(0, 0, 4, 0))
    }

    for (i in names(formula_list)) {
      plot(
        x = geog_data$date,
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
        title <- paste0("Deviance Residuals by Date: ", geog)
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
        title <- paste0("Deviance Residuals by Fitted Values: ", geog, sample_title)
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
        title <- paste0("Normal Q-Q Plot of Residuals: ", geog, sample_title)
        mtext(title, outer = TRUE, cex = 1.5, line = 1, font = 2)
      }
    }

    dev.off()

    # PACF plot
    if (save_fig == TRUE) {
      grid <- c(min(length(formula_list), 3), ceiling(length(formula_list) / 3))
      output_path <- file.path(output_folder_main, paste0(named_label, "_pacf.pdf"))
      pdf(output_path, width = grid[1] * 5.5, height = grid[2] * 4.5)

      par(mfrow = c(grid[2], grid[1]), oma = c(0, 0, 4, 0))
    }

    for (i in names(formula_list)) {
      residuals_clean <- stats::na.omit(formula_list[[i]]$residuals)
      stats::pacf(residuals_clean, main = unique(formula_list[[i]]$formula), col = "#7A855C")

      if (save_fig == TRUE) {
        title <- paste0("Partial autocorrelation function: ", geog)
        mtext(title, outer = TRUE, cex = 1.5, line = 1, font = 2)
      }
    }

    dev.off()
  }

  return(list(qaic_results, qaic_summary, vif_results, vif_summary, adf_results))
}


#' Define and run quasi-Poisson regression with distributed lag non-linear model
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
hc_quasipoisson_dlnm <- function(df_list,
                                 control_cols = NULL,
                                 cb_list,
                                 dfseas = 8) {
  model_list <- list()

  # build the formula with base formula and control variables
  if (!is.null(control_cols)) {
    # normalize type
    if (is.character(control_cols)) {
      control_cols <- c(control_cols)
    }

    # type check column names
    for (col in control_cols) {
      if (!is.character(col)) {
        stop(
          paste0(
            "'control_cols' expected a vector of strings or a string.",
            typeof(col)
          )
        )
      }
    }
  } else {
    control_cols <- c()
  }

  # define the base independent cols
  base_independent_cols <- c(
    "cb",
    "dow",
    paste0("splines::ns(date, df = ", dfseas, " * length(unique(year)))")
  )

  # model formula
  base_formula <- paste(
    "dependent ~",
    paste(base_independent_cols,
      collapse = " + "
    )
  )


  if (is.null(control_cols)) {
    formula <- as.formula(paste(base_formula))
  } else {
    formula <- as.formula(paste(
      base_formula,
      paste("+", paste(control_cols,
        collapse = " + "
      ))
    ))
  }


  # Run model
  for (geog in names(df_list)) {
    geog_data <- df_list[[geog]]
    cb <- cb_list[[geog]]

    model <- glm(formula,
      geog_data,
      family = quasipoisson,
      na.action = "na.exclude"
    )

    model_list[[geog]] <- model
  }

  return(model_list)
}

#' Calculate p-values for Wald test
#'
#' A function to calculate p-values for an explanatory variable.
#'
#' @param mm A model object.
#' @param var A character. The name of the variable in the model to calculate
#' p-values for.
#'
#' @keywords internal
#' @return A number. The p-value of the explanatory variable.
fwald <- function(mm, var) {
  if (!is.character(var)) {
    stop("Argument 'var' must be a character")
  }

  ind <- grep(var, names(coef(mm)))
  coef <- coef(mm)[ind]
  vcov <- vcov(mm)[ind, ind]
  waldstat <- coef %*% solve(vcov) %*% coef
  df <- length(coef)

  return(1 - pchisq(waldstat, df))
}

#' Run predictions from model
#'
#' @description Use model to run predictions. Predictions can be produced for a single input geography,
#' or multiple disaggregated geographies.
#'
#' @param df_list A list of dataframes containing daily timeseries data for a health outcome
#' and climate variables which may be disaggregated by a particular region.
#' @param var_fun Character. Exposure function for argvar
#' (see dlnm::crossbasis). Defaults to 'bs'.
#' @param var_per Vector. Internal knot positions for argvar
#' (see dlnm::crossbasis). Defaults to c(25,50,75).
#' @param var_degree Integer. Degree of the piecewise polynomial for argvar
#' (see dlnm::crossbasis). Defaults to 2 (quadratic).
#' @param mintempgeog_ Vector. Percentile of maximum suicide temperature for each region.
#' @param blup A list. BLUP (best linear unbiased predictions) from the
#' meta-analysis model for each region.
#' @param coef_ A matrix of coefficients for the reduced model.
#' @param vcov_ A list. Covariance matrices for each region for the reduced model.
#' @param meta_analysis Boolean. Whether to perform a meta-analysis.
#'
#' @return A list containing predictions by region
#'
#' @keywords internal
hc_predict_subnat <- function(df_list,
                              var_fun = "bs",
                              var_per = c(10, 75, 90),
                              var_degree = 2,
                              mintempgeog_,
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

  for (geog in names(df_list)) {
    geog_data <- df_list[[geog]]

    argvar <- list(
      x = geog_data$temp, # determines x axis for predictions
      fun = var_fun,
      knots = quantile(geog_data$temp,
        var_per / 100,
        na.rm = TRUE
      ),
      degree = var_degree
    )

    bvar <- do.call(dlnm::onebasis, argvar)

    pred <- dlnm::crosspred(bvar,
      coef = coef_list[[geog]],
      vcov = vcov_list[[geog]],
      model.link = "log",
      by = 0.1,
      cen = mintempgeog_[geog],
      from = min(geog_data$temp, na.rm = TRUE),
      to = max(geog_data$temp, na.rm = TRUE)
    )

    pred_list[[geog]] <- pred
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
#' @param lagn Integer. Number of days in the lag period. Defaults to 21.
#' (see dlnm::crossbasis).
#' @param lagnk Integer. Number of knots in lag function. Defaults to 3.
#' (see dlnm::logknots).
#' @param country Character. Name of country for national level estimates.
#' @param cb_list A list of cross-basis matrices by region.
#' @param mm A model object. A multivariate meta-analysis model.
#' @param minpercgeog_ Vector. Percentile of minumum mortality temperature for each region.
#'
#' @return
#' \itemize{
#'   \item `df_list` List. A list of data frames for each region and nation.
#'   \item `cb_list` List. A list of cross-basis matrices by region and nation.
#'   \item `minpercreg` Vector. Percentile of minimum suicide temperature for each region and nation.
#'   \item `mmpredall` List. A list of national coefficients and covariance matrices.
#'   }
#'
#' @keywords internal
hc_add_national_data <- function(df_list,
                                 pop_list,
                                 var_fun = "bs",
                                 var_per = c(10, 75, 90),
                                 var_degree = 2,
                                 lagn = 21,
                                 lagnk = 3,
                                 country = "National",
                                 cb_list,
                                 mm,
                                 minpercgeog_) {
  # Aggregate national level data
  national_data <- as.data.frame(do.call(rbind, df_list))

  nat_pop <- pop_list[[country]] %>%
    rename(nat_pop = .data$population)

  national_data <- national_data %>%
    dplyr::left_join(nat_pop, by = "year") %>%
    dplyr::mutate(
      weight = .data$population / nat_pop,
      weighted_temp = .data$temp * .data$weight
    ) %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(
      temp = round(sum(.data$weighted_temp, na.rm = TRUE), 2),
      dependent = sum(.data$dependent, na.rm = TRUE),
      population = unique(nat_pop)
    ) %>%
    mutate(
      year = as.factor(lubridate::year(date)),
      month = as.factor(lubridate::month(date)),
      geog = country
    )

  df_list[[country]] <- as.data.frame(national_data)


  # Create cross basis for national data
  argvar <- list(
    fun = var_fun,
    knots = quantile(national_data$temp,
      var_per / 100,
      na.rm = TRUE
    ),
    degree = var_degree
  )

  arglag <- list(knots = dlnm::logknots(
    lagn,
    lagnk
  ))

  cb_list[[country]] <- dlnm::crossbasis(national_data$temp,
    lag = lagn,
    argvar = argvar,
    arglag = arglag
  )

  # Add national min temperatures

  predvar <- quantile(national_data$temp, 1:99 / 100, na.rm = TRUE)

  argvar <- list(
    x = predvar,
    fun = var_fun,
    knots = quantile(national_data$temp,
      var_per / 100,
      na.rm = TRUE
    ),
    degree = var_degree,
    Boundary.knots = range(national_data$temp, na.rm = TRUE)
  )

  bvar <- do.call(dlnm::onebasis, argvar)

  datanew <- data.frame(
    temp_avg = mean(national_data$temp),
    temp_range = diff(range(national_data$temp, na.rm = TRUE))
  )

  mmpredall <- predict(mm, datanew, vcov = TRUE, format = "list")

  minpercnat <- (1:99)[which.min((bvar %*% mmpredall$fit)[1:99, ])]

  minpercgeog_[country] <- minpercnat

  return(list(df_list, cb_list, minpercgeog_, mmpredall))
}

#' Plot power calculations for temperature mortality analysis.
#'
#' @description Produces a plot displaying power calculation values for
#' temperature mortatlity analysis.
#'
#' @param power_list_high List. High power values.
#' @param power_list_low List. Low power values.
#' @param save_fig Logical. Whether or not to save the figure. Defailts to FALSE.
#' @param output_folder_path Character. The output location for the figure. Defaults
#' to NULL.
#' @param country Character. The name of the country or group of regions of analysis.
#' Defaults to "National".
#'
#' @returns Dataframe containing cumulative relative risk and confidence
#' intervals from analysis.
#'
#' @keywords internal
hc_plot_power <- function(power_list_high,
                          power_list_low,
                          save_fig = FALSE,
                          output_folder_path = NULL,
                          country = "National") {
  # High temperature
  if (save_fig == TRUE) {
    grid <- c(min(length(power_list_high), 3), ceiling(length(power_list_high) / 3))
    output_path <- file.path(output_folder_path, "model_validation", "power_vs_high_temperature.pdf")
    pdf(output_path, width = max(10, grid[1] * 5.5), height = max(7, grid[2] * 4.5))
    par(mfrow = c(grid[2], grid[1]), oma = c(0, 0, 4, 0), mar = c(8, 4, 5, 4))
  }
  plh <<- power_list_high
  pll <<- power_list_low
  for (geog in names(power_list_high)) {
    df <- power_list_high[[geog]]
    df <- df[order(df$temperature), ]

    plot(
      x = df$temperature,
      y = df$power,
      type = "l",
      xlab = "Temperature",
      ylab = "Power (%)",
      main = geog,
      col = "#C75E70",
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
    title <- paste0("Power vs. high temperatures by geography, ", country)
    mtext(title, outer = TRUE, cex = 1.5, line = 1, font = 2)
    dev.off()
  }


  # Low temperature
  if (save_fig == TRUE) {
    grid <- c(min(length(power_list_low), 3), ceiling(length(power_list_low) / 3))
    output_path <- file.path(output_folder_path, "model_validation", "power_vs_low_temperature.pdf")
    pdf(output_path, width = max(10, grid[1] * 5.5), height = max(7, grid[2] * 4.5))
    par(mfrow = c(grid[2], grid[1]), oma = c(0, 0, 4, 0), mar = c(8, 4, 5, 4))
  }

  for (geog in names(power_list_low)) {
    df <- power_list_low[[geog]]
    df <- df[order(df$temperature), ]

    plot(
      x = df$temperature,
      y = df$power,
      type = "l",
      xlab = "Temperature",
      ylab = "Power (%)",
      main = geog,
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
    title <- paste0("Power vs. low temperatures by geography, ", country)
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
#' @param minpercgeog_ Vector. Percentile of minimum suicide temperature for each area.
#' @param attr_thr_high Integer. Percentile at which to define the upper temperature threshold for
#' calculating attributable risk. Defaults to 97.5.
#' @param attr_thr_low Integer. Percentile at which to define the lower temperature threshold for
#' calculating attributable risk. Defaults to 2.5.
#'
#' @returns Dataframe containing cumulative relative risk and confidence
#' intervals from analysis.
#'
#' @keywords internal
hc_rr_results <- function(pred_list,
                          df_list,
                          minpercgeog_,
                          attr_thr_high = 97.5,
                          attr_thr_low = 2.5) {
  rr_results <- bind_rows(lapply(names(pred_list), function(geog_name) {
    geog_pred <- pred_list[[geog_name]]
    geog_temp <- df_list[[geog_name]]$temp

    mmt <- quantile(geog_temp, minpercgeog_[geog_name] / 100, na.rm = TRUE)
    attr_thr_high_temp <- quantile(geog_temp, attr_thr_high / 100, na.rm = TRUE)
    attr_thr_low_temp <- quantile(geog_temp, attr_thr_low / 100, na.rm = TRUE)

    temp_rounded <- round(geog_temp, 1)
    temp_freq_table <- table(temp_rounded)

    pred_temp_rounded <- round(geog_pred$predvar, 1)
    temp_freq <- as.numeric(temp_freq_table[as.character(pred_temp_rounded)])
    temp_freq[is.na(temp_freq)] <- 0 # Replace NAs with 0 for bins not present

    df <- data.frame(
      Area = geog_name,
      MMT = round(mmt, 1),
      Attr_Threshold_High_Temp = round(attr_thr_high_temp, 1),
      Attr_Threshold_Ligh_Temp = round(attr_thr_low_temp, 1),
      Temperature = round(geog_pred$predvar, 1),
      Temp_Frequency = temp_freq,
      RR = round(geog_pred$allRRfit, 2),
      RR_lower_CI = round(geog_pred$allRRlow, 2),
      RR_upper_CI = round(geog_pred$allRRhigh, 2)
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
#' @param df_list A list of dataframes containing daily timeseries data for a health outcome
#' and climate variables which may be disaggregated by a particular region.
#' @param pred_list A list containing predictions from the model by region.
#' @param attr_thr_high Integer. Percentile at which to define the upper temperature threshold for
#' calculating attributable risk. Defaults to 97.5.
#' @param attr_thr_low Integer. Percentile at which to define the lower temperature threshold for
#' calculating attributable risk. Defaults to 2.5.
#' @param minpercgeog_ Vector. Percentile of minimum suicide temperature for each area.
#' @param country Character. Name of country for national level estimates.
#' @param save_fig Boolean. Whether to save the plot as an output. Defaults to
#' FALSE.
#' @param output_folder_path Path to folder where plots should be saved.
#' Defaults to NULL.
#'
#' @returns Plots of cumulative lag exposure-response function with histogram of
#' temperature distribution for each region
#'
#' @keywords internal
hc_plot_rr <- function(df_list,
                       pred_list,
                       attr_thr_high = 97.5,
                       attr_thr_low = 2.5,
                       minpercgeog_,
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

    output_path <- file.path(output_folder_path, "temp_mortality_rr_plot.pdf")
    pdf(output_path, width = max(10, grid[1] * 5.5), height = max(7, grid[2] * 4))

    layout_ids <- seq_len(grid[1] * grid[2])
    layout_matrix <- matrix(layout_ids, nrow = grid[2], ncol = grid[1], byrow = TRUE)

    layout(layout_matrix, heights = rep(1, grid[2]), widths = rep(1, grid[1]))

    par(oma = c(0, 1, 4, 1))
  }

  for (geog in names(pred_list)) {
    geog_pred <- pred_list[[geog]]
    geog_temp <- df_list[[geog]]$temp

    par(mar = c(5, 5, 4, 5) + 0.1)

    plot(geog_pred,
      "overall",
      xlab = expression(paste("Temperature (", degree, "C)")),
      ylab = "RR",
      ylim = ylim,
      xlim = xlim,
      main = geog,
      col = "#296991"
    )


    # high temperature threshold line
    vline_pos_high_x <- quantile(geog_temp, attr_thr_high / 100, na.rm = TRUE)
    vline_pos_high_y <- max(geog_pred$allRRfit, na.rm = TRUE) + 0.3
    vline_lab_high <- paste0("High temp. threshold\n", round(vline_pos_high_x, 2), intToUtf8(176), "C (p", attr_thr_high, ")")

    # add dashed line  and label to plot
    abline(v = vline_pos_high_x, col = "#0A2E4D", lty = 2)
    text(x = vline_pos_high_x, y = vline_pos_high_y, labels = vline_lab_high, pos = 4, col = "black", cex = 0.8)

    # low temperature threshold line
    vline_pos_low_x <- quantile(geog_temp, attr_thr_low / 100, na.rm = TRUE)
    vline_pos_low_y <- max(geog_pred$allRRfit, na.rm = TRUE) + 0.3
    vline_lab_low <- paste0("Low temp. threshold\n", round(vline_pos_low_x, 2), intToUtf8(176), "C (p", attr_thr_low, ")")

    # add dashed line to plot
    abline(v = vline_pos_low_x, col = "#0A2E4D", lty = 2)
    text(x = vline_pos_low_x, y = vline_pos_low_y, labels = vline_lab_low, pos = 4, col = "black", cex = 0.8)

    # MMT (min RR) line
    vline_pos_min_x <- quantile(geog_temp, minpercgeog_[geog] / 100, na.rm = TRUE)
    min_rr <- min(geog_pred$allRRfit, na.rm = TRUE)

    if (dplyr::between(min_rr, 0.90, 1.1)) {
      vline_pos_min_y <- min_rr + 0.2
    } else {
      vline_pos_min_y <- min_rr + 0.1
    }

    vline_lab_min <- paste0("MMT\n", round(vline_pos_min_x, 2), intToUtf8(176), "C (p", round(minpercgeog_[geog], 2), ")")

    abline(v = vline_pos_min_x, col = "#C75E70", lty = 5)
    text(x = vline_pos_min_x, y = vline_pos_min_y, labels = vline_lab_min, pos = 2, col = "black", cex = 0.8)

    # create histogram on RR plot
    geog_temp_range <- range(geog_temp, na.rm = TRUE)

    hist_data <- hist(geog_temp,
      breaks = seq(floor(geog_temp_range[1]), ceiling(geog_temp_range[2]), by = 1),
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
        col = "#7A855C",
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
    mtext("Frequency (days)", side = 4, line = 3, adj = adj_val, cex = 0.7)
  }

  if (save_fig == TRUE) {
    year_range <- paste0(
      "(",
      min(sapply(df_list, function(x) min(lubridate::year(x$date), na.rm = TRUE))),
      "-",
      max(sapply(df_list, function(x) max(lubridate::year(x$date), na.rm = TRUE))),
      ")"
    )

    title <- paste0("Relative risk of mortality by mean temperature and geography, ", country, " ", year_range)

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
#' @param minpercgeog_ Vector. Percentile of maximum suicide temperature for each region.
#' @param attr_thr_high Integer. Percentile at which to define the upper temperature threshold for
#' calculating attributable risk. Defaults to 97.5.
#' @param attr_thr_low Integer. Percentile at which to define the lower temperature threshold for
#' calculating attributable risk. Defaults to 2.5.
#'
#' @return A list containing attributable numbers per region
#'
#' @keywords internal
hc_attr <- function(df_list,
                    cb_list,
                    pred_list,
                    minpercgeog_,
                    attr_thr_high = 97.5,
                    attr_thr_low = 2.5) {
  attr_list <- list()

  for (geog in names(df_list)) {
    geog_data <- df_list[[geog]]
    cb <- cb_list[[geog]]
    pred <- pred_list[[geog]]
    minperc <- minpercgeog_[geog]

    cen <- quantile(geog_data$temp, minperc / 100, na.rm = TRUE)
    min_temp <- min(geog_data$temp, na.rm = TRUE)
    low_temp <- quantile(geog_data$temp, attr_thr_low / 100, na.rm = TRUE)
    high_temp <- quantile(geog_data$temp, attr_thr_high / 100, na.rm = TRUE)
    max_temp <- max(geog_data$temp, na.rm = TRUE)

    heat <- an_attrdl(
      x = geog_data$temp,
      basis = cb,
      cases = geog_data$dependent,
      coef = pred$coefficients,
      vcov = pred$vcov,
      dir = "forw",
      cen = cen,
      range = c(high_temp, max_temp),
      tot = FALSE,
      nsim = 1000
    )

    cold <- an_attrdl(
      x = geog_data$temp,
      basis = cb,
      cases = geog_data$dependent,
      coef = pred$coefficients,
      vcov = pred$vcov,
      dir = "forw",
      cen = cen,
      range = c(min_temp, low_temp),
      tot = FALSE,
      nsim = 1000
    )

    if (!"region" %in% colnames(geog_data)) {
      geog_data$region <- geog
    }
    results <- geog_data %>%
      dplyr::select(
        any_of(c("region", "date", "temp", "year", "month", "dependent", "population"))
      ) %>%
      dplyr::mutate(
        # outputs for higher, hotter temperatures
        threshold_temp_high = round((high_temp), 2),
        af_heat = heat[[1]],
        af_heat_lower_ci = heat[[2]],
        af_heat_upper_ci = heat[[3]],
        an_heat = heat[[4]],
        an_heat_lower_ci = heat[[5]],
        an_heat_upper_ci = heat[[6]],
        ar_heat = (.data$an_heat / .data$population) * 100000,
        ar_heat_lower_ci = (.data$an_heat_lower_ci / .data$population) * 100000,
        ar_heat_upper_ci = (.data$an_heat_upper_ci / .data$population) * 100000,
        # outputs for lower, colder temperatures
        threshold_temp_low = round((low_temp), 2),
        af_cold = cold[[1]],
        af_cold_lower_ci = cold[[2]],
        af_cold_upper_ci = cold[[3]],
        an_cold = cold[[4]],
        an_cold_lower_ci = cold[[5]],
        an_cold_upper_ci = cold[[6]],
        ar_cold = (.data$an_cold / .data$population) * 100000,
        ar_cold_lower_ci = (.data$an_cold_lower_ci / .data$population) * 100000,
        ar_cold_upper_ci = (.data$an_cold_upper_ci / .data$population) * 100000
      )

    attr_list[[geog]] <- results
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
#' @param meta_analysis Boolean. Whether to perform a meta-analysis.
#'
#' @return
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
hc_attr_tables <- function(attr_list,
                           country = "National",
                           meta_analysis = FALSE) {
  attr_res <- do.call(rbind, attr_list) %>%
    dplyr::mutate(year = as.numeric(as.character(.data$year)))

  res_list <- list()

  groupings <- list(
    monthly = rlang::quos(.data$month, .data$region),
    yearly  = rlang::quos(.data$year, .data$region),
    overall = rlang::quos(.data$region)
  )

  for (grp_name in names(groupings)) {
    results <- attr_res %>%
      dplyr::group_by(!!!groupings[[grp_name]]) %>%
      dplyr::summarise(
        population = round(mean(.data$population, na.rm = TRUE), 0),
        temp = round(mean(.data$temp, na.rm = TRUE), 2),
        threshold_temp_high = mean(.data$threshold_temp_high, na.rm = TRUE),
        threshold_temp_low = mean(.data$threshold_temp_low, na.rm = TRUE),
        dplyr::across(
          c(
            "dependent", "an_heat", "an_heat_lower_ci", "an_heat_upper_ci",
            "an_cold", "an_cold_lower_ci", "an_cold_upper_ci"
          ),
          ~ sum(.x, na.rm = TRUE)
        ),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        af_heat           = .data$an_heat / .data$dependent * 100,
        af_heat_lower_ci  = .data$an_heat_lower_ci / .data$dependent * 100,
        af_heat_upper_ci  = .data$an_heat_upper_ci / .data$dependent * 100,
        ar_heat           = .data$an_heat / .data$population * 100000,
        ar_heat_lower_ci  = .data$an_heat_lower_ci / .data$population * 100000,
        ar_heat_upper_ci  = .data$an_heat_upper_ci / .data$population * 100000,
        af_cold           = .data$an_cold / .data$dependent * 100,
        af_cold_lower_ci  = .data$an_cold_lower_ci / .data$dependent * 100,
        af_cold_upper_ci  = .data$an_cold_upper_ci / .data$dependent * 100,
        ar_cold           = .data$an_cold / .data$population * 100000,
        ar_cold_lower_ci  = .data$an_cold_lower_ci / .data$population * 100000,
        ar_cold_upper_ci  = .data$an_cold_upper_ci / .data$population * 100000
      ) %>%
      dplyr::mutate(
        dplyr::across(
          c(
            "an_heat", "an_heat_lower_ci", "an_heat_upper_ci",
            "ar_heat", "ar_heat_lower_ci", "ar_heat_upper_ci",
            "af_heat", "af_heat_lower_ci", "af_heat_upper_ci",
            "an_cold", "an_cold_lower_ci", "an_cold_upper_ci",
            "ar_cold", "ar_cold_lower_ci", "ar_cold_upper_ci",
            "af_cold", "af_cold_lower_ci", "af_cold_upper_ci"
          ),
          ~ ifelse(abs(.x) < 1, signif(.x, 2), round(.x, 2))
        )
      )

    res_list[[grp_name]] <- results
  }

  geog_order <- if (isTRUE(meta_analysis)) {
    c(sort(setdiff(names(attr_list), country)), country)
  } else {
    sort(names(attr_list))
  }

  res_attr_tot <- res_list[["overall"]]

  attr_yr_list <- aggregate_by_column(res_list[["yearly"]], "region")
  attr_yr_list <- attr_yr_list[geog_order]

  attr_mth_list <- res_list[["monthly"]] %>%
    dplyr::mutate(month = month.name[.data$month]) %>%
    aggregate_by_column("region")
  attr_mth_list <- attr_mth_list[geog_order]

  return(list(res_attr_tot, attr_yr_list, attr_mth_list))
}


#' Plot total attributable fractions and rates - high
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
#'
#' @return Plots of total attributable fractions and rates by area
#'
#' @keywords internal
hc_plot_attr_heat_totals <- function(df_list,
                                     res_attr_tot,
                                     save_fig = FALSE,
                                     output_folder_path = NULL,
                                     country = "National") {
  if (save_fig == TRUE) {
    num_geogs <- nrow(res_attr_tot)

    # Dynamically adjust height based on number of regions
    chart_height <- 6
    chart_width <- 0.3 * num_geogs # adjust as needed
    total_width <- max(8, chart_width)

    output_path <- file.path(output_folder_path, "mortality_total_heat_attr_plot.pdf")
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
  af_heat_ci_range <- c(min(res_attr_tot$af_heat_lower_ci), max(res_attr_tot$af_heat_upper_ci))
  ar_heat_ci_range <- c(min(res_attr_tot$ar_heat_lower_ci), max(res_attr_tot$ar_heat_upper_ci))

  # Format warning messages
  af_warning <- sprintf("Warning: AF CI's range from %.2f%% to %.2f%%", af_heat_ci_range[1], af_heat_ci_range[2])
  ar_warning <- sprintf("Warning: AR CI's range from %.2f to %.2f per 100,000", ar_heat_ci_range[1], ar_heat_ci_range[2])
  ovr_warning <- "(Please refer to the associated data table for more information on the uncertainty around each estimate)"

  # Sort by AF descending
  sorted_indices <- order(res_attr_tot$af_heat, decreasing = TRUE)
  res_af_heat_tot <- res_attr_tot[sorted_indices, ]
  short_labs_af_heat <- short_labels[sorted_indices]

  # Define bar colors
  bar_col_af_heat <- rep("#a04b58", length(short_labs_af_heat))
  nat_ind_af_heat <- which(res_af_heat_tot$region == country)
  if (length(nat_ind_af_heat) > 0) {
    bar_col_af_heat[nat_ind_af_heat] <- "#7a855c" # Highlight color
  }

  barplot(
    names.arg = short_labs_af_heat,
    height = res_af_heat_tot$af_heat,
    ylab = "High temperature AF (%)",
    main = paste0("Attributable fraction of high temperature mortality by geography, ", country, " ", year_range),
    col = bar_col_af_heat,
    las = 2,
    horiz = FALSE
  )

  mtext(af_warning, side = 1, line = 7, cex = 0.8, col = "red", font = 3)
  mtext(ovr_warning, side = 1, line = 8, cex = 0.8, col = "red", font = 3)

  if (save_fig == TRUE) {
    par(mar = c(10, 5, 4, 2))
  }

  # Sort by AF descending
  sorted_indices <- order(res_attr_tot$ar_heat, decreasing = TRUE)
  res_ar_heat_tot <- res_attr_tot[sorted_indices, ]
  short_labs_ar_heat <- short_labels[sorted_indices]

  # Define bar colors
  bar_col_ar_heat <- rep("#c75e70", length(short_labs_ar_heat))
  nat_ind_ar_heat <- which(res_ar_heat_tot$region == country)
  if (length(nat_ind_ar_heat) > 0) {
    bar_col_ar_heat[nat_ind_ar_heat] <- "#7a855c" # Highlight color
  }

  barplot(
    names.arg = short_labs_ar_heat,
    height = res_ar_heat_tot$ar_heat,
    ylab = "High temperature AR (per 100,000 population)",
    main = paste0("Attributable rate of high temperature mortality by geography, ", country, " ", year_range),
    col = bar_col_ar_heat,
    las = 2,
    horiz = FALSE
  )

  mtext(ar_warning, side = 1, line = 7, cex = 0.8, col = "red", font = 3)
  mtext(ovr_warning, side = 1, line = 8, cex = 0.8, col = "red", font = 3)

  if (save_fig == TRUE) {
    dev.off()
  }
}



#' Plot total attributable fractions and rates - low temps
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
#'
#' @return Plots of total attributable fractions and rates by area
#'
#' @keywords internal
hc_plot_attr_cold_totals <- function(df_list,
                                     res_attr_tot,
                                     save_fig = FALSE,
                                     output_folder_path = NULL,
                                     country = "National") {
  if (save_fig == TRUE) {
    num_geogs <- nrow(res_attr_tot)

    # Dynamically adjust height based on number of regions
    chart_height <- 6
    chart_width <- 0.3 * num_geogs # adjust as needed
    total_width <- max(8, chart_width)

    output_path <- file.path(output_folder_path, "mortality_total_cold_attr_plot.pdf")
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
  af_cold_ci_range <- c(min(res_attr_tot$af_cold_lower_ci), max(res_attr_tot$af_cold_upper_ci))
  ar_cold_ci_range <- c(min(res_attr_tot$ar_cold_lower_ci), max(res_attr_tot$ar_cold_upper_ci))

  # Format warning messages
  af_warning <- sprintf("Warning: AF CI's range from %.2f%% to %.2f%%", af_cold_ci_range[1], af_cold_ci_range[2])
  ar_warning <- sprintf("Warning: AR CI's range from %.2f to %.2f per 100,000", ar_cold_ci_range[1], ar_cold_ci_range[2])
  ovr_warning <- "(Please refer to the associated data table for more information on the uncertainty around each estimate)"

  # Sort by AF descending
  sorted_indices <- order(res_attr_tot$af_cold, decreasing = TRUE)
  res_af_cold_tot <- res_attr_tot[sorted_indices, ]
  short_labs_af_cold <- short_labels[sorted_indices]

  # Define bar colors
  bar_col_af_cold <- rep("#0A2E4D", length(short_labs_af_cold))
  nat_ind_af_cold <- which(res_af_cold_tot$region == country)
  if (length(nat_ind_af_cold) > 0) {
    bar_col_af_cold[nat_ind_af_cold] <- "#7a855c" # Highlight color
  }

  barplot(
    names.arg = short_labs_af_cold,
    height = res_af_cold_tot$af_cold,
    ylab = "Low temperature AF (%)",
    main = paste0("Attributable fraction of low temperature mortality by geography, ", country, " ", year_range),
    col = bar_col_af_cold,
    las = 2,
    horiz = FALSE
  )

  mtext(af_warning, side = 1, line = 7, cex = 0.8, col = "red", font = 3)
  mtext(ovr_warning, side = 1, line = 8, cex = 0.8, col = "red", font = 3)

  if (save_fig == TRUE) {
    par(mar = c(10, 5, 4, 2))
  }

  # Sort by AF descending
  sorted_indices <- order(res_attr_tot$ar_cold, decreasing = TRUE)
  res_ar_cold_tot <- res_attr_tot[sorted_indices, ]
  short_labs_ar_cold <- short_labels[sorted_indices]

  # Define bar colors
  bar_col_ar_cold <- rep("#296991", length(short_labs_ar_cold))
  nat_ind_ar_cold <- which(res_ar_cold_tot$region == country)
  if (length(nat_ind_ar_cold) > 0) {
    bar_col_ar_cold[nat_ind_ar_cold] <- "#7a855c" # Highlight color
  }

  barplot(
    names.arg = short_labs_ar_cold,
    height = res_ar_cold_tot$ar_cold,
    ylab = "Low temperature AR (per 100,000 population)",
    main = paste0("Attributable rate of low temperature mortality by geography, ", country, " ", year_range),
    col = bar_col_ar_cold,
    las = 2,
    horiz = FALSE
  )

  mtext(ar_warning, side = 1, line = 7, cex = 0.8, col = "red", font = 3)
  mtext(ovr_warning, side = 1, line = 8, cex = 0.8, col = "red", font = 3)

  if (save_fig == TRUE) {
    dev.off()
  }
}


#' Plot attributable fractions for heat by year
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
#'
#' @return Plots of yearly attributable fractions per area
#'
#' @keywords internal
hc_plot_af_heat_yearly <- function(attr_yr_list,
                                   save_fig = FALSE,
                                   output_folder_path = NULL,
                                   country = "National") {
  if (save_fig == TRUE) {
    grid <- c(min(length(attr_yr_list), 3), ceiling(length(attr_yr_list) / 3))
    output_path <- file.path(output_folder_path, "mortality_af_heat_timeseries.pdf")
    pdf(output_path, width = max(10, grid[1] * 5.5), height = max(7, grid[2] * 4.5))

    par(mfrow = c(grid[2], grid[1]), oma = c(0, 0, 4, 0), mar = c(8, 4, 5, 4))
  }

  year_min <- min(sapply(attr_yr_list, function(x) min(x$year, na.rm = TRUE)))
  year_max <- max(sapply(attr_yr_list, function(x) max(x$year, na.rm = TRUE)))

  y_min <- min(sapply(attr_yr_list, function(x) min(x$af_heat, na.rm = TRUE)))
  y_max <- max(sapply(attr_yr_list, function(x) max(x$af_heat, na.rm = TRUE)))

  ylim <- c(min(c(0, y_min)), y_max) * 1.5

  for (geog in names(attr_yr_list)) {
    geog_af <- as.data.frame(attr_yr_list[[geog]])

    plot(
      x = geog_af$year,
      y = geog_af$af_heat,
      type = "l",
      xlim = c(year_min, year_max),
      ylim = ylim,
      xlab = "Year",
      ylab = "High temperature AF (%)",
      main = geog,
      col = "#c75e70"
    )

    # Ensure data is sorted by Year
    geog_af <- geog_af[order(geog_af$year), ]

    # Create x and y coordinates for the polygon
    x_poly <- c(geog_af$year, rev(geog_af$year))
    y_poly <- c(geog_af$af_heat_upper_ci, rev(geog_af$af_heat_lower_ci))

    # Draw shaded confidence interval
    polygon(
      x = x_poly,
      y = y_poly,
      col = adjustcolor("#c75e70", alpha.f = 0.2),
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
      col = adjustcolor("#c75e70", alpha.f = 0.2),
      pch = 15,
      pt.cex = 2,
      bty = "n",
      xpd = TRUE,
      horiz = TRUE,
      cex = 0.9
    )

    if (save_fig == TRUE) {
      af_heat_ci_range <- c(min(geog_af$af_heat_lower_ci), max(geog_af$af_heat_upper_ci))

      if (af_heat_ci_range[1] < ylim[1] || af_heat_ci_range[2] > ylim[2]) {
        ci_warning <- sprintf("Warning: CI's are outside the bounds of this chart. CI's range from %.2f%% to %.2f%%", af_heat_ci_range[1], af_heat_ci_range[2])
        ovr_warning <- "(Please refer to the associated data table for more information on the uncertainty around each estimate)"

        mtext(ci_warning, side = 1, line = 5, cex = 0.6, col = "red", font = 3)
        mtext(ovr_warning, side = 1, line = 6, cex = 0.6, col = "red", font = 3)
      }
    }
  }

  if (save_fig == TRUE) {
    year_range <- paste0("(", year_min, " - ", year_max, ")")
    title <- paste0("Yearly attributable fraction of high temperature mortality by geography, ", country, " ", year_range)

    mtext(title, outer = TRUE, cex = 1.5, line = 1, font = 2)

    dev.off()
  }
}



#' Plot attributable fractions for cold by year
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
#'
#' @return Plots of yearly attributable fractions per area
#'
#' @keywords internal
hc_plot_af_cold_yearly <- function(attr_yr_list,
                                   save_fig = FALSE,
                                   output_folder_path = NULL,
                                   country = "National") {
  if (save_fig == TRUE) {
    grid <- c(min(length(attr_yr_list), 3), ceiling(length(attr_yr_list) / 3))
    output_path <- file.path(output_folder_path, "mortality_af_cold_timeseries.pdf")
    pdf(output_path, width = max(10, grid[1] * 5.5), height = max(7, grid[2] * 4.5))

    par(mfrow = c(grid[2], grid[1]), oma = c(0, 0, 4, 0), mar = c(8, 4, 5, 4))
  }

  year_min <- min(sapply(attr_yr_list, function(x) min(x$year, na.rm = TRUE)))
  year_max <- max(sapply(attr_yr_list, function(x) max(x$year, na.rm = TRUE)))
  y_min <- min(sapply(attr_yr_list, function(x) min(x$af_cold, na.rm = TRUE)))
  y_max <- max(sapply(attr_yr_list, function(x) max(x$af_cold, na.rm = TRUE)))

  ylim <- c(min(c(0, y_min)), y_max) * 1.5

  for (geog in names(attr_yr_list)) {
    geog_af <- as.data.frame(attr_yr_list[[geog]])

    plot(
      x = geog_af$year,
      y = geog_af$af_cold,
      type = "l",
      xlim = c(year_min, year_max),
      ylim = ylim,
      xlab = "Year",
      ylab = "Low temperature AF (%)",
      main = geog,
      col = "#296991"
    )

    # Ensure data is sorted by Year
    geog_af <- geog_af[order(geog_af$year), ]

    # Create x and y coordinates for the polygon
    x_poly <- c(geog_af$year, rev(geog_af$year))
    y_poly <- c(geog_af$af_cold_upper_ci, rev(geog_af$af_cold_lower_ci))

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
      af_cold_ci_range <- c(min(geog_af$af_cold_lower_ci), max(geog_af$af_cold_upper_ci))

      if (af_cold_ci_range[1] < ylim[1] || af_cold_ci_range[2] > ylim[2]) {
        ci_warning <- sprintf("Warning: CI's are outside the bounds of this chart. CI's range from %.2f%% to %.2f%%", af_cold_ci_range[1], af_cold_ci_range[2])
        ovr_warning <- "(Please refer to the associated data table for more information on the uncertainty around each estimate)"

        mtext(ci_warning, side = 1, line = 5, cex = 0.6, col = "red", font = 3)
        mtext(ovr_warning, side = 1, line = 6, cex = 0.6, col = "red", font = 3)
      }
    }
  }

  if (save_fig == TRUE) {
    year_range <- paste0("(", year_min, " - ", year_max, ")")
    title <- paste0("Yearly attributable fraction of low temperature mortality by geography, ", country, " ", year_range)

    mtext(title, outer = TRUE, cex = 1.5, line = 1, font = 2)

    dev.off()
  }
}


#' Plot attributable rates by year - high temps
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
#'
#' @return Plots of yearly attributable rates per area
#'
#' @keywords internal
hc_plot_ar_heat_yearly <- function(attr_yr_list,
                                   save_fig = FALSE,
                                   output_folder_path = NULL,
                                   country = "National") {
  if (save_fig == TRUE) {
    grid <- c(min(length(attr_yr_list), 3), ceiling(length(attr_yr_list) / 3))
    output_path <- file.path(output_folder_path, "mortality_ar_heat_timeseries.pdf")
    pdf(output_path, width = max(10, grid[1] * 5.5), height = max(7, grid[2] * 4.5))

    par(mfrow = c(grid[2], grid[1]), oma = c(0, 0, 4, 0), mar = c(8, 4, 5, 4))
  }

  year_min <- min(sapply(attr_yr_list, function(x) min(x$year, na.rm = TRUE)))
  year_max <- max(sapply(attr_yr_list, function(x) max(x$year, na.rm = TRUE)))

  y_min <- min(sapply(attr_yr_list, function(x) min(x$ar_heat, na.rm = TRUE)))
  y_max <- max(sapply(attr_yr_list, function(x) max(x$ar_heat, na.rm = TRUE)))

  ylim <- c(min(c(0, y_min)), y_max) * 1.5

  for (geog in names(attr_yr_list)) {
    geog_ar <- as.data.frame(attr_yr_list[[geog]])

    plot(
      x = geog_ar$year,
      y = geog_ar$ar_heat,
      type = "l",
      xlim = c(year_min, year_max),
      ylim = ylim,
      xlab = "Year",
      ylab = "High temperature AR (per 100,000 population)",
      main = geog,
      col = "#C75E70"
    )

    # Ensure data is sorted by Year
    geog_ar <- geog_ar[order(geog_ar$year), ]

    # Create x and y coordinates for the polygon
    x_poly <- c(geog_ar$year, rev(geog_ar$year))
    y_poly <- c(geog_ar$ar_heat_upper_ci, rev(geog_ar$ar_heat_lower_ci))

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
      ar_heat_ci_range <- c(min(geog_ar$ar_heat_lower_ci), max(geog_ar$ar_heat_upper_ci))

      if (ar_heat_ci_range[1] < ylim[1] || ar_heat_ci_range[2] > ylim[2]) {
        ci_warning <- sprintf("Warning: CI's are outside the bounds of this chart. CI's range from %.2f to %.2f per 100,000", ar_heat_ci_range[1], ar_heat_ci_range[2])
        ovr_warning <- "(Please refer to the associated data table for more information on the uncertainty around each estimate)"

        mtext(ci_warning, side = 1, line = 5, cex = 0.6, col = "red", font = 3)
        mtext(ovr_warning, side = 1, line = 6, cex = 0.6, col = "red", font = 3)
      }
    }
  }

  if (save_fig == TRUE) {
    year_range <- paste0("(", year_min, " - ", year_max, ")")
    title <- paste0("Yearly attributable rate of high temperature mortality by geography, ", country, " ", year_range)

    mtext(title, outer = TRUE, cex = 1.5, line = 1, font = 2)

    dev.off()
  }
}


#' Plot attributable rates by year - low temps
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
#'
#' @return Plots of yearly attributable rates per area
#'
#' @keywords internal
hc_plot_ar_cold_yearly <- function(attr_yr_list,
                                   save_fig = FALSE,
                                   output_folder_path = NULL,
                                   country = "National") {
  if (save_fig == TRUE) {
    grid <- c(min(length(attr_yr_list), 3), ceiling(length(attr_yr_list) / 3))
    output_path <- file.path(output_folder_path, "mortality_ar_cold_timeseries.pdf")
    pdf(output_path, width = max(10, grid[1] * 5.5), height = max(7, grid[2] * 4.5))

    par(mfrow = c(grid[2], grid[1]), oma = c(0, 0, 4, 0), mar = c(8, 4, 5, 4))
  }

  year_min <- min(sapply(attr_yr_list, function(x) min(x$year, na.rm = TRUE)))
  year_max <- max(sapply(attr_yr_list, function(x) max(x$year, na.rm = TRUE)))

  y_min <- min(sapply(attr_yr_list, function(x) min(x$ar_cold, na.rm = TRUE)))
  y_max <- max(sapply(attr_yr_list, function(x) max(x$ar_cold, na.rm = TRUE)))

  ylim <- c(min(c(0, y_min)), y_max) * 1.5

  for (geog in names(attr_yr_list)) {
    geog_ar <- as.data.frame(attr_yr_list[[geog]])

    plot(
      x = geog_ar$year,
      y = geog_ar$ar_cold,
      type = "l",
      xlim = c(year_min, year_max),
      ylim = ylim,
      xlab = "Year",
      ylab = "Low temperature AR (per 100,000 population)",
      main = geog,
      col = "#296991"
    )

    # Ensure data is sorted by Year
    geog_ar <- geog_ar[order(geog_ar$year), ]

    # Create x and y coordinates for the polygon
    x_poly <- c(geog_ar$year, rev(geog_ar$year))
    y_poly <- c(geog_ar$ar_cold_upper_ci, rev(geog_ar$ar_cold_lower_ci))

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
      ar_cold_ci_range <- c(min(geog_ar$ar_cold_lower_ci), max(geog_ar$ar_cold_upper_ci))

      if (ar_cold_ci_range[1] < ylim[1] || ar_cold_ci_range[2] > ylim[2]) {
        ci_warning <- sprintf("Warning: CI's are outside the bounds of this chart. CI's range from %.2f to %.2f per 100,000", ar_cold_ci_range[1], ar_cold_ci_range[2])
        ovr_warning <- "(Please refer to the associated data table for more information on the uncertainty around each estimate)"

        mtext(ci_warning, side = 1, line = 5, cex = 0.6, col = "red", font = 3)
        mtext(ovr_warning, side = 1, line = 6, cex = 0.6, col = "red", font = 3)
      }
    }
  }

  if (save_fig == TRUE) {
    year_range <- paste0("(", year_min, " - ", year_max, ")")
    title <- paste0("Yearly attributable rate of low temperature mortality by geography, ", country, " ", year_range)

    mtext(title, outer = TRUE, cex = 1.5, line = 1, font = 2)

    dev.off()
  }
}


#' Plot attributable fractions by calendar month - high temps
#'
#' @description Plot attributable fractions grouped over the whole time series by
#' calendar month to explore seasonality.
#'
#' @param attr_mth_list A list of data frames containing total attributable
#' fractions, numbers and rates by calendar month and area.
#' @param df_list A list of dataframes containing daily timeseries data for a health outcome
#' and climate variables which may be disaggregated by a particular region.
#' @param country Character. Name of country for national level estimates.
#' @param attr_thr_high Integer. Percentile at which to define the upper temperature threshold for
#' calculating attributable risk. Defaults to 97.5.
#' @param save_fig Boolean. Whether to save the plot as an output. Defaults to
#' FALSE.
#' @param output_folder_path Path to folder where plots should be saved.
#' Defaults to NULL.
#'
#' @return Plots of attributable fractions by calendar month per area
#'
#' @keywords internal
hc_plot_af_heat_monthly <- function(attr_mth_list,
                                    df_list,
                                    country = "National",
                                    attr_thr_high = 97.5,
                                    save_fig = FALSE,
                                    output_folder_path = NULL) {
  if (save_fig == TRUE) {
    grid <- c(min(length(attr_mth_list), 3), ceiling(length(attr_mth_list) / 3))
    output_path <- file.path(output_folder_path, "mortality_af_heat_month_plot.pdf")
    pdf(output_path, width = max(10, grid[1] * 4.5), height = max(8, grid[2] * 4.5))

    par(mfrow = c(grid[2], grid[1]), mar = c(5, 5, 5, 5), oma = c(4, 0, 4, 0))
  }

  ylim_max <- max(sapply(attr_mth_list, function(x) max(x$af_heat, na.rm = TRUE)))

  ylim2_min <- min(sapply(attr_mth_list, function(x) min(x$temp, na.rm = TRUE)))
  ylim2_max <- max(sapply(attr_mth_list, function(x) max(x$temp, na.rm = TRUE)))

  scale_factor <- (1 / ylim2_max) * ylim_max

  temp_ticks <- pretty(c(min(0, ylim2_min), ylim2_max))

  ylim <- c(min(0, temp_ticks[1] * scale_factor), max(temp_ticks[length(temp_ticks)] * scale_factor, ylim_max))

  for (geog in names(attr_mth_list)) {
    geog_af <- attr_mth_list[[geog]]
    geog_temp <- df_list[[geog]]$temp

    temp_scaled <- geog_af$temp * scale_factor

    bar_pos <- barplot(
      names.arg = substr(geog_af$month, 1, 1),
      height = geog_af$af_heat,
      ylim = ylim,
      xlab = "Month",
      ylab = "High temperature AF (%)",
      main = geog,
      col = "#C75E70"
    )

    lines(
      x = bar_pos,
      y = temp_scaled,
      type = "o",
      col = "#a04b58",
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

    attr_thr_high_tmp <- round(quantile(geog_temp, attr_thr_high / 100, na.rm = TRUE), 2)
    af_leg_lab <- paste0("High temperature AF (%) - from treshold, ", attr_thr_high_tmp, "\u00b0C (", attr_thr_high, "p)")

    legend("topleft",
      inset = c(0, -0.05),
      legend = c(af_leg_lab, "Mean Temp (\u00b0C)"),
      fill = c("#C75E70", NA),
      border = NA,
      lty = c(NA, 1),
      pch = c(NA, 16),
      col = c("#C75E70", "#a04b58"),
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

    title <- paste0("Attributable fraction of high temperature mortality by calendar month and geography, ", country, " ", year_range)

    mtext(title, outer = TRUE, cex = 1.5, line = 1, font = 2)

    af_heat_ci_min <- min(sapply(attr_mth_list, function(x) min(x$af_heat_lower_ci, na.rm = TRUE)))
    af_heat_ci_max <- max(sapply(attr_mth_list, function(x) max(x$af_heat_upper_ci, na.rm = TRUE)))
    af_heat_ci_range <- c(af_heat_ci_min, af_heat_ci_max)

    ci_warning <- sprintf("Warning: CI's range from %.2f%% to %.2f%%", af_heat_ci_range[1], af_heat_ci_range[2])
    ovr_warning <- "(Please refer to the associated data table for more information on the uncertainty around each estimate)"

    mtext(ci_warning, outer = TRUE, side = 1, line = 1, cex = 0.8, col = "red", font = 3)
    mtext(ovr_warning, outer = TRUE, side = 1, line = 2, cex = 0.8, col = "red", font = 3)
  }

  dev.off()
}


#' Plot attributable fractions by calendar month - low temps
#'
#' @description Plot attributable fractions grouped over the whole time series by
#' calendar month to explore seasonality.
#'
#' @param attr_mth_list A list of data frames containing total attributable
#' fractions, numbers and rates by calendar month and area.
#' @param df_list A list of dataframes containing daily timeseries data for a health outcome
#' and climate variables which may be disaggregated by a particular region.
#' @param country Character. Name of country for national level estimates.
#' @param attr_thr_low Integer. Percentile at which to define the lower temperature threshold for
#' calculating attributable risk. Defaults to 2.5.
#' @param save_fig Boolean. Whether to save the plot as an output. Defaults to
#' FALSE.
#' @param output_folder_path Path to folder where plots should be saved.
#' Defaults to NULL.
#'
#' @return Plots of attributable fractions by calendar month per area
#'
#' @keywords internal
hc_plot_af_cold_monthly <- function(attr_mth_list,
                                    df_list,
                                    country = "National",
                                    attr_thr_low = 2.5,
                                    save_fig = FALSE,
                                    output_folder_path = NULL) {
  if (save_fig == TRUE) {
    grid <- c(min(length(attr_mth_list), 3), ceiling(length(attr_mth_list) / 3))
    output_path <- file.path(output_folder_path, "mortality_af_cold_month_plot.pdf")
    pdf(output_path, width = max(10, grid[1] * 4.5), height = max(8, grid[2] * 4.5))

    par(mfrow = c(grid[2], grid[1]), mar = c(5, 5, 5, 5), oma = c(4, 0, 4, 0))
  }

  ylim_max <- max(sapply(attr_mth_list, function(x) max(x$af_cold, na.rm = TRUE)))

  ylim2_min <- min(sapply(attr_mth_list, function(x) min(x$temp, na.rm = TRUE)))
  ylim2_max <- max(sapply(attr_mth_list, function(x) max(x$temp, na.rm = TRUE)))

  scale_factor <- (1 / ylim2_max) * ylim_max

  temp_ticks <- pretty(c(min(0, ylim2_min), ylim2_max))

  ylim <- c(min(0, temp_ticks[1] * scale_factor), max(temp_ticks[length(temp_ticks)] * scale_factor, ylim_max))

  for (geog in names(attr_mth_list)) {
    geog_af <- attr_mth_list[[geog]]
    geog_temp <- df_list[[geog]]$temp

    temp_scaled <- geog_af$temp * scale_factor

    bar_pos <- barplot(
      names.arg = substr(geog_af$month, 1, 1),
      height = geog_af$af_cold,
      ylim = ylim,
      xlab = "Month",
      ylab = "Low temperature AF (%)",
      main = geog,
      col = "#296991"
    )

    lines(
      x = bar_pos,
      y = temp_scaled,
      type = "o",
      col = "#0A2E4D",
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

    attr_thr_low_tmp <- round(quantile(geog_temp, attr_thr_low / 100, na.rm = TRUE), 2)
    af_leg_lab <- paste0("Low temperature AF (%) - from treshold, ", attr_thr_low_tmp, "\u00b0C (", attr_thr_low, "p)")

    legend("topleft",
      inset = c(0, -0.05),
      legend = c(af_leg_lab, "Mean Temp (\u00b0C)"),
      fill = c("#296991", NA),
      border = NA,
      lty = c(NA, 1),
      pch = c(NA, 16),
      col = c("#296991", "#0A2E4D"),
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

    title <- paste0("Attributable fraction of low temperature mortality by calendar month and geography, ", country, " ", year_range)

    mtext(title, outer = TRUE, cex = 1.5, line = 1, font = 2)

    af_cold_ci_min <- min(sapply(attr_mth_list, function(x) min(x$af_cold_lower_ci, na.rm = TRUE)))
    af_cold_ci_max <- max(sapply(attr_mth_list, function(x) max(x$af_cold_upper_ci, na.rm = TRUE)))
    af_cold_ci_range <- c(af_cold_ci_min, af_cold_ci_max)

    ci_warning <- sprintf("Warning: CI's range from %.2f%% to %.2f%%", af_cold_ci_range[1], af_cold_ci_range[2])
    ovr_warning <- "(Please refer to the associated data table for more information on the uncertainty around each estimate)"

    mtext(ci_warning, outer = TRUE, side = 1, line = 1, cex = 0.8, col = "red", font = 3)
    mtext(ovr_warning, outer = TRUE, side = 1, line = 2, cex = 0.8, col = "red", font = 3)
  }

  dev.off()
}


#' Plot attributable rates by calendar month - hight temps
#'
#' @description Plot attributable rates grouped over the whole time series by
#' calendar month to explore seasonality.
#'
#' @param attr_mth_list A list of data frames containing total attributable
#' fractions, numbers and rates by calendar month and area.
#' @param df_list A list of dataframes containing daily timeseries data for a health outcome
#' and climate variables which may be disaggregated by a particular region.
#' @param country Character. Name of country for national level estimates.
#' @param attr_thr_high Integer. Percentile at which to define the upper temperature threshold for
#' calculating attributable risk. Defaults to 97.5.
#' @param save_fig Boolean. Whether to save the plot as an output. Defaults to
#' FALSE.
#' @param output_folder_path Path to folder where plots should be saved.
#' Defaults to NULL.
#'
#' @return Plots of attributable rates by calendar month per area
#'
#' @keywords internal
hc_plot_ar_heat_monthly <- function(attr_mth_list,
                                    df_list,
                                    country = "National",
                                    attr_thr_high = 97.5,
                                    save_fig = FALSE,
                                    output_folder_path = NULL) {
  if (save_fig == TRUE) {
    grid <- c(min(length(attr_mth_list), 3), ceiling(length(attr_mth_list) / 3))
    output_path <- file.path(output_folder_path, "mortality_ar_heat_month_plot.pdf")
    pdf(output_path, width = max(10, grid[1] * 4.5), height = max(8, grid[2] * 4.5))

    par(mfrow = c(grid[2], grid[1]), mar = c(5, 5, 5, 5), oma = c(4, 0, 4, 0))
  }

  ylim_max <- max(sapply(attr_mth_list, function(x) max(x$ar_heat, na.rm = TRUE)))

  ylim2_min <- min(sapply(attr_mth_list, function(x) min(x$temp, na.rm = TRUE)))
  ylim2_max <- max(sapply(attr_mth_list, function(x) max(x$temp, na.rm = TRUE)))

  scale_factor <- (1 / ylim2_max) * ylim_max

  temp_ticks <- pretty(c(min(0, ylim2_min), ylim2_max))

  ylim <- c(min(0, temp_ticks[1] * scale_factor), max(temp_ticks[length(temp_ticks)] * scale_factor, ylim_max))

  for (geog in names(attr_mth_list)) {
    geog_ar <- attr_mth_list[[geog]]
    geog_temp <- df_list[[geog]]$temp

    temp_scaled <- geog_ar$temp * scale_factor

    bar_pos <- barplot(
      names.arg = substr(geog_ar$month, 1, 1),
      height = geog_ar$ar_heat,
      ylim = ylim,
      xlab = "Month",
      ylab = "High temperature AR (per 100,000 population)",
      main = geog,
      col = "#c75e70"
    )

    lines(
      x = bar_pos,
      y = temp_scaled,
      type = "o",
      col = "#a04b58",
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

    attr_thr_high_tmp <- round(quantile(geog_temp, attr_thr_high / 100, na.rm = TRUE), 2)
    ar_leg_lab <- paste0("High temperature AR - from threshold, ", attr_thr_high_tmp, "\u00b0C (", attr_thr_high, "p)")

    legend("topleft",
      inset = c(0, -0.05),
      legend = c(ar_leg_lab, "Mean Temp (\u00b0C)"),
      fill = c("#c75e70", NA),
      border = NA,
      lty = c(NA, 1),
      pch = c(NA, 16),
      col = c("#c75e70", "#a04b58"),
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

    title <- paste0("Attributable rate of high temperature mortality by calendar month and geography, ", country, " ", year_range)

    mtext(title, outer = TRUE, cex = 1.5, line = 1, font = 2)

    ar_heat_ci_min <- min(sapply(attr_mth_list, function(x) min(x$ar_heat_lower_ci, na.rm = TRUE)))
    ar_heat_ci_max <- max(sapply(attr_mth_list, function(x) max(x$ar_heat_upper_ci, na.rm = TRUE)))
    ar_heat_ci_range <- c(ar_heat_ci_min, ar_heat_ci_max)

    ci_warning <- sprintf("Warning: CI's range from %.2f to %.2f per 100,000 population", ar_heat_ci_range[1], ar_heat_ci_range[2])
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
#' @param attr_thr_low Integer. Percentile at which to define the lower temperature threshold for
#' calculating attributable risk. Defaults to 2.5.
#' @param save_fig Boolean. Whether to save the plot as an output. Defaults to
#' FALSE.
#' @param output_folder_path Path to folder where plots should be saved.
#' Defaults to NULL.
#'
#' @return Plots of attributable rates by calendar month per area
#'
#' @keywords internal
hc_plot_ar_cold_monthly <- function(attr_mth_list,
                                    df_list,
                                    country = "National",
                                    attr_thr_low = 2.5,
                                    save_fig = FALSE,
                                    output_folder_path = NULL) {
  if (save_fig == TRUE) {
    grid <- c(min(length(attr_mth_list), 3), ceiling(length(attr_mth_list) / 3))
    output_path <- file.path(output_folder_path, "mortality_ar_cold_month_plot.pdf")
    pdf(output_path, width = max(10, grid[1] * 4.5), height = max(8, grid[2] * 4.5))

    par(mfrow = c(grid[2], grid[1]), mar = c(5, 5, 5, 5), oma = c(4, 0, 4, 0))
  }

  ylim_max <- max(sapply(attr_mth_list, function(x) max(x$ar_cold, na.rm = TRUE)))

  ylim2_min <- min(sapply(attr_mth_list, function(x) min(x$temp, na.rm = TRUE)))
  ylim2_max <- max(sapply(attr_mth_list, function(x) max(x$temp, na.rm = TRUE)))

  scale_factor <- (1 / ylim2_max) * ylim_max

  temp_ticks <- pretty(c(min(0, ylim2_min), ylim2_max))

  ylim <- c(min(0, temp_ticks[1] * scale_factor), max(temp_ticks[length(temp_ticks)] * scale_factor, ylim_max))

  for (geog in names(attr_mth_list)) {
    geog_ar <- attr_mth_list[[geog]]
    geog_temp <- df_list[[geog]]$temp

    temp_scaled <- geog_ar$temp * scale_factor

    bar_pos <- barplot(
      names.arg = substr(geog_ar$month, 1, 1),
      height = geog_ar$ar_cold,
      ylim = ylim,
      xlab = "Month",
      ylab = "Low temperature AR (per 100,000 population)",
      main = geog,
      col = "#296991"
    )

    lines(
      x = bar_pos,
      y = temp_scaled,
      type = "o",
      col = "#0A2E4D",
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

    attr_thr_low_tmp <- round(quantile(geog_temp, attr_thr_low / 100, na.rm = TRUE), 2)
    ar_leg_lab <- paste0("Low temperature AR - from threshold, ", attr_thr_low_tmp, "\u00b0C (", attr_thr_low, "p)")

    legend("topleft",
      inset = c(0, -0.05),
      legend = c(ar_leg_lab, "Mean Temp (\u00b0C)"),
      fill = c("#296991", NA),
      border = NA,
      lty = c(NA, 1),
      pch = c(NA, 16),
      col = c("#296991", "#0A2E4D"),
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

    title <- paste0("Attributable rate of low temperature mortality by calendar month and geography, ", country, " ", year_range)

    mtext(title, outer = TRUE, cex = 1.5, line = 1, font = 2)

    ar_cold_ci_min <- min(sapply(attr_mth_list, function(x) min(x$ar_cold_lower_ci, na.rm = TRUE)))
    ar_cold_ci_max <- max(sapply(attr_mth_list, function(x) max(x$ar_cold_upper_ci, na.rm = TRUE)))
    ar_cold_ci_range <- c(ar_cold_ci_min, ar_cold_ci_max)

    ci_warning <- sprintf("Warning: CI's range from %.2f to %.2f per 100,000 population", ar_cold_ci_range[1], ar_cold_ci_range[2])
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
#' @param output_folder_path Path to folder where results should be saved.
#' Defaults to NULL.
#'
#' @keywords internal
hc_save_results <- function(rr_results,
                            res_attr_tot,
                            attr_yr_list,
                            attr_mth_list,
                            power_list_high,
                            power_list_low,
                            output_folder_path = NULL) {
  if (!is.null(output_folder_path)) {
    check_file_exists(file.path(output_folder_path))

    write.csv(rr_results, file = file.path(
      output_folder_path, "mortality_rr_results.csv"
    ), row.names = FALSE)

    write.csv(res_attr_tot, file = file.path(
      output_folder_path, "mortality_attr_tot_results.csv"
    ), row.names = FALSE)

    res_attr_yr <- do.call(rbind, attr_yr_list) %>%
      select(.data$region, everything())

    write.csv(res_attr_yr, file = file.path(
      output_folder_path, "mortality_attr_yr_results.csv"
    ), row.names = FALSE)

    res_attr_mth <- do.call(rbind, attr_mth_list) %>%
      select(.data$region, everything())

    write.csv(res_attr_mth, file = file.path(
      output_folder_path, "mortality_attr_mth_results.csv"
    ), row.names = FALSE)

    res_power_high <- do.call(rbind, power_list_high)

    write.csv(res_power_high, file = file.path(
      output_folder_path, "model_validation", "mortality_high_temp_power_results.csv"
    ), row.names = FALSE)

    res_power_low <- do.call(rbind, power_list_low)

    write.csv(res_power_low, file = file.path(
      output_folder_path, "model_validation", "mortality_low_temp_power_results.csv"
    ), row.names = FALSE)
  } else {
    stop("Output path not specified")
  }
}


#' Run pipeline to analyse the impact of extreme heat on suicides using a time-
#' stratified case-crossover approach with distributed lag non-linear model
#'
#' @description Runs full analysis pipeline for analysis of the impact of
#' extreme heat on suicides
#'
#' @param data_path Path to a csv file containing a daily time series of data
#' for a particular health outcome and climate variables, which may be
#' disaggregated by region.
#' @param date_col Character. Name of the column in the dataframe that contains
#' the date.
#' @param geography_col Character. Name of the column in the dataframe that contains
#' the region names. Defaults to NULL.
#' @param temperature_col Character. Name of the column in the dataframe that
#' contains the temperature column.
#' @param dependent_col Character. Name of the column in the dataframe that
#' contains the health outcome count column (e.g. number of deaths, hospital
#' admissions).
#' @param population_col Character. Name of the column in the dataframe that
#' contains the population estimate column.
#' @param independent_cols Additional independent variables to test in model validation
#' @param control_cols A list of confounders to include in the final model adjustment.
#' Defaults to NULL if none.
#' @param var_fun Character. Exposure function for argvar
#' (see dlnm::crossbasis). Defaults to 'bs'.
#' @param var_degree Integer. Degree of the piecewise polynomial for argvar
#' (see dlnm:crossbasis). Defaults to 2 (quadratic).
#' @param var_per Vector. Internal knot positions for argvar
#' (see dlnm::crossbasis). Defaults to c(25,50,75).
#' @param lagn Integer. Number of days in the lag period. Defaults to 21.
#' (see dlnm::crossbasis).
#' @param lagnk Integer. Number of knots in lag function. Defaults to 3.
#' (see dlnm::logknots).
#' @param dfseas Integer. Degrees of freedom for seasonality.
#' @param save_fig Boolean. Whether to save the plot as an output. Defaults to
#' FALSE.
#' @param save_csv Boolean. Whether to save the results as a CSV. Defaults to
#' FALSE.
#' @param country Character. Name of country for national level estimates.
#' @param meta_analysis Boolean. Whether to perform a meta-analysis.
#' @param attr_thr_high Integer. Percentile at which to define the upper temperature threshold for
#' calculating attributable risk. Defaults to 97.5.
#' @param attr_thr_low Integer. Percentile at which to define the lower temperature threshold for
#' calculating attributable risk. Defaults to 2.5.
#' @param output_folder_path Path to folder where plots and/or CSV should be
#' saved. Defaults to NULL.
#'
#' @return
#' \itemize{
#'   \item `rr_results` Dataframe containing cumulative relative risk and confidence
#' intervals from analysis.
#'   \item `res_attr_tot` Dataframe. Total attributable fractions, numbers and
#'   rates for each area over the whole time series.
#'   \item `attr_yr_list` List. Dataframes containing yearly estimates of
#'   attributable fractions, numbers and rates by area.
#'   \item `attr_mth_list` List. Dataframes containing total attributable
#'   fractions, numbers and rates by calendar month and area.
#'   }
#'
#' @export
temp_mortality_do_analysis <- function(data_path,
                                       date_col,
                                       geography_col,
                                       temperature_col,
                                       dependent_col,
                                       population_col,
                                       independent_cols = NULL,
                                       control_cols = NULL,
                                       var_fun = "bs",
                                       var_degree = 2,
                                       var_per = c(10, 75, 90),
                                       lagn = 21,
                                       lagnk = 3,
                                       dfseas = 8,
                                       save_fig = FALSE,
                                       save_csv = FALSE,
                                       country = "National",
                                       meta_analysis = FALSE,
                                       attr_thr_high = 97.5,
                                       attr_thr_low = 2.5,
                                       output_folder_path = NULL) {
  # Setup additional output DIR
  if (!is.null(output_folder_path)) {
    # Check output dir exists
    check_file_exists(output_folder_path, TRUE)
    new_fpath <- file.path(
      output_folder_path,
      paste0("temperature_mortality_analysis_", format(Sys.time(), "%d_%m_%Y_%H_%M"))
    )
    if (!is.null(new_fpath)) {
      (
        dir.create(new_fpath)
      )
    }
    output_folder_path <- new_fpath
  }

  # Param defences
  if (save_fig == TRUE && is.null(output_folder_path)) {
    stop("Output folder path must be specified if saving figures")
  }
  if (save_csv == TRUE && is.null(output_folder_path)) {
    stop("Output folder path must be specified if saving csv files")
  }
  if (!is.character(country) || length(country) != 1) {
    stop("Country must be a single character string")
  }
  if (attr_thr_high <= attr_thr_low) {
    stop("High temperature threshold must be greater than low temperature threshold")
  }
  if (attr_thr_high >= 100 || attr_thr_low <= 0) {
    stop("Temperature thresholds must be between 0 and 100")
  }

  df_list <- hc_read_data(
    input_csv_path = data_path,
    date_col = date_col,
    region_col = geography_col,
    temperature_col = temperature_col,
    dependent_col = dependent_col,
    population_col = population_col
  )

  pop_list <- dlnm_pop_totals(
    df_list = df_list,
    country = country,
    meta_analysis = meta_analysis
  )

  cb_list <- hc_create_crossbasis(
    df_list = df_list,
    var_fun = var_fun,
    var_degree = var_degree,
    var_per = var_per,
    lagn = lagn,
    lagnk = lagnk,
    dfseas = dfseas
  )

  model_validation <- hc_model_validation(
    df_list = df_list,
    cb_list = cb_list,
    independent_cols = independent_cols,
    dfseas = dfseas,
    save_fig = save_fig,
    save_csv = save_csv,
    output_folder_path = output_folder_path
  )
  qaic_results <- model_validation[[1]]
  qaic_summary <- model_validation[[2]]
  vif_results <- model_validation[[3]]
  vif_summary <- model_validation[[4]]

  model_list <- hc_quasipoisson_dlnm(
    df_list = df_list,
    control_cols = control_cols,
    cb_list = cb_list,
    dfseas = dfseas
  )

  reduced <- dlnm_reduce_cumulative(
    df_list = df_list,
    var_per = var_per,
    var_degree = var_degree,
    cb_list = cb_list,
    model_list = model_list
  )
  coef_ <- reduced[[1]]
  vcov_ <- reduced[[2]]

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
    blup <- NULL
  }

  min_mort_temp <- dlnm_min_mortality_temp(
    df_list = df_list,
    var_fun = var_fun,
    var_per = var_per,
    var_degree = var_degree,
    blup = blup,
    coef_ = coef_,
    meta_analysis = meta_analysis,
    outcome_type = "temperature"
  )
  minpercgeog_ <- min_mort_temp[[1]]
  mintempgeog_ <- min_mort_temp[[2]]

  pred_list <- hc_predict_subnat(
    df_list = df_list,
    var_fun = var_fun,
    var_per = var_per,
    var_degree = var_degree,
    mintempgeog_ = mintempgeog_,
    blup = blup,
    coef_ = coef_,
    vcov_ = vcov_,
    meta_analysis = meta_analysis
  )
  if (meta_analysis == TRUE) {
    nat_data <- hc_add_national_data(
      df_list = df_list,
      pop_list = pop_list,
      var_fun = var_fun,
      var_per = var_per,
      var_degree = var_degree,
      lagn = lagn,
      lagnk = lagnk,
      country = country,
      cb_list = cb_list,
      mm = mm,
      minpercgeog_ = minpercgeog_
    )
    df_list <- nat_data[[1]]
    cb_list <- nat_data[[2]]
    minpercgeog_ <- nat_data[[3]]
    mmpredall <- nat_data[[4]]

    pred_list <- dlnm_predict_nat(
      df_list = df_list,
      var_fun = var_fun,
      var_per = var_per,
      var_degree = var_degree,
      minpercreg = minpercgeog_,
      mmpredall = mmpredall,
      pred_list = pred_list,
      country = country
    )
  }
  power_list <- dlnm_power_list(
    df_list = df_list,
    pred_list = pred_list,
    minperc = minpercgeog_,
    attr_thr_high = attr_thr_high,
    attr_thr_low = attr_thr_low,
    compute_low = TRUE
  )
  power_list_high <- power_list$high
  power_list_low <- power_list$low

  hc_plot_power(
    power_list_high = power_list_high,
    power_list_low = power_list_low,
    save_fig = save_fig,
    output_folder_path = output_folder_path,
    country = country
  )

  rr_results <- hc_rr_results(
    pred_list = pred_list,
    df_list = df_list,
    minpercgeog_ = minpercgeog_,
    attr_thr_high = attr_thr_high,
    attr_thr_low = attr_thr_low
  )

  hc_plot_rr(
    df_list = df_list,
    pred_list = pred_list,
    attr_thr_high = attr_thr_high,
    attr_thr_low = attr_thr_low,
    minpercgeog_ = minpercgeog_,
    country = country,
    save_fig = save_fig,
    output_folder_path = output_folder_path
  )

  attr_list <- hc_attr(
    df_list = df_list,
    cb_list = cb_list,
    pred_list = pred_list,
    minpercgeog_ = minpercgeog_,
    attr_thr_high = attr_thr_high,
    attr_thr_low = attr_thr_low
  )

  attr_tables <- hc_attr_tables(
    attr_list = attr_list,
    country = country,
    meta_analysis = meta_analysis
  )
  res_attr_tot <- attr_tables[[1]]
  attr_yr_list <- attr_tables[[2]]
  attr_mth_list <- attr_tables[[3]]

  hc_plot_attr_heat_totals(
    df_list = df_list,
    res_attr_tot = res_attr_tot,
    save_fig = save_fig,
    output_folder_path = output_folder_path,
    country = country
  )

  hc_plot_attr_cold_totals(
    df_list = df_list,
    res_attr_tot = res_attr_tot,
    save_fig = save_fig,
    output_folder_path = output_folder_path,
    country = country
  )

  hc_plot_af_heat_yearly(
    attr_yr_list = attr_yr_list,
    save_fig = save_fig,
    output_folder_path = output_folder_path,
    country = country
  )

  hc_plot_af_cold_yearly(
    attr_yr_list = attr_yr_list,
    save_fig = save_fig,
    output_folder_path = output_folder_path,
    country = country
  )

  hc_plot_ar_heat_yearly(
    attr_yr_list = attr_yr_list,
    save_fig = save_fig,
    output_folder_path = output_folder_path,
    country = country
  )

  hc_plot_ar_cold_yearly(
    attr_yr_list = attr_yr_list,
    save_fig = save_fig,
    output_folder_path = output_folder_path,
    country = country
  )

  hc_plot_af_heat_monthly(
    attr_mth_list = attr_mth_list,
    df_list = df_list,
    country = country,
    attr_thr_high = attr_thr_high,
    save_fig = save_fig,
    output_folder_path = output_folder_path
  )

  hc_plot_af_cold_monthly(
    attr_mth_list = attr_mth_list,
    df_list = df_list,
    country = country,
    attr_thr_low = attr_thr_low,
    save_fig = save_fig,
    output_folder_path = output_folder_path
  )

  hc_plot_ar_heat_monthly(
    attr_mth_list = attr_mth_list,
    df_list = df_list,
    country = country,
    attr_thr_high = attr_thr_high,
    save_fig = save_fig,
    output_folder_path = output_folder_path
  )

  hc_plot_ar_cold_monthly(
    attr_mth_list = attr_mth_list,
    df_list = df_list,
    country = country,
    attr_thr_low = attr_thr_low,
    save_fig = save_fig,
    output_folder_path = output_folder_path
  )

  if (save_csv == TRUE) {
    hc_save_results(
      rr_results = rr_results,
      res_attr_tot = res_attr_tot,
      attr_yr_list = attr_yr_list,
      attr_mth_list = attr_mth_list,
      power_list_high = power_list_high,
      power_list_low = power_list_low,
      output_folder_path = output_folder_path
    )
  }

  return(
    list(
      rr_results = rr_results,
      an_ar_results = res_attr_tot,
      monthly_an_ar_results = attr_yr_list,
      annual_an_ar_results = attr_mth_list
    )
  )
}
