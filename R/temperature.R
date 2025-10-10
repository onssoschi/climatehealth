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
#' @param geog_col The geography column over which the data
#' are spatially aggregated e.g., geognames.
#' @param temp_col The temperature variable column e.g., tmean.
#' @param population_col The population estimate column e.g., pop.
#'
#' @return An alphabetically-ordered list of dataframes for each
#' geography comprising of dates, deaths, and temperatures.
#'
#' @export
hc_read_data <- function(input_csv_path,
                         dependent_col,
                         date_col,
                         geog_col,
                         temp_col,
                         population_col) {

  # Load the input dataset
  df <- read_input_data(input_csv_path)

  # Format the geography column. If geog_col is missing, then assume geographies are aggregated or only single input geography
  if (is.null(df$geog_col)) {
    df <- df %>%
      dplyr::mutate(geog_col = "aggregated")
    geog_col = "geog_col"
  }
  # Rename the columns
  df <- df %>%
    dplyr::rename(dependent = dependent_col,
                  date = date_col,
                  geog_col = geog_col,
                  temp = temp_col,
                  pop = population_col,
    ) %>%
    dplyr::mutate(date = as.Date(date, tryFormats = c("%d/%m/%Y", "%Y-%m-%d")),
                  year = as.factor(lubridate::year(date)),
                  month = as.factor(lubridate::month(date)),
                  dow = as.factor(lubridate::wday(date, label = TRUE)),
                  geog_col = as.factor(geog_col))

  # Reformat data and fill NaNs
  df <- reformat_data(df,
                      reformat_date = TRUE,
                      fill_na = c("dependent"),
                      year_from_date = TRUE)
  # Split the data by region
  df_list <- aggregate_by_column(df, "geog_col")

  return (list(df_list))

}


#' Create population totals
#'
#' @description Creates a list of population totals by year and geography for use
#' in the attributable number, fraction and rate calculations.
#'
#' @param df_list A list of dataframes containing daily timeseries data for a health outcome
#' and climate variables which may be disaggregated by a particular geography.
#' @param country Character. Name of country for national level estimate.
#' @param meta_analysis Boolean. Whether to perform a meta-analysis.
#'
#' @returns List of population totals by year and region
#'
#' @export
hc_pop_totals <- function(df_list,
                          country = "National",
                          meta_analysis = FALSE){

  pop_list <- lapply(df_list, function(x) aggregate(pop ~ year, data = x, mean))

  if (meta_analysis == TRUE){

    tot_pop <- do.call(rbind, pop_list)
    tot_pop <- aggregate(pop ~ year, data = tot_pop, sum)

    pop_list[[country]] <- tot_pop}


  return(pop_list)

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
#' @export
hc_create_crossbasis <- function(df_list,
                                 var_fun = "bs", #TODO What if natural spline (degree won't be needed as cubic) - if statement
                                 var_degree = 2,
                                 var_per = c(10,75,90),
                                 lagn = 21,
                                 lagnk = 3,
                                 dfseas = 8) {

  cb_list <- list()

  for(geog in names(df_list)){

    geog_data <- df_list[[geog]]
    argvar <- list(fun = var_fun,
                   knots = quantile(geog_data$temp,
                                    var_per/100,
                                    na.rm = TRUE),
                   degree = var_degree)

    lagn <- as.numeric(lagn)
    lagnk <- as.numeric(lagnk)
    dfseas <- as.numeric(dfseas)
    arglag = list(knots = dlnm::logknots(lagn,
                                         lagnk))
    cb <- dlnm::crossbasis(geog_data$temp,
                           lag = lagn,
                           argvar = argvar,
                           arglag = arglag)
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
#' @export
hc_model_combo_res <- function(df_list,
                               cb_list,
                               independent_cols = NULL){

  # define the base independent cols
  base_independent_cols <- c(
    'cb',
    'dow',
    'splines::ns(date, df = dfseas * length(unique(year)))'
  )

  qaic_results <- list()
  residuals_list <- list()

  if (!is.null(independent_cols)) {

    control_vars <- c(independent_cols)

    transformed_vars <- unlist(lapply(independent_cols, function(v) {
      paste0(v, "_ns")
    }))

  } else(transformed_vars <- NULL)

  all_combos <- unlist(lapply(0:length(transformed_vars), function(i) {
    combn(transformed_vars, i, simplify = FALSE)
  }), recursive = FALSE)

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

    for (vars in all_combos) {

      # Build the full formula string
      base_formula <- paste("dependent ~", paste(base_independent_cols, collapse = " + "))
      formula_str <- paste(base_formula, if (length(vars) > 0) paste("+", paste(vars, collapse = " + ")) else "")

      model <- glm(as.formula(formula_str),
                   geog_data,
                   family = quasipoisson,
                   na.action = "na.exclude")

      disp <- summary(model)$dispersion
      loglik <- sum(dpois(model$y,model$fitted.values,log=TRUE))
      k <- length(coef(model))
      qaic <- -2 * loglik / disp + 2 * k

      qaic_results[[length(qaic_results) + 1]] <- data.frame(geography = geog,
                                                             formula = formula_str,
                                                             disp = disp,
                                                             qaic = qaic)

      residuals_df <- data.frame(geography = geog,
                                 formula = formula_str,
                                 fitted = fitted(model),
                                 residuals = residuals(model, type = "deviance"))

      formula_list[[formula_str]] <- residuals_df

    }

    residuals_list[[geog]] <- formula_list

  }

  # Combine results into a single data frame
  qaic_results <- do.call(rbind, qaic_results)

  # Sort by geography and QAIC
  qaic_results <- qaic_results[order(qaic_results$geography, qaic_results$formula), ]

  return(list(qaic_results, residuals_list))

}


#' Produce variance inflation factor
#'
#' @description Produces variance inflation factor for the independent variables.
#'
#' @param df_list A list of dataframes containing daily timeseries data for a health outcome
#' and climate variables which may be disaggregated by a particular geography.
#' @param independent_cols Additional independent variables to test in model validation
#'
#' @return A list. Variance inflation factors for each independent variables by geography.
#'
#' @export
hc_vif <- function(df_list,
                   independent_cols = NULL){

  vif_list <- list()

  for (geog in names(df_list)){

    geog_data <- df_list[[geog]]

    # test whether temp and optional independent variable are correlated with each other
    formula_str <- paste(paste('dependent ~ temp'), paste("+", paste(independent_cols, collapse = " + ")))

    vif_model <- glm(as.formula(formula_str), data = geog_data, family = quasipoisson())
    vif_values <- car::vif(vif_model)


    vif_df <- data.frame(
      variable = names(vif_values),
      vif = as.numeric(vif_values),
      stringsAsFactors = FALSE
    )

    vif_list[[geog]] <- vif_df

  }

  return(vif_list)

}


#' Model Validation Assessment
#'
#' @description Produces results on QAIC for each model combination, variance inflation
#' factor for each independent variable, and plots for residuals to assess the models
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
#'   \item `vif_results` A dataframe. Variance inflation factors for each independent variables by geography.
#'   \item `vif_summary` A dataframe with the mean variance inflation factors for each independent variable.
#'   }
#'
#' @export
hc_model_validation <- function(df_list = df_list,
                                cb_list = cb_list,
                                independent_cols = NULL,
                                save_fig = FALSE,
                                save_csv = FALSE,
                                output_folder_path = NULL){

  c(qaic_results, residuals_list) %<-% hc_model_combo_res(df_list = df_list,
                                                          cb_list = cb_list,
                                                          independent_cols = independent_cols)

  if (save_csv == TRUE){

    dir.create(file.path(
      path_config$output_folder_path, "model_validation"), recursive = TRUE, showWarnings = FALSE)

    write.csv(qaic_results, file = file.path(
      path_config$output_folder_path, "model_validation", "qaic_results.csv"), row.names = FALSE)

  }

  if (!is.null(independent_cols)) {

    vif_list <- hc_vif(df_list = df_list,
                       independent_cols = independent_cols)

    vif_results <- dplyr::bind_rows(vif_list, .id = "Geography")

    if (save_csv == TRUE) {

      write.csv(vif_results, file = file.path(
        path_config$output_folder_path, "model_validation", "vif_results.csv"), row.names = FALSE)

    }

  } else vif_results <- NULL

  if (length(df_list) > 1){

    qaic_summary <- qaic_results %>%
      group_by(formula) %>%
      summarise(mean_disp = mean(disp),
                mean_qaic = mean(qaic))

    if (save_csv == TRUE){

      write.csv(qaic_summary, file = file.path(
        path_config$output_folder_path, "model_validation", "qaic_summary.csv"), row.names = FALSE)

    }

    if (!is.null(vif_results)){

      vif_summary <- vif_results %>%
        group_by(variable) %>%
        summarise(mean_vif = mean(vif, na.rm = TRUE))

      if (save_csv == TRUE){

        write.csv(vif_summary, file = file.path(
          path_config$output_folder_path, "model_validation", "vif_summary.csv"), row.names = FALSE)

      }

    }

  } else qaic_summary <- vif_summary <- NULL

  if (save_fig == TRUE){

    # Shorten the labels to a fixed length
    short_labels <- sapply(as.character(names(df_list)), function(x) {
      x_clean <- gsub(" ", "", x)  # remove all spaces
      if (nchar(x_clean) > 10) {
        substr(x_clean, 1, 10)     # truncate to first 10 characters
      } else {
        x_clean
      }
    })

    # Assign names to the list
    named_label_list <- as.list(short_labels)
    names(named_label_list) <- names(df_list)

  }

  if (nrow(do.call(rbind, do.call(rbind, residuals_list))) > 100000){
    sample_check <- TRUE
  } else sample_check <- FALSE

  for (geog in names(df_list)){

    geog_data <- df_list[[geog]]
    formula_list <- residuals_list[[geog]]
    named_label <- named_label_list[[geog]]

    if (save_fig == TRUE){

      geog_folder <- gsub(pattern = " ", replacement = "_", x = geog)

      output_folder_main <- file.path(path_config$output_folder_path, "model_validation", geog_folder)
      dir.create(output_folder_main, recursive = TRUE, showWarnings = FALSE)

      grid <- c(min(length(formula_list), 3), ceiling(length(formula_list) / 3))
      output_path <- paste0(output_folder_main, "/", named_label, "_residuals_timeseries.pdf")
      pdf(output_path, width = grid[1]*5.5, height = grid[2]*4.5)

      par(mfrow=c(grid[2], grid[1]), oma = c(0, 0, 4, 0))

    }

    for (i in names(formula_list)){

      plot(x = geog_data$date[geog_data$ind > 0],
           y = formula_list[[i]]$residuals,
           ylim=c(-5,10),
           pch=19,
           cex=0.2,
           col="#0A2E4D",
           main=unique(formula_list[[i]]$formula),
           ylab="Deviance residuals",
           xlab="Date")

      abline(h=0,lty=2,lwd=2)

      if (save_fig == TRUE){

        title <- paste0("Deviance Residuals by Date: ", geog)
        mtext(title, outer = TRUE, cex = 1.5, line = 1, font = 2)

      }

    }

    dev.off()

    if(sample_check == TRUE){

      all_residuals <- do.call(rbind, formula_list)

      set.seed(123)  # for reproducibility
      sampled_residuals <- all_residuals %>%
        group_by(formula) %>%
        sample_frac(0.2) %>%
        ungroup()

      new_res_list <- split(sampled_residuals, sampled_residuals$formula)

      sample_title <- " (20% sample)"

    } else {

      new_res_list <- formula_list
      sample_title <- ""

    }

    if (save_fig == TRUE){

      grid <- c(min(length(formula_list), 3), ceiling(length(new_res_list) / 3))
      output_path <- paste0(output_folder_main, "/", named_label, "_residuals_fitted.pdf")
      pdf(output_path, width = grid[1]*5.5, height = grid[2]*4.5)

      par(mfrow=c(grid[2], grid[1]), oma = c(0, 0, 4, 0))

    }

    for (i in names(new_res_list)){

      plot(x = jitter(new_res_list[[i]]$fitted, amount = 0.5),
           y = jitter(new_res_list[[i]]$residuals, amount = 0.5),
           pch=19,
           cex=0.2,
           col="#0A2E4D",
           main=unique(new_res_list[[i]]$formula),
           ylab="Deviance residuals",
           xlab="Fitted values")

      abline(h=0,lty=2,lwd=2)

      if (save_fig == TRUE){

        title <- paste0("Deviance Residuals by Fitted Values: ", geog, sample_title)
        mtext(title, outer = TRUE, cex = 1.5, line = 1, font = 2)

      }

    }

    dev.off()

    if (save_fig == TRUE){

      grid <- c(min(length(formula_list), 3), ceiling(length(new_res_list) / 3))
      output_path <- paste0(output_folder_main, "/", named_label, "_qq_plot.pdf")
      pdf(output_path, width = grid[1]*5.5, height = grid[2]*4.5)

      par(mfrow=c(grid[2], grid[1]), oma = c(0, 0, 4, 0))

    }

    for (i in names(new_res_list)){

      qqnorm(new_res_list[[i]]$residuals,
             pch = 19,
             cex = 0.2,
             col = "#0A2E4D",
             main = unique(new_res_list[[i]]$formula))

      qqline(new_res_list[[i]]$residuals, lwd = 2)

      if (save_fig == TRUE){

        title <- paste0("Normal Q-Q Plot of Residuals: ", geog, sample_title)
        mtext(title, outer = TRUE, cex = 1.5, line = 1, font = 2)

      }

    }

    dev.off()

  }

  return(list(qaic_results, qaic_summary, vif_results, vif_summary))

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
#' @export
hc_quasipoisson_dlnm <- function(df_list,
                                 control_cols = NULL,
                                 cb_list) {

  model_list <- list()

  # build the formula with base formula and control variables
  if (!is.null(control_cols)) {

    # normalize type
    if (is.character(control_cols)) {
      control_cols <- c(control_cols)
    }

    # type check column names
    for (col in control_cols){
      if (!is.character(col)){
        stop(
          paste0(
            "'control_cols' expected a vector of strings or a string.",
            typeof(col)
          )
        )
      }
    }
  } else {
    control_cols = c()
  }

  # define the base independent cols
  base_independent_cols <- c(
    'cb', 'dow',
    'splines::ns(date, df = dfseas * length(unique(year)))'
  )

  # model formula
  base_formula <- paste("dependent ~",
                        paste(base_independent_cols,
                              collapse = " + "))

  formula <- as.formula(paste(base_formula,
                              paste("+", paste(control_cols,
                                               collapse = " + "))))

  # Run model
  for(geog in names(df_list)){

    geog_data <- df_list[[geog]]
    cb <- cb_list[[geog]]

    model <- glm(formula,
                 geog_data,
                 family = quasipoisson,
                 na.action = "na.exclude")

    model_list[[geog]] <- model

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
#' @return
#'  \itemize{
#'   \item `coef_` A matrix of coefficients for the reduced model.
#'   \item `vcov_` A list. Covariance matrices for each region for the reduced model.
#'   }
#'
#' @export
hc_reduce_cumulative <- function(df_list,
                                 var_per = c(10,75,90),
                                 var_degree = 8,
                                 cb_list,
                                 model_list) {

  # Coefficients and vcov for overall cumulative summary
  coef_ <- matrix(data = NA,
                  nrow = length(names(df_list)),
                  ncol = length(var_per) + var_degree,
                  dimnames = list(names(df_list)))

  vcov_ <- vector("list", length(names(df_list)))
  names(vcov_) <- names(df_list)


  for(geog in names(df_list)){

    geog_data <- df_list[[geog]]
    cb <- cb_list[[geog]]

    cen_ <- mean(geog_data$temp, na.rm = TRUE)


    # Reduction to overall cumulative lag effect
    red <- dlnm::crossreduce(cb, model_list[[geog]], cen = cen_)

    coef_[geog,] <- coef(red)
    vcov_[[geog]] <- vcov(red)

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
#' @return
#' \itemize{
#'   \item `mm` A model object. A multivariate meta-analysis model.
#'   \item `blup` A list. BLUP (best linear unbiased predictions) from the
#'   meta-analysis model for each region.
#'   \item `meta_test_res` A dataframe of results from statistical tests on the meta model.
#'   }
#'
#' @export
hc_meta_analysis <- function(df_list,
                             coef_,
                             vcov_,
                             save_csv = FALSE,
                             output_folder_path = NULL){

  # Assert that df_list is a list of dataframes
  is_list_of_dfs(list_ = df_list)

  # Assert that coef is a numeric matrix
  if (!is.matrix(coef_) || !is.numeric(coef_)) {
    stop("Argument 'coef_' must be a numeric matrix")
  }


  # Assert that vcov is a list of matrices.
  # TODO: Functionalise this functionality into a defenses module
  if (is.list(vcov_)) {
    for (matr in vcov_){
      if (!is.matrix(matr)) {
        stop(paste(
          "'vcov_' expected a list of matrices. List contains item of",
          "type", toString(typeof(matr))
        )
        )
      }
    }
  } else {
    stop(paste("'vcov_' expected a list.", toString(typeof(vcov_))))
  }
  # Create average temperature and range as meta-predictors
  temp_avg <- sapply(df_list,
                     function(x)
                       mean(x$temp, na.rm = TRUE))

  temp_range <- sapply(df_list,
                       function(x)
                         diff(range(x$temp, na.rm = TRUE)))

  # Meta-analysis
  mm <- mixmeta::mixmeta(formula = coef_ ~ temp_avg + temp_range,
                         S = vcov_,
                         data = as.data.frame(names(df_list)),
                         method = "reml"
                         #, control = list(showiter = FALSE) - EW: left from original heat and cold code, check what it does
  )

  # Obtain BLUPs
  blup <- mixmeta::blup(mm, vcov = TRUE)

  names(blup) <- names(df_list)

  # Wald test
  # EW: ask Charlie

  temp_avg_wald <- climatehealth::fwald(mm, "temp_avg")
  temp_range_wald <- climatehealth::fwald(mm, "temp_range")

  # Cochran's Q-test

  qstat <- mixmeta::qtest(mm)

  # I^2 statistic

  i2stat <- ((qstat$Q - qstat$df) / qstat$Q)[1] * 100

  meta_test_res <- data.frame(test = c("temp_avg Wald p-value",
                                       "temp_range Wald p-value",
                                       "Cochran's Q test p-value",
                                       "I2 (%)",
                                       "AIC"),
                              result = round(c(temp_avg_wald,
                                               temp_range_wald,
                                               qstat[["pvalue"]][1],
                                               i2stat,
                                               summary(mm)$AIC),3))

  if (save_csv == TRUE){

    if (!is.null(output_folder_path)) {

      check_file_exists(file.path(output_folder_path))

      write.csv(meta_test_res, file = file.path(
        output_folder_path, "meta_model_stat_test_results.csv"), row.names = FALSE)

    } else {

      stop("Output path not specified")

    }

  }

  return(list(mm, blup, meta_test_res))

}


#' Define minimum mortality percentiles and temperatures
#'
#' @description Calculate the temperature at which there is minimum mortality risk
#' using the product of the basis matrix and BLUPs.
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
#' @returns Percentiles and corresponding temperatures for each geography.
#'
#' @export
hc_min_mortality_temp <- function(df_list,
                                  var_fun = "bs",
                                  var_per = c(10,75,90),
                                  var_degree = 8,
                                  blup = blup,
                                  coef_,
                                  meta_analysis = FALSE) {

  # Assert that df_list is a list of dataframes
  is_list_of_dfs(list_ = df_list)

  if (!is.null(blup) && !is.list(blup)) {
    stop("Argument 'blup' must be a list")
  }

  # Assert that coef is a numeric matrix
  if (!is.matrix(coef_) || !is.numeric(coef_)) {
    stop("Argument 'coef_' must be a numeric matrix")
  }


  # Assert that vcov is a list of matrices.
  # TODO: Functionalise this functionality into a defenses module
  if (is.list(vcov_)) {
    for (matr in vcov_){
      if (!is.matrix(matr)) {
        stop(paste(
          "'vcov_' expected a list of matrices. List contains item of",
          "type", toString(typeof(matr))
        )
        )
      }
    }
  } else {
    stop(paste("'vcov_' expected a list.", toString(typeof(vcov_))))
  }


  # if running a meta-analysis, then MMT is determined by BLUPs
  # else, MMT is determined by coefficients matrix
  if (meta_analysis == TRUE){

    coef_list <- lapply(blup, function(x) x$blup)

  } else coef_list <- split(coef_, rownames(coef_))


  # Generate matrix for storing results
  minpercgeog_ <- mintempgeog_ <- rep(NA,length(df_list))
  names(mintempgeog_) <- names(minpercgeog_) <- names(df_list)


  # Define minimum mortality percentile and corresponding temperature per geography: exclude low and very hot temperature
  for(geog in names(df_list)){

    geog_data <- df_list[[geog]]

    predvar <- quantile(geog_data$temp, 1:99/100, na.rm = TRUE)

    # Redefine the function using all arguments (boundary knots included)
    argvar <- list(x = predvar,
                   fun = var_fun,
                   knots = quantile(geog_data$temp,
                                    var_per / 100,
                                    na.rm = TRUE),
                   degree = var_degree,
                   Bound = range(geog_data$temp, na.rm = TRUE))

    bvar <- do.call(dlnm::onebasis, argvar)

    minpercgeog_[geog] <- (1:99)[which.min(bvar %*%
                                             coef_list[[geog]])]
    mintempgeog_[geog] <- quantile(geog_data$temp,
                                   minpercgeog_[geog]/100,
                                   na.rm = TRUE)
  }

  return(list(minpercgeog_, mintempgeog_))

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
#' @param minpercreg Vector. Percentile of maximum suicide temperature for each region.
#' @param blup A list. BLUP (best linear unbiased predictions) from the
#' meta-analysis model for each region.
#' @param coef_ A matrix of coefficients for the reduced model.
#' @param vcov_ A list. Covariance matrices for each region for the reduced model.
#' @param meta_analysis Boolean. Whether to perform a meta-analysis.
#'
#' @return A list containing predictions by region
#'
#' @export
hc_predict_reg <- function(df_list,
                           var_fun = "bs",
                           var_per = c(10,75,90),
                           var_degree = 8,
                           minpercgeog_,
                           blup,
                           coef_,
                           vcov_,
                           meta_analysis = FALSE){

  if (meta_analysis == TRUE){

    coef_list <- lapply(blup, function(x) x$blup)
    vcov_list <- lapply(blup, function(x) x$vcov)

  } else {

    coef_list <- split(coef_, rownames(coef_))
    vcov_list <- vcov_

  }

  pred_list <- list()

  for(geog in names(df_list)){

    geog_data <- df_list[[geog]]

    argvar <- list(x = geog_data$temp, # determines x axis for predictions
                   fun = var_fun,
                   knots = quantile(geog_data$temp,
                                    var_per/100,
                                    na.rm = TRUE),
                   degree = var_degree)

    bvar <- do.call(dlnm::onebasis, argvar)

    pred <- dlnm::crosspred(bvar,
                            coef = coef_list[[geog]],
                            vcov = vcov_list[[geog]],
                            model.link = "log",
                            by = 0.1,
                            cen = mintempgeog_[geog],
                            from = min(geog_data$temp, na.rm = TRUE),
                            to = max(geog_data$temp, na.rm = TRUE))

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
#' @param lag_fun Character. Exposure function for arglag
#' (see dlnm::crossbasis). Defaults to 'strata'.
#' @param lag_breaks Integer. Internal cut-off point defining the strata for arglag
#' (see dlnm::crossbasis). Defaults to 1.
#' @param lag_days Integer. Maximum lag. Defaults to 2.
#' (see dlnm::crossbasis).
#' @param country Character. Name of country for national level estimates.
#' @param cb_list A list of cross-basis matrices by region.
#' @param mm A model object. A multivariate meta-analysis model.
#' @param minpercgeog Vector. Percentile of minumum mortality temperature for each region.
#'
#' @return
#' \itemize{
#'   \item `df_list` List. A list of data frames for each region and nation.
#'   \item `cb_list` List. A list of cross-basis matrices by region and nation.
#'   \item `minpercreg` Vector. Percentile of minimum suicide temperature for each region and nation.
#'   \item `mmpredall` List. A list of national coefficients and covariance matrices.
#'   }
#'
#' @export
hc_add_national_data <- function(df_list,
                                 pop_list,
                                 var_fun = "bs",
                                 var_per = c(10, 75, 90),
                                 var_degree = 8,
                                 lagn = 21,
                                 country = "National",
                                 cb_list,
                                 mm,
                                 minpercgeog_){

  # Aggregate national level data
  national_data <- as.data.frame(do.call(rbind, df_list))

  nat_pop <- pop_list[[country]] %>%
    rename(nat_pop = pop)

  national_data <- national_data %>%
    left_join(nat_pop, by = "year") %>%
    mutate(weight = pop/nat_pop,
           weighted_temp = temp * weight) %>%
    group_by(date) %>%
    summarise(temp = round(sum(weighted_temp, na.rm = TRUE), 2),
              dependent = sum(dependent, na.rm = TRUE),
              pop = unique(nat_pop)) %>%
    mutate(year = as.factor(lubridate::year(date)),
           month = as.factor(lubridate::month(date)),
           geog_col = country)

  df_list[[country]] <- as.data.frame(national_data)


  # Create cross basis for national data

  argvar <- list(fun = var_fun,
                 knots = quantile(national_data$temp,
                                  var_per/100,
                                  na.rm = TRUE),
                 degree = var_degree)

  arglag <- list(knots = dlnm::logknots(lagn,
                                        lagnk))

  cb_list[[country]] <- dlnm::crossbasis(national_data$temp,
                                         lag = lagn,
                                         argvar = argvar,
                                         arglag = arglag)

  # Add national min temperatures

  predvar <- quantile(national_data$temp, 1:99/100, na.rm = TRUE)

  argvar <- list(x = predvar,
                 fun = var_fun,
                 knots = quantile(national_data$temp,
                                  var_per/100,
                                  na.rm = TRUE),
                 degree = var_degree,
                 Boundary.knots = range(national_data$temp, na.rm = TRUE))

  bvar <- do.call(dlnm::onebasis, argvar)

  datanew <- data.frame(
    temp_avg = mean(national_data$temp),
    temp_range = diff(range(national_data$temp, na.rm = TRUE)))

  mmpredall <- predict(mm, datanew, vcov=TRUE, format="list")

  minpercnat <- (1:99)[which.min((bvar%*%mmpredall$fit)[1:99,])]

  minpercgeog_[country] <- minpercnat

  return(list(df_list, cb_list, minpercgeog_, mintempgeog_, mmpredall))

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
#' (see dlnm::crossbasis). Defaults to c(10,75,90).
#' @param var_degree Integer. Degree of the piecewise polynomial for argvar
#' (see dlnm::crossbasis). Defaults to 2 (quadratic).
#' @param minpercgeog Vector. Percentile of maximum suicide temperature for each region.
#' @param mmpredall List of national coefficients and covariance matrices for the crosspred.
#' @param pred_list A list containing predictions from the model by region.
#' @param country Character. Name of country for national level estimates.
#'
#' @return A list containing predictions by region.
#'
#' @export
hc_predict_nat <- function(df_list,
                           var_fun = "bs",
                           var_per = c(10,75,90),
                           var_degree = 2,
                           minpercgeog,
                           mmpredall,
                           pred_list,
                           country = "National"){

  national_data <- df_list[[country]]

  argvar <- list(x = national_data$temp,
                 fun = var_fun,
                 knots = quantile(national_data$temp,
                                  var_per/100,
                                  na.rm = TRUE),
                 degree = var_degree)

  bvar <- do.call(dlnm::onebasis, argvar)

  cen <- quantile(national_data$temp,
                  minpercgeog_[country]/100,
                  na.rm=TRUE)

  pred_nat <- dlnm::crosspred(bvar,
                              coef=mixmpredall$fit,
                              vcov=mixmpredall$vcov,
                              cen=cen,
                              model.link="log",
                              by=0.1,
                              from = min(national_data$temp, na.rm = TRUE),
                              to = max(national_data$temp, na.rm = TRUE))

  pred_list[[country]] <- pred_nat


  return(pred_list)

}


#' Produce cumulative relative risk results of analysis
#'
#' @description Produces cumulative relative risk and confidence intervals
#' from analysis.
#'
#' @param pred_list A list containing predictions from the model by region.
#'
#' @returns Dataframe containing cumulative relative risk and confidence
#' intervals from analysis.
#'
#' @export
hc_rr_results <- function(pred_list) {

  rr_results <- bind_rows(lapply(names(pred_list), function(geog_name) {

    geog_pred <- pred_list[[geog_name]]

    df <- data.frame(
      Area = region_name,
      Temperature = reg_pred$predvar,
      RR = reg_pred$allRRfit,
      RR_lower_CI = reg_pred$allRRlow,
      RR_upper_CI = reg_pred$allRRhigh
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
#' @param attr_thr Integer. Percentile at which to define the temperature threshold for
#' calculating attributable risk.
#' @param minpercreg Vector. Percentile of minimum suicide temperature for each area.
#' @param country Character. Name of country for national level estimates.
#' @param save_fig Boolean. Whether to save the plot as an output. Defaults to
#' FALSE.
#' @param output_folder_path Path to folder where plots should be saved.
#' Defaults to NULL.
#'
#' @returns Plots of cumulative lag exposure-response function with histogram of
#' temperature distribution for each region
#'
#' @export
mh_plot_rr <- function(df_list,
                       pred_list,
                       attr_thr = 97.5,
                       minpercreg,
                       country = "National",
                       save_fig = FALSE,
                       output_folder_path = NULL) {

  xlim <- c(min(sapply(pred_list, function(x) min(x$predvar, na.rm = TRUE))),
            max(sapply(pred_list, function(x) max(x$predvar, na.rm = TRUE))))

  ylim <- c(min(c(min(sapply(pred_list, function(x) min(x$allRRfit, na.rm = TRUE))) - 0.5, 0.4)),
            max(c(max(sapply(pred_list, function(x) max(x$allRRfit, na.rm = TRUE))) + 0.5, 2.1)))

  hist_max <- max(unlist(lapply(df_list, function(x) {

    temp_range <- range(x$temp, na.rm = TRUE)
    breaks <- seq(floor(temp_range[1]), ceiling(temp_range[2]), by = 1)
    hist(x$temp, breaks = breaks, plot = FALSE)$counts

  })), na.rm = TRUE)


  if (save_fig==T) {

    grid <- c(min(length(pred_list), 3), ceiling(length(pred_list) / 3))

    output_path <- file.path(path_config$output_folder_path, "suicides_rr_plot.pdf")
    pdf(output_path, width=max(10,grid[1]*5.5), height=max(7, grid[2]*4))

    layout_ids <- seq_len(grid[1] * grid[2])
    layout_matrix <- matrix(layout_ids, nrow = grid[2], ncol = grid[1], byrow = TRUE)

    layout(layout_matrix, heights = rep(1, grid[2]), widths = rep(1, grid[1]))

    par(oma = c(0,1,4,1))

  }

  for(reg in names(pred_list)){

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
         col = "#296991")

    vline_pos_max_x <- quantile(region_temp, attr_thr/100, na.rm = TRUE)
    vline_pos_max_y <- max(region_pred$allRRfit, na.rm = TRUE) + 0.3
    vline_lab_max <- paste0("Attr. Risk Threshold\n", round(vline_pos_max_x, 2), intToUtf8(176), "C (p", attr_thr, ")")

    abline(v = vline_pos_max_x, col = "black", lty = 2)
    text(x = vline_pos_max_x, y = vline_pos_max_y, labels = vline_lab_max, pos = 2, col = "black", cex = 0.8)

    vline_pos_min_x <- quantile(region_temp, minpercreg[reg]/100, na.rm = TRUE)
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
                      plot = FALSE)

    hist_scale <- (0.3) / hist_max
    scaled_counts <- hist_data$counts * hist_scale

    for (i in seq_along(hist_data$counts)) {
      rect(xleft = hist_data$breaks[i],
           xright = hist_data$breaks[i + 1],
           ybottom = ylim[1],
           ytop = ylim[1] + scaled_counts[i],
           col = "#C75E70",
           border = "white")
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

  if (save_fig==T) {

    year_range <- paste0("(",
                         min(sapply(df_list, function(x) min(lubridate::year(x$date), na.rm = TRUE))),
                         "-",
                         max(sapply(df_list, function(x) max(lubridate::year(x$date), na.rm = TRUE))),
                         ")")

    title <- paste0("Relative Risk of Suicide by Mean Temperature and Area, ", country, " ",  year_range)

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
#' calculating attributable risk.
#'
#' @return A list containing attributable numbers per region
#'
#' @export
mh_attr <- function(df_list,
                    cb_list,
                    pred_list,
                    minpercreg,
                    attr_thr = 97.5) {

  attr_list <- list()

  for (reg in names(df_list)){

    region_data <- df_list[[reg]]
    cb <- cb_list[[reg]]
    pred <- pred_list[[reg]]
    minperc <- minpercreg[reg]

    cen <- quantile(region_data$temp, minperc/100, na.rm = TRUE)
    min_range <- quantile(region_data$temp, attr_thr/100, na.rm = TRUE)
    max_range <- max(region_data$temp, na.rm = TRUE)

    c(af, af_lower_ci, af_upper_ci,
      an, an_lower_ci, an_upper_ci)  %<-% an_attrdl(x = region_data$temp,
                                                    basis = cb,
                                                    cases = region_data$suicides,
                                                    coef = pred$coefficients,
                                                    vcov = pred$vcov,
                                                    dir = "forw",
                                                    cen = cen,
                                                    range = c(min_range, max_range),
                                                    tot = FALSE,
                                                    nsim = 1000)

    results <- region_data %>%
      select(region, date, temp, year, month, suicides, population) %>%
      mutate(threshold_temp = round(min_range, 2),
             af = af,
             af_lower_ci = af_lower_ci,
             af_upper_ci = af_upper_ci,
             an = an,
             an_lower_ci = an_lower_ci,
             an_upper_ci = an_upper_ci,
             ar = (an / population) * 100000,
             ar_lower_ci = (an_lower_ci / population) * 100000,
             ar_upper_ci = (an_upper_ci / population) * 100000)

    attr_list[[reg]] <- results

  }

  return(attr_list)

}






#' Compute attributable deaths
#'
#' @description Compute the attributable deaths for each regions,
#' with empirical CI estimated using the re-centered bases.
#'
#' @param df_list An alphabetically-ordered list
#' of dataframes for each region.
#' @param output_year The year to calculate attributable deaths for.
#' @param blup A list of BLUPs (best linear unbiased predictions).
#' @param mintempregions A named numeric vector.
#' Minimum (optimum) mortality temperature per region.
#' @param an_thresholds A dataframe with the optimal temperature range and
#' temperature thresholds for calculation of attributable deaths.
#' @param independent_cols column name (or list of names) of extra independent
#' variable to include in regression (excluding temperature). Defaults to NULL.
#' @param var_fun Exposure function
#' (see dlnm::crossbasis)
#' @param var_per Internal knot positions in exposure function
#' (see dlnm::crossbasis)
#' @param vardegree Degree of the piecewise polynomial for argvar
#' (see dlnm:crossbasis)
#' @param lag Lag length in time
#' (see dlnm::logknots)
#' @param lagnk Number of knots in lag function
#' (see dlnm::logknots)
#' @param dfseas Degrees of freedom for seasonality
#' @param nsim_ The number of simulation runs used for computing emprical CI
#'
#' @return A list of variables
#' \itemize{
#'   \item `totdeath` A named vector of integers.
#'   otal observed mortality per region.
#'   \item `arraysim` An array (numeric). Total (glob),
#'    cold and heat-attributable deaths per region for 1000 simulations.
#'   Used to derive confidence intervals.
#'   \item `matsim` A matrix (numeric). Total (glob),
#'   cold and heat-attributable deaths per region from reduced coefficients.
#'    \item `attrdl_yr_all` a dataframe containing attributable deaths by year
#'    for each region.
#' }
#'
#' @export
old_compute_attributable_deaths <- function(df_list,
                                            output_year,
                                            blup = NULL,
                                            mintempregions,
                                            an_thresholds,
                                            independent_cols = NULL,
                                            var_fun,
                                            varper,
                                            vardegree,
                                            lag,
                                            lagnk,
                                            dfseas,
                                            nsim_ = 1000) {

  # Create the vectors to store the total mortality (accounting for missing)
  totdeath <- rep(NA, length(names(df_list)))
  names(totdeath) <- names(df_list)

  # Create the matrix to store the attributable deaths
  matsim <- matrix(NA, length(names(df_list)), 7,
                   dimnames = list(names(df_list),
                                   c("glob_cold", "glob_heat", "moderate_cold",
                                     "moderate_heat", "high_cold", "high_heat",
                                     "heatwave")))

  # Validate the user-passed number of simulations
  if (!is.numeric(nsim_)) {
    stop("The number of simulations (nsim_) must be an integer.")
  }
  nsim_ <- round(nsim_, 0)

  # Create the array to store the CI of attributable deaths
  arraysim <- array(NA, dim = c(length(names(df_list)), 7, nsim_),
                    dimnames = list(names(df_list),
                                    c("glob_cold_ci", "glob_heat_ci",
                                      "moderate_cold_ci", "moderate_heat_ci",
                                      "high_cold_ci", "high_heat_ci", "heatwave_ci")))


  if (output_year == 0) {

    output_year = max(df_list[[1]]$year)
  }
  # Run the loop
  for(i in seq(df_list)){

    # Extract the data
    data <- df_list[[i]]

    # Derive the cross-basis
    if (!is.null(blup)) {

      coefs <- blup[[i]]$blup
      vcovs <- blup[[i]]$vcov

      c(model, cb) %<-% define_model(dataset = data,
                                     independent_cols = independent_cols,
                                     varfun = varfun,
                                     varper = varper,
                                     vardegree = vardegree,
                                     lag = lag,
                                     lagnk = lagnk,
                                     dfseas = dfseas)
      model <- NULL

    } else {

      coefs <- NULL
      vcovs <- NULL

      c(model, cb) %<-% define_model(dataset = data,
                                     independent_cols = independent_cols,
                                     varfun = varfun,
                                     varper = varper,
                                     vardegree = vardegree,
                                     lag = lag,
                                     lagnk = lagnk,
                                     dfseas = dfseas)

    }


    # Return heat attributable deaths for the output year

    data_output_year <- data %>% dplyr::filter(year %in% output_year) %>%
      dplyr::mutate(
        high_heat_flag = ifelse(
          temp > an_thresholds[i,"high_moderate_heat"], 1, 0
        )
      )

    # Prepare temperature column for attribution to heatwaves
    # Force the temperature to be the centering value for non-heatwave days
    data_output_year$heatwave_flag <- NA
    for (j in seq(nrow(data_output_year))){

      if(j == 1){

        data_output_year$heatwave_flag[j] <-
          ifelse(data_output_year$high_heat_flag[j] == 1 &
                   data_output_year$high_heat_flag[j+1] == 1, 1, 0)

      } else if (j == nrow(data_output_year)){

        data_output_year$heatwave_flag[j] <-
          ifelse(data_output_year$high_heat_flag[j] == 1 &
                   data_output_year$high_heat_flag[j-1] == 1, 1, 0)

      } else {

        data_output_year$heatwave_flag[j] <-
          ifelse((data_output_year$high_heat_flag[j] == 1 &
                    data_output_year$high_heat_flag[j-1] == 1) |
                   (data_output_year$high_heat_flag[j] == 1 &
                      data_output_year$high_heat_flag[j+1] == 1), 1, 0)
      }
    }

    data_output_year <- data_output_year %>%
      dplyr::mutate(
        heatwave_temp = ifelse(heatwave_flag == 1, temp, mintempregions[i])
      ) %>%
      dplyr::select(-high_heat_flag, -heatwave_flag)

    matsim[i, "glob_cold"] <- attrdl(x = data_output_year$temp,
                                     basis = cb,
                                     cases = data_output_year$dependent,
                                     coef = coefs,
                                     vcov = vcovs,
                                     type = "an",
                                     dir = "forw",
                                     cen = mintempregions[i],
                                     model = model,
                                     range = c(an_thresholds[i,"min_high_cold"],
                                               an_thresholds[i,"moderate_cold_OTR"]))

    matsim[i, "glob_heat"] <- attrdl(x = data_output_year$temp,
                                     basis = cb,
                                     cases = data_output_year$dependent,
                                     coef = coefs,
                                     vcov = vcovs,
                                     type = "an",
                                     dir = "forw",
                                     cen = mintempregions[i],
                                     model = model,
                                     range = c(an_thresholds[i,"moderate_heat_OTR"],
                                               an_thresholds[i,"max_high_heat"]))

    matsim[i, "moderate_cold"] <- attrdl(x = data_output_year$temp,
                                         basis = cb,
                                         cases = data_output_year$dependent,
                                         coef = coefs,
                                         vcov = vcovs,
                                         type = "an",
                                         dir = "forw",
                                         cen = mintempregions[i],
                                         model = model,
                                         range = c(an_thresholds[i,"high_moderate_cold"],
                                                   an_thresholds[i,"moderate_cold_OTR"]))

    matsim[i, "moderate_heat" ] <- attrdl(x = data_output_year$temp,
                                          basis = cb,
                                          cases = data_output_year$dependent,
                                          coef = coefs,
                                          vcov = vcovs,
                                          type="an",
                                          dir = "forw",
                                          cen = mintempregions[i],
                                          model = model,
                                          range = c(an_thresholds[i,"moderate_heat_OTR"],
                                                    an_thresholds[i,"high_moderate_heat"]))

    # Attributable deaths for extremes:
    matsim[i,"high_cold"] <- attrdl(x = data_output_year$temp,
                                    basis = cb,
                                    cases = data_output_year$dependent,
                                    coef = coefs,
                                    vcov = vcovs,
                                    model = model,
                                    type = "an",
                                    dir = "forw",
                                    cen = mintempregions[i],
                                    range = c(an_thresholds[i,"min_high_cold"],
                                              an_thresholds[i,"high_moderate_cold"]))

    matsim[i,"high_heat"] <- attrdl(x = data_output_year$temp,
                                    basis = cb,
                                    cases = data_output_year$dependent,
                                    coef = coefs,
                                    vcov = vcovs,
                                    model = model,
                                    type = "an",
                                    dir = "forw",
                                    cen = mintempregions[i],
                                    range = c(an_thresholds[i,"high_moderate_heat"],
                                              an_thresholds[i,"max_high_heat"]))


    matsim[i,"heatwave"] <- attrdl(x = data_output_year$heatwave_temp,
                                   basis = cb,
                                   cases = data_output_year$dependent,
                                   coef = coefs,
                                   vcov = vcovs,
                                   model = model,
                                   type = "an",
                                   dir = "forw",
                                   cen = mintempregions[i])

    # Compute empirical occurrences of the attributable deaths
    # Used to derive confidence intervals
    arraysim[i, "glob_cold_ci", ] <- attrdl(x = data_output_year$temp,
                                            basis = cb,
                                            cases = data_output_year$dependent,
                                            coef = coefs,
                                            vcov = vcovs,
                                            type = "an",
                                            dir = "forw",
                                            cen = mintempregions[i],
                                            model = model,
                                            range = c(an_thresholds[i,"min_high_cold"],
                                                      an_thresholds[i,"moderate_cold_OTR"]),
                                            sim = T, nsim = nsim_)

    arraysim[i, "glob_heat_ci", ] <- attrdl(x = data_output_year$temp,
                                            basis = cb,
                                            cases = data_output_year$dependent,
                                            coef = coefs,
                                            vcov = vcovs,
                                            type = "an",
                                            dir = "forw",
                                            cen = mintempregions[i],
                                            model = model,
                                            range = c(an_thresholds[i,"moderate_heat_OTR"],
                                                      an_thresholds[i,"max_high_heat"]),
                                            sim = T, nsim = nsim_)

    arraysim[i, "moderate_cold_ci", ] <- attrdl(x = data_output_year$temp,
                                                basis = cb,
                                                cases = data_output_year$dependent,
                                                coef = coefs,
                                                vcov = vcovs,
                                                type = "an",
                                                dir = "forw",
                                                cen = mintempregions[i],
                                                model = model,
                                                range = c(an_thresholds[i,"high_moderate_cold"],
                                                          an_thresholds[i,"moderate_cold_OTR"]),
                                                sim = T , nsim = nsim_)

    arraysim[i, "moderate_heat_ci", ] <- attrdl(x = data_output_year$temp,
                                                basis = cb,
                                                cases = data_output_year$dependent,
                                                coef = coefs,
                                                vcov = vcovs,
                                                type = "an",
                                                dir = "forw",
                                                cen = mintempregions[i],
                                                model = model,
                                                range = c(an_thresholds[i,"moderate_heat_OTR"],
                                                          an_thresholds[i,"high_moderate_heat"]),
                                                sim = T, nsim = nsim_)

    arraysim[i, "high_cold_ci", ] <- attrdl(x = data_output_year$temp,
                                            basis = cb,
                                            cases = data_output_year$dependent,
                                            coef = coefs,
                                            vcov = vcovs,
                                            type = "an",
                                            dir= "forw",
                                            cen = mintempregions[i],
                                            model = model,
                                            range = c(an_thresholds[i,"min_high_cold"],
                                                      an_thresholds[i,"high_moderate_cold"]),
                                            sim = T, nsim = nsim_)

    arraysim[i, "high_heat_ci", ] <- attrdl(x = data_output_year$temp,
                                            basis = cb,
                                            cases = data_output_year$dependent,
                                            coef = coefs,
                                            vcov = vcovs,
                                            type = "an",
                                            dir= "forw",
                                            cen = mintempregions[i],
                                            model = model,
                                            range = c(an_thresholds[i,"high_moderate_heat"],
                                                      an_thresholds[i,"max_high_heat"]),
                                            sim = T, nsim = nsim_)

    arraysim[i, "heatwave_ci", ] <- attrdl(x = data_output_year$heatwave_temp,
                                           basis = cb,
                                           cases = data_output_year$dependent,
                                           coef = coefs,
                                           vcov = vcovs,
                                           type = "an",
                                           dir= "forw",
                                           cen = mintempregions[i],
                                           model = model,
                                           sim = T, nsim = nsim_)

  }

  return (list(arraysim, matsim))

}



#' Compute attributable rates
#'
#' @param df_list An alphabetically-ordered list of dataframes for each
#' region comprising dates, deaths, and temperatures.
#' @param output_year Year(s) to calculate output for.
#' @param matsim A matrix (numeric). Total (glob),
#' cold and heat-attributable deaths per region from reduced coefficients.
#' @param arraysim An array (numeric). Total (glob),
#' cold and heat-attributable deaths per region for 1000 simulations.
#'  Used to derive confidence intervals.

#' @return
#' \itemize{
#'   \item `anregions_bind`
#'   \item `antot_bind`
#'   \item `arregions_bind`
#'   \item `artot_bind`
#' }
#'
#' @export
old_compute_attributable_rates <- function(df_list, output_year, matsim, arraysim){

  # Attributable numbers: estimates as well as the upper and lower ends of the
  # 95% confidence interval, derived from the simulated arraysim

  if (output_year == 0) {

    output_year = max(df_list[[1]]$year)

  }
  # Regions-specific
  anregions <- matsim
  anregionslow <- apply(arraysim, c(1,2), quantile, 0.025)
  anregionshigh <- apply(arraysim, c(1,2), quantile, 0.975)

  rownames(anregions) <-
    rownames(anregionslow) <-
    rownames(anregionshigh) <-
    names(df_list)

  # Whole country
  antot <- colSums(matsim)
  antotlow <- apply(apply(arraysim, c(2,3), sum), 1, quantile, 0.025)
  antothigh <- apply(apply(arraysim, c(2,3), sum), 1, quantile, 0.975)


  # Attributable rates

  # Populations to compute attributable rates with
  regions_pop <- rep(NA, length(df_list))
  names(regions_pop) <- names(df_list)
  years_pop <- rep(NA, length(output_year))

  for (i in seq(df_list)){
    for (j in seq(length(output_year))){
      data_output_year <- df_list[[i]] %>% dplyr::filter(year == output_year[j])
      years_pop[j] <- as.numeric(unique(data_output_year["pop_col"]))
    }
    regions_pop[i] <- mean(years_pop)
  }

  totpopulation <- sum(regions_pop)

  # regions-specific AR
  arregions <- anregions / as.numeric(regions_pop) * 100000
  arregionslow <- anregionslow / as.numeric(regions_pop) * 100000
  arregionshigh <- anregionshigh / as.numeric(regions_pop) * 100000

  # Total AR
  artot <- antot / totpopulation * 100000
  artotlow <- antotlow / totpopulation * 100000
  artothigh <- antothigh / totpopulation * 100000


  # Bind datasets

  colnames(anregionslow) <- paste(colnames(anregionslow), '2.5', sep = '_')
  colnames(anregionshigh) <- paste(colnames(anregionshigh), '97.5', sep = '_')
  colnames(arregionslow) <- paste(colnames(arregionslow), '2.5', sep = '_')
  colnames(arregionshigh) <- paste(colnames(arregionshigh), '97.5', sep = '_')

  anregions_bind <- t(cbind(anregions, anregionslow, anregionshigh))
  antot_bind <- t(cbind(antot, antotlow, antothigh))
  arregions_bind <- t(cbind(arregions, arregionslow, arregionshigh))
  artot_bind <- t(cbind(artot, artotlow, artothigh))

  return(list(anregions_bind,antot_bind,arregions_bind,artot_bind))
}


#' Write outputs to csv
#'
#' @description Write the attributable deaths and temperature for each regions,
#' with empirical CI estimated using the re-centered bases.
#' @param avgtmean_wald The Wald statistic P-value for the average temperature.
#' @param rangetmean_wald The wald statistic P-value for the temperature range.
#' @param anregions_bind A dataframe with attributable deaths for each region.
#' @param antot_bind A matrix of numbers of numbers of deaths attributable to
#'  temperature, heat, cold, extreme heat and extreme cold (with confidence
#'  intervals).
#' @param arregions_bind A dataframe with attributable rates by region.
#' @param artot_bind A matrix of fractions of all-cause mortality
#'  attributable to temperature, heat, cold, extreme heat and extreme cold
#'  (with confidence intervals).
#' @param save_csv Bool. Whether or not to save CSV files to output path.
#' Defaults to FALSE.
#' @param output_folder_path Path to folder for storing outputs.
#'
#' @return
#' \itemize{
#'   \item `wald_publication` A dataframe containing the Wald statistic P-values
#'   for average temperature and the temperature range.
#'   \item `anregions_publication` A matrix of numbers of deaths attributable to
#'   temperature, heat, cold, extreme heat and extreme cold (with confidence
#'   intervals), disaggregated by region.
#'   \item `antot_bind` A matrix of numbers of numbers of deaths attributable to
#'   temperature, heat, cold, extreme heat and extreme cold (with confidence
#'   intervals).
#'   \item `arregions_publication`
#'   \item `artot_bind` A matrix of fractions of all-cause mortality
#'   attributable to temperature, heat, cold, extreme heat and extreme cold
#'   (with confidence intervals).
#' }
#'
#' @examples output_folder_path = 'myfolder/output/'
#'
#' @export
write_attributable_deaths <- function(avgtmean_wald,
                                      rangetmean_wald,
                                      anregions_bind,
                                      antot_bind,
                                      arregions_bind,
                                      artot_bind,
                                      save_csv = FALSE,
                                      output_folder_path = NULL) {
  # convert data to publication format
  # wald test results
  if (!is.null(avgtmean_wald) & !is.null(rangetmean_wald)){
    wald_publication <- data.frame(cbind(avgtmean_wald,rangetmean_wald))
    colnames(wald_publication) <- c("region_mean_temp","region_temp_range")
    rownames(wald_publication) <- "Wald statistic p-value"
  } else {
    wald_publication <- NULL
  }

  # AN_regions (attributable deaths by region)
  anregions_publication <- anregions_bind %>%
    t() %>%
    as.data.frame() %>%
    dplyr::select(glob_cold, glob_cold_ci_2.5, glob_cold_ci_97.5,
                  glob_heat, glob_heat_ci_2.5, glob_heat_ci_97.5,
                  moderate_cold, moderate_cold_ci_2.5, moderate_cold_ci_97.5,
                  moderate_heat, moderate_heat_ci_2.5, moderate_heat_ci_97.5,
                  high_cold, high_cold_ci_2.5, high_cold_ci_97.5,
                  high_heat, high_heat_ci_2.5, high_heat_ci_97.5,
                  heatwave, heatwave_ci_2.5, heatwave_ci_97.5)


  # AR_regions (attributable rates by region)
  arregions_publication <- arregions_bind %>%
    t() %>%
    as.data.frame() %>%
    dplyr::select(glob_cold, glob_cold_ci_2.5, glob_cold_ci_97.5,
                  glob_heat, glob_heat_ci_2.5, glob_heat_ci_97.5,
                  moderate_cold, moderate_cold_ci_2.5, moderate_cold_ci_97.5,
                  moderate_heat, moderate_heat_ci_2.5, moderate_heat_ci_97.5,
                  high_cold, high_cold_ci_2.5, high_cold_ci_97.5,
                  high_heat, high_heat_ci_2.5, high_heat_ci_97.5,
                  heatwave, heatwave_ci_2.5, heatwave_ci_97.5)

  if (save_csv==TRUE) {
    # define output_folder_path as CWD if it is null
    if (is.null(output_folder_path)) {
      output_folder_path <- "/"
    }
    # normalise outputs paths
    else if (!endsWith(output_folder_path, "/")) {
      output_folder_path <- paste(output_folder_path, "/", sep="")
    }

    write.csv(wald_publication,
              file = paste(output_folder_path,
                           'heat_and_cold_wald_test_results.csv',
                           sep = ""))

    write.csv(anregions_publication,
              file = paste(output_folder_path,
                           'heat_and_cold_attributable_deaths_regions.csv',
                           sep = ""))
    write.csv(antot_bind,
              file = paste(output_folder_path,
                           'heat_and_cold_attributable_deaths_total.csv',
                           sep = ""))
    write.csv(arregions_publication,
              file = paste(output_folder_path,
                           'heat_and_cold_attributable_rates_regions.csv',
                           sep = ""))
    write.csv(artot_bind,
              file = paste(output_folder_path,
                           'heat_and_cold_attributable_rates_total.csv',
                           sep=""))
  }
  return(list(wald_publication, anregions_publication, antot_bind,
              arregions_publication, artot_bind))

}


#' Plot and write results of analysis
#'
#' @param df_list An alphabetically-ordered
#' list of dataframes for each region.
#' @param output_name The name of the output file. (.csv and .pdf added
#' accordingly).
#' @param aggregate_outputs Whether or not to output all geographical regions.
#' @param output_folder_path The directory to output the resultant data/plots to.
#' @param save_fig Whether to save output figure (Bool)
#' @param save_csv Whether to save output CSVs (Bool)
#' @param cb The generated crossbasis.
#' @param model The generated model.
#' @param blup A list of BLUPs (best linear unbiased predictions).
#' @param mintempregions A named numeric vector.
#'   Minimum (optimum) mortality temperature per region.
#' @param an_thresholds A dataframe with the optimal temperature range and
#' temperature thresholds for calculation of attributable deaths.
#' @param independent_cols column name (or list of names) of extra independent
#' variable to include in regression (excluding temperature). Defaults to NULL.
#' @param varfun Exposure function
#' (see dlnm::crossbasis)
#' @param varper Internal knot positions in exposure function
#' (see dlnm::crossbasis)
#' @param vardegree Degree of the piecewise polynomial for argvar
#' (see dlnm:crossbasis)
#' @param lag Lag length in time
#' (see dlnm::logknots)
#' @param lagnk Number of knots in lag function
#' (see dlnm::logknots)
#' @param dfseas Degrees of freedom for seasonality
#' @param dependent_col the column name of the
#' dependent variable of interest e.g. deaths
#'
#' @return
#' \itemize{
#'
#'   \item A PDF containing a line plot of temperature versus relative risk per
#'   region, and histogram of temperatures per region.
#'   \item A CSV of relative risk per temperature per region.
#'   \item `output_df` A dataframe with relative risk estimates and confidence
#'   intervals across the temperature range for each region.
#'   \item `temp_df` A dataframe with daily mean exposure values for each
#'   region.
#' }
#'
#' @export
plot_and_write <- function(
    df_list,
    output_name,
    aggregate_outputs = FALSE,
    output_folder_path = "",
    save_fig = TRUE,
    save_csv = TRUE,
    cb = NULL,
    model = NULL,
    blup = NULL,
    mintempregions,
    an_thresholds,
    independent_cols = NULL,
    varfun,
    varper,
    vardegree,
    lag = NULL,
    lagnk = NULL,
    dfseas = NULL,
    dependent_col = NULL) {
  # normalize output folder path
  if (is.null(output_folder_path)) {
    output_folder_path <- ""
  }
  if (!endsWith(output_folder_path, "/")) {
    output_folder_path <- paste(output_folder_path, "/", sep="")
  }
  # define output file paths
  pdf_output_path = paste(
    output_folder_path, paste(output_name, "plot.pdf", sep = "_"), sep = ""
  )
  data_output_path = paste(
    output_folder_path, paste(output_name, "data.csv", sep = "_"), sep = ""
  )
  # create pdf object
  if (save_fig == TRUE) {
    grid <- create_grid(length(df_list))
    if (aggregate_outputs) {
      grid <- c(1, 1)
    }
    pdf(paste(pdf_output_path, sep = ''),
        width=grid[1]*4, height=grid[2]*4)

    par(mfrow=c(grid[1],  grid[2]))
  }
  # structure the layout of the pdf to output
  if (aggregate_outputs) {
    return(plot_and_write_relative_risk_all(df_list = df_list,
                                            mintempregions = mintempregions,
                                            save_fig = save_fig,
                                            save_csv = save_csv,
                                            csv_output_path = data_output_path,
                                            cb = cb,
                                            model = model,
                                            dependent_col = dependent_col,
                                            varfun = varfun,
                                            varper = varper,
                                            vardegree = vardegree
    ))

  } else {
    return(plot_and_write_relative_risk(df_list = df_list,
                                        blup = blup,
                                        mintempregions = mintempregions,
                                        an_thresholds = an_thresholds,
                                        save_fig = save_fig,
                                        save_csv = save_csv,
                                        csv_output_path = data_output_path,
                                        independent_cols = independent_cols,
                                        varfun = varfun,
                                        varper = varper,
                                        vardegree = vardegree,
                                        lag = lag,
                                        lagnk = lagnk,
                                        dfseas = dfseas))
  }



}


#' Plot and write results of analysis
#'
#' @param df_list An alphabetically-ordered
#' list of dataframes for each region.
#' @param blup A list of BLUPs (best linear unbiased predictions).
#' @param mintempregions A named numeric vector.
#'   Minimum (optimum) mortality temperature per region.
#' @param an_thresholds A dataframe with the optimal temperature range and
#' temperature thresholds for calculation of attributable deaths.
#' @param save_fig Whether to save output figure (Bool)
#' @param save_csv Whether to save output CSVs (Bool)
#' @param csv_output_path The path to save the csv to, including file name and
#' file extensions
#' @param independent_cols column name (or list of names) of extra independent
#' variable to include in regression (excluding temperature). Defaults to NULL.
#' @param varfun Exposure function
#' (see dlnm::crossbasis)
#' @param varper Internal knot positions in exposure function
#' (see dlnm::crossbasis)
#' @param vardegree Degree of the piecewise polynomial for argvar
#' (see dlnm:crossbasis)
#' @param lag Lag length in time
#' (see dlnm::logknots)
#' @param lagnk Number of knots in lag function
#' (see dlnm::logknots)
#' @param dfseas Degrees of freedom for seasonality
#'
#' @return
#' \itemize{
#'
#'   \item A PDF containing a line plot of temperature versus relative risk per
#'   region, and histogram of temperatures per region.
#'   \item A CSV of relative risk per temperature per region.
#'   \item `output_df` A dataframe with relative risk estimates and confidence
#'   intervals across the temperature range for each region.
#'   \item `temp_df` A dataframe with daily mean exposure values for each
#'   region.
#' }
#'
#' @examples csv_output_path = "directory/sub_directory/file_name.csv"
#'
#' @export
plot_and_write_relative_risk <- function(df_list,
                                         blup = NULL,
                                         mintempregions,
                                         an_thresholds,
                                         save_fig = TRUE,
                                         save_csv = TRUE,
                                         csv_output_path = NULL,
                                         independent_cols,
                                         varfun,
                                         varper,
                                         vardegree,
                                         lag,
                                         lagnk,
                                         dfseas) {

  # create vectors for output data
  relative_risk_vector <- c()
  upper_vector <- c()
  lower_vector <- c()
  region_vector <- c()
  temp_vector <- c()
  cen_vector <- c()
  temperature_vector <- c()
  temperature_region_vector <- c()

  xlab <- expression(paste("Temperature (",degree,"C)"))
  no_of_regions <- seq(length(df_list))

  for(i in no_of_regions) {

    data <- df_list[[i]]

    # NB: Centering point different than original choice of 75th
    argvar <- list(x = data$temp,
                   fun = varfun,
                   degree = vardegree,
                   knots = quantile(data$temp,
                                    varper / 100, na.rm = TRUE))

    if (!is.null(blup)) {

      bvar <- do.call(dlnm::onebasis, argvar)

      coefs <- blup[[i]]$blup
      vcovs <- blup[[i]]$vcov
      model <- NULL
      cen <- mintempregions[i]

      pred <- dlnm::crosspred(bvar,
                              coef = blup[[i]]$blup,
                              vcov = blup[[i]]$vcov,
                              model.link = "log",
                              by = 0.1,
                              cen = cen)

    } else {

      # Run the model and obtain predictions
      c(model, cb) %<-% define_model(dataset = data,
                                     independent_cols = independent_cols,
                                     varfun = varfun,
                                     varper = varper,
                                     vardegree = vardegree,
                                     lag = lag,
                                     lagnk = lagnk,
                                     dfseas = dfseas)

      cen <- mean(data$temp, na.rm = TRUE)
      pred <- dlnm::crossreduce(cb, model, cen = cen)

      mintempregions[i] <- as.numeric(names(which.min(pred$RRfit)))
      cen <- mintempregions[i]
      pred <- dlnm::crossreduce(cb, model, cen = cen)

    }
    if (save_fig==TRUE) {
      plot(pred, type = "n",
           ylim = c(0, 3),
           yaxt = "n",
           lab = c(6, 5, 7),
           xlab = xlab,
           ylab = "RR",
           main = names(df_list)[i])
    }

    ind_a <- pred$predvar <= c(an_thresholds[i,c("high_moderate_cold")])
    ind_b <- pred$predvar >= c(an_thresholds[i,c("high_moderate_cold")]) &
      pred$predvar <= c(an_thresholds[i,c("moderate_cold_OTR")])
    ind_c <- pred$predvar >= c(an_thresholds[i,c("moderate_cold_OTR")]) &
      pred$predvar <= c(an_thresholds[i,c("moderate_heat_OTR")])
    ind_d <- pred$predvar >= c(an_thresholds[i,c("moderate_heat_OTR")]) &
      pred$predvar <= c(an_thresholds[i,c("high_moderate_heat")])
    ind_e <- pred$predvar >= c(an_thresholds[i,c("high_moderate_heat")])

    if (save_fig==TRUE) {
      if (!is.null(blup)) {

        relative_risk_vals <- pred$allRRfit

      } else {

        relative_risk_vals <- pred$RRfit
      }

      lines(pred$predvar[ind_a],
            relative_risk_vals[ind_a],
            col = c("#000FFF"),
            lwd = 1.5)
      lines(pred$predvar[ind_b],
            relative_risk_vals[ind_b],
            col = c("#ABAFFF"),
            lwd = 1.5)
      lines(pred$predvar[ind_c],
            relative_risk_vals[ind_c],
            col = c("black"),
            lwd = 1.5)
      lines(pred$predvar[ind_d],
            relative_risk_vals[ind_d],
            col = c("#FFA7A7"),
            lwd = 1.5)
      lines(pred$predvar[ind_e],
            relative_risk_vals[ind_e],
            col = c("#FF0000"),
            lwd = 1.5)

      axis(2, at = 1:5 * 0.5)

      breaks <- c(min(data$temp, na.rm = TRUE) - 1,
                  seq(pred$predvar[1],
                      pred$predvar[length(pred$predvar)],
                      length = 30),
                  max(data$temp, na.rm = TRUE) + 1)


      hist <- hist(data$temp, breaks = breaks, plot = FALSE)
      hist$density <- hist$density / max(hist$density) * 0.7
      prop <- max(hist$density) / max(hist$counts)
      counts <- pretty(hist$count, 3)

      plot(hist,
           ylim = c(0, max(hist$density) * 3.5),
           axes = FALSE, ann = FALSE, col = grey(0.95),
           breaks = breaks, freq = FALSE, add = TRUE)

      axis(4, at = counts * prop, labels = counts, cex.axis = 0.7)
      mtext("N", 4, line = -0.5, at = mean(counts * prop), cex = 0.5)

      abline(v = mintempregions[i], lty = 1, col = 3)
      abline(v = c(an_thresholds[i,c("moderate_cold_OTR", "moderate_cold_OTR")]),
             lty = 2)
      abline(v = c(an_thresholds[i,c("high_moderate_cold", "high_moderate_heat")]),
             lty = 3)
    }


    if (!is.null(blup)) {

      relative_risk_vector <- append(pred$allRRfit, relative_risk_vector)
      upper_vector <- append(pred$allRRhigh, upper_vector)
      lower_vector <- append(pred$allRRlow, lower_vector)
    } else {

      relative_risk_vector <- append(pred$RRfit, relative_risk_vector)
      upper_vector <- append(pred$RRhigh, upper_vector)
      lower_vector <- append(pred$RRlow, lower_vector)
    }
    region_vector <- append(rep(names(df_list)[i], length(pred$predvar)), region_vector)
    temp_vector <- append(pred$predvar, temp_vector)
    cen_vector <- append(rep(cen, length(pred$predvar)), cen_vector)
    temperature_vector <- append(temperature_vector, data$temp)
    temperature_region_vector <- append(
      rep(names(df_list)[i], length(data$temp)), temperature_region_vector
    )
  }
  if (save_fig == TRUE) {

    dev.off()

  }


  output_df <- data.frame(regions = region_vector,
                          temp = temp_vector,
                          rel_risk = relative_risk_vector,
                          centre_temp = cen_vector,
                          upper = upper_vector,
                          lower = lower_vector)

  optimal_temp_df <- output_df %>%
    dplyr::group_by(regions) %>%
    dplyr::filter(rel_risk < 1.1) %>%
    dplyr::summarise(optimal_temp_range_min = min(temp),
                     optimal_temp_range_max = max(temp))

  output_df <- dplyr::left_join(x = output_df,
                                y = optimal_temp_df,
                                by = "regions")


  temp_df <- data.frame(temp_mean = temperature_vector,
                        regions = temperature_region_vector)

  if (save_csv == TRUE) {

    write.csv(output_df,
              csv_output_path,
              row.names = FALSE)

  }

  return (list(output_df, temp_df))

}


#' Plot and write results of analysis
#'
#' @param df_list An alphabetically-ordered
#' list of dataframes for each region.
#' @param cb The generated crossbasis.
#' @param model The generated model.
#' @param mintempregions A named numeric vector.
#'   Minimum (optimum) mortality temperature per region.
#' @param save_fig Whether to save output figure (Bool)
#' @param save_csv Whether to save output CSVs (Bool)
#' @param csv_output_path The path to save the csv to, including file name and
#' file extensions
#' @param dependent_col the column name of the
#' dependent variable of interest e.g. deaths
#' @param varfun Exposure function
#' (see dlnm::crossbasis)
#' @param varper Internal knot positions in exposure function
#' (see dlnm::crossbasis)
#' @param vardegree Degree of the piecewise polynomial for argvar
#' (see dlnm:crossbasis)
#'
#' @return
#' \itemize{
#'
#'   \item A PDF containing a line plot of temperature versus relative risk per
#'   region, and histogram of temperatures per region.
#'   \item A CSV of relative risk per temperature per region.
#'   \item `output_df` A dataframe with relative risk estimates and confidence
#'   intervals across the temperature range for each region.
#'   \item `temp_df` A dataframe with daily mean exposure values for each
#'   region.
#' }
#'
#' @examples csv_output_path = "directory/sub_directory/file_name.csv"
#'
#' @export
plot_and_write_relative_risk_all <- function(df_list,
                                             cb,
                                             model,
                                             mintempregions,
                                             save_fig = TRUE,
                                             save_csv = TRUE,
                                             csv_output_path = NULL,
                                             dependent_col,
                                             varfun,
                                             varper,
                                             vardegree
) {

  data <- do.call(rbind, df_list)

  # All temps
  predvar <- data$temp

  cen <- median(mintempregions)

  pred <- dlnm::crosspred(basis = cb, model = model, cen = cen)

  if (save_fig==TRUE) {
    plot(pred,
         "overall",
         type = "n",
         ylab = "RR",
         ylim = c(.0, 3),
         xlim = c(min(data$temp), max(data$temp)),
         xlab = expression(paste("Temperature (", degree, "C)")),
         main = "All Regions"
    )
    abline(h = 1)
  }


  optimal_meta_lower <- as.numeric(names(
    which.min(which(pred$allRRfit >= 1 & pred$allRRfit <= 1.1))))
  optimal_meta_upper <- as.numeric(names(
    which.max(which(pred$allRRfit >= 1 & pred$allRRfit <= 1.1))))

  extreme_cold <- ifelse(optimal_meta_lower < quantile(data$temp, 2.5 / 100,
                                                       na.rm = TRUE),
                         optimal_meta_lower,
                         quantile(data$temp, 2.5 / 100, na.rm = TRUE))
  extreme_heat <- ifelse(optimal_meta_upper > quantile(data$temp, 97.5 / 100,
                                                       na.rm = TRUE),
                         optimal_meta_upper,
                         quantile(data$temp, 97.5 / 100, na.rm = TRUE))

  ind_a <- pred$predvar <= extreme_cold
  ind_b <- pred$predvar >= extreme_cold & pred$predvar <= optimal_meta_lower
  ind_c <- pred$predvar >= optimal_meta_lower & pred$predvar <= optimal_meta_upper
  ind_d <- pred$predvar >= optimal_meta_upper & pred$predvar <= extreme_heat
  ind_e <- pred$predvar >= extreme_heat

  if (save_fig==TRUE) {
    relative_risk_vals <- pred$allRRfit

    lines(pred$predvar,
          pred$allRRfit,
          col = 'black',
          lwd = 1)

    lines(pred$predvar[ind_a],
          pred$allRRfit[ind_a],
          col = c("#000FFF"),
          lwd = 1.5)
    lines(pred$predvar[ind_b],
          pred$allRRfit[ind_b],
          col = c("#ABAFFF"),
          lwd = 1.5)
    lines(pred$predvar[ind_c],
          pred$allRRfit[ind_c],
          col = c("black"),
          lwd = 1.5)
    lines(pred$predvar[ind_d],
          pred$allRRfit[ind_d],
          col = c("#FFA7A7"),
          lwd = 1.5)
    lines(pred$predvar[ind_e],
          pred$allRRfit[ind_e],
          col = c("#FF0000"),
          lwd = 1.5)


    axis(2, at = 1:5 * 0.5)

    breaks <- c(min(data$temp, na.rm = TRUE) - 1,
                seq(pred$predvar[1],
                    pred$predvar[length(pred$predvar)],
                    length = 30),
                max(data$temp, na.rm = TRUE) + 1)


    hist <- hist(data$temp, breaks = breaks, plot = FALSE)
    hist$density <- hist$density / max(hist$density) * 0.7
    prop <- max(hist$density) / max(hist$counts)
    counts <- pretty(hist$count, 3)

    plot(hist,
         ylim = c(0, max(hist$density) * 3.5),
         axes = FALSE, ann = FALSE, col = grey(0.95),
         breaks = breaks, freq = FALSE, add = TRUE)

    axis(4, at = counts * prop, labels = counts, cex.axis = 0.7)
    mtext("N", 4, line = -0.5, at = mean(counts * prop), cex = 0.5)

    abline(v = cen, lty = 1, col = 3)
    abline(v = c(optimal_meta_lower, optimal_meta_upper), lty = 2)
    abline(v = c(extreme_cold, extreme_heat), lty = 3)
  }

  relative_risk_vector <- pred$allRRfit
  upper_vector <- pred$allRRhigh
  lower_vector <- pred$allRRlow
  region_vector <- rep('all_regions', length(pred$predvar))
  temp_vector <- pred$predvar
  cen_vector <- rep(cen, length(pred$predvar))
  temperature_vector <- data$temp

  temperature_region_vector <- rep('all_regions', length(data$temp))

  if (save_fig == TRUE) {

    dev.off()

  }

  output_df <- data.frame(regions = region_vector,
                          temp = temp_vector,
                          rel_risk = relative_risk_vector,
                          centre_temp = cen_vector,
                          upper = upper_vector,
                          lower = lower_vector)

  temp_df <- data.frame(temp = temperature_vector,
                        regions = temperature_region_vector)

  if (save_csv == TRUE) {

    write.csv(output_df,
              csv_output_path,
              row.names = FALSE)

  }

  return (list(output_df, temp_df))

}

#' Do full DLNM analysis.
#'
#' @description Runs a sequence of functions to carry out
#' heat-related mortality analysis.
#'
#' @details Modified from Gasparrini A et al. (2015)
#' The Lancet. 2015;386(9991):369-375.
#'
#' @param input_csv_path_ Path to a CSV contain
#' daily time series of death and temperature per region.
#' @param output_folder_path_ Path to folder for storing outputs.
#' @param save_fig_ Boolean (TRUE or FALSE). Whether to save output figure.
#' @param save_csv_ Boolean (TRUE or FALSE). Whether to save output CSVs.
#' @param meta_analysis_ Boolean (TRUE or FALSE). Whether to include
#' meta-analysis. Must be TRUE if by_region argument is FALSE.
#' @param by_region_ Boolean (TRUE or FALSE). Whether to disaggregate by region.
#' Must be TRUE if meta-analysis is FALSE.
#' @param RR_distribution_length_ Number of years for the calculation of RR
#' distribution. Set both as 'NONE' to use full range in data.
#' @param output_year_ Year(s) to calculate output for.
#' @param dependent_col_ the column name of the  dependent variable of interest e.g. deaths
#' @param independent_cols_ column name (or list of names) of extra independent
#' variable to include in regression (excluding temperature). Defaults to NULL.
#' @param time_col_ The column name of column containing dates (e.g date, year).
#' @param region_col_ The column name of the column containing regions.
#' @param temp_col_ the column name of the column containing the exposure.
#' @param population_col_ the column name of the column containing population values.
#' @param varfun_ Exposure function
#' (see dlnm::crossbasis)
#' @param vardegree_ Degree of the piecewise polynomial for argvar
#' (see dlnm:crossbasis)
#' @param lag_ Lag length in time
#' (see dlnm::logknots)
#' @param lagnk_ Number of knots in lag function
#' (see dlnm::logknots)
#' @param dfseas_ Degrees of freedom for seasonality
#' @param nsim__ Integer. The number of simulations to run for the model.
#' Defaults to 1000.
#'
#' @return
#' \itemize{
#'
#'   \item A PDF containing a line plot of temperature versus
#'relative risk per region,
#' and histogram of temperatures per region.
#'   \item A CSV of relative risk per temperature per region.
#'   \item `output_df` A dataframe with relative risk estimates and confidence
#'   intervals across the temperature range for each region.
#'   \item `temp_df` A dataframe with daily mean exposure values for each
#'   region.
#'   \item `anregions_bind` A matrix of numbers of deaths attributable to
#'   temperature, heat, cold, extreme heat and extreme cold (with confidence
#'   intervals), disaggregated by region.
#'   \item `attrdl_yr_all` A dataframe with attributable deaths by year for each
#'   region.
#'   \item `attr_fractions_yr` A dataframe with attributable fractions by year
#'   and region.
#' }
#'
#' @seealso [dlnm] package
#'
#' @export
heat_and_cold_analysis <- function(input_csv_path_ = 'NONE',
                                   output_folder_path_ = NULL,
                                   save_fig_ = FALSE,
                                   save_csv_ = FALSE,
                                   meta_analysis_ = FALSE,
                                   by_region_ = FALSE,
                                   RR_distribution_length_ = 0,
                                   output_year_ = 0,
                                   dependent_col_,
                                   independent_cols_ = NULL,
                                   time_col_,
                                   region_col_,
                                   temp_col_,
                                   population_col_,
                                   varfun_ = 'bs',
                                   vardegree_ = 2,
                                   lag_  = 21,
                                   lagnk_ = 3,
                                   dfseas_ = 8,
                                   nsim__ = 1000) {
  varper_ <- c(10, 75, 90)

  c(df_list_) %<-%
    load_temperature_data(
      input_csv_path = input_csv_path_,
      dependent_col = dependent_col_,
      time_col = time_col_,
      region_col = region_col_,
      temp_col = temp_col_,
      population_col = population_col_,
      output_year = output_year_,
      RR_distribution_length = RR_distribution_length_
    )

  c(coef_, vcov_, cb_, model_) %<-%
    run_model(df_list = df_list_,
              independent_cols = independent_cols_,
              varfun = varfun_,
              varper = varper_,
              vardegree = vardegree_,
              lag = lag_,
              lagnk = lagnk_,
              dfseas = dfseas_
    )
  if (meta_analysis_ == TRUE) {

    c(mv_, blup_) %<-%
      run_meta_model(
        df_list = df_list_,
        coef = coef_,
        vcov = vcov_
      )

    c(avgtmean_wald_, rangetmean_wald_) %<-%
      wald_results(
        mv = mv_
      )

  } else {

    blup_ <- NULL
    avgtmean_wald_ <- NULL
    rangetmean_wald_ <- NULL

  }

  c(mintempregions_, an_thresholds_) %<-%
    calculate_min_mortality_temp(
      df_list = df_list_,
      blup = blup_,
      independent_cols = independent_cols_,
      varfun = varfun_,
      varper = varper_,
      vardegree = vardegree_,
      lag = lag_,
      lagnk = lagnk_,
      dfseas = dfseas_
    )

  if (save_csv_) {
    thresholds_fpath <- file.path(
      output_folder_path_,
      "heat_and_cold_an_thresholds.csv"
    )
    write.csv(an_thresholds_, thresholds_fpath)
  }

  c(arraysim_, matsim_) %<-%
    compute_attributable_deaths(
      df_list = df_list_,
      output_year = output_year_,
      blup = blup_,
      mintempregions = mintempregions_,
      an_thresholds = an_thresholds_,
      independent_cols = independent_cols_,
      varfun = varfun_,
      varper = varper_,
      vardegree = vardegree_,
      lag = lag_,
      lagnk = lagnk_,
      dfseas = dfseas_,
      nsim_ = nsim__
    )

  c(anregions_bind_,antot_bind_, arregions_bind_, artot_bind_) %<-%
    compute_attributable_rates(df_list = df_list_,
                               output_year = output_year_,
                               matsim = matsim_,
                               arraysim = arraysim_)

  c(wald_publication_, anregions_publication_, antot_bind_,
    arregions_publication_, artot_bind_) %<-%
    write_attributable_deaths(
      avgtmean_wald = avgtmean_wald_,
      rangetmean_wald = rangetmean_wald_,
      anregions_bind = anregions_bind_,
      antot_bind = antot_bind_,
      arregions_bind = arregions_bind_,
      artot_bind = artot_bind_,
      save_csv = save_csv_,
      output_folder_path = output_folder_path_
    )

  if (by_region_ == FALSE) {

    c(output_df, temp_df) %<-%
      plot_and_write(
        df_list = df_list_,
        output_name = "heat_and_cold",
        aggregate_outputs = TRUE,
        output_folder_path = output_folder_path_,
        save_fig = save_fig_,
        save_csv = save_csv_,
        cb = cb_,
        model = model_,
        mintempregions = mintempregions_,
        varfun = varfun_,
        varper = varper_,
        vardegree = vardegree_,
        dependent_col = dependent_col_
      )

  } else {

    c(output_df, temp_df) %<-%
      plot_and_write(
        df_list = df_list_,
        output_name = "heat_and_cold",
        aggregate_outputs = FALSE,
        output_folder_path = output_folder_path_,
        blup = blup_,
        mintempregions = mintempregions_,
        an_thresholds = an_thresholds_,
        save_fig = save_fig_,
        save_csv = save_csv_,
        independent_cols = independent_cols_,
        varfun = varfun_,
        varper = varper_,
        vardegree = vardegree_,
        lag = lag_,
        lagnk = lagnk_,
        dfseas = dfseas_
      )
  }

  return (list(output_df, temp_df, anregions_publication_, antot_bind_,
               arregions_publication_, artot_bind_))

}
