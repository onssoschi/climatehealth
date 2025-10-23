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
#' @param geog The geography column over which the data
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
                         geography_col,
                         temperature_col,
                         population_col) {

  # Load the input dataset
  df <- read_input_data(input_csv_path)

  # Format the geography column. If geog is missing, then assume geographies are aggregated or only single input geography
  if (is.null(geography_col)) {
    df <- df %>%
      dplyr::mutate(geog = "aggregated")
  }
  # Rename the columns
  df <- df %>%
    dplyr::rename(dependent = dependent_col,
                  date = date_col,
                  geog = geography_col,
                  temp = temperature_col,
                  pop = population_col,
    ) %>%
    dplyr::mutate(date = as.Date(date, tryFormats = c("%d/%m/%Y", "%Y-%m-%d")),
                  year = as.factor(lubridate::year(date)),
                  month = as.factor(lubridate::month(date)),
                  dow = as.factor(lubridate::wday(date, label = TRUE)),
                  geog = as.factor(geog))

  # Reformat data and fill NaNs
  df <- reformat_data(df,
                      reformat_date = TRUE,
                      fill_na = c("dependent"),
                      year_from_date = TRUE)
  # Split the data by region
  df_list <- aggregate_by_column(df, "geog")

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
                               independent_cols = NULL,
                               dfseas = 8){

  qaic_results <- list()
  residuals_list <- list()

  if (!is.null(independent_cols)) {

    control_vars <- c(independent_cols)

    transformed_vars <- unlist(lapply(independent_cols, function(v) {
      paste0(v, "_ns")
    }))

  } else(transformed_vars <- NULL)

  # EW poss no need to apply ns to independents - check literature whether should be controlled with splines
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

    # define the base independent cols
      base_independent_cols <- c(
      'cb',
      'dow',
      paste0('splines::ns(date, df = ', dfseas, ' * length(unique(year)))')
    )

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
  # Sort by geog and QAIC
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
                                dfseas = dfseas,
                                save_fig = FALSE,
                                save_csv = FALSE,
                                output_folder_path = NULL){

  c(qaic_results, residuals_list) %<-% hc_model_combo_res(df_list = df_list,
                                                          cb_list = cb_list,
                                                          dfseas = dfseas,
                                                          independent_cols = independent_cols)

  if (save_csv == TRUE){

    dir.create(file.path(
      output_folder_path, "model_validation"), recursive = TRUE, showWarnings = FALSE)

    write.csv(qaic_results, file = file.path(
      output_folder_path, "model_validation", "qaic_results.csv"), row.names = FALSE)

  }

  if (!is.null(independent_cols)) {

    vif_list <- hc_vif(df_list = df_list,
                       independent_cols = independent_cols)

    vif_results <- dplyr::bind_rows(vif_list, .id = "Geography")

    if (save_csv == TRUE) {

      write.csv(vif_results, file = file.path(
        output_folder_path, "model_validation", "vif_results.csv"), row.names = FALSE)

    }

  } else vif_results <- NULL

  if (length(df_list) > 1){

    qaic_summary <- qaic_results %>%
      group_by(formula) %>%
      summarise(mean_disp = mean(.data$disp),
                mean_qaic = mean(.data$qaic))

    if (save_csv == TRUE){

      write.csv(qaic_summary, file = file.path(
        output_folder_path, "model_validation", "qaic_summary.csv"), row.names = FALSE)

    }

    if (!is.null(vif_results)){

      vif_summary <- vif_results %>%
        dplyr::group_by(.data$variable) %>%
        dplyr::summarise(mean_vif = mean(.data$vif, na.rm = TRUE))

      if (save_csv == TRUE){

        write.csv(vif_summary, file = file.path(
          output_folder_path, "model_validation", "vif_summary.csv"), row.names = FALSE)

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

      output_folder_main <- file.path(output_folder_path, "model_validation", geog_folder)
      dir.create(output_folder_main, recursive = TRUE, showWarnings = FALSE)

      grid <- c(min(length(formula_list), 3), ceiling(length(formula_list) / 3))
      output_path <- paste0(output_folder_main, "/", named_label, "_residuals_timeseries.pdf")
      pdf(output_path, width = grid[1]*5.5, height = grid[2]*4.5)

      par(mfrow=c(grid[2], grid[1]), oma = c(0, 0, 4, 0))

    }

    for (i in names(formula_list)){

      plot(x = geog_data$date,
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
        group_by(.data$formula) %>%
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
    'cb',
    'dow',
    paste0('splines::ns(date, df = ', dfseas, ' * length(unique(year)))')
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
                                 var_degree = 2,
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


#' Calculate p-values for Wald test
#'
#' A function to calculate p-values for an explanatory variable.
#'
#' @param model A model object.
#' @param var A character. The name of the variable in the model to calculate
#' p-values for.
#'
#' @export
#' @return A number. The p-value of the explanatory variable.
fwald <- function(mm, var) {

  if(!is.character(var)) {
    stop("Argument 'var' must be a character")
  }

  ind <- grep(var, names(coef(mm)))
  coef <- coef(mm)[ind]
  vcov <- vcov(mm)[ind, ind]
  waldstat <- coef %*% solve(vcov) %*% coef
  df <- length(coef)

  return(1 - pchisq(waldstat, df))

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

  temp_avg_wald <- fwald(mm, "temp_avg")
  temp_range_wald <- fwald(mm, "temp_range")

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
hc_predict <- function(df_list,
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
                                 var_degree = 2,
                                 lagn = 21,
                                 lagnk = 3,
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
           geog = country)

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
                           minpercgeog_,
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
                              coef=mmpredall$fit,
                              vcov=mmpredall$vcov,
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
      Area = geog_name,
      Temperature = geog_pred$predvar,
      RR = geog_pred$allRRfit,
      RR_lower_CI = geog_pred$allRRlow,
      RR_upper_CI = geog_pred$allRRhigh
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
hc_plot_rr <- function(df_list,
                       pred_list,
                       attr_thr_high = 97.5,
                       attr_thr_low = 2.5,
                       minpercgeog_,
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

    output_path <- file.path(output_folder_path, "temp_mortality_rr_plot.pdf")
    pdf(output_path, width=max(10,grid[1]*5.5), height=max(7, grid[2]*4))

    layout_ids <- seq_len(grid[1] * grid[2])
    layout_matrix <- matrix(layout_ids, nrow = grid[2], ncol = grid[1], byrow = TRUE)

    layout(layout_matrix, heights = rep(1, grid[2]), widths = rep(1, grid[1]))

    par(oma = c(0,1,4,1))

  }

  for(geog in names(pred_list)){

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
         col = "#296991")


    # high temperature threshold line
    vline_pos_high_x <- quantile(geog_temp, attr_thr_high/100, na.rm = TRUE)
    vline_pos_high_y <- max(geog_pred$allRRfit, na.rm = TRUE) + 0.3
    vline_lab_high <- paste0("High temp. threshold\n", round(vline_pos_high_x, 2), intToUtf8(176), "C (p", attr_thr_high, ")")

    #add dashed line  and label to plot
    abline(v = vline_pos_high_x, col = "#0A2E4D", lty = 2)
    text(x = vline_pos_high_x, y = vline_pos_high_y, labels = vline_lab_high, pos = 4, col = "black", cex = 0.8)

    # low temperature threshold line
    vline_pos_low_x <- quantile(geog_temp, attr_thr_low/100, na.rm = TRUE)
    vline_pos_low_y <- max(geog_pred$allRRfit, na.rm = TRUE) + 0.3
    vline_lab_low <- paste0("Low temp. threshold\n", round(vline_pos_low_x, 2), intToUtf8(176), "C (p", attr_thr_low, ")")

    #add dashed line to plot
    abline(v = vline_pos_low_x, col = "#0A2E4D", lty = 2)
    text(x = vline_pos_low_x, y = vline_pos_low_y, labels = vline_lab_low, pos = 4, col = "black", cex = 0.8)

    # MMT (min RR) line
    vline_pos_min_x <- quantile(geog_temp, minpercgeog_[geog]/100, na.rm = TRUE)
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
                      plot = FALSE)

    hist_scale <- (0.3) / hist_max
    scaled_counts <- hist_data$counts * hist_scale

    for (i in seq_along(hist_data$counts)) {
      rect(xleft = hist_data$breaks[i],
           xright = hist_data$breaks[i + 1],
           ybottom = ylim[1],
           ytop = ylim[1] + scaled_counts[i],
           col = "#7A855C",
           border = "white")
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

  if (output_config$save_fig==TRUE) {

    year_range <- paste0("(",
                         min(sapply(df_list, function(x) min(lubridate::year(x$date), na.rm = TRUE))),
                         "-",
                         max(sapply(df_list, function(x) max(lubridate::year(x$date), na.rm = TRUE))),
                         ")")

    title <- paste0("Relative risk of mortality by mean temperature and geography, ", country, " ",  year_range)

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
hc_attr <- function(df_list,
                    cb_list,
                    pred_list,
                    minpercgeog_,
                    attr_thr_high = 97.5,
                    attr_thr_low = 2.5) {

  attr_list <- list()

  for (geog in names(df_list)){

    geog_data <- df_list[[geog]]
    cb <- cb_list[[geog]]
    pred <- pred_list[[geog]]
    minperc <- minpercgeog_[geog]

    cen <- quantile(geog_data$temp, minperc/100, na.rm = TRUE)
    min_temp <- min(geog_data$temp, na.rm = TRUE)
    low_temp <- quantile(geog_data$temp, attr_thr_low/100, na.rm = TRUE)
    high_temp <- quantile(geog_data$temp, attr_thr_high/100, na.rm = TRUE)
    max_temp <- max(geog_data$temp, na.rm = TRUE)

    c(af_heat, af_heat_lower_ci, af_heat_upper_ci,
      an_heat, an_heat_lower_ci, an_heat_upper_ci) %<-% an_attrdl(
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

    c(af_cold, af_cold_lower_ci, af_cold_upper_ci,
      an_cold, an_cold_lower_ci, an_cold_upper_ci) %<-% an_attrdl(
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

    results <- geog_data %>%
      dplyr::select(.data$geog, .data$date, .data$temp, .data$year,
                    .data$month, .data$dependent, .data$pop) %>%
      dplyr::mutate(
        #outputs for higher, hotter temperatures
        threshold_temp_high = round((high_temp), 2),
        af_heat = af_heat,
        af_heat_lower_ci = af_heat_lower_ci,
        af_heat_upper_ci = af_heat_upper_ci,
        an_heat = an_heat,
        an_heat_lower_ci = an_heat_lower_ci,
        an_heat_upper_ci = an_heat_upper_ci,
        ar_heat = (.data$an_heat / .data$pop) * 100000,
        ar_heat_lower_ci = (.data$an_heat_lower_ci / .data$pop) * 100000,
        ar_heat_upper_ci = (.data$an_heat_upper_ci / .data$pop) * 100000,
        #outputs for lower, colder temperatures
        threshold_temp_low = round((low_temp), 2),
        af_cold = af_cold,
        af_cold_lower_ci = af_cold_lower_ci,
        af_cold_upper_ci = af_cold_upper_ci,
        an_cold = an_cold,
        an_cold_lower_ci = an_cold_lower_ci,
        an_cold_upper_ci = an_cold_upper_ci,
        ar_cold = (.data$an_cold / .data$pop) * 100000,
        ar_cold_lower_ci = (.data$an_cold_lower_ci / .data$pop) * 100000,
        ar_cold_upper_ci = (.data$an_cold_upper_ci / .data$pop) * 100000
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
#' @export
hc_attr_tables <- function(attr_list,
                           country = "National",
                           meta_analysis = FALSE) {

  attr_res <- do.call(rbind, attr_list) %>%
    dplyr::mutate(year = as.numeric(as.character(.data$year)))

  res_list <- list()

  groupings <- list(
    monthly = rlang::quos(.data$month, .data$geog),
    yearly  = rlang::quos(.data$year, .data$geog),
    overall = rlang::quos(.data$geog)
  )

  for (grp_name in names(groupings)) {

    results <- attr_res %>%
      dplyr::group_by(!!!groupings[[grp_name]]) %>%
      dplyr::summarise(
        population     = round(mean(.data$pop, na.rm = TRUE), 0),
        temp           = round(mean(.data$temp, na.rm = TRUE), 2),
        threshold_temp_high = mean(.data$threshold_temp_high, na.rm = TRUE),
        threshold_temp_low = mean(.data$threshold_temp_low, na.rm = TRUE),
        dplyr::across(
          c("dependent", "an_heat", "an_heat_lower_ci", "an_heat_upper_ci"
            , "an_cold", "an_cold_lower_ci", "an_cold_upper_ci"),
          ~ sum(.x, na.rm = TRUE)
        ),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        af_heat           = .data$an_heat / .data$dependent * 100,
        af_heat_lower_ci  = .data$an_heat_lower_ci / .data$dependent * 100,
        af_heat_upper_ci  = .data$an_heat_upper_ci / .data$dependent * 100,
        ar_heat           = .data$an_heat / .data$population * 100000,
        ar_heat_lower_ci  = .data$an_heat_lower_ci / .data$population* 100000,
        ar_heat_upper_ci  = .data$an_heat_upper_ci / .data$population * 100000,
        af_cold           = .data$an_cold / .data$dependent * 100,
        af_cold_lower_ci  = .data$an_cold_lower_ci / .data$dependent * 100,
        af_cold_upper_ci  = .data$an_cold_upper_ci / .data$dependent * 100,
        ar_cold           = .data$an_cold / .data$population * 100000,
        ar_cold_lower_ci  = .data$an_cold_lower_ci / .data$population* 100000,
        ar_cold_upper_ci  = .data$an_cold_upper_ci / .data$population * 100000
      ) %>%
      dplyr::mutate(
        dplyr::across(
          c("an_heat", "an_heat_lower_ci", "an_heat_upper_ci",
            "ar_heat", "ar_heat_lower_ci", "ar_heat_upper_ci",
            "af_heat", "af_heat_lower_ci", "af_heat_upper_ci",
            "an_cold", "an_cold_lower_ci", "an_cold_upper_ci",
            "ar_cold", "ar_cold_lower_ci", "ar_cold_upper_ci",
            "af_cold", "af_cold_lower_ci", "af_cold_upper_ci"),
          ~ ifelse(abs(.x) < 1, signif(.x, 2), round(.x, 2))
        )
      )

    res_list[[grp_name]] <- results
  }

  geog_order <- if (isTRUE(model_config$meta_analysis)) {
    c(sort(setdiff(names(attr_list), model_config$country)), model_config$country)
  } else {
    sort(names(attr_list))
  }

  res_attr_tot <- res_list[["overall"]]

  attr_yr_list <- aggregate_by_column(res_list[["yearly"]], "geog")
  attr_yr_list <- attr_yr_list[geog_order]

  attr_mth_list <- res_list[["monthly"]] %>%
    dplyr::mutate(month = month.name[.data$month]) %>%
    aggregate_by_column("geog")
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
#' @export
hc_plot_attr_heat_totals <- function(df_list,
                                res_attr_tot,
                                save_fig = FALSE,
                                output_folder_path = NULL,
                                country = "National"){

  if (save_fig == TRUE){

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
  short_labels <- sapply(as.character(res_attr_tot$geog), function(x) {
    if (nchar(x)-3 > 10) {
      paste0(substr(x, 1, 10), "...")
    } else {
      x
    }
  })

  names(short_labels) <- NULL

  year_range <- paste0("(",
                       min(sapply(df_list, function(x) min(lubridate::year(x$date), na.rm = TRUE))),
                       "-",
                       max(sapply(df_list, function(x) max(lubridate::year(x$date), na.rm = TRUE))),
                       ")")

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
  nat_ind_af_heat <- which(res_af_heat_tot$geog == country)
  if (length(nat_ind_af_heat) > 0) {
    bar_col_af_heat[nat_ind_af_heat] <- "#7a855c" # Highlight color
  }

  barplot(names.arg = short_labs_af_heat,
          height = res_af_heat_tot$af_heat,
          ylab = "High temperature AF (%)",
          main = paste0("Attributable fraction of high temperature mortality by geography, ", country, " ",  year_range),
          col = bar_col_af_heat,
          las = 2,
          horiz = FALSE)

  mtext(af_warning, side = 1, line = 7, cex = 0.8, col = "red", font = 3)
  mtext(ovr_warning, side = 1, line = 8, cex = 0.8, col = "red", font = 3)

  if (save_fig == TRUE){

    par(mar = c(10, 5, 4, 2))

  }

  # Sort by AF descending
  sorted_indices <- order(res_attr_tot$ar_heat, decreasing = TRUE)
  res_ar_heat_tot <- res_attr_tot[sorted_indices, ]
  short_labs_ar_heat <- short_labels[sorted_indices]

  # Define bar colors
  bar_col_ar_heat <- rep("#c75e70", length(short_labs_ar_heat))
  nat_ind_ar_heat <- which(res_ar_heat_tot$geog == country)
  if (length(nat_ind_ar_heat) > 0) {
    bar_col_ar_heat[nat_ind_ar_heat] <- "#7a855c" # Highlight color
  }

  barplot(names.arg = short_labs_ar_heat,
          height = res_ar_heat_tot$ar_heat,
          ylab = "High temperature AR (per 100,000 population)",
          main = paste0("Attributable rate of high temperature mortality by geography, ", country, " ", year_range),
          col = bar_col_ar_heat,
          las = 2,
          horiz = FALSE)

  mtext(ar_warning, side = 1, line = 7, cex = 0.8, col = "red", font = 3)
  mtext(ovr_warning, side = 1, line = 8, cex = 0.8, col = "red", font = 3)

  if (save_fig == TRUE){

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
#' @export
hc_plot_attr_cold_totals <- function(df_list,
                                     res_attr_tot,
                                     save_fig = FALSE,
                                     output_folder_path = NULL,
                                     country = "National"){

  if (save_fig == TRUE){

    num_geogs <- nrow(res_attr_tot)

    # Dynamically adjust height based on number of regions
    chart_height <- 6
    chart_width <- 0.3 * num_geogs # adjust as needed
    total_width <- max(8, chart_width)

    output_path <- file.path(path_config$output_folder_path, "mortality_total_cold_attr_plot.pdf")
    pdf(output_path, width = total_width, height = chart_height * 2)

    # Set up layout: 1 row for barplot and 1 row for table
    layout(matrix(c(1, 2), nrow = 2), heights = c(chart_height, chart_height))

    # Set up plotting area for the bar chart
    par(mar = c(10, 5, 4, 2), oma = c(1, 0, 0, 0))

  }

  # Shorten the labels to a fixed length
  short_labels <- sapply(as.character(res_attr_tot$geog), function(x) {
    if (nchar(x)-3 > 10) {
      paste0(substr(x, 1, 10), "...")
    } else {
      x
    }
  })

  names(short_labels) <- NULL

  year_range <- paste0("(",
                       min(sapply(df_list, function(x) min(lubridate::year(x$date), na.rm = TRUE))),
                       "-",
                       max(sapply(df_list, function(x) max(lubridate::year(x$date), na.rm = TRUE))),
                       ")")

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
  nat_ind_af_cold <- which(res_af_cold_tot$geog == country)
  if (length(nat_ind_af_cold) > 0) {
    bar_col_af_cold[nat_ind_af_cold] <- "#7a855c" # Highlight color
  }

  barplot(names.arg = short_labs_af_cold,
          height = res_af_cold_tot$af_cold,
          ylab = "Low temperature AF (%)",
          main = paste0("Attributable fraction of low temperature mortality by geography, ", country, " ",  year_range),
          col = bar_col_af_cold,
          las = 2,
          horiz = FALSE)

  mtext(af_warning, side = 1, line = 7, cex = 0.8, col = "red", font = 3)
  mtext(ovr_warning, side = 1, line = 8, cex = 0.8, col = "red", font = 3)

  if (save_fig == TRUE){

    par(mar = c(10, 5, 4, 2))

  }

  # Sort by AF descending
  sorted_indices <- order(res_attr_tot$ar_cold, decreasing = TRUE)
  res_ar_cold_tot <- res_attr_tot[sorted_indices, ]
  short_labs_ar_cold <- short_labels[sorted_indices]

  # Define bar colors
  bar_col_ar_cold <- rep("#296991", length(short_labs_ar_cold))
  nat_ind_ar_cold <- which(res_ar_cold_tot$geog == country)
  if (length(nat_ind_ar_cold) > 0) {
    bar_col_ar_cold[nat_ind_ar_cold] <- "#7a855c" # Highlight color
  }

  barplot(names.arg = short_labs_ar_cold,
          height = res_ar_cold_tot$ar_cold,
          ylab = "Low temperature AR (per 100,000 population)",
          main = paste0("Attributable rate of low temperature mortality by geography, ", country, " ", year_range),
          col = bar_col_ar_cold,
          las = 2,
          horiz = FALSE)

  mtext(ar_warning, side = 1, line = 7, cex = 0.8, col = "red", font = 3)
  mtext(ovr_warning, side = 1, line = 8, cex = 0.8, col = "red", font = 3)

  if (save_fig == TRUE){

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
#' @export
hc_plot_af_heat_yearly <- function(attr_yr_list,
                              save_fig = FALSE,
                              output_folder_path = NULL,
                              country = "National"){

  if (save_fig == TRUE){

    grid <- c(min(length(attr_yr_list), 3), ceiling(length(attr_yr_list) / 3))
    output_path <- file.path(output_folder_path, "mortality_af_heat_timeseries.pdf")
    pdf(output_path, width = max(10, grid[1]*5.5), height = max(7, grid[2]*4.5))

    par(mfrow=c(grid[2], grid[1]), oma = c(0, 0, 4, 0), mar = c(8, 4, 5, 4))

  }

  year_min <- min(sapply(attr_yr_list, function(x) min(x$year, na.rm = TRUE)))
  year_max <- max(sapply(attr_yr_list, function(x) max(x$year, na.rm = TRUE)))

  y_min <- min(sapply(attr_yr_list, function(x) min(x$af_heat, na.rm = TRUE)))
  y_max <- max(sapply(attr_yr_list, function(x) max(x$af_heat, na.rm = TRUE)))

  ylim <- c(min(c(0, y_min)), y_max) * 1.5

  for (geog in names(attr_yr_list)){

    geog_af <- as.data.frame(attr_yr_list[[geog]])

    plot(x = geog_af$year,
         y = geog_af$af_heat,
         type = "l",
         xlim = c(year_min, year_max),
         ylim = ylim,
         xlab = "Year",
         ylab = "High temperature AF (%)",
         main = geog,
         col = "#c75e70")

    # Ensure data is sorted by Year
    geog_af <- geog_af[order(geog_af$year), ]

    # Create x and y coordinates for the polygon
    x_poly <- c(geog_af$year, rev(geog_af$year))
    y_poly <- c(geog_af$af_heat_upper_ci, rev(geog_af$af_heat_lower_ci))

    # Draw shaded confidence interval
    polygon(x = x_poly,
            y = y_poly,
            col = adjustcolor("#c75e70", alpha.f = 0.2),
            border = NA)

    abline(h = 0,
           col = "black",
           lty = 2)

    legend("topright",
           inset = c(0, -0.1),
           legend = "95% CI",
           col = adjustcolor("#c75e70", alpha.f = 0.2),
           pch = 15,
           pt.cex = 2,
           bty = "n",
           xpd = TRUE,
           horiz = TRUE,
           cex = 0.9)

    if (save_fig == TRUE){

      af_heat_ci_range <- c(min(geog_af$af_heat_lower_ci), max(geog_af$af_heat_upper_ci))

      if (af_heat_ci_range[1] < ylim[1] || af_heat_ci_range [2] > ylim[2]){

        ci_warning <- sprintf("Warning: CI's are outside the bounds of this chart. CI's range from %.2f%% to %.2f%%", af_heat_ci_range[1], af_heat_ci_range[2])
        ovr_warning <- "(Please refer to the associated data table for more information on the uncertainty around each estimate)"

        mtext(ci_warning, side = 1, line = 5, cex = 0.6, col = "red", font = 3)
        mtext(ovr_warning, side = 1, line = 6, cex = 0.6, col = "red", font = 3)

      }

    }

  }

  if (save_fig == TRUE){

    year_range <- paste0("(", year_min, " - ", year_max, ")")
    title <- paste0("Yearly attributable fraction of high temperature mortality by geography, ", country, " ",  year_range)

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
#' @export
hc_plot_af_cold_yearly <- function(attr_yr_list,
                                   save_fig = FALSE,
                                   output_folder_path = NULL,
                                   country = "National"){

  if (save_fig == TRUE){

    grid <- c(min(length(attr_yr_list), 3), ceiling(length(attr_yr_list) / 3))
    output_path <- file.path(output_folder_path, "mortality_af_cold_timeseries.pdf")
    pdf(output_path, width = max(10, grid[1]*5.5), height = max(7, grid[2]*4.5))

    par(mfrow=c(grid[2], grid[1]), oma = c(0, 0, 4, 0), mar = c(8, 4, 5, 4))

  }

  year_min <- min(sapply(attr_yr_list, function(x) min(x$year, na.rm = TRUE)))
  year_max <- max(sapply(attr_yr_list, function(x) max(x$year, na.rm = TRUE)))

  y_min <- min(sapply(attr_yr_list, function(x) min(x$af_cold, na.rm = TRUE)))
  y_max <- max(sapply(attr_yr_list, function(x) max(x$af_cold, na.rm = TRUE)))

  ylim <- c(min(c(0, y_min)), y_max) * 1.5

  for (geog in names(attr_yr_list)){

    geog_af <- as.data.frame(attr_yr_list[[geog]])

    plot(x = geog_af$year,
         y = geog_af$af_cold,
         type = "l",
         xlim = c(year_min, year_max),
         ylim = ylim,
         xlab = "Year",
         ylab = "Low temperature AF (%)",
         main = geog,
         col = "#296991")

    # Ensure data is sorted by Year
    geog_af <- geog_af[order(geog_af$year), ]

    # Create x and y coordinates for the polygon
    x_poly <- c(geog_af$year, rev(geog_af$year))
    y_poly <- c(geog_af$af_cold_upper_ci, rev(geog_af$af_cold_lower_ci))

    # Draw shaded confidence interval
    polygon(x = x_poly,
            y = y_poly,
            col = adjustcolor("#296991", alpha.f = 0.2),
            border = NA)

    abline(h = 0,
           col = "black",
           lty = 2)

    legend("topright",
           inset = c(0, -0.1),
           legend = "95% CI",
           col = adjustcolor("#296991", alpha.f = 0.2),
           pch = 15,
           pt.cex = 2,
           bty = "n",
           xpd = TRUE,
           horiz = TRUE,
           cex = 0.9)

    if (save_fig == TRUE){

      af_cold_ci_range <- c(min(geog_af$af_cold_lower_ci), max(geog_af$af_cold_upper_ci))

      if (af_cold_ci_range[1] < ylim[1] || af_cold_ci_range [2] > ylim[2]){

        ci_warning <- sprintf("Warning: CI's are outside the bounds of this chart. CI's range from %.2f%% to %.2f%%", af_cold_ci_range[1], af_cold_ci_range[2])
        ovr_warning <- "(Please refer to the associated data table for more information on the uncertainty around each estimate)"

        mtext(ci_warning, side = 1, line = 5, cex = 0.6, col = "red", font = 3)
        mtext(ovr_warning, side = 1, line = 6, cex = 0.6, col = "red", font = 3)

      }

    }

  }

  if (save_fig == TRUE){

    year_range <- paste0("(", year_min, " - ", year_max, ")")
    title <- paste0("Yearly attributable fraction of low temperature mortality by geography, ", country, " ",  year_range)

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
#' @export
hc_plot_ar_heat_yearly <- function(attr_yr_list,
                              save_fig = FALSE,
                              output_folder_path = NULL,
                              country = "National"){

  if (save_fig == TRUE){

    grid <- c(min(length(attr_yr_list), 3), ceiling(length(attr_yr_list) / 3))
    output_path <- file.path(output_folder_path, "mortality_ar_heat_timeseries.pdf")
    pdf(output_path, width = max(10, grid[1]*5.5), height = max(7, grid[2]*4.5))

    par(mfrow=c(grid[2], grid[1]), oma = c(0, 0, 4, 0), mar = c(8, 4, 5, 4))

  }

  year_min <- min(sapply(attr_yr_list, function(x) min(x$year, na.rm = TRUE)))
  year_max <- max(sapply(attr_yr_list, function(x) max(x$year, na.rm = TRUE)))

  y_min <- min(sapply(attr_yr_list, function(x) min(x$ar_heat, na.rm = TRUE)))
  y_max <- max(sapply(attr_yr_list, function(x) max(x$ar_heat, na.rm = TRUE)))

  ylim <- c(min(c(0, y_min)), y_max) * 1.5

  for (geog in names(attr_yr_list)){

    geog_ar <- as.data.frame(attr_yr_list[[geog]])

    plot(x = geog_ar$year,
         y = geog_ar$ar_heat,
         type = "l",
         xlim = c(year_min, year_max),
         ylim = ylim,
         xlab = "Year",
         ylab = "High temperature AR (per 100,000 population)",
         main = geog,
         col = "#C75E70")

    # Ensure data is sorted by Year
    geog_ar <- geog_ar[order(geog_ar$year), ]

    # Create x and y coordinates for the polygon
    x_poly <- c(geog_ar$year, rev(geog_ar$year))
    y_poly <- c(geog_ar$ar_heat_upper_ci, rev(geog_ar$ar_heat_lower_ci))

    # Draw shaded confidence interval
    polygon(x = x_poly,
            y = y_poly,
            col = adjustcolor("#C75E70", alpha.f = 0.2),
            border = NA)

    abline(h = 0,
           col = "black",
           lty = 2)

    legend("topright",
           inset = c(0, -0.1),
           legend = "95% CI",
           col = adjustcolor("#C75E70", alpha.f = 0.2),
           pch = 15,
           pt.cex = 2,
           bty = "n",
           xpd = TRUE,
           horiz = TRUE,
           cex = 0.9)

    if (save_fig == TRUE){

      ar_heat_ci_range <- c(min(geog_ar$ar_heat_lower_ci), max(geog_ar$ar_heat_upper_ci))

      if (ar_heat_ci_range[1] < ylim[1] || ar_heat_ci_range [2] > ylim[2]){

        ci_warning <- sprintf("Warning: CI's are outside the bounds of this chart. CI's range from %.2f to %.2f per 100,000", ar_heat_ci_range[1], ar_heat_ci_range[2])
        ovr_warning <- "(Please refer to the associated data table for more information on the uncertainty around each estimate)"

        mtext(ci_warning, side = 1, line = 5, cex = 0.6, col = "red", font = 3)
        mtext(ovr_warning, side = 1, line = 6, cex = 0.6, col = "red", font = 3)

      }

    }

  }

  if (save_fig == TRUE){

    year_range <- paste0("(", year_min, " - ", year_max, ")")
    title <- paste0("Yearly attributable rate of high temperature mortality by geography, ", country, " ",  year_range)

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
#' @export
hc_plot_ar_cold_yearly <- function(attr_yr_list,
                                   save_fig = FALSE,
                                   output_folder_path = NULL,
                                   country = "National"){

  if (save_fig == TRUE){

    grid <- c(min(length(attr_yr_list), 3), ceiling(length(attr_yr_list) / 3))
    output_path <- file.path(output_folder_path, "mortality_ar_cold_timeseries.pdf")
    pdf(output_path, width = max(10, grid[1]*5.5), height = max(7, grid[2]*4.5))

    par(mfrow=c(grid[2], grid[1]), oma = c(0, 0, 4, 0), mar = c(8, 4, 5, 4))

  }

  year_min <- min(sapply(attr_yr_list, function(x) min(x$year, na.rm = TRUE)))
  year_max <- max(sapply(attr_yr_list, function(x) max(x$year, na.rm = TRUE)))

  y_min <- min(sapply(attr_yr_list, function(x) min(x$ar_cold, na.rm = TRUE)))
  y_max <- max(sapply(attr_yr_list, function(x) max(x$ar_cold, na.rm = TRUE)))

  ylim <- c(min(c(0, y_min)), y_max) * 1.5

  for (geog in names(attr_yr_list)){

    geog_ar <- as.data.frame(attr_yr_list[[geog]])

    plot(x = geog_ar$year,
         y = geog_ar$ar_cold,
         type = "l",
         xlim = c(year_min, year_max),
         ylim = ylim,
         xlab = "Year",
         ylab = "Low temperature AR (per 100,000 population)",
         main = geog,
         col = "#296991")

    # Ensure data is sorted by Year
    geog_ar <- geog_ar[order(geog_ar$year), ]

    # Create x and y coordinates for the polygon
    x_poly <- c(geog_ar$year, rev(geog_ar$year))
    y_poly <- c(geog_ar$ar_cold_upper_ci, rev(geog_ar$ar_cold_lower_ci))

    # Draw shaded confidence interval
    polygon(x = x_poly,
            y = y_poly,
            col = adjustcolor("#296991", alpha.f = 0.2),
            border = NA)

    abline(h = 0,
           col = "black",
           lty = 2)

    legend("topright",
           inset = c(0, -0.1),
           legend = "95% CI",
           col = adjustcolor("#296991", alpha.f = 0.2),
           pch = 15,
           pt.cex = 2,
           bty = "n",
           xpd = TRUE,
           horiz = TRUE,
           cex = 0.9)

    if (save_fig == TRUE){

      ar_cold_ci_range <- c(min(geog_ar$ar_cold_lower_ci), max(geog_ar$ar_cold_upper_ci))

      if (ar_cold_ci_range[1] < ylim[1] || ar_cold_ci_range [2] > ylim[2]){

        ci_warning <- sprintf("Warning: CI's are outside the bounds of this chart. CI's range from %.2f to %.2f per 100,000", ar_cold_ci_range[1], ar_cold_ci_range[2])
        ovr_warning <- "(Please refer to the associated data table for more information on the uncertainty around each estimate)"

        mtext(ci_warning, side = 1, line = 5, cex = 0.6, col = "red", font = 3)
        mtext(ovr_warning, side = 1, line = 6, cex = 0.6, col = "red", font = 3)

      }

    }

  }

  if (save_fig == TRUE){

    year_range <- paste0("(", year_min, " - ", year_max, ")")
    title <- paste0("Yearly attributable rate of low temperature mortality by geography, ", country, " ",  year_range)

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
#' @param attr_thr Integer. Percentile at which to define the temperature threshold for
#' calculating attributable risk.
#' @param save_fig Boolean. Whether to save the plot as an output. Defaults to
#' FALSE.
#' @param output_folder_path Path to folder where plots should be saved.
#' Defaults to NULL.
#'
#' @return Plots of attributable fractions by calendar month per area
#'
#' @export
hc_plot_af_heat_monthly <- function(attr_mth_list,
                               df_list,
                               country = "National",
                               attr_thr_high = 97.5,
                               save_fig = FALSE,
                               output_folder_path = NULL){


  if (save_fig == TRUE){

    grid <- c(min(length(attr_mth_list), 3), ceiling(length(attr_mth_list) / 3))
    output_path <- file.path(output_folder_path, "mortality_af_heat_month_plot.pdf")
    pdf(output_path, width = max(10, grid[1]*4.5), height = max(8, grid[2]*4.5))

    par(mfrow=c(grid[2], grid[1]), mar = c(5, 5, 5, 5), oma = c(4, 0, 4, 0))

  }

  ylim_max <- max(sapply(attr_mth_list, function(x) max(x$af_heat, na.rm = TRUE)))

  ylim2_min <- min(sapply(attr_mth_list, function(x) min(x$temp, na.rm = TRUE)))
  ylim2_max <- max(sapply(attr_mth_list, function(x) max(x$temp, na.rm = TRUE)))

  scale_factor <- (1 / ylim2_max) * ylim_max

  temp_ticks <- pretty(c(min(0, ylim2_min), ylim2_max))

  ylim <- c(min(0, temp_ticks[1] * scale_factor), max(temp_ticks[length(temp_ticks)] * scale_factor, ylim_max))

  for (geog in names(attr_mth_list)){

    geog_af <- attr_mth_list[[geog]]
    geog_temp <- df_list[[geog]]$temp

    temp_scaled <- geog_af$temp * scale_factor

    bar_pos <- barplot(names.arg = substr(geog_af$month, 1, 1),
                       height = geog_af$af_heat,
                       ylim = ylim,
                       xlab = "Month",
                       ylab = "High temperature AF (%)",
                       main = geog,
                       col = "#C75E70")

    lines(x = bar_pos,
          y = temp_scaled,
          type = "o",
          col = "#a04b58",
          pch = 16)

    # Add secondary axis on the right

    axis(side = 4,
         at = temp_ticks * scale_factor,
         labels = temp_ticks,
         col.axis = "black",
         col = "black")

    mtext("Mean Temp (\u00b0C)", side = 4, line = 3, col = "black", cex = 0.7)

    abline(h = 0,
           col = "black",
           lty = 1)

    attr_thr_high_tmp <- round(quantile(geog_temp, attr_thr_high/100, na.rm = TRUE), 2)
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
           xpd = TRUE)

  }

  if (save_fig == TRUE) {

    year_range <- paste0("(",
                         min(sapply(df_list, function(x) min(lubridate::year(x$date), na.rm = TRUE))),
                         "-",
                         max(sapply(df_list, function(x) max(lubridate::year(x$date), na.rm = TRUE))),
                         ")")

    title <- paste0("Attributable fraction of high temperature mortality by calendar month and geography, ", country, " ",  year_range)

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
#' @param attr_thr Integer. Percentile at which to define the temperature threshold for
#' calculating attributable risk.
#' @param save_fig Boolean. Whether to save the plot as an output. Defaults to
#' FALSE.
#' @param output_folder_path Path to folder where plots should be saved.
#' Defaults to NULL.
#'
#' @return Plots of attributable fractions by calendar month per area
#'
#' @export
hc_plot_af_cold_monthly <- function(attr_mth_list,
                                    df_list,
                                    country = "National",
                                    attr_thr_low = 2.5,
                                    save_fig = FALSE,
                                    output_folder_path = NULL){

  if (save_fig == TRUE){

    grid <- c(min(length(attr_mth_list), 3), ceiling(length(attr_mth_list) / 3))
    output_path <- file.path(output_folder_path, "mortality_af_cold_month_plot.pdf")
    pdf(output_path, width = max(10, grid[1]*4.5), height = max(8, grid[2]*4.5))

    par(mfrow=c(grid[2], grid[1]), mar = c(5, 5, 5, 5), oma = c(4, 0, 4, 0))

  }

  ylim_max <- max(sapply(attr_mth_list, function(x) max(x$af_cold, na.rm = TRUE)))

  ylim2_min <- min(sapply(attr_mth_list, function(x) min(x$temp, na.rm = TRUE)))
  ylim2_max <- max(sapply(attr_mth_list, function(x) max(x$temp, na.rm = TRUE)))

  scale_factor <- (1 / ylim2_max) * ylim_max

  temp_ticks <- pretty(c(min(0, ylim2_min), ylim2_max))

  ylim <- c(min(0, temp_ticks[1] * scale_factor), max(temp_ticks[length(temp_ticks)] * scale_factor, ylim_max))

  for (geog in names(attr_mth_list)){

    geog_af <- attr_mth_list[[geog]]
    geog_temp <- df_list[[geog]]$temp

    temp_scaled <- geog_af$temp * scale_factor

    bar_pos <- barplot(names.arg = substr(geog_af$month, 1, 1),
                       height = geog_af$af_cold,
                       ylim = ylim,
                       xlab = "Month",
                       ylab = "Low temperature AF (%)",
                       main = geog,
                       col = "#296991")

    lines(x = bar_pos,
          y = temp_scaled,
          type = "o",
          col = "#0A2E4D",
          pch = 16)

    # Add secondary axis on the right

    axis(side = 4,
         at = temp_ticks * scale_factor,
         labels = temp_ticks,
         col.axis = "black",
         col = "black")

    mtext("Mean Temp (\u00b0C)", side = 4, line = 3, col = "black", cex = 0.7)

    abline(h = 0,
           col = "black",
           lty = 1)

    attr_thr_low_tmp <- round(quantile(geog_temp, attr_thr_low/100, na.rm = TRUE), 2)
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
           xpd = TRUE)

  }

  if (save_fig == TRUE) {

    year_range <- paste0("(",
                         min(sapply(df_list, function(x) min(lubridate::year(x$date), na.rm = TRUE))),
                         "-",
                         max(sapply(df_list, function(x) max(lubridate::year(x$date), na.rm = TRUE))),
                         ")")

    title <- paste0("Attributable fraction of low temperature mortality by calendar month and geography, ", country, " ",  year_range)

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
#' @param attr_thr Integer. Percentile at which to define the temperature threshold for
#' calculating attributable risk.
#' @param save_fig Boolean. Whether to save the plot as an output. Defaults to
#' FALSE.
#' @param output_folder_path Path to folder where plots should be saved.
#' Defaults to NULL.
#'
#' @return Plots of attributable rates by calendar month per area
#'
#' @export
hc_plot_ar_heat_monthly <- function(attr_mth_list,
                               df_list,
                               country = "National",
                               attr_thr_high = 97.5,
                               save_fig = FALSE,
                               output_folder_path = NULL){

  if (save_fig == TRUE){

    grid <- c(min(length(attr_mth_list), 3), ceiling(length(attr_mth_list) / 3))
    output_path <- file.path(output_folder_path, "mortality_ar_heat_month_plot.pdf")
    pdf(output_path, width = max(10, grid[1]*4.5), height = max(8, grid[2]*4.5))

    par(mfrow=c(grid[2], grid[1]), mar = c(5, 5, 5, 5), oma = c(4, 0, 4, 0))

  }

  ylim_max <- max(sapply(attr_mth_list, function(x) max(x$ar_heat, na.rm = TRUE)))

  ylim2_min <- min(sapply(attr_mth_list, function(x) min(x$temp, na.rm = TRUE)))
  ylim2_max <- max(sapply(attr_mth_list, function(x) max(x$temp, na.rm = TRUE)))

  scale_factor <- (1 / ylim2_max) * ylim_max

  temp_ticks <- pretty(c(min(0, ylim2_min), ylim2_max))

  ylim <- c(min(0, temp_ticks[1] * scale_factor), max(temp_ticks[length(temp_ticks)] * scale_factor, ylim_max))

  for (geog in names(attr_mth_list)){

    geog_ar <- attr_mth_list[[geog]]
    geog_temp <- df_list[[geog]]$temp

    temp_scaled <- geog_ar$temp * scale_factor

    bar_pos <- barplot(names.arg = substr(geog_ar$month, 1, 1),
                       height = geog_ar$ar_heat,
                       ylim = ylim,
                       xlab = "Month",
                       ylab = "High temperature AR (per 100,000 population)",
                       main = geog,
                       col = "#c75e70")

    lines(x = bar_pos,
          y = temp_scaled,
          type = "o",
          col = "#a04b58",
          pch = 16)

    axis(side = 4,
         at = temp_ticks * scale_factor,
         labels = temp_ticks,
         col.axis = "black",
         col = "black")

    mtext("Mean Temp (\u00b0C)", side = 4, line = 3, col = "black", cex = 0.7)

    abline(h = 0,
           col = "black",
           lty = 1)

    attr_thr_high_tmp <- round(quantile(geog_temp, attr_thr_high/100, na.rm = TRUE), 2)
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
           xpd = TRUE)

  }

  if (save_fig == TRUE) {

    year_range <- paste0("(",
                         min(sapply(df_list, function(x) min(lubridate::year(x$date), na.rm = TRUE))),
                         "-",
                         max(sapply(df_list, function(x) max(lubridate::year(x$date), na.rm = TRUE))),
                         ")")

    title <- paste0("Attributable rate of high temperature mortality by calendar month and geography, ", country, " ",  year_range)

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
#' @param attr_thr Integer. Percentile at which to define the temperature threshold for
#' calculating attributable risk.
#' @param save_fig Boolean. Whether to save the plot as an output. Defaults to
#' FALSE.
#' @param output_folder_path Path to folder where plots should be saved.
#' Defaults to NULL.
#'
#' @return Plots of attributable rates by calendar month per area
#'
#' @export
hc_plot_ar_cold_monthly <- function(attr_mth_list,
                                    df_list,
                                    country = "National",
                                    attr_thr_low = 2.5,
                                    save_fig = FALSE,
                                    output_folder_path = NULL){

  if (save_fig == TRUE){

    grid <- c(min(length(attr_mth_list), 3), ceiling(length(attr_mth_list) / 3))
    output_path <- file.path(output_folder_path, "mortality_ar_cold_month_plot.pdf")
    pdf(output_path, width = max(10, grid[1]*4.5), height = max(8, grid[2]*4.5))

    par(mfrow=c(grid[2], grid[1]), mar = c(5, 5, 5, 5), oma = c(4, 0, 4, 0))

  }

  ylim_max <- max(sapply(attr_mth_list, function(x) max(x$ar_cold, na.rm = TRUE)))

  ylim2_min <- min(sapply(attr_mth_list, function(x) min(x$temp, na.rm = TRUE)))
  ylim2_max <- max(sapply(attr_mth_list, function(x) max(x$temp, na.rm = TRUE)))

  scale_factor <- (1 / ylim2_max) * ylim_max

  temp_ticks <- pretty(c(min(0, ylim2_min), ylim2_max))

  ylim <- c(min(0, temp_ticks[1] * scale_factor), max(temp_ticks[length(temp_ticks)] * scale_factor, ylim_max))

  for (geog in names(attr_mth_list)){

    geog_ar <- attr_mth_list[[geog]]
    geog_temp <- df_list[[geog]]$temp

    temp_scaled <- geog_ar$temp * scale_factor

    bar_pos <- barplot(names.arg = substr(geog_ar$month, 1, 1),
                       height = geog_ar$ar_cold,
                       ylim = ylim,
                       xlab = "Month",
                       ylab = "Low temperature AR (per 100,000 population)",
                       main = geog,
                       col = "#296991")

    lines(x = bar_pos,
          y = temp_scaled,
          type = "o",
          col = "#0A2E4D",
          pch = 16)

    axis(side = 4,
         at = temp_ticks * scale_factor,
         labels = temp_ticks,
         col.axis = "black",
         col = "black")

    mtext("Mean Temp (\u00b0C)", side = 4, line = 3, col = "black", cex = 0.7)

    abline(h = 0,
           col = "black",
           lty = 1)

    attr_thr_low_tmp <- round(quantile(geog_temp, attr_thr_low/100, na.rm = TRUE), 2)
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
           xpd = TRUE)

  }

  if (output_config$save_fig == TRUE) {

    year_range <- paste0("(",
                         min(sapply(df_list, function(x) min(lubridate::year(x$date), na.rm = TRUE))),
                         "-",
                         max(sapply(df_list, function(x) max(lubridate::year(x$date), na.rm = TRUE))),
                         ")")

    title <- paste0("Attributable rate of low temperature mortality by calendar month and geography, ", country, " ",  year_range)

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
#' @export
hc_save_results <- function(rr_results,
                            res_attr_tot,
                            attr_yr_list,
                            attr_mth_list,
                            output_folder_path = NULL) {

  if (!is.null(output_folder_path)) {

    check_file_exists(file.path(output_folder_path))

    write.csv(rr_results, file = file.path(
      output_folder_path, "mortality_rr_results.csv"), row.names = FALSE)

    write.csv(res_attr_tot, file = file.path(
      output_folder_path, "mortality_attr_tot_results.csv"), row.names = FALSE)

    res_attr_yr <- do.call(rbind, attr_yr_list) %>%
      select(.data$geog, everything())

    write.csv(res_attr_yr, file = file.path(
      output_folder_path, "mortality_attr_yr_results.csv"), row.names = FALSE)

    res_attr_mth <- do.call(rbind, attr_mth_list) %>%
      select(.data$geog, everything())

    write.csv(res_attr_mth, file = file.path(
      output_folder_path, "mortality_attr_mth_results.csv"), row.names = FALSE)

    #TODO also in wildfire functions, generalise and put in files.utils

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
#' @param region_col Character. Name of the column in the dataframe that contains
#' the region names. Defaults to NULL.
#' @param temperature_col Character. Name of the column in the dataframe that
#' contains the temperature column.
#' @param health_outcome_col Character. Name of the column in the dataframe that
#' contains the health outcome count column (e.g. number of deaths, hospital
#' admissions).
#' @param population_col Character. Name of the column in the dataframe that
#' contains the population estimate coloumn.
#' @param independent_cols Additional independent variables to test in model validation
#' @param control_cols A list of confounders to include in the final model adjustment.
#' Defaults to NULL if none.
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
#' @param save_fig Boolean. Whether to save the plot as an output. Defaults to
#' FALSE.
#' @param save_csv Boolean. Whether to save the results as a CSV. Defaults to
#' FALSE.
#' @param cenper Integer. Value for the percentile in calculating the centering
#' value 0-100. Defaults to 50.
#' @param country Character. Name of country for national level estimates.
#' @param meta_analysis Boolean. Whether to perform a meta-analysis.
#' @param attr_thr Integer. Percentile at which to define the temperature threshold for
#' calculating attributable risk.
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
temp_mortality_do_analysis <- function(input_csv_path,
                                      date_col,
                                      geography_col,
                                      temperature_col,
                                      dependent_col,
                                      population_col,
                                      independent_cols = NULL,
                                      control_cols = NULL,
                                      var_fun = "bs",
                                      var_degree = 2,
                                      var_per = c(10,75,90),
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

  df_list <- hc_read_data(data_path = data_path,
                                     date_col = date_col,
                                     geography_col = geograpphy_col,
                                     temperature_col = temperature_col,
                                     dependent_col = dependent_col,
                                     population_col = population_col)

  pop_list <- mh_pop_totals(df_list = df_list, #EW: same as MH but pop vs population name difference
                            country = country,
                            meta_analysis = meta_analysis)

  cb_list <- hc_create_crossbasis(df_list = df_list, #EW: different variables to MH
                                  var_fun = var_fun,
                                  var_degree = var_degree,
                                  var_per = var_per,
                                  lagn = lagn,
                                  lagnk = lagnk,
                                  dfseas = dfseas)

  c(qaic_results, qaic_summary, #EW: functions called within this differs slightly from MH
    vif_results, vif_summary) %<-% hc_model_validation(df_list = df_list,
                                                       cb_list = cb_list,
                                                       independent_cols = independent_cols,
                                                       save_fig = save_fig,
                                                       save_csv = save_csv,
                                                       output_folder_path = output_folder_path)

  model_list <- hc_quasipoisson_dlnm(df_list = df_list,
                                     control_cols = control_cols,
                                     cb_list = cb_list,
                                     dfseas = dfseas) #EW: may not need dfseas - depends on how formula defined - need QA

  c(coef_, vcov_) %<-% hc_reduce_cumulative(df_list = df_list,
                                            var_per = var_per,
                                            var_degree = var_degree,
                                            #cenper = cenper, #EW: only difference between MH and HC?
                                            cb_list = cb_list,
                                            model_list = model_list)

  if (meta_analysis == TRUE){

    c(mm, blup, meta_test_res) %<-% hc_meta_analysis(df_list = df_list, #EW: think can use MH function, HC has some validation in
                                                     coef_ = coef_,
                                                     vcov_ = vcov_,
                                                     save_csv = save_csv,
                                                     output_folder_path = output_folder_path)

  } else blup <- NULL

  c(minpercgeog_, mintempgeog_) <- hc_min_mortality_temp(df_list = df_list,
                                                        var_fun = var_fun,
                                                        var_per = var_per,
                                                        var_degree = var_degree,
                                                        blup = blup,
                                                        coef_ = coef_,
                                                        meta_analysis = meta_analysis)

  pred_list <- hc_predict(df_list = df_list, #EW: think can use MH function
                              var_fun = var_fun,
                              var_per = var_per,
                              var_degree = var_degree,
                              minpercgeog_ = minpercgeog_, #EW: name difference
                              blup = blup,
                              coef_ = coef_,
                              vcov_ = vcov_,
                              meta_analysis = meta_analysis)

  if (meta_analysis == TRUE){

    c(df_list, cb_list, minpercgeog_, mmpredall) %<-% hc_add_national_data(df_list = df_list, #EW: different model to MH
                                                                         pop_list = pop_list,
                                                                         var_fun = var_fun,
                                                                         var_per = var_per,
                                                                         var_degree = var_degree,
                                                                         lagn = lagn,
                                                                         lagnk = lagnk,
                                                                         country = country,
                                                                         cb_list = cb_list,
                                                                         mm = mm,
                                                                         minpercgeog_ = minpercgeog_)

    pred_list <- mh_predict_nat(df_list = df_list, # EW: think can use MH function
                                var_fun = var_fun,
                                var_per = var_per,
                                var_degree = var_degree,
                                minpercgeog_ = minpercgeog_, #EW: name difference
                                mmpredall = mmpredall,
                                pred_list = pred_list,
                                country = country)

  }

  hc_plot_rr(df_list = df_list,
             pred_list = pred_list,
             attr_thr_high = attr_thr_high, #EW: additional threshold to MH
             attr_thr_low = attr_thr_low,
             minpercgeog_ = minpercgeog_, #Ew: name difference
             country = country,
             save_fig = save_fig,
             output_folder_path = output_folder_path)

  rr_results <- hc_rr_results(pred_list) # EW: has name differences inside function

  attr_list <- hc_attr(df_list = df_list,
                       cb_list = cb_list,
                       pred_list = pred_list,
                       minpercgeog_ = minpercgeog_, #Ew: name difference
                       attr_thr_high = attr_thr_high, #EW: additional threshold to MH
                       attr_thr_low = attr_thr_low)

  c(res_attr_tot, attr_yr_list, attr_mth_list) %<-% hc_attr_tables(attr_list = attr_list, #Ew: different to MH - extra threshold
                                                                   country = country,
                                                                   meta_analysis = meta_analysis)

#EW: plots all diff to MH due to extra threshold
  hc_plot_attr_totals(df_list = df_list,
                      res_attr_tot = res_attr_tot,
                      save_fig = save_fig,
                      output_folder_path = output_folder_path,
                      country = country)

  hc_plot_af_yearly(attr_yr_list = attr_yr_list,
                    save_fig = save_fig,
                    output_folder_path = output_folder_path,
                    country = country)

  hc_plot_ar_yearly(attr_yr_list = attr_yr_list,
                    save_fig = save_fig,
                    output_folder_path = output_folder_path,
                    country = country)

  hc_plot_af_monthly(attr_mth_list = attr_mth_list,
                     df_list = df_list,
                     country = country,
                     attr_thr_high = attr_thr_high,
                     attr_thr_low = attr_thr_low,
                     save_fig = save_fig,
                     output_folder_path = output_folder_path)

  hc_plot_ar_monthly(attr_mth_list = attr_mth_list,
                     df_list = df_list,
                     country = country,
                     attr_thr_high = attr_thr_high,
                     attr_thr_low = attr_thr_low,
                     save_fig = save_fig,
                     output_folder_path = output_folder_path)

  if (save_csv == TRUE) {

    hc_save_results(rr_results = rr_results, #EW: think can use MH but diff file names in function
                    res_attr_tot = res_attr_tot,
                    attr_yr_list = attr_yr_list,
                    attr_mth_list = attr_mth_list,
                    output_folder_path = output_folder_path)

  }

  return(list(rr_results, res_attr_tot, attr_yr_list, attr_mth_list))

}
