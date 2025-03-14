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
#'
#' @returns A list of dataframes with formatted and renamed columns.

mh_read_and_format_data <- function(data_path,
                                 date_col,
                                 region_col = NULL,
                                 temperature_col,
                                 health_outcome_col) {

  # make sure data_path is a csv if a path is passed
  if(is.character(data_path)) {
    check_file_extension(data_path, ".csv", "data_path")
  }
  # read data
  df <- read_input_data(data_path)

  if(is.null(region_col)) {

    df <- df %>%
      dplyr::mutate(region = "aggregated")
      region_col = "region"
  }

  df <- df %>%
    dplyr::rename(date = date_col,
                  temp = temperature_col,
                  suicides = health_outcome_col) %>%
    dplyr::mutate(date = lubridate::ymd(date),
                  year = as.factor(lubridate::year(date)),
                  month = as.factor(lubridate::month(date)),
                  dow = as.factor(lubridate::wday(date, label = TRUE)),
                  region = as.factor(region),
                  stratum = as.factor(region:year:month:dow),
                  ind = tapply(suicides, stratum, sum)[stratum])

  df_list <- aggregate_by_column(df, "region")

  return(df_list)
}


#' Create cross-basis matrix
#'
#' @description Creates a cross-basis matrix for each region
#'
#' @param data A list of dataframes containing daily timeseries data for a health outcome
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

mh_create_crossbasis <- function(data,
                              var_fun = "bs",
                              var_degree = 2,
                              var_per = c(25,50,75),
                              lag_fun = "strata",
                              lag_breaks = 1,
                              lag_days = 2) {

  cb_list <- list()

  for(reg in names(data)){

    region_data <- data[[reg]]
    argvar <- list(fun = var_fun,
                   knots = quantile(region_data$temp, var_per/100, na.rm = T),
                   degree = var_degree)
    arglag <- list(fun = lag_fun, breaks = lag_breaks)
    cb <- dlnm::crossbasis(region_data$temp, lag = lag_days, argvar = argvar, arglag = arglag)

    cb_list[[reg]] <- cb
  }

  return(cb_list)
}


#' Quasi-Poisson Case-Crossover model with DLNM
#'
#' @description Fits a quasi-Poisson case-crossover with a distributed lag
#' non-linear model
#'
#' @param data A list of dataframes containing daily timeseries data for a health outcome
#' and climate variables which may be disaggregated by a particular region.
#' @param cb_list List of cross_basis matrices from create_crossbasis function.
#'
#' @returns List containing models by region

mh_casecrossover_dlnm <- function(data,
                               cb_list) {

  model_list <- list()

  for(reg in names(data)){

    region_data <- data[[reg]]
    cb <- cb_list[[reg]]
    model <- gnm::gnm(suicides ~ cb, eliminate = stratum, family = quasipoisson(), data = region_data,
                 na.action = "na.exclude", subset = ind > 0)
    model_list[[reg]] <- model

  }

  return(model_list)

}


#' Reduce to overall cumulative
#'
#' @description Reduce model to the overall cumulative association
#'
#' @param data A list of dataframes containing daily timeseries data for a health outcome
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


mh_reduce_cumulative <- function(data,
                                 var_per = c(25,50,75),
                                 var_degree = 2,
                                 cenper = 50,
                                 cb_list,
                                 model_list) {

  coef_ <- matrix(data = NA,
                  nrow = length(names(data)),
                  ncol = length(var_per) + var_degree,
                  dimnames = list(names(data)))
  vcov_ <- vector("list", length(names(data)))
  names(vcov_) <- names(data)

  for(reg in names(data)){

    region_data <- data[[reg]]
    cb <- cb_list[[reg]]


    red <- dlnm::crossreduce(cb, model_list[[reg]], cen = quantile(region_data$temp, cenper/100, na.rm = T))

    coef_[reg,] <- coef(red)
    vcov_[[reg]] <- vcov(red)

  }

  return(list(coef_, vcov_))

}


#' Meta-analysis and BLUPs
#'
#' @description Run meta-analysis using temperature average and range as meta
#' predictors. Then create the best linear unbiased predictions (BLUPs).
#'
#' @param data A list of dataframes containing daily timeseries data for a health outcome
#' and climate variables which may be disaggregated by a particular region.
#' @param coef_ A matrix of coefficients for the reduced model.
#' @param vcov_ A list. Covariance matrices for each region for the reduced model.
#'
#' @return
#' \itemize{
#'   \item `mm` A model object. A multivariate meta-analysis model.
#'   \item `blup` A list. BLUP (best linear unbiased predictions) from the
#'   meta-analysis model for each region.
#'   }

mh_meta_analysis <- function(data,
                             coef_,
                             vcov_){

  # Create temperature average and range as meta predictors

  temp_avg <- sapply(df_list, function(x) mean(x$temp, na.rm = TRUE))
  temp_range <- sapply(df_list, function(x) diff(range(x$temp, na.rm = TRUE)))

  # Meta-analysis

  mm <- mixmeta::mixmeta(formula = coef_ ~ temp_avg + temp_range,
                         S = vcov_,
                         data = as.data.frame(names(df_list)),
                         method = "reml")
  #TODO potentially add random effects random = ~ 1 | region (I think already covered by reml)

  # BLUP

  blup <- mixmeta::blup(object = mm,
                        vcov = TRUE)

  names(blup) <- names(df_list)

  return(list(mm, blup))

}


#' Redefine function ahead of predictions
#'
#' @description Redefine the function including boundary knots ahead of running
#' predictions from the model
#'
#' @param data A list of dataframes containing daily timeseries data for a health outcome
#' and climate variables which may be disaggregated by a particular region.
#' @param var_fun Character. Exposure function for argvar
#' (see dlnm::crossbasis). Defaults to 'bs'.
#' @param var_per Vector. Internal knot positions for argvar
#' (see dlnm::crossbasis). Defaults to c(25,50,75).
#' @param var_degree Integer. Degree of the piecewise polynomial for argvar
#' (see dlnm::crossbasis). Defaults to 2 (quadratic).
#'
#' @return List containing onebasis of exposure variable for each region.

mh_redefine_function_reg <- function(data,
                                     var_fun = "bs",
                                     var_per = c(25,50,75),
                                     var_degree = 2){

  bvar_list <- list()

  for(reg in names(data)){

    region_data <- data[[reg]]
    predvar <- quantile(region_data$temp, 1:99/100, na.rm = TRUE)

    # Redefine the function using all arguments (boundary knots included)

    argvar <- list(x = predvar,
                   fun = var_fun,
                   knots = quantile(region_data$temp, var_per/100, na.rm = TRUE),
                   degree = var_degree,
                   Boundary.knots = range(region_data$temp, na.rm = TRUE))

    bvar <- do.call(dlnm::onebasis, argvar)

    bvar_list[[reg]] <- bvar

  }

  return(bvar_list)

}


#' Define min and max suicide values
#'
#' @description Define the minimum (between 1st and 50th percentiles) and
#' maximum (between 51st and 99th percentiles) suicide temperature values
#'
#' @param data A list of dataframes containing daily timeseries data for a health outcome
#' and climate variables which may be disaggregated by a particular region.
#' @param bvar_list List containing onebasis of exposure variable for each region.
#' @param blup A list. BLUP (best linear unbiased predictions) from the
#' meta-analysis model for each region.
#'
#' @return
#' \itemize{
#'   \item `minpercreg` Vector. Percentile of minimum suicide temperature for each region.
#'   \item `maxpercreg` Vector. Percentile of maximum suicide temperature for each region.
#'   \item `minpercnat` Integer. Percentile of minimum suicide temperature for country.
#'   \item `maxpercnat` Integer. Percentile of maximum suicide temperature for country.
#'   }

mh_minmax_suicide_temp <- function(data,
                                   bvar_list,
                                   blup) {

  # Generate matrix for storing results

  minpercreg <- mintempreg <- maxpercreg <- maxtempreg <- rep(NA, length(data))
  names(minpercreg) <- names(mintempreg) <- names(maxpercreg) <- names(maxtempreg) <- names(data)

  # Define min and max suicide values: exclude low and very hot temperature

  for(reg in names(data)){

    region_data <- data[[reg]]

    minpercreg[reg] <- (1:50)[which.min((bvar_list[[reg]]%*%blup[[reg]]$blup)[1:50,])]
    maxpercreg[reg] <- (51:99)[which.max((bvar_list[[reg]]%*%blup[[reg]]$blup)[51:99,])]

  }

  # National level points of min and max suicide temperature

  minpercnat <- median(minpercreg)
  maxpercnat <- median(maxpercreg)

  return(list(minpercreg, maxpercreg, minpercnat, maxpercnat))

}


#' Run predictions from model
#'
#' @description Use model to run predictions
#'
#' @param data A list of dataframes containing daily timeseries data for a health outcome
#' and climate variables which may be disaggregated by a particular region.
#' @param bvar_list List containing onebasis of exposure variable for each region.
#' @param minpercreg Vector. Percentile of minimum suicide temperature for each region.
#' @param blup A list. BLUP (best linear unbiased predictions) from the
#' meta-analysis model for each region.
#'
#' @returns A list containing predictions by region

mh_predict <- function(data,
                       bvar_list,
                       minpercreg,
                       blup) {

  pred_list <- list()

  for(reg in names(data)){

    region_data <- data[[reg]]
    cen <- quantile(region_data$temp, minpercreg[reg]/100, na.rm = TRUE)

    pred <- dlnm::crosspred(bvar_list[[reg]],
                            coef = blup[[reg]]$blup,
                            vcov = blup[[reg]]$vcov,
                            model.link = "log",
                            by = 0.1,
                            cen = cen,
                            from = min(region_data$temp, na.rm = TRUE),
                            to = max(region_data$temp, na.rm = TRUE))

    pred_list[[reg]] <- pred

  }

  return(pred_list)

}


#' Plot results of analysis - Mental Health
#'
#' @description Plots cumulative lag exposure-response function for each region
#'
#' @param pred_list A list containing predictions from the model by region.
#' @param save_fig Boolean. Whether to save the plot as an output. Defaults to
#' FALSE.
#' @param output_folder_path Path to folder where plots should be saved.
#' Defaults to NULL.
#'
#' @returns Plots of cumulative lag exposure-response function for each region

mh_plot_results <- function(pred_list,
                            save_fig = FALSE,
                            output_folder_path = NULL) {

  xlim <- c(min(sapply(pred_list, function(x) min(x$predvar, na.rm = TRUE))),
            max(sapply(pred_list, function(x) max(x$predvar, na.rm = TRUE))))

  if (save_fig==T) {
    # create grid dynamically
    grid <- create_grid(length(pred_list))
    output_path <- file.path(output_folder_path,
                             "suicides_plot.pdf")
    pdf(output_path, width=grid[1]*4, height=grid[2]*4)
    par(mfrow=c(grid[1],  grid[2]))
  }

  for(reg in names(pred_list)){

    region_pred <- pred_list[[reg]]

    plot(region_pred,
         "overall",
         xlab = expression(paste("Temperature (", degree, "C)")),
         ylab = "RR",
         ylim = c(0,3),
         xlim = xlim,
         main = reg,
         col = "#f25574")

  }

  if (save_fig == TRUE) {

    dev.off()

    }

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

produce_results <- function(pred_list) {

    results <- data.frame()

    for (reg in names(pred_list)) {

      region_pred <- pred_list[[reg]]

      regname <- tolower(gsub(' ', '_', reg))
      rrfit <- paste0("RRfit_", regname)
      rrlow <- paste0("RRlow_", regname)
      rrhigh <- paste0("RRhigh_", regname)

      results_add <- data.frame(
        Temperature = region_pred$predvar,
        setNames(list(region_pred$allRRfit), rrfit),
        setNames(list(region_pred$allRRlow), rrlow),
        setNames(list(region_pred$allRRhigh), rrhigh))

      if (nrow(results) == 0) {
        results <- results_add
      } else {
          results <- merge(results, results_add, by = "Temperature", all = TRUE)
      }
    }

   return(results)

}


#' Save results of analysis - Mental Health
#'
#' @description Saves a CSV file of cumulative relative risk and
#' confidence intervals.
#'
#' @param results Dataframe containing cumulative relative risk and confidence
#' intervals from analysis.
#' @param output_folder_path Path to folder where results should be saved.
#' Defaults to NULL.

mh_save_results <- function(results,
                         output_folder_path = NULL) {

  if (!is.null(output_folder_path)) {

    climatehealth::check_file_exists(file.path(output_folder_path))

    write.csv(results, file = file.path(
      output_folder_path, "suicides_results.csv"), row.names = FALSE)

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
#' @param cenper Integer. Value for the percentile in calculating the centering
#' value 0-100. Defaults to 50.
#' @param save_fig Boolean. Whether to save the plot as an output. Defaults to
#' FALSE.
#' @param save_csv Boolean. Whether to save the results as a CSV. Defaults to
#' FALSE.
#' @param descriptive_stats Boolean. Whether to calculate descriptive stats.
#' @param ds_correlation_method character. The correlation method used in correlation matrices.
#' Defaults to 'pearson'.
#' @param ds_dist_columns character vector. The names of columns to plot distributions for.
#' Defaults to c().
#' @param ds_ma_days integer. How many days to use for moving average calculations.
#' Defaults to 100.
#' @param ds_ma_sides integer. How many sides to use for moving average calculations (1 or 2).
#' Defaults to 2.
#' @param ds_ma_columns character vector. The names of columns to plot moving average for.
#' @param output_folder_path Path to folder where plots and/or CSV should be
#' saved. Defaults to NULL.
#'
#' @returns Dataframe containing cumulative relative risk and confidence
#' intervals from analysis.
#'
#' @export
suicides_heat_do_analysis <- function(data_path,
                                      date_col,
                                      region_col = NULL,
                                      temperature_col,
                                      health_outcome_col,
                                      var_fun = "bs",
                                      var_degree = 2,
                                      var_per = c(25,50,75),
                                      lag_fun = "strata",
                                      lag_breaks = 1,
                                      lag_days = 2,
                                      cenper = 50,
                                      save_fig = FALSE,
                                      save_csv = FALSE,
                                      descriptive_stats = FALSE,
                                      ds_correlation_method = "pearson",
                                      ds_dist_columns = c(),
                                      ds_ma_days = 100,
                                      ds_ma_sides = 2,
                                      ds_ma_columns = c(),
                                      output_folder_path = NULL) {

  df_list <- mh_read_and_format_data(data_path = data_path,
                             date_col = date_col,
                             region_col = region_col,
                             temperature_col = temperature_col,
                             health_outcome_col = health_outcome_col)

  if(descriptive_stats) {
    common_descriptive_stats(
      dataset_title = "mental health",
      df_list = df_list,
      use_individual_dfs = stats_config$use_individual_dfs,
      output_path = path_config$output_folder_path,
      correlation_method = ds_correlation_method,
      dist_columns =  ds_dist_columns,
      ma_days = ds_ma_days,
      ma_sides = ds_ma_sides,
      ma_columns = ds_ma_column,
      dependent_col = "suicides", # col is renamed in data
      independent_cols = c()
    )
  }

  cb_list <- mh_create_crossbasis(data = df_list,
                               var_fun = var_fun,
                               var_degree = var_degree,
                               var_per = var_per,
                               lag_fun = lag_fun,
                               lag_breaks = lag_breaks,
                               lag_days = lag_days)

  model_list <- mh_casecrossover_dlnm(data = df_list,
                                   cb_list = cb_list)

  c(coef_, vcov_) %<-% mh_reduce_cumulative(data = df_list,
                                            var_per = var_per,
                                            var_degree = var_degree,
                                            cenper = cenper, #TODO Add to config file
                                            cb_list = cb_list,
                                            model_list = model_list)

  c(mm, blup) %<-% mh_meta_analysis(data = df_list,
                                    coef_ = coef_,
                                    vcov_ = vcov_)

  bvar_list <- mh_redefine_function(data = df_list,
                                    var_fun = var_fun,
                                    var_per = var_per,
                                    var_degree = var_degree)

  c(minpercreg, maxpercreg, minpercnat, maxpercnat) %<-% mh_minmax_suicide_temp(data = df_list,
                                                                                bvar_list = bvar_list,
                                                                                blup = blup)

  pred_list <- mh_predict(data = df_list,
                          bvar_list = bvar_list,
                          mintempreg = mintempreg,
                          blup = blup)


  mh_plot_results(pred_list = pred_list,
               save_fig = save_fig,
               output_folder_path = output_folder_path)

  results <- produce_results(pred_list)

  if (save_csv == TRUE) {

    mh_save_results(results = results,
               output_folder_path = output_folder_path)

  }

  return(results)

}
