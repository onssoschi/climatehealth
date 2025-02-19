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
#' (see dlnm::crossbasis). Defaults to 'ns'.
#' @param var_dof Integer. Degrees of freedom in exposure function for argvar
#' (see dlnm:crossbasis). Defaults to 4.
#' @param lag_fun Character. Exposure function for arglag
#' (see dlnm::crossbasis). Defaults to 'ns'.
#' @param lag_dof Integer. Degrees of freedom in exposure function for arglag
#' (see dlnm:crossbasis). Defaults to 4.
#' @param lag_days Integer. Maximum lag. Defaults to 3.
#' (see dlnm:crossbasis).
#'
#' @returns A list of cross-basis matrices by region

mh_create_crossbasis <- function(data,
                              var_fun = "ns",
                              var_dof = 4,
                              lag_fun = "ns",
                              lag_dof = 4,
                              lag_days = 3) {

  cb_list <- list()

  for(reg in names(data)){

    region_data <- data[[reg]]
    argvar <- list(fun = var_fun, df = var_dof)
    arglag <- list(fun = lag_fun, df = lag_dof)
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


#' Run predictions from model
#'
#' @description Use model to run predictions
#'
#' @param data A list of dataframes containing daily timeseries data for a health outcome
#' and climate variables which may be disaggregated by a particular region.
#' @param cb_list List of cross_basis matrices from create_crossbasis function.
#' @param model_list List of models produced from case-crossover and DLNM
#' analysis.
#'
#' @returns A list containing predictions by region

mh_predict <- function(data,
                       cb_list,
                       model_list) {

  pred_list <- list()

  for(reg in names(data)){

    region_data <- data[[reg]]
    cb <- cb_list[[reg]]
    cen <- mean(region_data$temp, na.rm = T)
    pred <- dlnm::crosspred(cb, model_list[[reg]], cen = cen,
                            from = min(region_data$temp, na.rm = TRUE),
                            to = max(region_data$temp, na.rm = TRUE), by = 1)
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
#' @param var_fun Character. Exposure function (see dlnm::crossbasis). Defaults
#' to 'ns'.
#' @param var_dof Integer. Degrees of freedom in exposure function
#' (see dlnm:crossbasis). Defaults to 4.
#' @param lag_fun Character. Exposure function (see dlnm::crossbasis). Defaults
#' to 'ns'.
#' @param lag_dof Integer. Degrees of freedom in exposure function
#' (see dlnm:crossbasis). Defaults to 4.
#' @param lag_days Integer. Maximum lag (see dlnm:crossbasis). Defaults to 3.
#' @param save_fig Boolean. Whether to save the plot as an output. Defaults to
#' FALSE.
#' @param save_csv Boolean. Whether to save the results as a CSV. Defaults to
#' FALSE.
#' @param output_folder_path Path to folder where plots and/or CSV should be
#' saved. Defaults to NULL.
#'
#' @returns Dataframe containing cumulative relative risk and confidence
#' intervals from analysis.

suicides_heat_do_analysis <- function(data_path,
                                      date_col,
                                      region_col = NULL,
                                      temperature_col,
                                      health_outcome_col,
                                      var_fun = "ns",
                                      var_dof = 4,
                                      lag_fun = "ns",
                                      lag_dof = 4,
                                      lag_days = 3,
                                      save_fig = FALSE,
                                      save_csv = FALSE,
                                      output_folder_path = NULL) {

  df_list <- mh_read_and_format_data(data_path = data_path,
                             date_col = date_col,
                             region_col = region_col,
                             temperature_col = temperature_col,
                             health_outcome_col = health_outcome_col)

  cb_list <- mh_create_crossbasis(data = df_list,
                               var_fun = var_fun,
                               var_dof = var_dof,
                               lag_fun = lag_fun,
                               lag_dof = lag_dof,
                               lag_days = lag_days)

  model_list <- mh_casecrossover_dlnm(data = df_list,
                                   cb_list = cb_list)

  pred_list <- mh_predict(data = df_list,
                          cb_list = cb_list,
                          model_list = model_list)


  mh_plot_results(pred_list = pred_list,
               save_fig = save_fig,
               output_folder_path = output_folder_path)

  results <- produce_results(data = df_list,
                             cb_list = cb_list,
                             model_list = model_list)

  if (save_csv == TRUE) {

    mh_save_results(results = results,
               output_folder_path = output_folder_path)

  }

  return(results)

}



