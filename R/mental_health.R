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
#' the region names.
#' @param temperature_col Character. Name of the column in the dataframe that
#' contains the temperature column.
#' @param health_outcome_col Character. Name of the column in the dataframe that
#' contains the health outcome count column (e.g. number of deaths, hospital
#' admissions).
#'
#' @returns Dataframe with formatted and renamed columns.

mh_read_and_format_data <- function(data_path,
                                 date_col,
                                 region_col,
                                 temperature_col,
                                 health_outcome_col) {

  #TODO: Include file extension function to check data_path is a csv

  df <- read.csv(data_path)

  #TODO use read_input_data (check row.names argument)

  if(is.null(region_col)) {

    df <- df %>%
      dplyr::rename(region = "no_region")

  } else {

    df <- df %>%
      dplyr::rename(region = region_col)

  }

  df <- df %>%
    dplyr::rename(date = date_col,
                  temp = temperature_col,
                  suicides = health_outcome_col) %>%
    dplyr::mutate(date = lubridate::ymd(date),
                  year = as.factor(lubridate::year(date)),
                  month = as.factor(lubridate::month(date)),
                  day = as.factor(lubridate::day(date)),
                  dow = as.factor(lubridate::wday(date, label = TRUE)),
                  region = as.factor(region),
                  stratum = as.factor(region:year:month:dow),
                  ind = tapply(suicides, stratum, sum)[stratum])

  return(df)
}


#' Create cross-basis matrix
#'
#' @description Creates a cross-basis matrix for each region
#'
#' @param data Dataframe containing daily timeseries data for a health outcome
#' and climate variables which may be disaggregated by a particular region.
#' @param var_fun Character. Exposure function
#' (see dlnm::crossbasis).
#' @param var_dof Integer. Degrees of freedom in exposure function
#' (see dlnm:crossbasis).
#' @param lag_fun Character. Exposure function
#' (see dlnm::crossbasis).
#' @param lag_dof Integer. Degrees of freedom in exposure function
#' (see dlnm:crossbasis).
#' @param lag_days Integer. Maximum lag
#' (see dlnm:crossbasis).
#'
#' @returns Large list of cross-basis matrices by region

create_crossbasis <- function(data,
                              var_fun,
                              var_dof,
                              lag_fun,
                              lag_dof,
                              lag_days) {

  data <- split(data, f = data$region)
  cb_list <- list()

  for(reg in names(data)){

    region_data <- data[[reg]]
    argvar <- list(fun = var_fun, df = var_dof)
    arglag <- list(fun = lag_fun, df = lag_dof)
    cb <- dlnm::crossbasis(region_data$temp, lag = lag_days, argvar = argvar, arglag =arglag)

    cb_list[[reg]] <- cb
  }

  return(cb_list)
}


#' Quasi-Poisson Case-Crossover model with DLNM
#'
#' @description Fits a quasi-Poisson case-crossover with a distributed lag
#' non-linear model
#'
#' @param data Dataframe containing daily timeseries data for a health outcome
#' and climate variables which may be disaggregated by a particular region.
#' @param cb_list List of cross_basis matrices from create_crossbasis function.
#'
#' @returns List containing models by region

casecrossover_dlnm <- function(data,
                               cb_list) {

  data <- split(data, f = data$region)
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


#' Plot results of analysis - Mental Health
#'
#' @description Plots cumulative lag exposure-response function for each region
#'
#' @param data Dataframe containing daily timeseries data for a health outcome
#' and climate variables which may be disaggregated by a particular region.
#' @param cb_list List of cross_basis matrices from create_crossbasis function.
#' @param model_list List of models produced from case-crossover and DLNM
#' analysis.
#' @param save_fig Boolean. Whether to save the plot as an output.
#' @param output_folder_path Path to folder where plots should be saved.
#'
#' @returns Plots of cumulative lag exposure-response function for each region

mh_plot_results <- function(data,
                         cb_list,
                         model_list,
                         save_fig,
                         output_folder_path) {

  xlim <- c(min(data$temp), max(data$temp))
  data <- split(data, f = data$region)

  for(reg in names(data)){

    region_data <- data[[reg]]
    cb <- cb_list[[reg]]
    cen <- mean(region_data$temp, na.rm = T)
    pred <- dlnm::crosspred(cb, model_list[[reg]], cen = cen,
                      from = min(region_data$temp, na.rm = TRUE),
                      to = max(region_data$temp, na.rm = TRUE), by = 1)

    if (save_fig == TRUE) {

      if (!is.null(output_folder_path)) {
        pdf(file.path(output_folder_path, paste0("suicides_plot_", stringr::str_replace_all(reg,' ','_'), ".pdf")),
            width = 8, height = 8)
        plot(pred,
            "overall",
            xlab = expression(paste("Temperature (", degree, "C)")),
            ylab = "RR",
            ylim = c(0,3),
            xlim = xlim,
            main = reg) # NOTE: this print() is required to produce the plot pdf
        dev.off()
      }


    }
    else{

      plot(pred,
            "overall",
            xlab = expression(paste("Temperature (", degree, "C)")),
            ylab = "RR",
            ylim = c(0,3),
            xlim = xlim,
            main = reg)

    }

  }

}


#' Produce cumulative relative risk results of analysis
#'
#' @description Produces cumulative relative risk and confidence intervals
#' from analysis.
#'
#' @param data Dataframe containing daily timeseries data for a health outcome
#' and climate variables which may be disaggregated by a particular region.
#' @param cb_list List of cross_basis matrices from create_crossbasis function.
#' @param model_list List of models produced from case-crossover and DLNM
#' analysis.
#'
#' @returns Dataframe containing cumulative relative risk and confidence
#' intervals from analysis.

produce_results <- function(data,
                            cb_list,
                            model_list) {

    data <- split(data, f = data$region)
    results <- data.frame()

    for (reg in names(data)) {
      region_data <- data[[reg]]
      cb <- cb_list[[reg]]
      cen <- mean(region_data$temp, na.rm = TRUE)
      pred <- dlnm::crossreduce(cb, model_list[[reg]], cen = cen,
                          from = min(region_data$temp, na.rm = TRUE),
                          to = max(region_data$temp, na.rm = TRUE), by = 1)

      rrfit <- paste0("RRfit_", gsub(' ', '_', reg))
      rrlow <- paste0("RRlow_", gsub(' ', '_', reg))
      rrhigh <- paste0("RRhigh_", gsub(' ', '_', reg))

      if (nrow(results) == 0) {
        results <- data.frame(
          Temperature = pred$predvar,
          setNames(list(pred$RRfit), rrfit),
          setNames(list(pred$RRlow), rrlow),
          setNames(list(pred$RRhigh), rrhigh)
        )
      } else {
        results_add <- data.frame(
          Temperature = pred$predvar,
          setNames(list(pred$RRfit), rrfit),
          setNames(list(pred$RRlow), rrlow),
          setNames(list(pred$RRhigh), rrhigh)
        )
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

mh_save_results <- function(results,
                         output_folder_path) {

  if (!is.null(output_folder_path)) {

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
#' the region names.
#' @param temperature_col Character. Name of the column in the dataframe that
#' contains the temperature column.
#' @param health_outcome_col Character. Name of the column in the dataframe that
#' contains the health outcome count column (e.g. number of deaths, hospital
#' admissions).
#' @param var_fun Character. Exposure function (see dlnm::crossbasis).
#' @param var_dof Integer. Degrees of freedom in exposure function
#' (see dlnm:crossbasis).
#' @param lag_fun Character. Exposure function (see dlnm::crossbasis).
#' @param lag_dof Integer. Degrees of freedom in exposure function
#' (see dlnm:crossbasis).
#' @param lag_days Integer. Maximum lag (see dlnm:crossbasis).
#' @param save_fig Boolean. Whether to save the plot as an output.
#' @param save_csv Boolean. Whether to save the results as a CSV.
#' @param output_folder_path Path to folder where plots and/or CSV should be
#' saved.
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
                                      output_folder_path) {

  df <- mh_read_and_format_data(data_path = data_path,
                             date_col = date_col,
                             region_col = region_col,
                             temperature_col = temperature_col,
                             health_outcome_col = health_outcome_col)
  cb_list <- create_crossbasis(data = df,
                               var_fun = var_fun,
                               var_dof = var_dof,
                               lag_fun = lag_fun,
                               lag_dof = lag_dof,
                               lag_days = lag_days)

  model_list <- casecrossover_dlnm(data = df,
                                   cb_list = cb_list)

  mh_plot_results(data = df,
               cb_list = cb_list,
               model_list = model_list,
               save_fig = save_fig,
               output_folder_path = output_folder_path)

  results <- produce_results(data = df,
                             cb_list = cb_list,
                             model_list = model_list)

  if (save_csv == TRUE) {

    mh_save_results(results = results,
               output_folder_path = output_folder_path)

  }

  return(results)

}



