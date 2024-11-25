#' Read in and format data
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

read_and_format_data <- function(data_path,
                                 date_col,
                                 region_col = NULL,
                                 temperature_col,
                                 health_outcome_col) {

  #TODO: Include file extension function to check data_path is a csv

  df <- read.csv(data_path)

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
    cb <- crossbasis(region_data$temp, lag = lag_days, argvar = argvar, arglag =arglag)

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
#'
#' @returns List containing models by region

casecrossover_dlnm <- function(data) {

  data <- split(data, f = data$region)
  model_list <- list()

  for(reg in names(data)){

    region_data <- data[[reg]]
    print(head(region_data))
    model <- gnm(suicides ~ cb_list[[reg]], eliminate = stratum, family = quasipoisson(), data = region_data,
                 na.action = "na.exclude", subset = ind > 0)
    print(model)
    assign(x = paste0('model_', stringr::str_replace_all(reg, " ", "_")), value = model)

  }

  return(model_list)

}


plot_results <- function(data,
                         model_list,
                         cb_list){

  data <- split(data, f = data$region)

  for(reg in names(data)){

    region_data <- data[[reg]]
    cen <- mean(region_data$temp, na.rm = T)
    pred <- crosspred(cb_list[[reg]], model_list[[reg]], cen = cen)

    plot(pred,
         "overall",
         xlab = expression(paste("Temperature (", degree, "C)")),
         ylab = "RR",
         ylim = c(0,3),
         xlim = c(min(data$temp, max(data$temp))),
         main = reg)

    dev.off()

  }

}

data <- split(df, f = df$region)

region_data <- data[["London"]]
cen <- mean(region_data$temp, na.rm = T)


pred <- crosspred(cb_list[["London"]], model_list[["London"]], cen = cen)
cb1 <- cb_list[["London"]]
model1 <- model_list[["London"]]

plot(pred,
     "overall",
     xlab = expression(paste("Temperature (", degree, "C)")),
     ylab = "RR",
     ylim = c(0,3),
     xlim = c(min(data$temp, max(data$temp))),
     main = reg)

dev.off()
