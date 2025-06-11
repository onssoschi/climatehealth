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
#'
#' @exports
mh_read_and_format_data <- function(data_path,
                                 date_col,
                                 region_col = NULL,
                                 temperature_col,
                                 health_outcome_col,
                                 population_col) {

  # make sure data_path is a csv if a path is passed
  if(is.character(data_path)) {
    check_file_extension(data_path, ".csv", "data_path")
  }
  # read data
  df <- read_input_data(data_path)

  if(is.null(region_col)) {

    df <- df %>%
      dplyr::mutate(region = "aggregated")
  }

  df <- df %>%
    dplyr::rename(date = date_col,
                  region = region_col,
                  temp = temperature_col,
                  suicides = health_outcome_col,
                  population = population_col) %>%
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


#' Create population totals
#'
#' @description Creates a list of population totals by year and region for use
#' in the attributable rate calculations.
#'
#' @param data A list of dataframes containing daily timeseries data for a health outcome
#' and climate variables which may be disaggregated by a particular region.
#' @param country Character. Name of country for national level estimates.
#'
#' @returns List of population totals by year and region
#'
#' @exports
mh_pop_totals <- function(df_list,
                          country = "National"){

  pop_list <- lapply(df_list, function(x) aggregate(population ~ year, data = x, mean))

  tot_pop <- do.call(rbind, pop_list)
  tot_pop <- aggregate(population ~ year, data = tot_pop, sum)

  pop_list[[country]] <- tot_pop

  return(pop_list)

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
#'
#' @exports
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
#'
#' @exports
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
#'
#' @exports
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
#'
#' @exports
mh_meta_analysis <- function(data,
                             coef_,
                             vcov_){

  # Create temperature average and range as meta predictors

  temp_avg <- sapply(data, function(x) mean(x$temp, na.rm = TRUE))
  temp_range <- sapply(data, function(x) diff(range(x$temp, na.rm = TRUE)))

  # Meta-analysis

  mm <- mixmeta::mixmeta(formula = coef_ ~ temp_avg + temp_range,
                         S = vcov_,
                         data = as.data.frame(names(data)),
                         method = "reml")
  #TODO potentially add random effects random = ~ 1 | region (I think already covered by reml)

  # BLUP

  blup <- mixmeta::blup(object = mm,
                        vcov = TRUE)

  names(blup) <- names(data)

  return(list(mm, blup))

}


#' Define min and max suicide values
#'
#' @description Define the minimum (between 1st and 50th percentiles) and
#' maximum (between 51st and 99th percentiles) suicide temperature values
#'
#' @param data A list of dataframes containing daily timeseries data for a health outcome
#' and climate variables which may be disaggregated by a particular region.
#' @param var_fun Character. Exposure function for argvar
#' (see dlnm::crossbasis). Defaults to 'bs'.
#' @param var_per Vector. Internal knot positions for argvar
#' (see dlnm::crossbasis). Defaults to c(25,50,75).
#' @param var_degree Integer. Degree of the piecewise polynomial for argvar
#' (see dlnm::crossbasis). Defaults to 2 (quadratic).
#' @param blup A list. BLUP (best linear unbiased predictions) from the
#' meta-analysis model for each region.
#'
#' @return
#' \itemize{
#'   \item `minpercreg` Vector. Percentile of minimum suicide temperature for each region.
#'   \item `maxpercreg` Vector. Percentile of maximum suicide temperature for each region.
#'   }
#'
#' @exports
mh_minmax_suicide_temp <- function(data,
                                   var_fun = "bs",
                                   var_per = c(25,50,75),
                                   var_degree = 2,
                                   blup) {

  # Generate matrix for storing results

  minpercreg <- mintempreg <- maxpercreg <- maxtempreg <- rep(NA, length(data))
  names(minpercreg) <- names(mintempreg) <- names(maxpercreg) <- names(maxtempreg) <- names(data)

  # Define min and max suicide values: exclude low and very hot temperature

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

    minpercreg[reg] <- (1:50)[which.min((bvar%*%blup[[reg]]$blup)[1:50,])]
    maxpercreg[reg] <- (51:99)[which.max((bvar%*%blup[[reg]]$blup)[51:99,])]

  }

  return(list(minpercreg, maxpercreg))

}


#' Run regional predictions from model
#'
#' @description Use model to run regional predictions
#'
#' @param data A list of dataframes containing daily timeseries data for a health outcome
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
#'
#' @return A list containing predictions by region
#'
#' @exports
mh_predict_reg <- function(data,
                           var_fun = "bs",
                           var_per = c(25,50,75),
                           var_degree = 2,
                           minpercreg,
                           blup){

  pred_list <- list()

  for(reg in names(data)){

    region_data <- data[[reg]]

    argvar <- list(x = region_data$temp,
                   fun = var_fun,
                   knots = quantile(region_data$temp, var_per/100, na.rm = TRUE),
                   degree = var_degree)

    bvar <- do.call(dlnm::onebasis, argvar)

    cen <- quantile(region_data$temp, minpercreg[reg]/100, na.rm = TRUE)

    pred <- dlnm::crosspred(bvar,
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
#' @param maxpercreg Vector. Percentile of maximum suicide temperature for each region.
#'
#' @return
#' \itemize{
#'   \item `df_list` List. A list of data frames for each region and nation.
#'   \item `cb_list` List. A list of cross-basis matrices by region and nation.
#'   \item `minpercreg` Vector. Percentile of minimum suicide temperature for each region and nation.
#'   \item `maxpercreg` Vector. Percentile of maximum suicide temperature for each region and nation.
#'   \item `mmpredall` List. A list of national coefficients and covariance matrices.
#'   }
#'
#' @exports
mh_add_national_data <- function(df_list,
                                 pop_list,
                                 var_fun = "bs",
                                 var_per = c(25,50,75),
                                 var_degree = 2,
                                 lag_fun = "strata",
                                 lag_breaks = 1,
                                 lag_days = 2,
                                 country = "National",
                                 cb_list,
                                 mm,
                                 minpercreg,
                                 maxpercreg){

  # Aggregate national level data

  national_data <- as.data.frame(do.call(rbind, df_list))

  nat_pop <- pop_list[[country]] %>%
    rename(nat_population = population)

  national_data <- national_data %>%
    left_join(nat_pop, by = "year") %>%
    mutate(weight = population / nat_population,
           weighted_temp = temp * weight) %>%
    group_by(date) %>%
    summarise(#temp = round(mean(temp, na.rm = TRUE), 1))#, #TODO Remove if happy with weighted mean
              temp = round(sum(weighted_temp, na.rm = TRUE), 1),
              suicides = sum(suicides, na.rm = TRUE)) %>%
    mutate(year = as.factor(lubridate::year(date)),
           month = as.factor(lubridate::month(date)))

  df_list[[country]] <- as.data.frame(national_data)

  # Create cross basis for national data

  argvar <- list(fun = var_fun,
                 knots = quantile(national_data$temp, var_per/100, na.rm = T),
                 degree = var_degree)
  arglag <- list(fun = lag_fun, breaks = lag_breaks)

  cb_list[[country]] <- dlnm::crossbasis(national_data$temp, lag = lag_days, argvar = argvar, arglag = arglag)

  # Add national min and max suicide temperatures

  predvar <- quantile(national_data$temp, 1:99/100, na.rm = TRUE)

  argvar <- list(x = predvar,
                 fun = var_fun,
                 knots = quantile(national_data$temp, var_per/100, na.rm = TRUE),
                 degree = var_degree,
                 Boundary.knots = range(national_data$temp, na.rm = TRUE))

  bvar <- do.call(dlnm::onebasis, argvar)

  datanew <- data.frame(
    temp_avg = mean(national_data$temp),
    temp_range = diff(range(national_data$temp, na.rm = TRUE)))

  mmpredall <- predict(mm,datanew,vcov=T,format="list")

  minpercnat <- (1:50)[which.min((bvar%*%mmpredall$fit)[1:50,])]
  maxpercnat <- (51:99)[which.max((bvar%*%mmpredall$fit)[51:99,])]

  #minpercnat <- median(minpercreg)
  #maxpercnat <- median(maxpercreg)

  minpercreg[country] <- minpercnat
  maxpercreg[country] <- maxpercnat

  return(list(df_list, cb_list, minpercreg, maxpercreg, mmpredall))

}



#' Run national predictions from meta analysis
#'
#' @description Use the meta analysis to create national level predictions
#'
#' @param data A list of dataframes containing daily timeseries data for a health outcome
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
#' @return A list containing predictions by region.
#'
#' @exports
mh_predict_nat <- function(df_list,
                           var_fun = "bs",
                           var_per = c(25,50,75),
                           var_degree = 2,
                           minpercreg,
                           mmpredall,
                           pred_list,
                           country = "National"){

  national_data <- df_list[[country]]

  argvar <- list(x = national_data$temp,
                 fun = var_fun,
                 knots = quantile(national_data$temp, var_per/100, na.rm = TRUE),
                 degree = var_degree)

  bvar <- do.call(dlnm::onebasis, argvar)

  cen <- quantile(national_data$temp, minpercreg[country]/100,na.rm=T)

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
#' @exports
mh_rr_results <- function(pred_list) {

  rr_results <- bind_rows(lapply(names(pred_list), function(region_name) {

    reg_pred <- pred_list[[region_name]]

    df <- data.frame(
      Area = region_name,
      Temperature = reg_pred$predvar,
      RR = reg_pred$allRRfit,
      RR_lower_CI = reg_pred$allRRlow,
      RR_upper_CI = reg_pred$allRRhigh
    )

    return(df)

  })
  )

  rownames(rr_results) <- NULL

  return(rr_results)

}


#' Plot results of relative risk analysis - Mental Health
#'
#' @description Plots cumulative lag exposure-response function for each region
#'
#' @param df_list A list of dataframes containing daily timeseries data for a health outcome
#' and climate variables which may be disaggregated by a particular region.
#' @param pred_list A list containing predictions from the model by region.
#' @param minpercreg Vector. Percentile of minimum suicide temperature for each area.
#' @param maxpercreg Vector. Percentile of maximum suicide temperature for each area.
#' @param save_fig Boolean. Whether to save the plot as an output. Defaults to
#' FALSE.
#' @param output_folder_path Path to folder where plots should be saved.
#' Defaults to NULL.
#'
#' @returns Plots of cumulative lag exposure-response function for each region
#'
#' @exports
mh_plot_rr <- function(df_list,
                       pred_list,
                       maxpercreg,
                       minpercreg,
                       save_fig = FALSE,
                       output_folder_path = NULL) {

  xlim <- c(min(sapply(pred_list, function(x) min(x$predvar, na.rm = TRUE))),
            max(sapply(pred_list, function(x) max(x$predvar, na.rm = TRUE))))

  ylim <- c(min(c(min(sapply(pred_list, function(x) min(x$allRRfit, na.rm = TRUE))) - 0.1, 0.4)),
            max(c(max(sapply(pred_list, function(x) max(x$allRRfit, na.rm = TRUE))) + 0.5, 2.1)))

  if (save_fig==T) {
    # create grid dynamically
    grid <- create_grid(length(pred_list))
    output_path <- file.path(output_folder_path,
                             "suicides_rr_plot.pdf")
    pdf(output_path, width=grid[1]*4, height=grid[2]*4)
    par(mfrow=c(grid[1],  grid[2]))
  }

  for(reg in names(pred_list)){

    region_pred <- pred_list[[reg]]

    plot(region_pred,
         "overall",
         xlab = expression(paste("Temperature (", degree, "C)")),
         ylab = "RR",
         ylim = ylim,
         xlim = xlim,
         main = reg,
         col = "#f25574")

    vline_pos_max_x <- quantile(df_list[[reg]]$temp, maxpercreg[reg]/100, na.rm = TRUE)
    vline_pos_max_y <- max(pred_list[[reg]]$allRRfit, na.rm = TRUE) + 0.3
    vline_lab_max <- paste0("Max ST\n", round(vline_pos_max_x, 2), intToUtf8(176), "C (p", round(maxpercreg[reg], 2), ")")

    abline(v = vline_pos_max_x, col = "black", lty = 2)

    text(x = vline_pos_max_x, y = vline_pos_max_y, labels = vline_lab_max, pos = 2, col = "black", cex = 0.8)

    vline_pos_min <- quantile(df_list[[reg]]$temp, minpercreg[reg]/100, na.rm = TRUE)
    vline_lab_min <- paste0("Min ST\n", round(vline_pos_min, 2), intToUtf8(176), "C (p", round(minpercreg[reg], 2), ")")

    abline(v = vline_pos_min, col = "black", lty = 2)

    text(x = vline_pos_min, y = 0.5, labels = vline_lab_min, pos = 4, col = "black", cex = 0.8)

  }

  if (save_fig == TRUE) {

    dev.off()

  }

}


#' Produce total attributable numbers and rates
#'
#' @description Create matrix of total attributable fractions, numbers and rates
#' for each area over the whole time series, with confidence intervals.
#'
#' @param df_list A list of dataframes containing daily timeseries data for a health outcome
#' and climate variables which may be disaggregated by a particular region.
#' @param cb_list List of cross_basis matrices from create_crossbasis function.
#' @param minpercreg Vector. Percentile of minimum suicide temperature for each area.
#' @param maxpercreg Vector. Percentile of maximum suicide temperature for each area.
#' @param pred_list A list containing predictions from the model by region.
#' @param pop_list List of population totals by year and region.
#'
#' @return Matrix containing total attributable fractions, numbers and rates for each
#' area over the whole time series.
#'
#' @exports
mh_an_ar_totals <- function(df_list,
                            cb_list,
                            minpercreg,
                            maxpercreg,
                            pred_list,
                            pop_list){

  res_an_ar_tot <- matrix(NA, length(names(df_list)), 12,
                          dimnames = list(names(df_list),
                                    c("Population", "MaxST", "Suicides", "AF", "AF_lower_ci", "AF_upper_ci",
                                      "AN", "AN_lower_ci", "AN_upper_ci",
                                      "AR", "AR_lower_ci", "AR_upper_ci")))

for (reg in names(df_list)){

  region_data <- df_list[[reg]]
  cb <- cb_list[[reg]]
  minperc <- minpercreg[reg]
  maxperc <- maxpercreg[reg]
  pred <- pred_list[[reg]]
  pop <- pop_list[[reg]]

  pop_mean <- round(mean(pop$population), 0)

  tot_suicides <- sum(region_data$suicides)

  cen <- quantile(region_data$temp, minperc/100, na.rm = TRUE)
  min_range <- quantile(region_data$temp, maxperc/100, na.rm = TRUE)
  max_range <- max(region_data$temp, na.rm = TRUE)

  res_an_ar_tot[reg, "Population"] <- pop_mean

  res_an_ar_tot[reg, "MaxST"] <- round(min_range, 1)

  res_an_ar_tot[reg, "Suicides"] <- tot_suicides

  res_an_ar_tot[reg, "AN"] <- FluMoDL::attrdl(x = region_data$temp,
                                          basis = cb,
                                          cases = region_data$suicides,
                                          coef = pred$coefficients,
                                          vcov = pred$vcov,
                                          type = "an",
                                          dir = "forw",
                                          cen = cen,
                                          range = c(min_range, max_range),
                                          tot = TRUE)

  res_an_ar_tot[reg, "AN_lower_ci"] <- quantile(FluMoDL::attrdl(x = region_data$temp,
                                                            basis = cb,
                                                            cases = region_data$suicides,
                                                            coef = pred$coefficients,
                                                            vcov = pred$vcov,
                                                            type = "an",
                                                            dir = "forw",
                                                            cen = cen,
                                                            range = c(min_range, max_range),
                                                            tot = TRUE,
                                                            sim = TRUE,
                                                            nsim = 1000), 0.025)

  res_an_ar_tot[reg, "AN_upper_ci"] <- quantile(FluMoDL::attrdl(x = region_data$temp,
                                                            basis = cb,
                                                            cases = region_data$suicides,
                                                            coef = pred$coefficients,
                                                            vcov = pred$vcov,
                                                            type = "an",
                                                            dir = "forw",
                                                            cen = cen,
                                                            range = c(min_range, max_range),
                                                            tot = TRUE,
                                                            sim = TRUE,
                                                            nsim = 1000), 0.975)

  res_an_ar_tot[reg, "AF"] <- signif((res_an_ar_tot[reg, "AN"] / res_an_ar_tot[reg, "Suicides"]), 2)

  res_an_ar_tot[reg, "AF_lower_ci"] <- signif((res_an_ar_tot[reg, "AN_lower_ci"] / res_an_ar_tot[reg, "Suicides"]), 2)

  res_an_ar_tot[reg, "AF_upper_ci"] <- signif((res_an_ar_tot[reg, "AN_upper_ci"] / res_an_ar_tot[reg, "Suicides"]), 2)

  res_an_ar_tot[reg, "AR"] <- round((res_an_ar_tot[reg, "AN"] / res_an_ar_tot[reg, "Population"]) * 100000, 2)

  res_an_ar_tot[reg, "AR_lower_ci"] <- round((res_an_ar_tot[reg, "AN_lower_ci"] / res_an_ar_tot[reg, "Population"]) * 100000, 2)

  res_an_ar_tot[reg, "AR_upper_ci"] <- round((res_an_ar_tot[reg, "AN_upper_ci"] / res_an_ar_tot[reg, "Population"]) * 100000, 2)

  res_an_ar_tot[reg, "AN"] <- round(res_an_ar_tot[reg, "AN"], 2)

  res_an_ar_tot[reg, "AN_lower_ci"] <- round(res_an_ar_tot[reg, "AN_lower_ci"], 2)

  res_an_ar_tot[reg, "AN_upper_ci"] <- round(res_an_ar_tot[reg, "AN_upper_ci"], 2)

}

  return(res_an_ar_tot)

}


#' Plot total attributable rates
#'
#' @description Plot total attributable rates over the whole time series by area
#' including data tables of both rates and numbers with confidence intervals
#'
#' @param res_an_ar_tot Matrix containing total attributable fractions, numbers and rates for each
#' area over the whole time series.
#' @param save_fig Boolean. Whether to save the plot as an output. Defaults to
#' FALSE.
#' @param output_folder_path Path to folder where plots should be saved.
#' Defaults to NULL.
#'
#' @return Plot of total attributable rates with tables of rates, numbers and
#' confidence intervals by area
#'
#' @exports
mh_plot_ar_totals <- function(df_list,
                              res_an_ar_tot,
                              save_fig = FALSE,
                              output_folder_path = NULL){

  if (save_fig == TRUE){


  num_regions <- nrow(res_an_ar_tot)

  # Dynamically adjust height based on number of regions
  chart_height <- 8
  chart_width <- 0.3 * num_regions
  table_height <- 0.2 * num_regions # adjust as needed
  total_height <- max(15, table_height)
  total_width <- max(6, chart_width)

  output_path <- file.path(output_folder_path, "suicides_AR_plot_total.pdf")
  pdf(output_path, width = total_width, height = total_height)

  # Set up layout: 1 row for barplot and 1 row for table
  layout(matrix(c(1, 2), nrow = 2), heights = c(chart_height, (total_height - chart_height)))


  # Set up plotting area for the bar chart
  par(mar = c(5, 5, 4, 2) + 0.1)

  }

  # Shorten the labels to a fixed length
  short_labels <- sapply(rownames(res_an_ar_tot), function(x) {
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

  barplot(names.arg = short_labels,
          height = res_an_ar_tot[,"AR"],
          ylab = "AR (per 100,000 population)",
          main = paste0("Total Attributable Rates of Suicides by Area ", year_range),
          col = "#f25574",
          las = 2)

  if (save_fig == TRUE){

  par(mar = c(0, 0, 0, 0))
  plot.new()


  # Get the figure region dimensions
  usr <- par("usr")
  x_right <- usr[2]
  y_bottom <- usr[3]

  # Add annotation text in the bottom right
  text(x = x_right - 0.05, y = y_bottom + 0.05, # adjust y offset as needed
       labels = "AN = Attributable Number, AR = Attributable Rate (per 100,000 population)",
       adj = c(1, 0), # right-bottom alignment
       cex = 0.9, font = 3)

  # Create the table data
  table_data <- data.frame(
    Area = short_labels,
    AN = as.numeric(res_an_ar_tot[, "AN"]),
    AN_Lower_CI = as.numeric(res_an_ar_tot[, "AN_lower_ci"]),
    AN_Upper_CI = as.numeric(res_an_ar_tot[, "AN_upper_ci"]),
    AR = as.numeric(res_an_ar_tot[, "AR"]),
    AR_Lower_CI = as.numeric(res_an_ar_tot[, "AR_lower_ci"]),
    AR_Upper_CI = as.numeric(res_an_ar_tot[, "AR_upper_ci"])
  )

  # Plot the table using grid.table
  grid::grid.newpage()
  gridExtra::grid.table(table_data)

  dev.off()

  }

}


#' Produce total attributable numbers and rates
#'
#' #' @description Create list of matrices of yearly attributable fractions,
#' numbers and rates for each area with confidence intervals.
#'
#' @param df_list A list of dataframes containing daily timeseries data for a health outcome
#' and climate variables which may be disaggregated by a particular region.
#' @param cb_list List of cross_basis matrices from create_crossbasis function.
#' @param minpercreg Vector. Percentile of minimum suicide temperature for each area.
#' @param maxpercreg Vector. Percentile of maximum suicide temperature for each area.
#' @param pred_list A list containing predictions from the model by region.
#' @param pop_list List of population totals by year and region.
#'
#' @return A list of matrices containing yearly estimates of attributable
#' fractions, numbers and rates by area
#'
#' @exports
mh_an_ar_yearly <- function(df_list,
                            cb_list,
                            minpercreg,
                            maxpercreg,
                            pred_list,
                            pop_list){

  an_ar_yr_list <- list()

  for (reg in names(df_list)){

    region_data_all <- df_list[[reg]]
    cb <- cb_list[[reg]]
    minperc <- minpercreg[reg]
    maxperc <- maxpercreg[reg]
    pred <- pred_list[[reg]]
    pop <- pop_list[[reg]]

    cen <- quantile(region_data_all$temp, minperc/100, na.rm = TRUE) # TODO Check if for each year or whole timeseries
    min_range <- quantile(region_data_all$temp, maxperc/100, na.rm = TRUE) # TODO Check if for each year or whole timeseries
    max_range <- max(region_data_all$temp, na.rm = TRUE) # TODO Check if for each year or whole timeseries

    year_list <- unique(region_data_all$year)

    mat_an_ar <- matrix(NA, length(year_list), 13,
                        dimnames = list(year_list,
                                        c("Year", "Population", "Suicides", "MaxST", "AF", "AF_lower_ci", "AF_upper_ci",
                                          "AN", "AN_lower_ci", "AN_upper_ci",
                                          "AR", "AR_lower_ci", "AR_upper_ci")))

    for (yr in year_list){

      region_data <- filter(region_data_all, year == yr)

      tot_suicides <- sum(region_data$suicides)

      mat_an_ar[yr, "Year"] <- as.numeric(as.character(yr))

      mat_an_ar[yr, "Population"] <- pop$population[pop$year == yr]

      mat_an_ar[yr, "Suicides"] <- tot_suicides

      mat_an_ar[yr, "MaxST"] <- min_range

      mat_an_ar[yr, "AN"] <- FluMoDL::attrdl(x = region_data$temp,
                                             basis = cb,
                                             cases = region_data$suicides,
                                             coef = pred$coefficients,
                                             vcov = pred$vcov,
                                             type = "an",
                                             dir = "forw",
                                             cen = cen,
                                             range = c(min_range, max_range),
                                             tot = TRUE)

      mat_an_ar[yr, "AN_lower_ci"] <- quantile(FluMoDL::attrdl(x = region_data$temp,
                                                               basis = cb,
                                                               cases = region_data$suicides,
                                                               coef = pred$coefficients,
                                                               vcov = pred$vcov,
                                                               type = "an",
                                                               dir = "forw",
                                                               cen = cen,
                                                               range = c(min_range, max_range),
                                                               tot = TRUE,
                                                               sim = TRUE,
                                                               nsim = 1000), 0.025)

      mat_an_ar[yr, "AN_upper_ci"] <- quantile(FluMoDL::attrdl(x = region_data$temp,
                                                               basis = cb,
                                                               cases = region_data$suicides,
                                                               coef = pred$coefficients,
                                                               vcov = pred$vcov,
                                                               type = "an",
                                                               dir = "forw",
                                                               cen = cen,
                                                               range = c(min_range, max_range),
                                                               tot = TRUE,
                                                               sim = TRUE,
                                                               nsim = 1000), 0.975)

      mat_an_ar[yr, "AF"] <- signif((mat_an_ar[yr, "AN"] / mat_an_ar[yr, "Suicides"]), 2)

      mat_an_ar[yr, "AF_lower_ci"] <- signif((mat_an_ar[yr, "AN_lower_ci"] / mat_an_ar[yr, "Suicides"]), 2)

      mat_an_ar[yr, "AF_upper_ci"] <- signif((mat_an_ar[yr, "AN_upper_ci"] / mat_an_ar[yr, "Suicides"]), 2)

      mat_an_ar[yr, "AR"] <- round((mat_an_ar[yr, "AN"] / mat_an_ar[yr, "Population"]) * 100000, 2)

      mat_an_ar[yr, "AR_lower_ci"] <- round((mat_an_ar[yr, "AN_lower_ci"] / mat_an_ar[yr, "Population"]) * 100000, 2)

      mat_an_ar[yr, "AR_upper_ci"] <- round((mat_an_ar[yr, "AN_upper_ci"] / mat_an_ar[yr, "Population"]) * 100000, 2)

      mat_an_ar[yr, "AN"] <- round(mat_an_ar[yr, "AN"], 2)

      mat_an_ar[yr, "AN_lower_ci"] <- round(mat_an_ar[yr, "AN_lower_ci"], 2)

      mat_an_ar[yr, "AN_upper_ci"] <- round(mat_an_ar[yr, "AN_upper_ci"], 2)

    }

    row.names(mat_an_ar) <- NULL
    an_ar_yr_list[[reg]] <- mat_an_ar

  }

  return(an_ar_yr_list)

}


#' Plot attributable numbers by year
#'
#' @description Plot attributable numbers by year and area with confidence intervals
#'
#' @param an_ar_yr_list A list of matrices containing yearly estimates of attributable
#' fractions, numbers and rates by area
#' @param save_fig Boolean. Whether to save the plot as an output. Defaults to
#' FALSE.
#' @param output_folder_path Path to folder where plots should be saved.
#' Defaults to NULL.
#'
#' @return Plots of yearly attributable numbers per area
#'
#' @exports
mh_plot_an_yearly <- function(an_ar_yr_list,
                              save_fig = FALSE,
                              output_folder_path = NULL){

  if (save_fig == TRUE){

    grid <- create_grid(length(an_ar_yr_list))
    output_path <- file.path(output_folder_path, "suicides_an_timeseries.pdf")
    pdf(output_path, width = grid[1]*4, height = grid[2]*4)
    par(mfrow=c(grid[1], grid[2]))

  }

  year_min <- min(sapply(an_ar_yr_list, function(x) min(x[, "Year"], na.rm = TRUE)))
  year_max <- max(sapply(an_ar_yr_list, function(x) max(x[, "Year"], na.rm = TRUE)))

  for (reg in names(an_ar_yr_list)){

    region_an <- as.data.frame(an_ar_yr_list[[reg]])
    ylim <- c(min(c(0, min(region_an$AN_lower_ci))), max(region_an$AN_upper_ci))

    plot(x = region_an$Year,
         y = region_an$AN,
         xlim = c(year_min, year_max),
         ylim = ylim,
         xlab = "Year",
         ylab = "AN",
         main = reg,
         col = "#f25574")

    # Identify rows where the CI range is non-zero
    valid_ci <- region_an$AN_lower_ci != region_an$AN_upper_ci

    # Draw arrows only for those rows
    arrows(x0 = region_an$Year[valid_ci],
           y0 = region_an$AN_lower_ci[valid_ci],
           x1 = region_an$Year[valid_ci],
           y1 = region_an$AN_upper_ci[valid_ci],
           angle = 90,
           code = 3,
           length = 0.02,
           col = "gray40")

    abline(h = 0,
      col = "black",
      lty = 2)

  }

  if (save_fig == TRUE){

  dev.off()

  }

}


#' Plot attributable rates by year
#'
#' @description Plot attributable rates by year and area with confidence intervals
#'
#' @param an_ar_yr_list A list of matrices containing yearly estimates of attributable
#' fractions, numbers and rates by area
#' @param save_fig Boolean. Whether to save the plot as an output. Defaults to
#' FALSE.
#' @param output_folder_path Path to folder where plots should be saved.
#' Defaults to NULL.
#'
#' @return Plots of yearly attributable rates per area
#'
#' @exports
mh_plot_ar_yearly <- function(an_ar_yr_list,
                              save_fig = FALSE,
                              output_folder_path = NULL){

  if (save_fig == TRUE){

    grid <- create_grid(length(an_ar_yr_list))
    output_path <- file.path(output_folder_path, "suicides_ar_timeseries.pdf")
    pdf(output_path, width = grid[1]*4, height = grid[2]*4)
    par(mfrow=c(grid[1], grid[2]))

  }

  year_min <- min(sapply(an_ar_yr_list, function(x) min(x[, "Year"], na.rm = TRUE)))
  year_max <- max(sapply(an_ar_yr_list, function(x) max(x[, "Year"], na.rm = TRUE)))

  for (reg in names(an_ar_yr_list)){

    region_ar <- as.data.frame(an_ar_yr_list[[reg]])
    ylim <- c(min(c(0, min(region_ar$AR_lower_ci))), max(region_ar$AR_upper_ci))

    plot(x = region_ar$Year,
         y = region_ar$AR,
         xlim = c(year_min, year_max),
         ylim = ylim,
         xlab = "Year",
         ylab = "AR",
         main = reg,
         col = "#f25574")

    # Identify rows where the CI range is non-zero
    valid_ci <- region_ar$AR_lower_ci != region_ar$AR_upper_ci

    # Draw arrows only for those rows
    arrows(x0 = region_ar$Year[valid_ci],
           y0 = region_ar$AR_lower_ci[valid_ci],
           x1 = region_ar$Year[valid_ci],
           y1 = region_ar$AR_upper_ci[valid_ci],
           angle = 90,
           code = 3,
           length = 0.02,
           col = "gray40")

    abline(h = 0,
           col = "black",
           lty = 2)

  }

  if (save_fig == TRUE) {

    dev.off()

  }

}


#' Produce total attributable numbers and rates by calendar month
#'
#' @description Create list of data frames containing total attributable numbers
#' and rates by calendar month over the whole time series.
#'
#' @param df_list A list of dataframes containing daily timeseries data for a health outcome
#' and climate variables which may be disaggregated by a particular region.
#' @param cb_list List of cross_basis matrices from create_crossbasis function.
#' @param minpercreg Vector. Percentile of minimum suicide temperature for each area.
#' @param maxpercreg Vector. Percentile of maximum suicide temperature for each area.
#' @param pred_list A list containing predictions from the model by region.
#' @param pop_list List of population totals by year and region.
#'
#' @return A list of data frames containing total attributable numbers and rates by
#' calendar month and area
#'
#' @exports
mh_an_ar_month <- function(df_list,
                           cb_list,
                           minpercreg,
                           maxpercreg,
                           pred_list,
                           pop_list){

  month_list <- unique(unlist(lapply(df_list, function(x) x$month)))

  an_ar_mth_list <- list()

  for (reg in names(df_list)){

    region_data <- df_list[[reg]]
    cb <- cb_list[[reg]]
    minperc <- minpercreg[reg]
    maxperc <- maxpercreg[reg]
    pred <- pred_list[[reg]]
    pop <- pop_list[[reg]]

    df_an_ar <- data.frame(date = region_data$date,
                           year = region_data$year,
                           month = region_data$month,
                           suicides = region_data$suicides)

    cen <- quantile(region_data$temp, minperc/100, na.rm = TRUE)
    min_range <- quantile(region_data$temp, maxperc/100, na.rm = TRUE)
    max_range <- max(region_data$temp, na.rm = TRUE)

    df_an_ar["Region"] <- reg

    df_an_ar["MaxST"] <- min_range

    df_an_ar["AF"]  <- FluMoDL::attrdl(x = region_data$temp,
                                        basis = cb,
                                        cases = region_data$suicides,
                                        coef = pred$coefficients,
                                        vcov = pred$vcov,
                                        type = "af",
                                        dir = "forw",
                                        cen = cen,
                                        range = c(min_range, max_range),
                                        tot = FALSE)

    df_an_ar["AN"] <- df_an_ar["AF"] * df_an_ar["suicides"]

    df_an_ar <- left_join(df_an_ar, pop, by = "year")

    df_an_ar["AR"] <- (df_an_ar["AN"] / df_an_ar["population"]) * 100000

    mth_an_ar <- df_an_ar %>%
      group_by(month) %>%
      summarise(an_mth_tot = sum(AN, na.rm = TRUE),
                ar_mth_tot = sum(AR, na.rm = TRUE)) %>%
      mutate(month = month.name[month],
             an_mth_tot = ifelse(abs(an_mth_tot) < 1, signif(an_mth_tot, 2), round(an_mth_tot, 2)),
             ar_mth_tot = ifelse(abs(ar_mth_tot) < 1, signif(ar_mth_tot, 2), round(ar_mth_tot, 2)))

    an_ar_mth_list[[reg]] <- mth_an_ar

  }

  return(an_ar_mth_list)

}


#' Plot attributable rates by calendar month
#'
#' @descripton Plot attributable rates grouped over the whole time series by
#' calendar month to explore seasonality.
#'
#' @param an_ar_mth_list A list of data frames containing total attributable
#' numbers and rates by calendar month and area.
#' @param save_fig Boolean. Whether to save the plot as an output. Defaults to
#' FALSE.
#' @param output_folder_path Path to folder where plots should be saved.
#' Defaults to NULL.
#'
#' @return Plots of attributable rates by calendar month per area
#'
#' @exports
mh_plot_ar_monthly <- function(an_ar_mth_list,
                               df_list,
                               country = "National",
                               save_fig = FALSE,
                               output_folder_path = NULL){

  if (save_fig == TRUE){

    grid <- create_grid(length(an_ar_mth_list))
    output_path <- file.path(output_folder_path, "suicides_seasonal_plot.pdf")
    pdf(output_path, width = grid[1]*4, height = grid[2]*4)
    par(mfrow=c(grid[1], grid[2]), oma = c(0, 0, 6, 0))

  }

  ylim_max <- max(sapply(an_ar_mth_list, function(x) max(x$ar_mth_tot, na.rm = TRUE)))

  for (reg in names(an_ar_mth_list)){

    region_ar <- an_ar_mth_list[[reg]]
    ylim <- c(0, ylim_max)

    barplot(names.arg = substr(region_ar$month, 1, 1),
            height = region_ar$ar_mth_tot,
            ylim = ylim,
            xlab = "Month",
            ylab = "AR (per 100,000 population)",
            main = reg,
            col = "#f25574")

  }

  if (save_fig == TRUE) {

    mtext("Attributable Rates of Suicide to Extreme Heat by Calendar Month and Area (per 100,000 population)",
          outer = TRUE, cex = 1.5, line = 3)

    year_range <- paste0("(",
                         min(sapply(df_list, function(x) min(lubridate::year(x$date), na.rm = TRUE))),
                         "-",
                         max(sapply(df_list, function(x) max(lubridate::year(x$date), na.rm = TRUE))),
                         ")")

    mtext(paste0(country, " ", year_range), outer = TRUE, cex = 1.2, line = 1)

  }

  dev.off()

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
#'
#' @exports
mh_save_results <- function(rr_results,
                            res_an_ar_tot,
                            an_ar_yr_list,
                            an_ar_mth_list,
                         output_folder_path = NULL) {

  if (!is.null(output_folder_path)) {

    check_file_exists(file.path(output_folder_path))

    write.csv(rr_results, file = file.path(
      output_folder_path, "suicides_rr_results.csv"), row.names = FALSE)

    write.csv(res_an_ar_tot, file = file.path(
      output_folder_path, "suicides_an_ar_tot_results.csv"), row.names = TRUE)

    res_an_ar_yr <- bind_rows(lapply(names(an_ar_yr_list), function(x) {
      df <- as.data.frame(an_ar_yr_list[[x]])
      df$region <- x
      df <- df[, c("region", setdiff(names(df), "region"))]
      df
    }))

    write.csv(res_an_ar_yr, file = file.path(
      output_folder_path, "suicides_an_ar_yr_results.csv"), row.names = FALSE)

    res_an_ar_mth <- bind_rows(lapply(names(an_ar_mth_list), function(x) {
      df <- as.data.frame(an_ar_mth_list[[x]])
      df$region <- x
      df <- df[, c("region", setdiff(names(df), "region"))]
      df
    }))

    write.csv(res_an_ar_mth, file = file.path(
      output_folder_path, "suicides_an_ar_mth_results.csv"), row.names = FALSE)

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
#' @param save_fig Boolean. Whether to save the plot as an output. Defaults to
#' FALSE.
#' @param save_csv Boolean. Whether to save the results as a CSV. Defaults to
#' FALSE.
#' @param cenper Integer. Value for the percentile in calculating the centering
#' value 0-100. Defaults to 50.
#' @param country Character. Name of country for national level estimates.
#' @param descriptive_stats Boolean. Whether to calculate descriptive stats.
#' @param ds_correlation_method character. The correlation method used in correlation matrices.
#' Defaults to 'pearson'.
#' @param ds_use_individual_dfs Boolean. Whether to calculate descriptive stats for each individual
#' df in df_list. Default to TRUE.
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
                                      population_col,
                                      var_fun = "bs",
                                      var_degree = 2,
                                      var_per = c(25,50,75),
                                      lag_fun = "strata",
                                      lag_breaks = 1,
                                      lag_days = 2,
                                      save_fig = FALSE,
                                      save_csv = FALSE,
                                      cenper = 50,
                                      country = "National",
                                      descriptive_stats = FALSE,
                                      ds_correlation_method = "pearson",
                                      ds_use_individual_dfs = TRUE,
                                      ds_dist_columns = c(),
                                      ds_ma_days = 100,
                                      ds_ma_sides = 2,
                                      ds_ma_columns = c(),
                                      output_folder_path = NULL) {

  df_list <- mh_read_and_format_data(data_path = data_path,
                             date_col = date_col,
                             region_col = region_col,
                             temperature_col = temperature_col,
                             health_outcome_col = health_outcome_col,
                             population_col = population_col)

  if(descriptive_stats) {
    common_descriptive_stats(
      dataset_title = "mental health",
      df_list = df_list,
      use_individual_dfs = ds_use_individual_dfs,
      output_path = output_folder_path,
      correlation_method = ds_correlation_method,
      dist_columns =  ds_dist_columns,
      ma_days = ds_ma_days,
      ma_sides = ds_ma_sides,
      ma_columns = ds_ma_columns,
      dependent_col = "suicides", # col is renamed in data
      independent_cols = c()
    )
  }

  pop_list <- mh_pop_totals(data = df_list,
                            country = country)

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

  c(minpercreg, maxpercreg) %<-% mh_minmax_suicide_temp(data = df_list,
                                                        var_fun = var_fun,
                                                        var_per = var_per,
                                                        var_degree = var_degree,
                                                        blup = blup)

  pred_list <- mh_predict_reg(data = df_list,
                              var_fun = var_fun,
                              var_per = var_per,
                              var_degree = var_degree,
                              minpercreg = minpercreg,
                              blup = blup)

  c(df_list, cb_list, minpercreg, maxpercreg, mmpredall) %<-% mh_add_national_data(df_list = df_list,
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
                                                                                   minpercreg = minpercreg,
                                                                                   maxpercreg = maxpercreg)

  pred_list <- mh_predict_nat(df_list = df_list,
                              var_fun = var_fun,
                              var_per = var_per,
                              var_degree = var_degree,
                              minpercreg = minpercreg,
                              mmpredall = mmpredall,
                              pred_list = pred_list,
                              country = country)

  mh_plot_rr(df_list = df_list,
             pred_list = pred_list,
             maxpercreg = maxpercreg,
             minpercreg = minpercreg,
             save_fig = save_fig,
             output_folder_path = output_folder_path)

  rr_results <- mh_rr_results(pred_list)

  res_an_ar_tot <- mh_an_ar_totals(df_list = df_list,
                                   cb_list = cb_list,
                                   minpercreg = minpercreg,
                                   maxpercreg = maxpercreg,
                                   pred_list = pred_list,
                                   pop_list = pop_list)

  mh_plot_ar_totals(df_list = df_list,
                    res_an_ar_tot = res_an_ar_tot,
                    save_fig = save_fig,
                    output_folder_path = output_folder_path)

  an_ar_yr_list <- mh_an_ar_yearly(df_list = df_list,
                                   cb_list = cb_list,
                                   minpercreg = minpercreg,
                                   maxpercreg = maxpercreg,
                                   pred_list = pred_list,
                                   pop_list = pop_list)

  mh_plot_an_yearly(an_ar_yr_list = an_ar_yr_list,
                    save_fig = save_fig,
                    output_folder_path = output_folder_path)

  mh_plot_ar_yearly(an_ar_yr_list = an_ar_yr_list,
                    save_fig = save_fig,
                    output_folder_path = output_folder_path)

  an_ar_mth_list <- mh_an_ar_month(df_list = df_list,
                                   cb_list = cb_list,
                                   minpercreg = minpercreg,
                                   maxpercreg = maxpercreg,
                                   pred_list = pred_list,
                                   pop_list = pop_list)

  mh_plot_ar_monthly(an_ar_mth_list = an_ar_mth_list,
                     df_list = df_list,
                     country = country,
                     save_fig = save_fig,
                     output_folder_path = output_folder_path)

  if (save_csv == TRUE) {

    mh_save_results(rr_results = rr_results,
                    res_an_ar_tot = res_an_ar_tot,
                    an_ar_yr_list = an_ar_yr_list,
                    an_ar_mth_list = an_ar_mth_list,
               output_folder_path = output_folder_path)

  }

  return(results)

}
