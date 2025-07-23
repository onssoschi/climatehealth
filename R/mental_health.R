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
#' @export
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
                  suicides = health_outcome_col) %>%
    dplyr::mutate(date = as.Date(date, tryFormats = c("%d/%m/%Y", "%Y-%m-%d")),
                  year = as.factor(lubridate::year(date)),
                  month = as.factor(lubridate::month(date)),
                  dow = as.factor(lubridate::wday(date, label = TRUE)),
                  region = as.factor(region),
                  stratum = as.factor(region:year:month:dow),
                  ind = tapply(suicides, stratum, sum)[stratum])
  df_test <<- df
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
#' @export
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
#' @export
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
#' @export
mh_casecrossover_dlnm <- function(data,
                                  independent_cols = NULL,
                                  cb_list) {

  model_list <- list()

  if (!is.null(independent_cols)) {

    # normalize type
    if (is.character(independent_cols)) {
      independent_cols <- c("cb", independent_cols)
    }

    # type check column names
    for (col in independent_cols){
      if (!is.character(col)){
        stop(
          paste0(
            "'independent_cols' expected a vector of strings or a string. Got",
            typeof(col)
          )
        )
      }
    }
  } else {
    independent_cols = c("cb")
  }

  formula <- as.formula(paste(paste('suicides'),
                              " ~ ",
                              paste(independent_cols,
                                    collapse = " + ")))

  for(reg in names(data)){

    region_data <- data[[reg]]
    cb <- cb_list[[reg]]
    model <- gnm::gnm(formula, eliminate = stratum, family = quasipoisson(), data = region_data,
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
#' @export
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
#' @export
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
#' @export
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
#' @export
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
#' @param maxpercreg Vector. Percentile of maximum suicide temperature for each region.
#'
#' @return A list containing attributable numbers per region
#'
#' @export
mh_attr <- function(df_list,
                    cb_list,
                    pred_list,
                    minpercreg,
                    maxpercreg) {

  attr_list <- list()

  for (reg in names(df_list)){

    region_data <- df_list[[reg]]
    cb <- cb_list[[reg]]
    pred <- pred_list[[reg]]
    minperc <- minpercreg[reg]
    maxperc <- maxpercreg[reg]

    cen <- quantile(region_data$temp, minperc/100, na.rm = TRUE)
    min_range <- quantile(region_data$temp, maxperc/100, na.rm = TRUE)
    max_range <- max(region_data$temp, na.rm = TRUE)

    c(cases, an, an_lower_ci, an_upper_ci)  %<-% an_attrdl(x = region_data$temp,
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
      mutate(max_st = round(min_range, 2),
             lag_suicides = cases,
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
#' @param country Character. Name of country for national level estimates.
#' @param mm A model object. A multivariate meta-analysis model.
#' @param minpercreg Vector. Percentile of maximum suicide temperature for each region.
#' @param maxpercreg Vector. Percentile of maximum suicide temperature for each region.
#'
#' @return
#' \itemize{
#'   \item `df_list` List. A list of data frames for each region and nation.
#'   \item `minpercreg` Vector. Percentile of minimum suicide temperature for each region and nation.
#'   \item `maxpercreg` Vector. Percentile of maximum suicide temperature for each region and nation.
#'   \item `mmpredall` List. A list of national coefficients and covariance matrices.
#'   }
#'
#' @export
mh_add_national_data <- function(df_list,
                                 pop_list,
                                 var_fun = "bs",
                                 var_per = c(25,50,75),
                                 var_degree = 2,
                                 country = "National",
                                 mm,
                                 minpercreg,
                                 maxpercreg,
                                 attr_list){

  # Aggregate national level data

  national_data <- as.data.frame(do.call(rbind, df_list))

  nat_pop <- pop_list[[country]] %>%
    rename(nat_population = population)

  national_data <- national_data %>%
    left_join(nat_pop, by = "year") %>%
    mutate(weight = population / nat_population,
           weighted_temp = temp * weight) %>%
    group_by(date) %>%
    summarise(population = mean(nat_population, na.rm = TRUE),
              temp = round(sum(weighted_temp, na.rm = TRUE), 1),
              suicides = sum(suicides, na.rm = TRUE)) %>%
    mutate(region = country,
           year = as.factor(lubridate::year(date)),
           month = as.factor(lubridate::month(date)))

  df_list[[country]] <- as.data.frame(national_data)

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

  minpercreg[country] <- minpercnat
  maxpercreg[country] <- maxpercnat

  attr_res <- do.call(rbind, attr_list) %>%
    select(date, lag_suicides, an, an_lower_ci, an_upper_ci) %>%
    group_by(date) %>%
    summarise(across(c(lag_suicides, an, an_lower_ci, an_upper_ci),
                     sum, na.rm = TRUE))

  attr_nat <- national_data %>%
    select(region, date, temp, year, month, suicides, population) %>%
    mutate(max_st = quantile(temp, maxpercnat/100, na.rm = TRUE)) %>%
    left_join(attr_res, by = "date") %>%
    mutate(ar = (an / population) * 100000,
           ar_lower_ci = (an_lower_ci / population) * 100000,
           ar_upper_ci = (an_upper_ci / population) * 100000)

  attr_list[[country]] <- attr_nat

  return(list(df_list, minpercreg, maxpercreg, mmpredall, attr_list))

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
#' @export
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
#' @export
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
#' @param minpercreg Vector. Percentile of minimum suicide temperature for each area.
#' @param maxpercreg Vector. Percentile of maximum suicide temperature for each area.
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
                       maxpercreg,
                       minpercreg,
                       country = "National",
                       save_fig = FALSE,
                       output_folder_path = NULL) {

  xlim <- c(min(sapply(pred_list, function(x) min(x$predvar, na.rm = TRUE))),
            max(sapply(pred_list, function(x) max(x$predvar, na.rm = TRUE))))

  ylim <- c(min(c(min(sapply(pred_list, function(x) min(x$allRRfit, na.rm = TRUE))) - 0.5, 0.4)),
            max(c(max(sapply(pred_list, function(x) max(x$allRRfit, na.rm = TRUE))) + 0.5, 2.1)))

  hist_max <- max(sapply(df_list, function(x) {
    hist(x$temp, breaks = seq(floor(xlim[1]), ceiling(xlim[2]), by = 1), plot = FALSE)$counts
  }), na.rm = TRUE)


  if (save_fig==T) {

    grid <- c(3, ceiling(length(pred_list) / 3))

    output_path <- file.path(path_config$output_folder_path, "suicides_rr_plot.pdf")
    pdf(output_path, width=grid[1]*5.5, height=grid[2]*4)

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
         col = "#206095")

    vline_pos_max_x <- quantile(region_temp, maxpercreg[reg]/100, na.rm = TRUE)
    vline_pos_max_y <- max(region_pred$allRRfit, na.rm = TRUE) + 0.3
    vline_lab_max <- paste0("Max ST\n", round(vline_pos_max_x, 2), intToUtf8(176), "C (p", round(maxpercreg[reg], 2), ")")

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

    hist_data <- hist(region_temp,
                      breaks = seq(floor(xlim[1]), ceiling(xlim[2]), by = 1),
                      plot = FALSE)

    hist_scale <- (0.3) / hist_max
    scaled_counts <- hist_data$counts * hist_scale

    for (i in seq_along(hist_data$counts)) {
      rect(xleft = hist_data$breaks[i],
           xright = hist_data$breaks[i + 1],
           ybottom = ylim[1],
           ytop = ylim[1] + scaled_counts[i],
           col = "#118c7b",
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


#' Create attributable estimates tables
#'
#' @description Aggregate tables of attributable numbers, rates and fractions
#' for total, yearly and monthly by region and nation
#'
#'
#' @param attr_list A list containing attributable numbers per region.
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
mh_attr_tables <- function(attr_list,
                           country = "National"){

  attr_res <- do.call(rbind, attr_list) %>%
    mutate(year = as.numeric(as.character(year)))

  res_list <- list()

  groupings <- list(monthly = rlang::quos(month, region),
                    yearly = rlang::quos(year, region),
                    overall = rlang::quos(region))

  for (grp_name in names(groupings)){

    results <- attr_res %>%
      group_by(!!!groupings[[grp_name]]) %>%
      summarise(population = round(mean(population, na.rm = TRUE), 0),
                temp = round(mean(temp, na.rm = TRUE), 2),
                max_st = mean(max_st, na.rm = TRUE),
                across(c(suicides, lag_suicides,
                         an, an_lower_ci, an_upper_ci,
                         ar, ar_lower_ci, ar_upper_ci),
                       sum, na.rm = TRUE)) %>%
      mutate(af = an/lag_suicides * 100,
             af_lower_ci = an_lower_ci/lag_suicides * 100,
             af_upper_ci = an_upper_ci/lag_suicides * 100,
             across(c(an, an_lower_ci, an_upper_ci,
                      ar, ar_lower_ci, ar_upper_ci,
                      af, af_lower_ci, af_upper_ci),
                    ~ ifelse(abs(.) < 1, signif(., 2), round(., 2)))) %>%
      select(-lag_suicides)

    res_list[[grp_name]] <- results

  }

  region_order <- c(sort(setdiff(names(attr_list), country)), country)

  res_attr_tot <- res_list[["overall"]]

  attr_yr_list <- aggregate_by_column(res_list[["yearly"]], "region")
  attr_yr_list <- attr_yr_list[region_order]

  attr_mth_list <- res_list[["monthly"]] %>%
    mutate(month = month.name[month]) %>%
    aggregate_by_column("region")
  attr_mth_list <- attr_mth_list[region_order]

  return(list(res_attr_tot, attr_yr_list, attr_mth_list))

}


#' Plot total attributable fractions and rates
#'
#' @description Plot total attributable fractions and rates over the whole time series by area.
#'
#' @param res_attr_tot Matrix containing total attributable fractions, numbers and rates for each
#' area over the whole time series.
#' @param save_fig Boolean. Whether to save the plot as an output. Defaults to
#' FALSE.
#' @param output_folder_path Path to folder where plots should be saved.
#' Defaults to NULL.
#'
#' @return Plots of total attributable fractions and rates by area
#'
#' @export
mh_plot_attr_totals <- function(df_list,
                              res_attr_tot,
                              save_fig = FALSE,
                              output_folder_path = NULL,
                              country = "National"){

  if (save_fig == TRUE){

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
  bar_col_af <- rep("#206095", length(short_labs_af))
  nat_ind_af <- which(res_af_tot$region == country)
  if (length(nat_ind_af) > 0) {
    bar_col_af[nat_ind_af] <- "#f39431" # Highlight color
  }

  barplot(names.arg = short_labs_af,
          height = res_af_tot$af,
          ylab = "AF (%)",
          main = paste0("Attributable Fraction of Suicides by Area, ", country, " ",  year_range),
          col = bar_col_af,
          las = 2,
          horiz = FALSE)

  mtext(af_warning, side = 1, line = 7, cex = 0.8, col = "red", font = 3)
  mtext(ovr_warning, side = 1, line = 8, cex = 0.8, col = "red", font = 3)

  if (save_fig == TRUE){

  par(mar = c(10, 5, 4, 2))

  }

  # Sort by AF descending
  sorted_indices <- order(res_attr_tot$ar, decreasing = TRUE)
  res_ar_tot <- res_attr_tot[sorted_indices, ]
  short_labs_ar <- short_labels[sorted_indices]

  # Define bar colors
  bar_col_ar <- rep("#118c7b", length(short_labs_ar))
  nat_ind_ar <- which(res_ar_tot$region == country)
  if (length(nat_ind_ar) > 0) {
    bar_col_ar[nat_ind_ar] <- "#f39431" # Highlight color
  }

  barplot(names.arg = short_labs_ar,
          height = res_ar_tot$ar,
          ylab = "AR (per 100,000 population)",
          main = paste0("Attributable Rate of Suicides by Area, ", country, " ", year_range),
          col = bar_col_ar,
          las = 2,
          horiz = FALSE)

  mtext(ar_warning, side = 1, line = 7, cex = 0.8, col = "red", font = 3)
  mtext(ovr_warning, side = 1, line = 8, cex = 0.8, col = "red", font = 3)

  if (save_fig == TRUE){

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
#'
#' @return Plots of yearly attributable fractions per area
#'
#' @export
mh_plot_af_yearly <- function(attr_yr_list,
                              save_fig = FALSE,
                              output_folder_path = NULL,
                              country = "National"){

  if (save_fig == TRUE){

    grid <- c(3, ceiling(length(attr_yr_list) / 3))
    output_path <- file.path(output_folder_path, "suicides_af_timeseries.pdf")
    pdf(output_path, width = grid[1]*5.5, height = grid[2]*4.5)

    par(mfrow=c(grid[2], grid[1]), oma = c(0, 0, 4, 0), mar = c(8, 4, 5, 4))

  }

  year_min <- min(sapply(attr_yr_list, function(x) min(x$year, na.rm = TRUE)))
  year_max <- max(sapply(attr_yr_list, function(x) max(x$year, na.rm = TRUE)))

  y_min <- min(sapply(attr_yr_list, function(x) min(x$af, na.rm = TRUE)))
  y_max <- max(sapply(attr_yr_list, function(x) max(x$af, na.rm = TRUE)))

  ylim <- c(min(c(0, y_min)), y_max) * 1.5

  for (reg in names(attr_yr_list)){

    region_af <- as.data.frame(attr_yr_list[[reg]])

    plot(x = region_af$year,
         y = region_af$af,
         type = "l",
         xlim = c(year_min, year_max),
         ylim = ylim,
         xlab = "Year",
         ylab = "AF (%)",
         main = reg,
         col = "#206095")

    # Ensure data is sorted by Year
    region_af <- region_af[order(region_af$year), ]

    # Create x and y coordinates for the polygon
    x_poly <- c(region_af$year, rev(region_af$year))
    y_poly <- c(region_af$af_upper_ci, rev(region_af$af_lower_ci))

    # Draw shaded confidence interval
    polygon(x = x_poly,
            y = y_poly,
            col = adjustcolor("#206095", alpha.f = 0.2),
            border = NA)

    abline(h = 0,
      col = "black",
      lty = 2)

    legend("topright",
           inset = c(0, -0.1),
           legend = "95% CI",
           col = adjustcolor("#206095", alpha.f = 0.2),
           pch = 15,
           pt.cex = 2,
           bty = "n",
           xpd = TRUE,
           horiz = TRUE,
           cex = 0.9)

    if (save_fig == TRUE){

      af_ci_range <- c(min(region_af$af_lower_ci), max(region_af$af_upper_ci))

      if (af_ci_range[1] < ylim[1] || af_ci_range [2] > ylim[2]){

        ci_warning <- sprintf("Warning: CI's are outside the bounds of this chart. CI's range from %.2f%% to %.2f%%", af_ci_range[1], af_ci_range[2])
        ovr_warning <- "(Please refer to the associated data table for more information on the uncertainty around each estimate)"

        mtext(ci_warning, side = 1, line = 5, cex = 0.6, col = "red", font = 3)
        mtext(ovr_warning, side = 1, line = 6, cex = 0.6, col = "red", font = 3)

      }

    }

  }

  if (save_fig == TRUE){

    year_range <- paste0("(", year_min, " - ", year_max, ")")
    title <- paste0("Yearly Attributable Fraction of Suicide by Area, ", country, " ",  year_range)

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
#'
#' @return Plots of yearly attributable rates per area
#'
#' @export
mh_plot_ar_yearly <- function(attr_yr_list,
                              save_fig = FALSE,
                              output_folder_path = NULL,
                              country = "National"){

  if (save_fig == TRUE){

    grid <- c(3, ceiling(length(attr_yr_list) / 3))
    output_path <- file.path(output_folder_path, "suicides_ar_timeseries.pdf")
    pdf(output_path, width = grid[1]*5.5, height = grid[2]*4.5)

    par(mfrow=c(grid[2], grid[1]), oma = c(0, 0, 4, 0), mar = c(8, 4, 5, 4))

  }

  year_min <- min(sapply(attr_yr_list, function(x) min(x$year, na.rm = TRUE)))
  year_max <- max(sapply(attr_yr_list, function(x) max(x$year, na.rm = TRUE)))

  y_min <- min(sapply(attr_yr_list, function(x) min(x$ar, na.rm = TRUE)))
  y_max <- max(sapply(attr_yr_list, function(x) max(x$ar, na.rm = TRUE)))

  ylim <- c(min(c(0, y_min)), y_max) * 1.5

  for (reg in names(attr_yr_list)){

    region_ar <- as.data.frame(attr_yr_list[[reg]])

    plot(x = region_ar$year,
         y = region_ar$ar,
         type = "l",
         xlim = c(year_min, year_max),
         ylim = ylim,
         xlab = "Year",
         ylab = "AR (per 100,000 population)",
         main = reg,
         col = "#118c7b")

    # Ensure data is sorted by Year
    region_ar <- region_ar[order(region_ar$year), ]

    # Create x and y coordinates for the polygon
    x_poly <- c(region_ar$year, rev(region_ar$year))
    y_poly <- c(region_ar$ar_upper_ci, rev(region_ar$ar_lower_ci))

    # Draw shaded confidence interval
    polygon(x = x_poly,
            y = y_poly,
            col = adjustcolor("#118c7b", alpha.f = 0.2),
            border = NA)

    abline(h = 0,
           col = "black",
           lty = 2)

    legend("topright",
           inset = c(0, -0.1),
           legend = "95% CI",
           col = adjustcolor("#118c7b", alpha.f = 0.2),
           pch = 15,
           pt.cex = 2,
           bty = "n",
           xpd = TRUE,
           horiz = TRUE,
           cex = 0.9)

    if (save_fig == TRUE){

      ar_ci_range <- c(min(region_ar$ar_lower_ci), max(region_ar$ar_upper_ci))

      if (ar_ci_range[1] < ylim[1] || ar_ci_range [2] > ylim[2]){

        ci_warning <- sprintf("Warning: CI's are outside the bounds of this chart. CI's range from %.2f to %.2f per 100,000", ar_ci_range[1], ar_ci_range[2])
        ovr_warning <- "(Please refer to the associated data table for more information on the uncertainty around each estimate)"

        mtext(ci_warning, side = 1, line = 5, cex = 0.6, col = "red", font = 3)
        mtext(ovr_warning, side = 1, line = 6, cex = 0.6, col = "red", font = 3)

      }

    }

  }

  if (save_fig == TRUE){

    year_range <- paste0("(", year_min, " - ", year_max, ")")
    title <- paste0("Yearly Attributable Rate of Suicide by Area, ", country, " ",  year_range)

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
#' @param save_fig Boolean. Whether to save the plot as an output. Defaults to
#' FALSE.
#' @param output_folder_path Path to folder where plots should be saved.
#' Defaults to NULL.
#'
#' @return Plots of attributable fractions by calendar month per area
#'
#' @export
mh_plot_af_monthly <- function(attr_mth_list,
                               df_list,
                               country = "National",
                               maxpercreg,
                               save_fig = FALSE,
                               output_folder_path = NULL){

  if (save_fig == TRUE){

    grid <- c(3, ceiling(length(attr_mth_list) / 3))
    output_path <- file.path(output_folder_path, "suicides_af_month_plot.pdf")
    pdf(output_path, width = grid[1]*4.5, height = grid[2]*4.5)

    par(mfrow=c(grid[2], grid[1]), mar = c(5, 5, 5, 5), oma = c(4, 0, 4, 0))

  }

  ylim_max <- max(sapply(attr_mth_list, function(x) max(x$af, na.rm = TRUE)))

  ylim2_min <- min(sapply(attr_mth_list, function(x) min(x$temp, na.rm = TRUE)))
  ylim2_max <- max(sapply(attr_mth_list, function(x) max(x$temp, na.rm = TRUE)))

  scale_factor <- (1 / ylim2_max) * ylim_max

  temp_ticks <- pretty(c(ylim2_min, ylim2_max))

  ylim <- c(min(0, temp_ticks[1] * scale_factor), max(temp_ticks[length(temp_ticks)] * scale_factor, ylim_max))

  for (reg in names(attr_mth_list)){

    region_af <- attr_mth_list[[reg]]

    temp_scaled <- region_af$temp * scale_factor

    bar_pos <- barplot(names.arg = substr(region_af$month, 1, 1),
            height = region_af$af,
            ylim = ylim,
            xlab = "Month",
            ylab = "AF (%)",
            main = reg,
            col = "#206095")

    lines(x = bar_pos,
          y = temp_scaled,
          type = "o",
          col = "#871a5b",
          pch = 16)

    # Add secondary axis on the right

    axis(side = 4,
         at = temp_ticks * scale_factor,
         labels = temp_ticks,
         col.axis = "black",
         col = "black")

    mtext("Mean Temp (°C)", side = 4, line = 3, col = "black", cex = 0.7)

    abline(h = 0,
           col = "black",
           lty = 1)

    maxst <- unique(region_af$max_st)
    af_leg_lab <- paste0("AF (%) - from MaxST ", maxst, "°C (", maxpercreg[reg], "p)")

    legend("topleft",
           inset = c(0, -0.05),
           legend = c(af_leg_lab, "Mean Temp (°C)"),
           fill = c("#206095", NA),
           border = NA,
           lty = c(NA, 1),
           pch = c(NA, 16),
           col = c("#206095", "#871a5b"),
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

    title <- paste0("Mean Attributable Fraction of Suicide by Calendar Month and Area, ", country, " ",  year_range)

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
#' @param save_fig Boolean. Whether to save the plot as an output. Defaults to
#' FALSE.
#' @param output_folder_path Path to folder where plots should be saved.
#' Defaults to NULL.
#'
#' @return Plots of attributable rates by calendar month per area
#'
#' @export
mh_plot_ar_monthly <- function(attr_mth_list,
                               df_list,
                               country = "National",
                               maxpercreg,
                               save_fig = FALSE,
                               output_folder_path = NULL){

  if (save_fig == TRUE){

    grid <- c(3, ceiling(length(attr_mth_list) / 3))
    output_path <- file.path(output_folder_path, "suicides_ar_month_plot.pdf")
    pdf(output_path, width = grid[1]*4.5, height = grid[2]*4.5)

    par(mfrow=c(grid[2], grid[1]), mar = c(5, 5, 5, 5), oma = c(4, 0, 4, 0))

  }

  ylim_max <- max(sapply(attr_mth_list, function(x) max(x$ar, na.rm = TRUE)))

  ylim2_min <- min(sapply(attr_mth_list, function(x) min(x$temp, na.rm = TRUE)))
  ylim2_max <- max(sapply(attr_mth_list, function(x) max(x$temp, na.rm = TRUE)))

  scale_factor <- (1 / ylim2_max) * ylim_max

  temp_ticks <- pretty(c(ylim2_min, ylim2_max))

  ylim <- c(min(0, temp_ticks[1] * scale_factor), max(temp_ticks[length(temp_ticks)] * scale_factor, ylim_max))

  for (reg in names(attr_mth_list)){

    region_ar <- attr_mth_list[[reg]]

    temp_scaled <- region_ar$temp * scale_factor

    bar_pos <- barplot(names.arg = substr(region_ar$month, 1, 1),
                       height = region_ar$ar,
                       ylim = ylim,
                       xlab = "Month",
                       ylab = "AR (per 100,000 population)",
                       main = reg,
                       col = "#118c7b")

    lines(x = bar_pos,
          y = temp_scaled,
          type = "o",
          col = "#871a5b",
          pch = 16)

    axis(side = 4,
         at = temp_ticks * scale_factor,
         labels = temp_ticks,
         col.axis = "black",
         col = "black")

    mtext("Mean Temp (°C)", side = 4, line = 3, col = "black", cex = 0.7)

    abline(h = 0,
           col = "black",
           lty = 1)

    maxst <- unique(region_ar$max_st)
    ar_leg_lab <- paste0("AR - from MaxST ", maxst, "°C (", maxpercreg[reg], "p)")

    legend("topleft",
           inset = c(0, -0.05),
           legend = c(ar_leg_lab, "Mean Temp (°C)"),
           fill = c("#118c7b", NA),
           border = NA,
           lty = c(NA, 1),
           pch = c(NA, 16),
           col = c("#118c7b", "#871a5b"),
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

    title <- paste0("Attributable Rate of Suicide by Calendar Month and Area, ", country, " ",  year_range)

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
#' @param results Dataframe containing cumulative relative risk and confidence
#' intervals from analysis.
#' @param output_folder_path Path to folder where results should be saved.
#' Defaults to NULL.
#'
#' @export
mh_save_results <- function(rr_results,
                            res_attr_tot,
                            attr_yr_list,
                            attr_mth_list,
                            output_folder_path = NULL) {

  if (!is.null(output_folder_path)) {

    check_file_exists(file.path(output_folder_path))

    write.csv(rr_results, file = file.path(
      output_folder_path, "suicides_rr_results.csv"), row.names = FALSE)

    write.csv(res_attr_tot, file = file.path(
      output_folder_path, "suicides_attr_tot_results.csv"), row.names = FALSE)

    res_attr_yr <- do.call(rbind, attr_yr_list) %>%
      select(region, everything())

    write.csv(res_attr_yr, file = file.path(
      output_folder_path, "suicides_attr_yr_results.csv"), row.names = FALSE)

    res_attr_mth <- do.call(rbind, attr_mth_list) %>%
      select(region, everything())

    write.csv(res_attr_mth, file = file.path(
      output_folder_path, "suicides_attr_mth_results.csv"), row.names = FALSE)

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

  attr_list <- mh_attr(df_list = df_list,
                       cb_list = cb_list,
                       pred_list = pred_list,
                       minpercreg = minpercreg,
                       maxpercreg = maxpercreg)

  c(df_list, minpercreg, maxpercreg, mmpredall, attr_list) %<-% mh_add_national_data(df_list = df_list,
                                                                                     pop_list = pop_list,
                                                                                     var_fun = var_fun,
                                                                                     var_per = var_per,
                                                                                     var_degree = var_degree,
                                                                                     country = country,
                                                                                     mm = mm,
                                                                                     minpercreg = minpercreg,
                                                                                     maxpercreg = maxpercreg,
                                                                                     attr_list = attr_list)

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
             country = country,
             save_fig = save_fig,
             output_folder_path = output_folder_path)

  rr_results <- mh_rr_results(pred_list)

  c(res_attr_tot, attr_yr_list, attr_mth_list) %<-% mh_attr_tables(attr_list = attr_list,
                                                                   country = country)

  res_attr_tot <- mh_attr_totals(df_list = df_list,
                                   cb_list = cb_list,
                                   minpercreg = minpercreg,
                                   maxpercreg = maxpercreg,
                                   pred_list = pred_list,
                                   pop_list = pop_list)

  mh_plot_attr_totals(df_list = df_list,
                    res_attr_tot = res_attr_tot,
                    save_fig = save_fig,
                    output_folder_path = output_folder_path,
                    country = country)

  attr_yr_list <- mh_attr_yearly(df_list = df_list,
                                   cb_list = cb_list,
                                   minpercreg = minpercreg,
                                   maxpercreg = maxpercreg,
                                   pred_list = pred_list,
                                   pop_list = pop_list)

  mh_plot_af_yearly(attr_yr_list = attr_yr_list,
                    save_fig = save_fig,
                    output_folder_path = output_folder_path,
                    country = country)

  mh_plot_ar_yearly(attr_yr_list = attr_yr_list,
                    save_fig = save_fig,
                    output_folder_path = output_folder_path)

  attr_mth_list <- mh_attr_month(df_list = df_list,
                                   cb_list = cb_list,
                                   minpercreg = minpercreg,
                                   maxpercreg = maxpercreg,
                                   pred_list = pred_list,
                                   pop_list = pop_list)

  mh_plot_af_monthly(attr_mth_list = attr_mth_list,
                     df_list = df_list,
                     country = country,
                     maxpercreg = maxpercreg,
                     save_fig = save_fig,
                     output_folder_path = output_folder_path)

  mh_plot_ar_monthly(attr_mth_list = attr_mth_list,
                     df_list = df_list,
                     country = country,
                     maxpercreg = maxpercreg,
                     save_fig = save_fig,
                     output_folder_path = output_folder_path)

  if (save_csv == TRUE) {

    mh_save_results(rr_results = rr_results,
                    res_attr_tot = res_attr_tot,
                    attr_yr_list = attr_yr_list,
                    attr_mth_list = attr_mth_list,
               output_folder_path = output_folder_path)

  }

  return(results)

}
