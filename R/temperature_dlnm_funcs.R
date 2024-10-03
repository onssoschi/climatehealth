library(dlnm)
library(mvmeta)
library(splines)
library(tsModel)
library(zeallot)
library(FluMoDL)

#' Load data for analysis
#'
#' @description Loads data and names of regions for analysis from a CSV file.
#'
#' @param input_csv_path Path to a CSV containing a
#' daily time series of death and temperature per region.
#' @param dependent_col the column name of the
#' dependent variable of interest e.g. deaths
#' @param time_col Time column e.g. date
#' @param region_col The region column over which the data
#' are spatially aggregated e.g. regnames
#' @param temp_col The temperature column e.g. tmean
#' @param population_col The population column e.g. pop
#' @param output_year_ Year(s) to calculate output for.
#' @param RR_distribution_length Number of years for the calculation of RR distribution. Set both as 'NONE' to use full range in data.
#' @return `df_list` An alphabetically-ordered list of dataframes for each
#' region comprising dates, deaths, and temperatures.
#' @export
load_data <- function(input_csv_path,
                      dependent_col,
                      time_col,
                      region_col,
                      temp_col,
                      population_col,
                      output_year,
                      RR_distribution_length) {

  if (is.list(input_csv_path) == TRUE) {
    df <- data.frame(input_csv_path)

    df <- df %>%
      dplyr::rename(dependent = dependent_col,
                    date = time_col,
                    regnames = region_col,
                    temp = temp_col,
                    pop_col = population_col)

  } else if ((is.character(input_csv_path) == TRUE) && (!population_col == 'NONE')) {
    df <- read.csv(input_csv_path, row.names = 1) %>%
      dplyr::rename(dependent = dependent_col,
                    date = time_col,
                    regnames = region_col,
                    temp = temp_col,
                    pop_col = population_col)

  } else if ((is.character(input_csv_path) == TRUE) && (population_col == 'NONE')) {

    df <- read.csv(input_csv_path, row.names = 1) %>%
      dplyr::mutate(pop_col = 'NONE') %>%
      dplyr::rename(dependent = dependent_col,
                    date = time_col,
                    regnames = region_col,
                    temp = temp_col)

  } else {
    # Raise an error when the input_csv argument isn't valid
    stop("The value passed for 'input_csv' is invalid.")
  }

  # Reformat data and fill NaNs
  df <- df %>%
    dplyr::mutate(date = as.Date(date,
                                 tryFormats = c("%Y-%m-%d", "%d/%m/%Y"))) %>%
    dplyr::mutate(dependent = ifelse(is.na(dependent), 0, dependent)) %>%
    dplyr::mutate(year = as.numeric(format(date, "%Y")))

  if (output_year == 0) {

    output_year <- max(df$year, na.rm = TRUE)

  }

  # Calculate the RR distribution length if it is 0
  if (RR_distribution_length == 0) {

    RR_distribution_length = max(df$year, na.rm = TRUE) - min(df$year, na.rm = TRUE)

  }

  # Raise an error if the RR distribution length is out of range (5-15)
  if(RR_distribution_length < 5) {

    stop("Timeseries to calculate the RR is less than 5 years.")

  } else if (RR_distribution_length > 15) {

    stop("Timeseries to calculate the RR is more than 15 years.")

  }


  df <- df %>%
    dplyr::filter(year >= (max(as.integer(output_year)) - RR_distribution_length + 1)
                  & year <= max(as.integer(output_year)))

  regions <- sort(as.character(unique(df$regnames)))

  df_list <- lapply(regions,
                    function(x)
                      df %>%
                      dplyr::filter(regnames == x))

  names(df_list) <- regions

  return (list(df_list))

}



#' Define regression model
#'
#' @param dataset dataframe with temp column to be modelled
#' @param independent_cols column name (or list of names) of extra independent
#' variable to include in regression (excluding temperature). Defaults to NULL.
#' @param varfun Exposure function
#' (see dlnm::crossbasis)
#' @param varper Internal knot positions in exposure function
#' (see dlnm::crossbasis)
#' @param vardegree Degrees of freedom in exposure function
#' (see dlnm:crossbasis)
#' @param lag Lag length in time
#' (see dlnm::logknots)
#' @param lagnk Number of knots in lag function
#' (see dlnm::logknots)
#' @param dfseas Degrees of freedom for seasonality
#' @return
#' \itemize{
#'   \item `model` A quasi-poission generalised linear model object.
#'   See: https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/glm
#'   \item `cb` Basis matrices for the two dimensions of predictor and lags.
#'   }
#' @export
define_model <- function(dataset,
                         independent_cols = NULL,
                         varfun,
                         varper,
                         vardegree,
                         lag,
                         lagnk,
                         dfseas) {
  # define the base independent cols
  base_independent_cols <- c(
    'cb',
    'splines::ns(date, df = dfseas * length(unique(year)))'
  )

  if (!is.null(independent_cols)) {

    # normalize type
    if (is.character(independent_cols)) {
      independent_cols <- c(independent_cols)
    }

    # type check column names
    for (col in independent_cols){
      if (!is.character(col)){
        stop(
          cat(
            "'independent_cols' expected a vector of strings or a string. Got",
            typeof(i)
          )
        )
      }
    }
  } else {
    independent_cols = c()
  }
  # Join on user-defined independent cols
  independent_cols <- c(base_independent_cols, independent_cols)

  # Model formula
  formula <- as.formula(paste(paste('dependent'),
                              " ~ ",
                              paste(independent_cols,
                                    collapse = " + ")))

  # Define crossbasis
  argvar_ <- list(fun = varfun,
                  knots = quantile(dataset$temp,
                                   varper / 100,
                                   na.rm = TRUE),
                  degree = vardegree)

  lag <- as.numeric(lag)
  lagnk <- as.numeric(lagnk)
  dfseas <- as.numeric(dfseas)

  cb <- dlnm::crossbasis(dataset$temp,
                   lag = lag,
                   argvar = argvar_,
                   arglag = list(knots = dlnm::logknots(lag,
                                                  lagnk)))

  # Run the model and obtain predictions
  model <- glm(formula,
                      dataset,
                      family = quasipoisson,
                      na.action = "na.exclude")

  return (list(model, cb))

}

#' Define and run poisson regression model for each dataframe
#'
#' @param df_list An alphabetically-ordered list of dataframes for each region.
#' @param independent_cols column name (or list of names) of extra independent
#' variable to include in regression (excluding temperature). Defaults to NULL.
#' @param varfun Exposure function
#' (see dlnm::crossbasis)
#' @param varper Internal knot positions in exposure function
#' (see dlnm::crossbasis)
#' @param vardegree Degrees of freedom in exposure function
#' (see dlnm:crossbasis)
#' @param lag Lag length in time
#' (see dlnm::logknots)
#' @param lagnk Number of knots in lag function
#' (see dlnm::logknots)
#' @param dfseas Degrees of freedom for seasonality
#' @return
#' \itemize{
#'   \item `coef` A matrix of coefficients for reduced model.
#'   \item `vcov` A list. Co-variance matrices for each region for reduced model.
#'   }
#' @export
run_model <- function(df_list,
                      independent_cols = NULL,
                      varfun,
                      varper,
                      vardegree,
                      lag,
                      lagnk,
                      dfseas) {

  minperregions <- mintempregions <- rep(NA,
                                         length(df_list))

  # Loop
  timer <- proc.time()[3]

  # Coefficients and vcov for overall cumulative summary
  coef_ <- matrix(NA,
                 length(names(df_list)),
                 length(varper) + vardegree,
                 dimnames = list(names(df_list)))

  vcov_ <- vector("list", length(names(df_list)))

  names(vcov_) <- names(df_list)

  for(i in seq(length(df_list))) {
    # Extract data
    data <- df_list[[i]]

    c(model, cb) %<-% define_model(dataset = data,
                                   independent_cols=independent_cols,
                                   varfun = varfun,
                                   varper = varper,
                                   vardegree = vardegree,
                                   lag = lag,
                                   lagnk = lagnk,
                                   dfseas = dfseas)

    cen_ <- mean(data$temp, na.rm = TRUE)

    # Reduction to overall cumulative
    pred <- dlnm::crossreduce(cb, model, cen = cen_)
    mintempregions[i] <- as.numeric(names(which.min(pred$RRfit)))

    coef_[i,] <- coef(pred)
    vcov_[[i]] <- vcov(pred)

  }

  proc.time()[3]-timer

  return (list(coef_, vcov_))
}


#' Meta-analysis model
#'
#' Runs meta-analysis model and estimates best linear unbiased predictions
#' (BLUPs) from this model.
#'
#' @param df_list An alphabetically-ordered list of dataframes for each region.
#' @param coef A matrix of coefficients for reduced model.
#' @param vcov A list. Co-variance matrices for each region for reduced model.
#'
#' @return
#' \itemize{
#'   \item `mvmeta` A model object. A multivariate meta-analysis model.
#'   \item `blup` A list. BLUP (best linear unbiased predictions) from the
#'   meta-analysis model for each region.
#'   }
#' @export
run_meta_model <- function(df_list, coef, vcov) {

  # Assert that df_list is a list of dataframes.
  if (is.list(df_list)) {
    for (df in df_list){
      if (!is.data.frame(df)) {
        stop(paste(
          "'df_list' expected a list of dataframes. List contains item of",
          "type", toString(typeof(df))
          )
        )
      }
    }
  }
  else {
    stop(paste("'df_list' expected a list. Got", class(df_list)))
  }

  # Assert that coef is a numeric matrix
  if(!is.matrix(coef)) {
    stop("Argument 'coef' must be a numeric matrix")
  }
  else {
    if (!is.numeric(coef)) {
      stop("Argument 'coef' must be a numeric matrix")
    }
  }

  # Assert that vcov is a list of matrices.
  # TODO: Functionalise this functionality into a defences module
  if (is.list(vcov)) {
    for (matr in vcov){
      if (!is.matrix(matr)) {
        stop(paste(
          "'vcov' expected a list of matrices. List contains item of",
          "type", toString(typeof(matr))
        )
        )
      }
    }
  }
  else {
    stop(paste("'vcov' expected a list. Got", toString(typeof(vcov))))
  }


  # Create average temperature and range as meta-predictors
  avgtmean <- sapply(df_list,
                     function(x)
                       mean(x$temp, na.rm = TRUE))

  rangetmean <- sapply(df_list,
                       function(x)
                         diff(range(x$temp, na.rm = TRUE)))

  # Meta-analysis
  # NB: country effects is not included in this example
  mv <- mvmeta::mvmeta(coef ~ avgtmean + rangetmean,
               vcov,
               data = as.data.frame(names(df_list)), # was data = regions_df
               control = list(showiter = TRUE))

  # Obtain blups
  blup <- mvmeta::blup(mv, vcov = TRUE)

  return(list(mv, blup))
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
fwald <- function(model, var) {

   if(!is.character(var)) {
     stop("Argument 'var' must be a character")
   }

   ind <- grep(var, names(coef(model)))
   coef <- coef(model)[ind]
   vcov <- vcov(model)[ind, ind]
   waldstat <- coef %*% solve(vcov) %*% coef
   df <- length(coef)

   return(1 - pchisq(waldstat, df))

 }

#' Get Wald statistic for a meta-analysis model
#'
#' @param mv A model object (multivariate meta-analysis model)
#'
#' @return P-values for average and range of temperatures
#' (avgtmean_wald, rangetmean_wald).
#' @export
wald_results <- function(mv) {

  avgtmean_wald <- fwald(mv, "avgtmean")
  rangetmean_wald <- fwald(mv, "rangetmean")

  return(list(avgtmean_wald, rangetmean_wald))

}

#' Calculate minimum mortality values
#'
#' Calculate the temperature at which there is minimum mortality
#' using the product of the basis matrix and blup
#'
#' @param df_list An alphabetically-ordered list of dataframes for each region.
#' @param blup A list of BLUPs (best linear unbiased predictions).
#' @param independent_cols column name (or list of names) of extra independent
#' variable to include in regression (excluding temperature). Defaults to NULL.
#' @param varfun Exposure function
#' (see dlnm::crossbasis)
#' @param varper Internal knot positions in exposure function
#' (see dlnm::crossbasis)
#' @param vardegree Degrees of freedom in exposure function
#' (see dlnm:crossbasis)
#' @param lag Lag length in time
#' (see dlnm::logknots)
#' @param lagnk Number of knots in lag function
#' (see dlnm::logknots)
#' @param dfseas Degrees of freedom for seasonality
#'
#' @return
#' \itemize{
#'   \item `mintempregions_` A named numeric vector.
#'   Minimum (optimum) mortality temperature per region.
#'    \item `an_thresholds` A dataframe with the optimal temperature range and
#' temperature thresholds for calculation of attributable deaths.
#' }
#'
#' @export
calculate_min_mortality_temp <-  function(df_list,
                                          blup = NULL,
                                          independent_cols = NULL,
                                          varfun,
                                          varper,
                                          vardegree,
                                          lag,
                                          lagnk,
                                          dfseas) {

  # TODO: functionalise this since it is now repeated code
  if (is.list(df_list)) {
    for (df in df_list){
      if (!is.data.frame(df)) {
        stop(paste(
          "'df_list' expected a list of dataframes. List contains item of",
          "type", toString(typeof(df))
        )
        )
      }
    }
  }
  else {
    stop(paste("'df_list' expected a list. Got", class(df_list)))
  }

  if (!is.null(blup) && !is.list(blup)) {
    stop("Argument 'blup' must be a list")
  }

  # Re-centering
  # Generate the matrix for storing results
  minpercregions_ <- mintempregions_ <- rep(NA,
                                            length(df_list))
  names(mintempregions_) <- names(minpercregions_) <- names(df_list)

  optimal_temp_range <- matrix(NA,
                               length(df_list),
                               2,
                               dimnames = list(names(df_list),
                                               c("lower","upper")))

  ranges <- t(sapply(df_list, function(x)
    range(x$temp,na.rm=T)))

  if (!is.null(blup)) {

    # Define minimum mortality values: exclude low and very hot temperatures
    for(i in seq(length(df_list))) {

      data <- df_list[[i]]
      predvar <- quantile(data$temp, 1:99 / 100, na.rm = TRUE)

      # Redefine the function using all arguments (boundary knots included)
      argvar_ <- list(x = predvar, fun = varfun,
                   knots = quantile(data$temp,
                                    varper / 100,
                                    na.rm = TRUE),
                   degree = vardegree,
                   Bound = range(data$temp, na.rm = TRUE))

      bvar_ <- do.call(dlnm::onebasis, argvar_)

      minpercregions_[i] <- (1:99)[which.min(bvar_ %*%
                                                blup[[i]]$blup)]
      mintempregions_[i] <- quantile(data$temp,
                                     minpercregions_[i] / 100,
                                     na.rm = TRUE)

      # OVERALL CUMULATIVE SUMMARY ASSOCIATION FOR MAIN MODEL
      cp <- dlnm::crosspred(bvar_,
                      coef = blup[[i]]$blup,
                      vcov = blup[[i]]$vcov,
                      cen = mintempregions_[i],
                      model.link = "log",
                      by = 0.1,
                      from = ranges[i,1],
                      to = ranges[i,2])

      optimal_temp_range[i,"lower"] <- as.numeric(names(
        which.min(which(cp$allRRfit >= 1 & cp$allRRfit <= 1.1))))
      optimal_temp_range[i, "upper"] <- as.numeric(names(
        which.max(which(cp$allRRfit >= 1 & cp$allRRfit <= 1.1))))

      below_one <- which(cp$allRRfit < 1)
      above_OTR <- which(as.numeric(names(cp$allRRfit)) > optimal_temp_range[i, "upper"])
      below_OTR <- which(as.numeric(names(cp$allRRfit))< optimal_temp_range[i, "lower"])

      if (length(which((below_one %in% above_OTR) | (below_one %in% below_OTR))) > 0) {
        print(warning("Predicted RR goes below 1 in the ends"))
      }

    }

  } else {

    for(i in seq(length(df_list))) {

      # Extract data
      data <- df_list[[i]]

      c(model, cb) %<-% define_model(dataset = data,
                                     independent_cols = independent_cols,
                                     varfun = varfun,
                                     varper = varper,
                                     vardegree = vardegree,
                                     lag = lag,
                                     lagnk = lagnk,
                                     dfseas = dfseas)

      cen_ <- mean(data$temp, na.rm = TRUE)

      # Reduction to overall cumulative
      pred <- dlnm::crossreduce(cb, model, cen = cen_)
      mintempregions_[i] <- as.numeric(names(which.min(pred$RRfit)))

      cen_ <- mintempregions_[i]
      pred <- dlnm::crossreduce(cb, model, cen = cen_)

      optimal_temp_range[i,"lower"] <- as.numeric(names(
        which.min(which(pred$RRfit >= 1 & pred$RRfit <= 1.1))))
      optimal_temp_range[i, "upper"] <- as.numeric(names(
        which.max(which(pred$RRfit >= 1 & pred$RRfit <= 1.1))))

      below_one <- which(pred$RRfit < 1)
      above_OTR <- which(as.numeric(names(pred$RRfit)) > optimal_temp_range[i, "upper"])
      below_OTR <- which(as.numeric(names(pred$RRfit))< optimal_temp_range[i, "lower"])

      if (length(which((below_one %in% above_OTR) | (below_one %in% below_OTR))) > 0) {
        print(warning("Predicted RR goes below 1 in the ends"))
      }

    }

  }

  per <- t(sapply(df_list, function(x)
    quantile(x$temp, c(2.5, 97.5)/100, na.rm = T)))

  # data frame with final thresholds to use for hot and cold days to attribute deaths to
  an_thresholds <- as.data.frame(cbind(per,optimal_temp_range)) %>%
    dplyr::mutate(
      min_high_cold = -100,
      max_high_heat = 100,
      moderate_cold_OTR = lower,
      moderate_heat_OTR = upper,
      high_moderate_cold = ifelse(moderate_cold_OTR < `2.5%`,
                                  moderate_cold_OTR,
                                  `2.5%`),
      high_moderate_heat = ifelse(moderate_heat_OTR > `97.5%`,
                                  moderate_heat_OTR,
                                  `97.5%`)) %>%
    dplyr::select(min_high_cold, high_moderate_cold, moderate_cold_OTR,
                  moderate_heat_OTR, high_moderate_heat, max_high_heat)

  # Country-specific points of minimum mortality
  (minperccountry <- median(minpercregions_))

  return(list(mintempregions = mintempregions_, an_thresholds))

}

#' Compute attributable deaths
#'
#' Compute the attributable deaths for each regions,
#' with empirical CI estimated using the re-centered bases.
#'
#' @param df_list An alphabetically-ordered list
#' of dataframes for each region.
#' @param blup A list of BLUPs (best linear unbiased predictions).
#' @param mintempregions A named numeric vector.
#' Minimum (optimum) mortality temperature per region.
#' @param an_thresholds A dataframe with the optimal temperature range and
#' temperature thresholds for calculation of attributable deaths.
#' @param independent_cols column name (or list of names) of extra independent
#' variable to include in regression (excluding temperature). Defaults to NULL.
#' @param varfun Exposure function
#' (see dlnm::crossbasis)
#' @param varper Internal knot positions in exposure function
#' (see dlnm::crossbasis)
#' @param vardegree Degrees of freedom in exposure function
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
#' @export
compute_attributable_deaths <- function(df_list,
                                        output_year,
                                        blup = NULL,
                                        mintempregions,
                                        an_thresholds,
                                        independent_cols = NULL,
                                        varfun,
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

    #############################################
    # Return heat attributable deaths for the output year

    data_output_year <- data %>% dplyr::filter(year %in% output_year) %>%
      dplyr::mutate(high_heat_flag = ifelse(temp > an_thresholds[i,"high_moderate_heat"],1, 0))

    # Prepare temperature column for attribution to heatwaves
    # Force the temperature to be the centering value for non-heatwave days
    data_output_year$heatwave_flag <- NA
    for (j in seq(nrow(data_output_year))){

      if(j==1){

        data_output_year$heatwave_flag[j] <-
          ifelse(data_output_year$high_heat_flag[j] == 1 &
                   data_output_year$high_heat_flag[j+1] == 1, 1, 0)

      } else if (j==nrow(data_output_year)){

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
      dplyr::mutate(heatwave_temp = ifelse(heatwave_flag == 1, temp, mintempregions[i])) %>%
      dplyr::select(-high_heat_flag,-heatwave_flag)

    matsim[i, "glob_cold"] <- FluMoDL::attrdl(x = data_output_year$temp,
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

    matsim[i, "glob_heat"] <- FluMoDL::attrdl(x = data_output_year$temp,
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

    matsim[i, "moderate_cold"] <- FluMoDL::attrdl(x = data_output_year$temp,
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

    matsim[i, "moderate_heat" ] <- FluMoDL::attrdl(x = data_output_year$temp,
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
    matsim[i,"high_cold"] <- FluMoDL::attrdl(x = data_output_year$temp,
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

    matsim[i,"high_heat"] <- FluMoDL::attrdl(x = data_output_year$temp,
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


    matsim[i,"heatwave"] <- FluMoDL::attrdl(x = data_output_year$heatwave_temp,
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
    arraysim[i, "glob_cold_ci", ] <- FluMoDL::attrdl(x = data_output_year$temp,
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

    arraysim[i, "glob_heat_ci", ] <- FluMoDL::attrdl(x = data_output_year$temp,
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

    arraysim[i, "moderate_cold_ci", ] <- FluMoDL::attrdl(x = data_output_year$temp,
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

    arraysim[i, "moderate_heat_ci", ] <- FluMoDL::attrdl(x = data_output_year$temp,
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

    arraysim[i, "high_cold_ci", ] <- FluMoDL::attrdl(x = data_output_year$temp,
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

    arraysim[i, "high_heat_ci", ] <- FluMoDL::attrdl(x = data_output_year$temp,
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

    arraysim[i, "heatwave_ci", ] <- FluMoDL::attrdl(x = data_output_year$heatwave_temp,
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
#' @description
#' @param df_list An alphabetically-ordered list of dataframes for each
#' region comprising dates, deaths, and temperatures.
#' @param output_year Year(s) to calculate output for.
#' @param arraysim` An array (numeric). Total (glob),
#' cold and heat-attributable deaths per region for 1000 simulations.
#'  Used to derive confidence intervals.
#' @param matsim A matrix (numeric). Total (glob),
#' cold and heat-attributable deaths per region from reduced coefficients.
#'
#' @export
#'
#'
#' @return
#' \itemize{
#'   \item `anregions_bind`
#'   \item `antot_bind`
#'   \item `arregions_bind`
#'   \item `artot_bind`
#' }
#' @examples
compute_attributable_rates <- function(df_list, output_year, matsim, arraysim){

  ###################################################
  # Attributable numbers: estimates as well as the upper and lower ends of the 95% confidence interval, derived from the simulated arraysim

  if (output_year == 0) {

    output_year = max(df_list[[1]]$year)

  } else {

    output_year = output_year
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
  antotlow <- apply(apply(arraysim,c(2,3),sum),1,quantile,0.025)
  antothigh <- apply(apply(arraysim,c(2,3),sum),1,quantile,0.975)

  ###################################################
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
  arregions <- anregions/as.numeric(regions_pop) * 100000
  arregionslow <- anregionslow/as.numeric(regions_pop) * 100000
  arregionshigh <- anregionshigh/as.numeric(regions_pop) * 100000

  # Total AR
  artot <- antot/totpopulation * 100000
  artotlow <- antotlow/totpopulation * 100000
  artothigh <- antothigh/totpopulation * 100000

  ###################################################
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
#' @param output_folder_path Path to folder for storing outputs.
#'
#' @export
#'
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
#' @examples output_folder_path = 'myfolder/output/'
write_attributable_deaths <- function(avgtmean_wald,
                                      rangetmean_wald,
                                      anregions_bind,
                                      antot_bind,
                                      arregions_bind,
                                      artot_bind,
                                      output_folder_path = NULL) {

  ###################################################
  # Convert data to publication format

  # Wald test results
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
                  moderate_cold,moderate_cold_ci_2.5,moderate_cold_ci_97.5,
                  moderate_heat,moderate_heat_ci_2.5,moderate_heat_ci_97.5,
                  high_cold,high_cold_ci_2.5,high_cold_ci_97.5,
                  high_heat,high_heat_ci_2.5,high_heat_ci_97.5,
                  heatwave, heatwave_ci_2.5,heatwave_ci_97.5)


  # AR_regions (attributable rates by region)
  arregions_publication <- arregions_bind %>%
    t() %>%
    as.data.frame() %>%
    dplyr::select(glob_cold, glob_cold_ci_2.5, glob_cold_ci_97.5,
                  glob_heat, glob_heat_ci_2.5, glob_heat_ci_97.5,
                  moderate_cold,moderate_cold_ci_2.5,moderate_cold_ci_97.5,
                  moderate_heat,moderate_heat_ci_2.5,moderate_heat_ci_97.5,
                  high_cold,high_cold_ci_2.5,high_cold_ci_97.5,
                  high_heat,high_heat_ci_2.5,high_heat_ci_97.5,
                  heatwave, heatwave_ci_2.5,heatwave_ci_97.5)

  ####

  # define output_folder_path as CWD if it is null
  if (is.null(output_folder_path)) {
   output_folder_path <- ""
  }

  write.csv(wald_publication,
            file = paste(output_folder_path,
                         'wald_test_results.csv',
                         sep = ""))

  write.csv(anregions_publication,
            file = paste(output_folder_path,
                         'attributable_deaths_regions.csv',
                         sep = ""))
  write.csv(antot_bind,
            file = paste(output_folder_path,
                         'attributable_deaths_total.csv',
                         sep = ""))
  write.csv(arregions_publication,
            file = paste(output_folder_path,
                         'attributable_rates_regions.csv',
                         sep = ""))
  write.csv(artot_bind,
            file = paste(output_folder_path,
                         'attributable_rates_total.csv',
                         sep=""))

  return(list(wald_publication, anregions_publication, antot_bind, arregions_publication, artot_bind))

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
#' @param independent_cols column name (or list of names) of extra independent
#' variable to include in regression (excluding temperature). Defaults to NULL.
#' @param varfun Exposure function
#' (see dlnm::crossbasis)
#' @param varper Internal knot positions in exposure function
#' (see dlnm::crossbasis)
#' @param vardegree Degrees of freedom in exposure function
#' (see dlnm:crossbasis)
#' @param lag Lag length in time
#' (see dlnm::logknots)
#' @param lagnk Number of knots in lag function
#' (see dlnm::logknots)
#' @param dfseas Degrees of freedom for seasonality
#'
#' @export
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
#' @examples output_folder_path = 'myfolder/output/'
plot_and_write_relative_risk <- function(df_list,
                                         blup = NULL,
                                         mintempregions,
                                         an_thresholds,
                                         save_fig = TRUE,
                                         save_csv = TRUE,
                                         output_folder_path,
                                         independent_cols,
                                         varfun,
                                         varper,
                                         vardegree,
                                         lag,
                                         lagnk,
                                         dfseas) {

  if (save_fig == TRUE) {

      if (!is.null(output_folder_path)) {

        pdf(paste(output_folder_path,
              "output_all_regions_plot.pdf",
              sep = ''),
        width = 8, height = 9)

        } else {

        pdf("output_all_regions_plot.pdf", width = 8, height = 9)

    }

    layout(matrix(c(0, 1, 1, 2, 2, 0,
                  rep(3:8, each = 2), 0, 9, 9, 10, 10, 0),
                ncol = 6,
                byrow = T))

    par(mar=c(4, 3.8, 3, 2.4), mgp = c(2.5, 1, 0), las = 1)

  }

  xlab <- expression(paste("Temperature (",degree,"C)"))

  region_vector <- c()
  temp_vector <- c()
  relative_risk_vector <- c()
  cen_vector <- c()
  upper_vector <- c()
  lower_vector <- c()

  temperature_vector <- c()
  temperature_region_vector <- c()

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

     plot(pred, type = "n",
          ylim = c(0, 3),
          yaxt = "n",
          lab = c(6, 5, 7),
          xlab = xlab,
          ylab = "RR",
          main = names(df_list)[i])

     ind_a <- pred$predvar <= c(an_thresholds[i,c("high_moderate_cold")])
     ind_b <- pred$predvar >= c(an_thresholds[i,c("high_moderate_cold")]) & pred$predvar <= c(an_thresholds[i,c("moderate_cold_OTR")])
     ind_c <- pred$predvar >= c(an_thresholds[i,c("moderate_cold_OTR")]) & pred$predvar <= c(an_thresholds[i,c("moderate_heat_OTR")])
     ind_d <- pred$predvar >= c(an_thresholds[i,c("moderate_heat_OTR")]) & pred$predvar <= c(an_thresholds[i,c("high_moderate_heat")])
     ind_e <- pred$predvar >= c(an_thresholds[i,c("high_moderate_heat")])

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
     abline(v = c(an_thresholds[i,c("moderate_cold_OTR", "moderate_cold_OTR")]), lty = 2)
     abline(v = c(an_thresholds[i,c("high_moderate_cold", "high_moderate_heat")]), lty = 3)


     if (!is.null(blup)) {

       relative_risk_vector <- append(relative_risk_vector,
                               pred$allRRfit)
      } else {

        relative_risk_vector <- append(relative_risk_vector,
                               pred$RRfit)
      }

     region_vector <-
       append(region_vector,
              rep(names(df_list)[i],
                  length(pred$predvar)))

     temp_vector <- append(temp_vector,
                           pred$predvar)

     cen_vector <- append(cen_vector,
                          rep(cen,
                              length(pred$predvar)))

     temperature_vector <- append(temperature_vector,
                            data$temp)

     temperature_region_vector <-
       append(temperature_region_vector,
              rep(names(df_list)[i],
                  length(data$temp)))


     if (!is.null(blup)) {

     upper_vector <- append(upper_vector,
                            pred$allRRhigh)


     lower_vector <- append(lower_vector,
                            pred$allRRlow)

     } else {

       upper_vector <- append(upper_vector,
                              pred$RRhigh)


       lower_vector <- append(lower_vector,
                              pred$RRlow)

     }

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
             paste(output_folder_path,
                   'output_all_regions_data.csv', sep = ''),
             row.names = FALSE)

    if (!is.null(blup)) {

      relative_risk <- pred$allRRfit
      rr_low <- pred$allRRlow
      rr_high <- pred$allRRhigh

    } else {

      relative_risk <- pred$RRfit
      rr_low <- pred$RRlow
      rr_high <- pred$RRhigh

    }

  }

  return (list(output_df, temp_df))

}

#' Plot and write results of analysis
#'
#' @param df_list An alphabetically-ordered
#' list of dataframes for each region.
#' @param mintempregions A named numeric vector.
#'   Minimum (optimum) mortality temperature per region.
#' @param save_fig Whether to save output figure (Bool)
#' @param save_csv Whether to save output CSVs (Bool)
#' @param dependent_col the column name of the
#' dependent variable of interest e.g. deaths
#' @param varfun Exposure function
#' (see dlnm::crossbasis)
#' @param varper Internal knot positions in exposure function
#' (see dlnm::crossbasis)
#' @param vardegree Degrees of freedom in exposure function
#' (see dlnm:crossbasis)
#' @param coef A matrix of coefficients for reduced model.
#' @param vcov A list. Co-variance matrices for each region for reduced model.
#' @param output_folder_path Path to folder for storing outputs.
#'
#' @export
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
#' @examples output_folder_path = 'myfolder/output/'
plot_and_write_relative_risk_all <- function(df_list,
                                         mintempregions,
                                         save_fig = TRUE,
                                         save_csv = TRUE,
                                         dependent_col,
                                         varfun,
                                         varper,
                                         vardegree,
                                         coef,
                                         vcov,
                                         output_folder_path
                                         ) {

  if (save_fig == TRUE) {

    if (!is.null(output_folder_path)) {

      pdf(paste(output_folder_path,
                "output_all_plot.pdf",
                sep = ''),
          width = 8, height = 9)

    } else {

      pdf("output_all_plot.pdf", width = 8, height = 9)

    }

  }

  par(mar = c(4, 3.8, 3, 2.4), mgp = c(2.5, 1, 0), las = 1)

  layout(matrix(1:1, ncol = 1))

  region_vector <- c()
  temp_vector <- c()
  relative_risk_vector <- c()
  upper_vector <- c()
  lower_vector <- c()
  cen_vector <- c()

  temperature_vector <- c()
  temperature_region_vector <- c()

  data <- do.call(rbind, df_list)

  # Estimation method
  method <- "reml"

  # Overall cumulative summary for main model
  mvall <- mvmeta::mvmeta(coef ~ 1, vcov, method = method)

  # Exclude extreme temps
  # predvar <- quantile(data$tmean, 1:99/100, na.rm=T)

  # All temps
  predvar <- data$temp

  argvar <- list(x = predvar,
                 fun = varfun,
                 degree = vardegree,
                 knots = quantile(data$temp,
                                varper / 100, na.rm = TRUE),
                 Bound = range(data$temp, na.rm = TRUE))

  bvar <- do.call(dlnm::onebasis, argvar)

  model <- NULL
  cen <- median(mintempregions)

  pred <- dlnm::crosspred(bvar,
                    coef = coef(mvall),
                    vcov = vcov(mvall),
                    model.link = "log",
                    by = 0.1,
                    cen = cen)

  plot(pred,
       type = "n",
       ylab = "RR",
       ylim = c(.0, 3),
       xlim = c(-8, 30),
       xlab=expression(paste("Temperature (", degree, "C)")),
       main = dependent_col)

  abline(h = 1)

  optimal_meta_lower <- as.numeric(names(
    which.min(which(pred$allRRfit >= 1 & pred$allRRfit <= 1.1))))
  optimal_meta_upper <- as.numeric(names(
    which.max(which(pred$allRRfit >= 1 & pred$allRRfit <= 1.1))))

  extreme_cold <- ifelse(optimal_meta_lower < quantile(data$temp, 2.5/100, na.rm = TRUE),
                         optimal_meta_lower,
                         quantile(data$temp, 2.5/100, na.rm = TRUE))
  extreme_heat <- ifelse(optimal_meta_upper > quantile(data$temp, 97.5/100, na.rm = TRUE),
                         optimal_meta_upper,
                         quantile(data$temp, 97.5/100, na.rm = TRUE))

  ind_a <- pred$predvar <= extreme_cold
  ind_b <- pred$predvar >= extreme_cold & pred$predvar <= optimal_meta_lower
  ind_c <- pred$predvar >= optimal_meta_lower & pred$predvar <= optimal_meta_upper
  ind_d <- pred$predvar >= optimal_meta_upper & pred$predvar <= extreme_heat
  ind_e <- pred$predvar >= extreme_heat

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

  # axis(1,at = c(-5,0,5,10,15,20,25,30))
  # axis(2,at = 1:6*0.5)

  abline(v = cen, lty = 1, col = 3)
  abline(v = c(optimal_meta_lower, optimal_meta_upper), lty = 2)
  abline(v = c(extreme_cold, extreme_heat), lty = 3)

  relative_risk_vector <- append(relative_risk_vector,
                                   pred$allRRfit)

  upper_vector <- append(upper_vector,
                         pred$allRRhigh)

  lower_vector <- append(lower_vector,
                         pred$allRRlow)

  region_vector <-
    append(region_vector,
           rep('England',
               length(pred$predvar)))

  temp_vector <- append(temp_vector,
                        pred$predvar)

  cen_vector <- append(cen_vector,
                       rep(cen,
                           length(pred$predvar)))

  temperature_vector <- append(temperature_vector,
                         data$temp)

  temperature_region_vector <-
    append(temperature_region_vector,
           rep('England',
               length(data$temp)))

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
              paste(output_folder_path,
                    'output_all_data.csv', sep = ''),
              row.names = FALSE)

  }

  return (list(output_df, temp_df))

}

#' Do full DLNM analysis
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
#' @param meta_analysis Boolean (TRUE or FALSE). Whether to include
#' meta-analysis. Must be TRUE if by_region argument is FALSE.
#' @param by_region Boolean (TRUE or FALSE). Whether to disaggregate by region.
#' Must be TRUE if meta-analysis is FALSE.
#' @param RR_distribution_length Number of years for the calculation of RR distribution. Set both as 'NONE' to use full range in data.
#' @param output_year_ Year(s) to calculate output for.
#' @param dependent_col_ the column name of the
#' dependent variable of interest e.g. deaths
#' @param independent_cols_ column name (or list of names) of extra independent
#' variable to include in regression (excluding temperature). Defaults to NULL.
#' @param time_col_ The column name of column containing dates (e.g date, year).
#' @param region_col_ The column name of the column containing regions.
#' @param temp_col_ the column name of the column containing the exposure.
#' @param population_col_ the column name of the column containing population values.
#' @param varfun_ Exposure function
#' (see dlnm::crossbasis)
#' @param vardegree_ Degrees of freedom in exposure function
#' (see dlnm:crossbasis)
#' @param lag_ Lag length in time
#' (see dlnm::logknots)
#' @param lagnk_ Number of knots in lag function
#' (see dlnm::logknots)
#' @param dfseas_ Degrees of freedom for seasonality
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
do_analysis <- function(input_csv_path_ = 'NONE',
                        output_folder_path_ = NULL,
                        save_fig_ = TRUE,
                        save_csv_ = TRUE,
                        meta_analysis_ = TRUE,
                        by_region_ = TRUE,
                        RR_distribution_length_ = 0,
                        output_year_ = 0,
                        dependent_col_ = 'death',
                        independent_cols_ = NULL,
                        time_col_ = 'date',
                        region_col_ = 'regnames',
                        temp_col_ = 'tmean',
                        population_col_ = 'pop',
                        varfun_ = 'bs',
                        vardegree_ = 2,
                        lag_  = 21,
                        lagnk_ = 3,
                        dfseas_ = 8,
                        nsim__ = 1000
                        ) {

  varper_ <- c(10, 75, 90)

  c(df_list_) %<-%
    load_data(
      input_csv_path = input_csv_path_,
      dependent_col = dependent_col_,
      time_col = time_col_,
      region_col = region_col_,
      temp_col = temp_col_,
      population_col = population_col_,
      output_year = output_year_,
      RR_distribution_length = RR_distribution_length_
      )

  if (meta_analysis_ == TRUE) {

    c(coef_, vcov_) %<-%
    run_model(df_list = df_list_,
              independent_cols = independent_cols_,
              varfun = varfun_,
              varper = varper_,
              vardegree = vardegree_,
              lag = lag_,
              lagnk = lagnk_,
              dfseas = dfseas_
              )

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

  c(wald_publication_, anregions_publication_, antot_bind_, arregions_publication_, artot_bind_) %<-%
    write_attributable_deaths(
      avgtmean_wald = avgtmean_wald_,
      rangetmean_wald = rangetmean_wald_,
      anregions_bind = anregions_bind_,
      antot_bind = antot_bind_,
      arregions_bind = arregions_bind_,
      artot_bind = artot_bind_,
      output_folder_path = output_folder_path_
    )

  if (by_region_ == FALSE) {

    c(output_df, temp_df) %<-%
      plot_and_write_relative_risk_all(
        df_list = df_list_,
        mintempregions = mintempregions_,
        save_fig = save_fig_,
        save_csv = save_csv_,
        dependent_col = dependent_col_,
        varfun = varfun_,
        varper = varper_,
        vardegree = vardegree_,
        coef = coef_,
        vcov = vcov_,
        output_folder_path = output_folder_path_
      )

  } else {

    c(output_df, temp_df) %<-%
      plot_and_write_relative_risk(
      df_list = df_list_,
      blup = blup_,
      mintempregions = mintempregions_,
      an_thresholds = an_thresholds_,
      save_fig = save_fig_,
      save_csv = save_csv_,
      output_folder_path = output_folder_path_,
      independent_cols = independent_cols_,
      varfun = varfun_,
      varper = varper_,
      vardegree = vardegree_,
      lag = lag_,
      lagnk = lagnk_,
      dfseas = dfseas_
      )
  }

  return (list(output_df, temp_df, anregions_publication_, antot_bind_, arregions_publication_, artot_bind_))

}
