# Functions to generate analysis for the temperature indicators.

#' @title Filter the dataframe based on the relative risk distribution.
#'
#' @param df The dataframe to filter.
#' @param RR_distribution_length The RR distribution length. Defaults to 0.
#' @param lower_range The lower range. Defaults to 5.
#' @param upper_range The upper range. Defaults to 15.
#' @param output_year The output year. Defaults to 0.
#'
#' @return The filtered dataframe.
#'
#' @keywords internal
filter_on_rr_distribution <- function(df,
                                      RR_distribution_length = 0,
                                      lower_range = 5,
                                      upper_range = 200,
                                      output_year = 0) {
  # Set the output year if the user has not passed one.
  if (output_year == 0) {

    output_year <- max(as.integer(df$year), na.rm = TRUE)

  }

  # Calculate the RR distribution length if it is 0
  if (RR_distribution_length == 0) {
    RR_distribution_length = max(df$year, na.rm = TRUE) - min(df$year, na.rm = TRUE)
  }

  # Raise an error if the RR distribution length is out of range (5-15)
  if(RR_distribution_length < lower_range) {

    stop(paste0("Timeseries to calculate the RR is less than ", lower_range, " years."))

  } else if (RR_distribution_length > upper_range) {

    stop(paste0("Timeseries to calculate the RR is more than ", upper_range, " years."))

  }
  # Filter
  df <- df %>%
    dplyr::filter(.data$year >= (output_year - RR_distribution_length + 1)
                  & .data$year <= output_year)

  return(df)
}


#' Load temperature (heat/cold indicator) for analysis.
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
#' @param output_year Year(s) to calculate output for.
#' @param RR_distribution_length Number of years for the calculation of RR
#' distribution. Set both as 'NONE' to use full range in data.
#'
#' @return `df_list` An alphabetically-ordered list of dataframes for each
#' region comprising dates, deaths, and temperatures.
#'
#' @keywords internal
load_temperature_data <- function(input_csv_path,
                                  dependent_col,
                                  time_col,
                                  region_col,
                                  temp_col,
                                  population_col = NULL,
                                  output_year = 0,
                                  RR_distribution_length = 0) {

  # Load the input dataset
  df <- read_input_data(input_csv_path)
  # Format the population column
  if (is.null(population_col)) {
    df <- df %>%
      dplyr::mutate(pop_col = "NONE")
    population_col = "pop_col"
  }
  # Format the region column
  if (is.null(region_col)) {
    df <- df %>%
      dplyr::mutate(regnames = "aggregated")
    region_col = "regnames"
  }
  # Rename the columns
  df <- df %>%
    dplyr::rename(dependent = dependent_col,
                  date = time_col,
                  regnames = region_col,
                  temp = temp_col,
                  pop_col = population_col)
  # Reformat data and fill NaNs
  df <- reformat_data(df,
                      reformat_date = TRUE,
                      fill_na = c("dependent"),
                      year_from_date = TRUE)
  # Filter the data based on RR_distribution_length
  df <- filter_on_rr_distribution(df,
                                  RR_distribution_length,
                                  output_year = output_year)
  # Split the data by region
  df_list <- aggregate_by_column(df, "regnames")

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
#'
#' @return
#' \itemize{
#'   \item `model` A quasi-poission generalised linear model object.
#'   See: https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/glm
#'   \item `cb` Basis matrices for the two dimensions of predictor and lags.
#'   }
#'
#' @keywords internal
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

  #TODO: add type check for independent_cols

  if (!is.null(independent_cols)) {

    # normalize type
    if (is.character(independent_cols)) {
      independent_cols <- c(independent_cols)
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
#'
#' @return
#' \itemize{
#'   \item `coef_` A matrix of coefficients for reduced model.
#'   \item `vcov_` A list. Co-variance matrices for each region for reduced model.
#'   \item `cb` Basis matrices for the two dimensions of predictor and lags.
#'   \item `model` A quasi-poission generalised linear model object.
#'   See: https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/glm
#'   }
#'
#' @keywords internal
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
    mintempregions[i] <- as.numeric(names(which.min(pred$RRfit)))

    coef_[i,] <- coef(pred)
    vcov_[[i]] <- vcov(pred)

  }
  return (list(coef_, vcov_, cb, model))
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
#' @keywords internal
run_meta_model <- function(df_list, coef, vcov) {

  # Assert that df_list is a list of dataframes
  is_list_of_dfs(list_ = df_list)

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
                       control = list(showiter = FALSE))

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
#' @keywords internal
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
#'
#' @keywords internal
wald_results <- function(mv) {

  avgtmean_wald <- fwald(mv, "avgtmean")
  rangetmean_wald <- fwald(mv, "rangetmean")

  return(list(avgtmean_wald, rangetmean_wald))

}



#' Define and validate the optimal temperature range from the model predictions.
#'
#' @param optimal_temp_range Matrix. A matrix used to store the optimal temperature
#' ranges.
#' @param prediction Data. The models prediction.
#' @param RR_fit_col Character. The column containing the relative risk values.
#' @param index Integer. The index to use to obtain the RR values.
#'
#' @return The optimal temperature range.
#' @keywords internal
define_and_validate_optimal_temps <- function(optimal_temp_range,
                                              prediction,
                                              RR_fit_col = "allRRfit",
                                              index) {
  optimal_temp_range[index,"lower"] <- as.numeric(names(
    which.min(which(prediction[[RR_fit_col]] >= 1 & prediction[[RR_fit_col]] <= 1.1))))
  optimal_temp_range[index, "upper"] <- as.numeric(names(
    which.max(which(prediction[[RR_fit_col]] >= 1 & prediction[[RR_fit_col]] <= 1.1))))
  below_one <- which(prediction[[RR_fit_col]] < 1)
  above_OTR <- which(
    as.numeric(names(prediction[[RR_fit_col]])) > optimal_temp_range[index, "upper"]
  )
  below_OTR <- which(
    as.numeric(names(prediction[[RR_fit_col]]))< optimal_temp_range[index, "lower"]
  )
  if (length(which((below_one %in% above_OTR) | (below_one %in% below_OTR))) > 0) {
    # TODO: Create a better warning
    warning("Predicted RR goes below 1 in the ends")
  }

  return (optimal_temp_range)

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
#' @keywords internal
calculate_min_mortality_temp <-  function(df_list,
                                          blup = NULL,
                                          independent_cols = NULL,
                                          varfun,
                                          varper,
                                          vardegree,
                                          lag,
                                          lagnk,
                                          dfseas) {
  # Assert that df_list is a list of dataframes
  is_list_of_dfs(list_ = df_list)

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
    range(x$temp,na.rm = TRUE)))
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


      optimal_temp_range <- define_and_validate_optimal_temps(
        optimal_temp_range = optimal_temp_range,
        prediction = cp,
        index = i
      )

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

      optimal_temp_range <- define_and_validate_optimal_temps(
        optimal_temp_range = optimal_temp_range,
        RR_fit_col = "RRfit",
        prediction = pred,
        index = i
      )

    }

  }
  # calculate percentiles
  per <- t(sapply(df_list, function(x)
    quantile(x$temp, c(2.5, 97.5) / 100, na.rm = TRUE)))

  # data frame with final thresholds to use for hot and cold days to attribute deaths to
  an_thresholds <- as.data.frame(cbind(per, optimal_temp_range)) %>%
    dplyr::mutate(
      min_high_cold = -100,
      max_high_heat = 100,
      moderate_cold_OTR = .data$lower,
      moderate_heat_OTR = .data$upper,
      high_moderate_cold = ifelse(.data$moderate_cold_OTR < .data$`2.5%`,
                                  .data$moderate_cold_OTR,
                                  .data$`2.5%`),
      high_moderate_heat = ifelse(.data$moderate_heat_OTR > .data$`97.5%`,
                                  .data$moderate_heat_OTR,
                                  .data$`97.5%`)
    ) %>%
    dplyr::select(
      .data$min_high_cold,
      .data$high_moderate_cold,
      .data$moderate_cold_OTR,
      .data$moderate_heat_OTR,
      .data$high_moderate_heat,
      .data$max_high_heat
    )

  # Country-specific points of minimum mortality
  (minperccountry <- median(minpercregions_))


  return(list(mintempregions = mintempregions_, an_thresholds))

}

# TODO: Explore functionalising this?

#' Compute attributable deaths
#'
#' Compute the attributable deaths for each regions,
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
#'
#' @keywords internal
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

    data_output_year <- data %>% dplyr::filter(.data$year %in% output_year) %>%
      dplyr::mutate(
        high_heat_flag = ifelse(
          .data$temp > an_thresholds[i,"high_moderate_heat"], 1, 0
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
        heatwave_temp = ifelse(.data$heatwave_flag == 1, .data$temp, mintempregions[i])
      ) %>%
      dplyr::select(-.data$high_heat_flag, -.data$heatwave_flag)

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
#' @keywords internal
compute_attributable_rates <- function(df_list, output_year, matsim, arraysim){

  ###################################################
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

  ###################################################
  # Attributable rates

  # Populations to compute attributable rates with
  regions_pop <- rep(NA, length(df_list))
  names(regions_pop) <- names(df_list)
  years_pop <- rep(NA, length(output_year))

  for (i in seq(df_list)){
    for (j in seq(length(output_year))){
      data_output_year <- df_list[[i]] %>% dplyr::filter(.data$year == output_year[j])
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

  return(list(anregions_bind, antot_bind, arregions_bind, artot_bind))
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
#' @keywords internal
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
  if (!is.null(avgtmean_wald) & !is.null(rangetmean_wald)) {
    wald_publication <- data.frame(avgtmean_wald, rangetmean_wald)
    colnames(wald_publication) <- c("region_mean_temp", "region_temp_range")
    rownames(wald_publication) <- "Wald statistic p-value"
  } else {
    wald_publication <- NULL
  }

  # AN_regions (attributable deaths by region)
  anregions_publication <- anregions_bind %>%
    t() %>%
    as.data.frame() %>%
    dplyr::select(
      .data$glob_cold, .data$glob_cold_ci_2.5, .data$glob_cold_ci_97.5,
      .data$glob_heat, .data$glob_heat_ci_2.5, .data$glob_heat_ci_97.5,
      .data$moderate_cold, .data$moderate_cold_ci_2.5, .data$moderate_cold_ci_97.5,
      .data$moderate_heat, .data$moderate_heat_ci_2.5, .data$moderate_heat_ci_97.5,
      .data$high_cold, .data$high_cold_ci_2.5, .data$high_cold_ci_97.5,
      .data$high_heat, .data$high_heat_ci_2.5, .data$high_heat_ci_97.5,
      .data$heatwave, .data$heatwave_ci_2.5, .data$heatwave_ci_97.5
    )

  # AR_regions (attributable rates by region)
  arregions_publication <- arregions_bind %>%
    t() %>%
    as.data.frame() %>%
    dplyr::select(
      .data$glob_cold, .data$glob_cold_ci_2.5, .data$glob_cold_ci_97.5,
      .data$glob_heat, .data$glob_heat_ci_2.5, .data$glob_heat_ci_97.5,
      .data$moderate_cold, .data$moderate_cold_ci_2.5, .data$moderate_cold_ci_97.5,
      .data$moderate_heat, .data$moderate_heat_ci_2.5, .data$moderate_heat_ci_97.5,
      .data$high_cold, .data$high_cold_ci_2.5, .data$high_cold_ci_97.5,
      .data$high_heat, .data$high_heat_ci_2.5, .data$high_heat_ci_97.5,
      .data$heatwave, .data$heatwave_ci_2.5, .data$heatwave_ci_97.5
    )

  if (isTRUE(save_csv)) {
    # define output_folder_path as CWD if it is null
    if (is.null(output_folder_path)) {
      output_folder_path <- "/"
    } else if (!endsWith(output_folder_path, "/")) {
      output_folder_path <- paste0(output_folder_path, "/")
    }

    write.csv(wald_publication,
              file = paste0(output_folder_path, "heat_and_cold_wald_test_results.csv"))
    write.csv(anregions_publication,
              file = paste0(output_folder_path, "heat_and_cold_attributable_deaths_regions.csv"))
    write.csv(antot_bind,
              file = paste0(output_folder_path, "heat_and_cold_attributable_deaths_total.csv"))
    write.csv(arregions_publication,
              file = paste0(output_folder_path, "heat_and_cold_attributable_rates_regions.csv"))
    write.csv(artot_bind,
              file = paste0(output_folder_path, "heat_and_cold_attributable_rates_total.csv"))
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
#' @param vardegree Degrees of freedom in exposure function
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
#' @keywords internal
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
#' @keywords internal
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
    dplyr::group_by(.data$regions) %>%
    dplyr::filter(.data$rel_risk < 1.1) %>%
    dplyr::summarise(optimal_temp_range_min = min(.data$temp),
                     optimal_temp_range_max = max(.data$temp))

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
#' @param vardegree Degrees of freedom in exposure function
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
#' @keywords internal
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
#' @importFrom zeallot %<-%
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
#' @param vardegree_ Degrees of freedom in exposure function
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
