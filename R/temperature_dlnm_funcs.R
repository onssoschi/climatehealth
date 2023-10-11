library(dlnm)
library(mvmeta)
library(splines)
library(tsModel)
library(config)
library(zeallot)

# Load config file
config <- config::get()

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
#' @param time_range_start_ Start of time range over which to run the analysis.  'None' if over full range.
#' @param time_range_end_ End of time range over which to run the analysis.  'None' if over full range.
#' @return `df_list` An alphabetically-ordered list of dataframes for each
#' region comprising dates, deaths, and temperatures.
#' @export
load_data <- function(input_csv_path,
                      dependent_col,
                      time_col,
                      region_col,
                      temp_col,
                      time_range_start,
                      time_range_end) {

  if(substr(input_csv_path, nchar(input_csv_path) - 3, nchar(input_csv_path)) !=
     '.csv') {

    stop("Input path must be a CSV")

  }


  df <- read.csv(input_csv_path, row.names = 1) %>%
    dplyr::rename(dependent = dependent_col,
                  date = time_col,
                  regnames = region_col,
                  tmean = temp_col) %>%
    dplyr::mutate(date = as.Date(date))


  if (!'NONE' %in% c(time_range_start, time_range_end)) {

    df <- df %>%
      dplyr::filter(date >= time_range_start
                    & date <= time_range_end)

  }

  regions <- sort(as.character(unique(df$regnames)))

  df_list <- lapply(regions,
                    function(x)
                      df %>%
                      dplyr::filter(regnames == x)
                    )

  names(df_list) <- regions

  return (list(df_list))

}

#' Define regression model
#'
#' @param dataset dataframe with tmean column to be modelled
#' @param indepedent_col1_ column name of first extra independent
#' variable to include in regression (excluding temperature,
#' see config file for formula structure). 'None' if none.
#' @param indepedent_col2_ column name of second independent
#' variable to include in regression (excluding temperature,
#' see config file for formula structure). 'None' if none.
#' @param indepedent_col3_ column name of third independent
#' variable to include in regression (excluding temperature,
#' see config file for formula structure). 'None' if none.
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
                         independent_col1,
                         independent_col2,
                         independent_col3,
                         varfun,
                         varper,
                         vardegree,
                         lag,
                         lagnk,
                         dfseas) {

  if ('NONE' %in% c(independent_col1,
                    independent_col2,
                    independent_col3)) {

    independent_cols <- c('cb',
                          'dow',
                          'ns(date, df = dfseas * length(unique(year)))')

    }

  # Model formula
  formula <- as.formula(paste(paste('dependent'),
                              " ~ ",
                              paste(independent_cols,
                                    collapse = " + ")))

  # Define crossbasis
  argvar_ <- list(fun = varfun,
                  knots = quantile(dataset$tmean,
                                   varper / 100,
                                   na.rm = TRUE),
                  degree = vardegree)

  lag <- as.numeric(lag)
  lagnk <- as.numeric(lagnk)
  dfseas <- as.numeric(dfseas)

  cb <- crossbasis(dataset$tmean,
                   lag = lag,
                   argvar = argvar_,
                   arglag = list(knots = logknots(lag,
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
#' @param indepedent_col1_ column name of first extra independent
#' variable to include in regression (excluding temperature,
#' see config file for formula structure). 'None' if none.
#' @param indepedent_col2_ column name of second independent
#' variable to include in regression (excluding temperature,
#' see config file for formula structure). 'None' if none.
#' @param indepedent_col3_ column name of third independent
#' variable to include in regression (excluding temperature,
#' see config file for formula structure). 'None' if none.
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
                      independent_col1,
                      independent_col2,
                      independent_col3,
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

    cat(i,"")

    # Extract data
    data <- df_list[[i]]

    c(model, cb) %<-% define_model(dataset = data,
                                   independent_col1 = independent_col1,
                                   independent_col2 = independent_col2,
                                   independent_col3 = independent_col3,
                                   varfun = varfun,
                                   varper = varper,
                                   vardegree = vardegree,
                                   lag = lag,
                                   lagnk = lagnk,
                                   dfseas = dfseas)

    cen_ <- mean(data$tmean, na.rm = TRUE)

    # Reduction to overall cumulative
    pred <- crossreduce(cb, model, cen = cen_)
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

  if(!is.list(df_list) | !is.data.frame(df_list[[1]])) {
    stop("Argument 'df_list' must be a list of data frames")
  }

  if(!is.matrix(coef) | !is.numeric(coef)) {
    stop("Argument 'coef' must be a numeric matrix")
  }

  if(!is.list(vcov) | !is.matrix(vcov[[1]])) {
    stop("Argument 'vcov' must be a list of matrices")
  }


  # Create average temperature and range as meta-predictors
  avgtmean <- sapply(df_list,
                     function(x)
                       mean(x$tmean, na.rm = TRUE))

  rangetmean <- sapply(df_list,
                       function(x)
                         diff(range(x$tmean, na.rm = TRUE)))

  # Meta-analysis
  # NB: country effects is not included in this example
  mv <- mvmeta(coef ~ avgtmean + rangetmean,
               vcov,
               data = as.data.frame(names(df_list)), # was data = regions_df
               control = list(showiter = TRUE))

  # Obtain blups
  blup <- blup(mv, vcov = TRUE)

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
#' @param indepedent_col1_ column name of first extra independent
#' variable to include in regression (excluding temperature,
#' see config file for formula structure). 'None' if none.
#' @param indepedent_col2_ column name of second independent
#' variable to include in regression (excluding temperature,
#' see config file for formula structure). 'None' if none.
#' @param indepedent_col3_ column name of third independent
#' variable to include in regression (excluding temperature,
#' see config file for formula structure). 'None' if none.
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
#' }
#'
#' @export
calculate_min_mortality_temp <-  function(df_list,
                                          blup = NULL,
                                          independent_col1,
                                          independent_col2,
                                          independent_col3,
                                          varfun,
                                          varper,
                                          vardegree,
                                          lag,
                                          lagnk,
                                          dfseas) {

  if (!is.list(df_list) | !is.data.frame(df_list[[1]])) {
    stop("Argument 'df_list' must be a list of data frames")
  }

  if (!is.null(blup) && !is.list(blup)) {
    stop("Argument 'blup' must be a list")
  }

  # Re-centering
  # Generate the matrix for storing results
  minpercregions_ <- mintempregions_ <- rep(NA,
                                            length(df_list))
  names(mintempregions_) <- names(minpercregions_) <- names(df_list)

  if (!is.null(blup)) {

    # Define minimum mortality values: exclude low and very hot temperatures
    for(i in seq(length(df_list))) {

      data <- df_list[[i]]
      predvar <- quantile(data$tmean, 1:99 / 100, na.rm = TRUE)

      # Redefine the function using all arguments (boundary knots included)
      argvar_ <- list(x = predvar, fun = varfun,
                   knots = quantile(data$tmean,
                                    varper / 100,
                                    na.rm = TRUE),
                   degree = vardegree,
                   Bound = range(data$tmean, na.rm = TRUE))

      bvar_ <- do.call(onebasis, argvar_)

      minpercregions_[i] <- (1:99)[which.min((bvar_ %*%
                                                blup[[i]]$blup))]
      mintempregions_[i] <- quantile(data$tmean,
                                     minpercregions_[i] / 100,
                                     na.rm = TRUE)

    }

  } else {

    for(i in seq(length(df_list))) {

      cat(i,"")

      # Extract data
      data <- df_list[[i]]

      c(model, cb) %<-% define_model(dataset = data,
                                     independent_col1 = independent_col1,
                                     independent_col2 = independent_col2,
                                     independent_col3 = independent_col3,
                                     varfun = varfun,
                                     varper = varper,
                                     vardegree = vardegree,
                                     lag = lag,
                                     lagnk = lagnk,
                                     dfseas = dfseas)

      cen_ <- mean(data$tmean, na.rm = TRUE)

      # Reduction to overall cumulative
      pred <- crossreduce(cb, model, cen = cen_)
      mintempregions_[i] <- as.numeric(names(which.min(pred$RRfit)))

    }

  }

  # Country-specific points of minimum mortality
  (minperccountry <- median(minpercregions_))

  return(list(mintempregions = mintempregions_))

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
#' @param indepedent_col1_ column name of first extra independent
#' variable to include in regression (excluding temperature,
#' see config file for formula structure). 'None' if none.
#' @param indepedent_col2_ column name of second independent
#' variable to include in regression (excluding temperature,
#' see config file for formula structure). 'None' if none.
#' @param indepedent_col3_ column name of third independent
#' variable to include in regression (excluding temperature,
#' see config file for formula structure). 'None' if none.
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
                                        blup = NULL,
                                        mintempregions,
                                        independent_col1,
                                        independent_col2,
                                        independent_col3,
                                        varfun,
                                        varper,
                                        vardegree,
                                        lag,
                                        lagnk,
                                        dfseas) {


  # if (file.exists('R/attrdl.R')) {
  #
  #   source('R/attrdl.R')
  #
  # } else {
  #
  #   source('attrdl.R')
  #
  # }

  per <- t(sapply(df_list, function(x)
    quantile(x$tmean, c(2.5, 10, 25, 50, 75, 90, 97.5) / 100, na.rm = TRUE)))

  # Create the vectors to store the total mortality (accounting for missing)
  totdeath <- rep(NA, length(names(df_list)))
  names(totdeath) <- names(df_list)

  # Create the matrix to store the attributable deaths
  matsim <- matrix(NA, length(names(df_list)), 5,
                   dimnames = list(names(df_list),
                                   c("glob", "cold", "heat", "extreme_cold",
                                     "extreme_heat")))

  # Number of simulation runs for computing empirical CI
  nsim_ <- 1000

  # Create the array to store the CI of attributable deaths
  arraysim <- array(NA, dim = c(length(names(df_list)), 5, nsim_),
                    dimnames = list(names(df_list),
                                    c("glob_ci", "cold_ci", "heat_ci",
                                      "extreme_cold_ci", "extreme_heat_ci")))

  attrdl_yr_all <- list()

  # Run the loop
  for(i in seq(df_list)){

    # Print
    cat(i, "")

    # Extract the data
    data <- df_list[[i]]

    # Derive the cross-basis
    if (!is.null(blup)) {

      coefs <- blup[[i]]$blup
      vcovs <- blup[[i]]$vcov
      c(model, cb) %<-% define_model(dataset = data,
                                     independent_col1 = independent_col1,
                                     independent_col2 = independent_col2,
                                     independent_col3 = independent_col3,
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
                                     independent_col1 = independent_col1,
                                     independent_col2 = independent_col2,
                                     independent_col3 = independent_col3,
                                     varfun = varfun,
                                     varper = varper,
                                     vardegree = vardegree,
                                     lag = lag,
                                     lagnk = lagnk,
                                     dfseas = dfseas)

    }

    #############################################
    # Return heat attributable deaths per year

    # PROBLEM: mintempregions currently calculated
    # across all years and not for an individual year

    year_range <- sort(unique(lubridate::year(data$date)))

    attrdl_years <- function(ind_year, temp_range) {

      subset <- data %>%
        dplyr::filter(year == ind_year)

      attrdl_year <- attrdl(x = subset$tmean,
                            basis = cb,
                            cases = subset$dependent,
                            coef = coefs,
                            vcov = vcovs,
                            type = "an",
                            dir = "forw",
                            cen = mintempregions[i],
                            model = model,
                            range = temp_range)

      return(list(ind_year = ind_year, attrdl_year = attrdl_year))

    }

    attrdl_glob <- lapply(year_range,
                          attrdl_years,
                          temp_range = NULL) %>%
      purrr:::map(as.data.frame) %>%
      purrr::list_rbind()

    attrdl_cold <- lapply(year_range,
                          attrdl_years,
                          temp_range = c(-100, mintempregions[i])) %>%
      purrr:::map(as.data.frame) %>%
      purrr::list_rbind()

    attrdl_heat <- lapply(year_range,
                          attrdl_years,
                          temp_range = c(mintempregions[i], 100)) %>%
      purrr:::map(as.data.frame) %>%
      purrr::list_rbind()

    attrdl_ext_cold <- lapply(year_range,
                              attrdl_years,
                              temp_range = c(-100, per[i, 1])) %>%
      purrr:::map(as.data.frame) %>%
      purrr::list_rbind()

    attrdl_ext_heat <- lapply(year_range,
                              attrdl_years,
                              temp_range = c(per[i, 7], 100)) %>%
      purrr:::map(as.data.frame) %>%
      purrr::list_rbind()


    # for (attrdl_year in attrdl_heat) {
    #
    #   cat("region:", regions_df$regions[i], "/",
    #       "year:", attrdl_year$ind_year, "/",
    #       "heat attr deaths:", round(attrdl_year$attrdl_year, 0), "\n")
    # }

    attrdl_yr_df <- data.frame(region = rep(names(df_list)[i],
                                            length(year_range)),
                               year = rep(year_range),
                               glob = attrdl_glob$attrdl_year,
                               heat = attrdl_heat$attrdl_year,
                               cold = attrdl_cold$attrdl_year,
                               extreme_cold = attrdl_ext_cold$attrdl_year,
                               extreme_heat = attrdl_ext_heat$attrdl_year
                               )

    attrdl_yr_all[[i]] <- attrdl_yr_df

    #############################################

    # Compute the attributable deaths
    # NB: The reduced coefficients are used here
    matsim[i, "glob"] <- attrdl(x = data$tmean,
                                basis = cb,
                                cases = data$dependent,
                                coef = coefs,
                                vcov = vcovs,
                                type = "an",
                                dir = "forw",
                                cen = mintempregions[i],
                                model = model)

    matsim[i, "cold"] <- attrdl(x = data$tmean,
                                basis = cb,
                                cases = data$dependent,
                                coef = coefs,
                                vcov = vcovs,
                                type = "an",
                                dir = "forw",
                                cen = mintempregions[i],
                                model = model,
                                range = c(-100, mintempregions[i]))

    matsim[i, "heat" ] <- attrdl(x = data$tmean,
                                 basis = cb,
                                 cases = data$dependent,
                                 coef = coefs,
                                 vcov = vcovs,
                                 type="an",
                                 dir = "forw",
                                 cen = mintempregions[i],
                                 model = model,
                                 range = c(mintempregions[i], 100))

    # Attributable deaths for extremes:
    matsim[i,"extreme_cold"] <- attrdl(x = data$tmean,
                                       basis = cb,
                                       cases = data$dependent,
                                       coef = coefs,
                                       vcov = vcovs,
                                       model = model,
                                       type = "an",
                                       dir = "forw",
                                       cen = mintempregions[i],
                                       range = c(-100, per[i, 1]))

    matsim[i,"extreme_heat"] <- attrdl(x = data$tmean,
                                       basis = cb,
                                       cases = data$dependent,
                                       coef = coefs,
                                       vcov = vcovs,
                                       model = model,
                                       type = "an",
                                       dir = "forw",
                                       cen = mintempregions[i],
                                       range = c(per[i, 7], 100))

    # Compute empirical occurrences of the attributable deaths
    # Used to derive confidence intervals
    arraysim[i, "glob_ci", ] <- attrdl(x = data$tmean,
                                       basis = cb,
                                       cases = data$dependent,
                                       coef = coefs,
                                       vcov = vcovs,
                                       type = "an",
                                       dir = "forw",
                                       cen = mintempregions[i],
                                       model = model,
                                       sim = T, nsim = nsim_)

    arraysim[i, "cold_ci", ] <- attrdl(x = data$tmean,
                                       basis = cb,
                                       cases = data$dependent,
                                       coef = coefs,
                                       vcov = vcovs,
                                       type = "an",
                                       dir = "forw",
                                       cen = mintempregions[i],
                                       model = model,
                                       range = c(-100, mintempregions[i]),
                                       sim = T , nsim = nsim_)

    arraysim[i, "heat_ci", ] <- attrdl(x = data$tmean,
                                       basis = cb,
                                       cases = data$dependent,
                                       coef = coefs,
                                       vcov = vcovs,
                                       type = "an",
                                       dir = "forw",
                                       cen = mintempregions[i],
                                       model = model,
                                       range = c(mintempregions[i], 100),
                                       sim = T, nsim = nsim_)

    arraysim[i, "extreme_cold_ci", ] <- attrdl(x = data$tmean,
                                               basis = cb,
                                               cases = data$dependent,
                                               coef = coefs,
                                               vcov = vcovs,
                                               type = "an",
                                               dir= "forw",
                                               cen = mintempregions[i],
                                               model = model,
                                               range = c(-100, per[i, 1]),
                                               sim = T, nsim = nsim_)

    arraysim[i, "extreme_heat_ci", ] <- attrdl(x = data$tmean,
                                               basis = cb,
                                               cases = data$dependent,
                                               coef = coefs,
                                               vcov = vcovs,
                                               type = "an",
                                               dir= "forw",
                                               cen = mintempregions[i],
                                               model = model,
                                               range = c(per[i, 7], 100),
                                               sim = T, nsim = nsim_)

    # Store the denominator of attributable deaths, i.e. total observed
    # mortality
    # Correct denominator to compute the attributable fraction later, as in
    # attrdl
    totdeath[i] <- sum(data$dependent, na.rm = TRUE)

  }

  attrdl_yr_all <- dplyr::bind_rows(attrdl_yr_all)

  # Compute attributable fraction

  all_data <- dplyr::bind_rows(df_list)

  attr_fractions <- attrdl_yr_all

  totregyear <- all_data %>%
    dplyr::group_by(regnames, year) %>%
    dplyr::summarise(total_deaths = sum(dependent)) %>%
    dplyr::rename(region = regnames)

  attr_fractions_regions <- dplyr::left_join(x = attr_fractions,
                            y = totregyear,
                            by = c("year", "region")) %>%
    dplyr::mutate(glob = glob / total_deaths * 100,
                  heat = heat / total_deaths * 100,
                  cold = cold / total_deaths * 100,
                  extreme_cold = extreme_cold / total_deaths * 100,
                  extreme_heat = extreme_heat / total_deaths * 100) %>%
    dplyr::select(-total_deaths)

  # aggregated across regions

  attr_deaths_all <- attrdl_yr_all %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(glob = sum(glob),
                     heat = sum(heat),
                     cold = sum(cold),
                     extreme_cold = sum(extreme_cold),
                     extreme_heat = sum(extreme_heat))

  totyear <- all_data %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(total_deaths = sum(dependent))

  attr_fractions_all <- dplyr::left_join(x = attr_deaths_all,
                                             y = totyear,
                                             by = "year") %>%
    dplyr::mutate(region = "All",
                  glob = glob / total_deaths * 100,
                  heat = heat / total_deaths * 100,
                  cold = cold / total_deaths * 100,
                  extreme_cold = extreme_cold / total_deaths * 100,
                  extreme_heat = extreme_heat / total_deaths * 100) %>%
    dplyr::select(-total_deaths)

  attr_fractions_yr <- dplyr::bind_rows(attr_fractions_all,
                                         attr_fractions_regions)

  return (list(totdeath, arraysim, matsim, attrdl_yr_all,
               attr_fractions_yr))

}

#' Write outputs to csv
#'
#' @description Write the attributable deaths and temperature for each regions,
#' with empirical CI estimated using the re-centered bases.
#' @param df_list An alphabetically-ordered list of dataframes for each region.
#' @param totdeath A named vector of integers. Total observed mortality per region.
#' @param arraysim An array (numeric). Total (glob),
#' cold and heat-attributable deaths per region for 1000 simulations.
#'   Used to derive confidence intervals.
#' @param matsim A matrix (numeric). Total (glob),
#' cold and heat-attributable deaths per region from reduced coefficients.
#' @param attrdl_yr_all A dataframe with attributable deaths by year for each region.
#' @param attr_fractions_yr A dataframe with attributable fractions by year and region.
#' @param output_folder_path Path to folder for storing outputs.
#'
#' @export
#'
#'
#' @return
#' \itemize{
#'   \item `anregions_bind` A matrix of numbers of deaths attributable to
#'   temperature, heat, cold, extreme heat and extreme cold (with confidence
#'   intervals), disaggregated by region.
#'   \item `antot_bind` A matrix of numbers of numbers of deaths attributable to
#'   temperature, heat, cold, extreme heat and extreme cold (with confidence
#'   intervals).
#'   \item `afregions_bind` A matrix of fractions of all-cause mortality
#'   attributable to temperature, heat, cold, extreme heat and extreme cold
#'   (with confidence intervals), disaggregated by region.
#'    \item `aftot_bind` A matrix of fractions of all-cause mortality
#'   attributable to temperature, heat, cold, extreme heat and extreme cold
#'   (with confidence intervals).
#' }
#' @examples output_folder_path = 'myfolder/output/'
write_attributable_deaths <- function(df_list,
                                      matsim,
                                      arraysim,
                                      totdeath,
                                      attrdl_yr_all,
                                      attr_fractions_yr,
                                      output_folder_path = NULL) {

  # Attributable numbers
  # regions-specific
  anregions <- matsim
  anregionslow <- apply(arraysim, c(1,2), quantile, 0.025)
  anregionshigh <- apply(arraysim, c(1,2), quantile, 0.975)

  colnames(anregionslow) <- paste(colnames(anregionslow), 'low', sep = '_')
  colnames(anregionshigh) <- paste(colnames(anregionshigh), 'high', sep = '_')

  rownames(anregions) <-
    rownames(anregionslow) <-
    rownames(anregionshigh) <-
    names(df_list)

  # Total
  # NB: first sum through regions_df
  antot <- colSums(matsim)
  antotlow <- apply(apply(arraysim, c(2,3), sum), 1, quantile, 0.025)
  antothigh <- apply(apply(arraysim, c(2,3), sum), 1, quantile, 0.975)

  # Total mortality
  # By country
  totdeathtot <- sum(totdeath)

  # Attributable fractions
  # regions-specific
  afregions <- anregions/totdeath * 100
  afregionslow <- anregionslow/totdeath * 100
  afregionshigh <- anregionshigh/totdeath * 100

  # Total
  aftot <- antot/totdeathtot * 100
  aftotlow <- antotlow/totdeathtot * 100
  aftothigh <- antothigh/totdeathtot * 100

  # Bind datasets
  anregions_bind <- t(cbind(anregions, anregionslow, anregionshigh))
  antot_bind <- t(cbind(antot, antotlow, antothigh))
  afregions_bind <- t(cbind(afregions, afregionslow, afregionshigh))
  aftot_bind <- t(cbind(aftot, aftotlow, aftothigh))

  if (!is.null(output_folder_path)) {

    write.csv(anregions_bind,
              file = paste(output_folder_path,
                           'attributable_deaths_regions.csv',
                           sep = ""))
    write.csv(antot_bind,
              file = paste(output_folder_path,
                           'attributable_deaths_total.csv',
                           sep = ""))
    write.csv(afregions_bind,
              file = paste(output_folder_path,
                           'attributable_fraction_regions.csv',
                           sep = ""))
    write.csv(aftot_bind,
              file = paste(output_folder_path,
                           'attributable_fraction_total.csv',
                           sep = ""))

    write.csv(attrdl_yr_all,
              file = paste(output_folder_path,
                           'attributable_deaths_year.csv',
                           sep = ""))

    write.csv(attr_fractions_yr,
              file = paste(output_folder_path,
                           'attributable_fraction_year.csv',
                           sep = ""))

  } else {

    write.csv(anregions_bind,
              'attributable_deaths_regions.csv')
    write.csv(antot_bind,
              'attributable_deaths_total.csv')
    write.csv(afregions_bind,
              'attributable_fraction_regions.csv')
    write.csv(aftot_bind,
              'attributable_fraction_total.csv')
    write.csv(attrdl_yr_all,
              'attributable_deaths_year.csv')
    write.csv(attr_fractions_yr,
              'attributable_fraction_year.csv')

  }

  return(list(anregions_bind, antot_bind, afregions_bind, aftot_bind))

}

#' Plot and write results of analysis
#'
#' @param df_list An alphabetically-ordered
#' list of dataframes for each region.
#' @param blup A list of BLUPs (best linear unbiased predictions).
#' @param mintempregions A named numeric vector.
#'   Minimum (optimum) mortality temperature per region.
#' @param save_fig Whether to save output figure (Bool)
#' @param save_csv Whether to save output CSVs (Bool)
#' @param indepedent_col1_ column name of first extra independent
#' variable to include in regression (excluding temperature,
#' see config file for formula structure). 'None' if none.
#' @param indepedent_col2_ column name of second independent
#' variable to include in regression (excluding temperature,
#' see config file for formula structure). 'None' if none.
#' @param indepedent_col3_ column name of third independent
#' variable to include in regression (excluding temperature,
#' see config file for formula structure). 'None' if none.
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
#' @param output_folder_path Path to folder for storing outputs.
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
#'   \item `tmean_df` A dataframe with daily mean exposure values for each
#'   region.
#' }
#'
#' @examples output_folder_path = 'myfolder/output/'
plot_and_write_relative_risk <- function(df_list,
                                         blup = NULL,
                                         mintempregions,
                                         save_fig = TRUE,
                                         save_csv = TRUE,
                                         output_folder_path,
                                         independent_col1,
                                         independent_col2,
                                         independent_col3,
                                         varfun,
                                         varper,
                                         vardegree,
                                         lag,
                                         lagnk,
                                         dfseas
                                         ) {

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

  per <- t(sapply(df_list,function(x)
    quantile(x$tmean, c(2.5, 10, 25, 50, 75, 90, 97.5) / 100, na.rm = TRUE)))

  xlab <- expression(paste("Temperature (",degree,"C)"))

  region_vector <- c()
  temp_vector <- c()
  relative_risk_vector <- c()
  cen_vector <- c()
  upper_vector <- c()
  lower_vector <- c()

  tmean_vector <- c()
  tmean_region_vector <- c()

  no_of_regions <- seq(length(df_list))

  for(i in no_of_regions) {

    data <- df_list[[i]]

    # NB: Centering point different than original choice of 75th
    argvar <- list(x = data$tmean,
                   fun = varfun,
                   degree = vardegree,
                   knots = quantile(data$tmean,
                                  varper / 100, na.rm = TRUE))

    if (!is.null(blup)) {

      bvar <- do.call(onebasis, argvar)

      coefs <- blup[[i]]$blup
      vcovs <- blup[[i]]$vcov
      model <- NULL
      cen <- mintempregions[i]

      pred <- crosspred(bvar,
                        coef = blup[[i]]$blup,
                        vcov = blup[[i]]$vcov,
                        model.link = "log",
                        by = 0.1,
                        cen = cen)

    } else {

      # Run the model and obtain predictions
      c(model, cb) %<-% define_model(dataset = data,
                                     independent_col1 = independent_col1,
                                     independent_col2 = independent_col2,
                                     independent_col3 = independent_col3,
                                     varfun = varfun,
                                     varper = varper,
                                     vardegree = vardegree,
                                     lag = lag,
                                     lagnk = lagnk,
                                     dfseas = dfseas)

      cen <- mean(data$tmean)
      pred <- crossreduce(cb, model, cen = cen)

      mintempregions[i] <- as.numeric(names(which.min(pred$RRfit)))
      cen <- mintempregions[i]
      pred <- crossreduce(cb, model, cen = cen)

      }

     plot(pred, type = "n",
          ylim = c(0, 3),
          yaxt = "n",
          lab = c(6, 5, 7),
          xlab = xlab,
          ylab = "RR",
          main = names(df_list)[i])

     ind_a <- pred$predvar <= c(per[i,c("2.5%")])
     ind_b <- pred$predvar >= c(per[i,c("2.5%")]) & pred$predvar <= cen
     ind_c <- pred$predvar >= cen & pred$predvar <= c(per[i,c("97.5%")])
     ind_d <- pred$predvar >= c(per[i,c("97.5%")])

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
           col = c("#FFA7A7"),
           lwd = 1.5)
     lines(pred$predvar[ind_d],
           relative_risk_vals[ind_d],
           col = c("#C00000"),
           lwd = 1.5)

     axis(2, at = 1:5 * 0.5)

     breaks <- c(min(data$tmean, na.rm = TRUE) - 1,
                 seq(pred$predvar[1],
                     pred$predvar[length(pred$predvar)],
                     length = 30),
                 max(data$tmean, na.rm = TRUE) + 1)


     hist <- hist(data$tmean, breaks = breaks, plot = FALSE)
     hist$density <- hist$density / max(hist$density) * 0.7
     prop <- max(hist$density) / max(hist$counts)
     counts <- pretty(hist$count, 3)

     plot(hist,
          ylim = c(0, max(hist$density) * 3.5),
          axes = FALSE, ann = FALSE, col = grey(0.95),
          breaks = breaks, freq = FALSE, add = TRUE)

     axis(4, at = counts * prop, labels = counts, cex.axis = 0.7)
     mtext("N", 4, line = -0.5, at = mean(counts * prop), cex = 0.5)

     abline(v = mintempregions[i], lty = 3)
     abline(v = c(per[i, c("2.5%", "97.5%")]), lty = 2)

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

     tmean_vector <- append(tmean_vector,
                            data$tmean)

     tmean_region_vector <-
       append(tmean_region_vector,
              rep(names(df_list)[i],
                  length(data$tmean)))


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

  tmean_df <- data.frame(temp_mean = tmean_vector,
                         regions = tmean_region_vector)

  if (save_csv == TRUE) {

      write.csv(output_df,
             paste(output_folder_path,
                   'output_all_regions_data.csv', sep = ''),
             row.names = FALSE)

    if (!is.null(blup)) {

      relative_risk <- pred$allRRfit

    } else {

      relative_risk <- pred$RRfit
    }

    # Output for testing
     output_df_test <- data.frame(
                             temperature = pred$predvar,
                             relative_risk = relative_risk
                             )
    # Output for testing
     write.csv(output_df_test,
               paste(output_folder_path,
                     'output_one_region_data_new.csv', sep = ''),
               row.names = FALSE
               )
  }

  return (list(output_df, tmean_df))

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
#'   \item `tmean_df` A dataframe with daily mean exposure values for each
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
  cen_vector <- c()

  tmean_vector <- c()
  tmean_region_vector <- c()

  data <- do.call(rbind, df_list)

  # Estimation method
  method <- "reml"

  # Overall cumulative summary for main model
  mvall <- mvmeta(coef ~ 1, vcov, method = method)

  # Exclude extreme temps
  # predvar <- quantile(data$tmean, 1:99/100, na.rm=T)

  # All temps
  predvar <- data$tmean

  argvar <- list(x = predvar,
                 fun = varfun,
                 degree = vardegree,
                 knots = quantile(data$tmean,
                                varper / 100, na.rm = TRUE),
                 Bound = range(data$tmean, na.rm = TRUE))

  bvar <- do.call(onebasis, argvar)

  model <- NULL
  cen <- median(mintempregions)

  pred <- crosspred(bvar,
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
       main = dependent_col,
)

  abline(h = 1)

  optimal_meta_lower <- as.numeric(names(
    which.min(which(pred$allRRfit >= 1 & pred$allRRfit <= 1.1))))
  optimal_meta_upper <- as.numeric(names(
    which.max(which(pred$allRRfit >= 1 & pred$allRRfit <= 1.1))))

  ind_a <- pred$predvar <= optimal_meta_lower
  ind_b <- pred$predvar >= optimal_meta_lower & pred$predvar <= cen
  ind_c <- pred$predvar >= cen & pred$predvar <= optimal_meta_upper
  ind_d <- pred$predvar >= optimal_meta_upper

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
        col = c("#FFA7A7"),
        lwd = 1.5)
  lines(pred$predvar[ind_d],
        pred$allRRfit[ind_d],
        col = c("#C00000"),
        lwd = 1.5)

  axis(2, at = 1:5 * 0.5)

  breaks <- c(min(data$tmean, na.rm = TRUE) - 1,
              seq(pred$predvar[1],
                  pred$predvar[length(pred$predvar)],
                  length = 30),
              max(data$tmean, na.rm = TRUE) + 1)


  # hist <- hist(data$tmean, breaks = breaks, plot = F)
  # hist$density <- hist$density / max(hist$density) * 0.7
  # prop <- max(hist$density) / max(hist$counts)
  # counts <- pretty(hist$count, 3)

  # plot(hist,
  #      ylim = c(0,max(hist$density)*3.5),
  #      axes = F, ann = F, col = grey(0.95),
  #      breaks = breaks, freq = F, add = T)
  #
  # axis(4, at = counts*prop, labels = counts, cex.axis = 0.7)
  # mtext("N",4,line = -0.5,at = mean(counts*prop),cex=0.5)
  #
  # axis(1,at = c(-5,0,5,10,15,20,25,30))
  # axis(2,at = 1:6*0.5)

  abline(v = optimal_meta_lower, lty = 3)
  abline(v = optimal_meta_upper, lty = 3)

  relative_risk_vector <- append(relative_risk_vector,
                                   pred$allRRfit)

  region_vector <-
    append(region_vector,
           rep('England',
               length(pred$predvar)))

  temp_vector <- append(temp_vector,
                        pred$predvar)

  cen_vector <- append(cen_vector,
                       rep(cen,
                           length(pred$predvar)))

  tmean_vector <- append(tmean_vector,
                         data$tmean)

  tmean_region_vector <-
    append(tmean_region_vector,
           rep('England',
               length(data$tmean)))

  if (save_fig == TRUE) {

    dev.off()

  }

  output_df <- data.frame(regions = region_vector,
                          temp = temp_vector,
                          rel_risk = relative_risk_vector,
                          centre_temp = cen_vector,
                          upper = upper_vector,
                          lower = lower_vector)

  tmean_df <- data.frame(temp_mean = tmean_vector,
                         regions = tmean_region_vector)

  if (save_csv == TRUE) {

    write.csv(output_df,
              paste(output_folder_path,
                    'output_all_data.csv', sep = ''),
              row.names = FALSE)


    relative_risk <- pred$allRRfit

    # Output for testing
    output_df_test <- data.frame(
      temperature = pred$predvar,
      relative_risk = relative_risk
    )
    # Output for testing
    write.csv(output_df_test,
              paste(output_folder_path,
                    'output_one_data_new.csv', sep = ''),
              row.names = FALSE
    )

  }

  return (list(output_df, tmean_df))

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
#' @param time_range_start_ Start of time range over which to run the analysis.  'None' if over full range.
#' @param time_range_end_ End of time range over which to run the analysis.  'None' if over full range.
#' @param dependent_col_ the column name of the
#' dependent variable of interest e.g. deaths
#' @param indepedent_col1_ column name of first extra independent
#' variable to include in regression (excluding temperature,
#' see config file for formula structure). 'None' if none.
#' @param indepedent_col2_ column name of second independent
#' variable to include in regression (excluding temperature,
#' see config file for formula structure). 'None' if none.
#' @param indepedent_col3_ column name of third independent
#' variable to include in regression (excluding temperature,
#' see config file for formula structure). 'None' if none.
#' @param time_col_ The column name of column containing dates (e.g date, year).
#' @param region_col_ The column name of the column containing regions.
#' @param temp_col_ the column name of the column containing the exposure.
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
#'   \item `tmean_df` A dataframe with daily mean exposure values for each
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
do_analysis <- function(input_csv_path_,
                        output_folder_path_ = NULL,
                        save_fig_ = TRUE,
                        save_csv_ = TRUE,
                        meta_analysis = TRUE,
                        by_region = TRUE,
                        time_range_start_ = 'NONE',
                        time_range_end_ = 'NONE',
                        dependent_col_ = 'death',
                        independent_col1_ = 'NONE',
                        independent_col2_ = 'NONE',
                        independent_col3_ = 'NONE',
                        time_col_ = 'date',
                        region_col_ = 'regnames',
                        temp_col_ = 'tmean',
                        varfun_ = 'bs',
                        vardegree_ = 2,
                        lag_  = 21,
                        lagnk_ = 3,
                        dfseas_ = 8
                        ) {

  varper_ <- c(10, 75, 90)

  c(df_list_) %<-%
    load_data(
      input_csv_path = input_csv_path_,
      dependent_col = dependent_col_,
      time_col = time_col_,
      region_col = region_col_,
      temp_col = temp_col_,
      time_range_start = time_range_start_,
      time_range_end = time_range_end_
      )

  if (meta_analysis == TRUE) {

    c(coef_, vcov_) %<-%
    run_model(df_list = df_list_,
              independent_col1 = independent_col1_,
              independent_col2 = independent_col2_,
              independent_col3 = independent_col3_,
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

    c(avgtmean_wald, rangetmean_wald) %<-%
      wald_results(
        mv = mv_
        )

  } else {

    blup_ <- NULL

  }

  c(mintempregions_) %<-%
    calculate_min_mortality_temp(
      df_list = df_list_,
      blup = blup_,
      independent_col1 = independent_col1_,
      independent_col2 = independent_col2_,
      independent_col3 = independent_col3_,
      varfun = varfun_,
      varper = varper_,
      vardegree = vardegree_,
      lag = lag_,
      lagnk = lagnk_,
      dfseas = dfseas_
      )

  c(totdeath_, arraysim_, matsim_, attrdl_yr_all,
    attr_fractions_yr) %<-%
    compute_attributable_deaths(
      df_list = df_list_,
      blup = blup_,
      mintempregions = mintempregions_,
      independent_col1 = independent_col1_,
      independent_col2 = independent_col2_,
      independent_col3 = independent_col3_,
      varfun = varfun_,
      varper = varper_,
      vardegree = vardegree_,
      lag = lag_,
      lagnk = lagnk_,
      dfseas = dfseas_
      )

  c(anregions_bind, antot_bind, afregions_bind, aftot_bind) %<-%
    write_attributable_deaths(
      df_list = df_list_,
      matsim = matsim_,
      arraysim = arraysim_,
      totdeath = totdeath_,
      output_folder_path = output_folder_path_,
      attrdl_yr_all = attrdl_yr_all,
      attr_fractions_yr = attr_fractions_yr
      )

  if (by_region == FALSE) {

    c(output_df, tmean_df) %<-%
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

    c(output_df, tmean_df) %<-%
      plot_and_write_relative_risk(
      df_list = df_list_,
      blup = blup_,
      mintempregions = mintempregions_,
      save_fig = save_fig_,
      save_csv = save_csv_,
      output_folder_path = output_folder_path_,
      independent_col1 = independent_col1_,
      independent_col2 = independent_col2_,
      independent_col3 = independent_col3_,
      varfun = varfun_,
      varper = varper_,
      vardegree = vardegree_,
      lag = lag_,
      lagnk = lagnk_,
      dfseas = dfseas_
      )

  }

  return (list(output_df, tmean_df, anregions_bind, attrdl_yr_all,
               attr_fractions_yr))

}

###
### (c) Antonio Gasparrini 2015-2017
#
################################################################################
# FUNCTION FOR COMPUTING ATTRIBUTABLE MEASURES FROM DLNM
#   REQUIRES dlnm VERSION 2.2.0 AND ON
################################################################################
#
# DISCLAIMER:
#   THE CODE COMPOSING THIS FUNCTION HAS NOT BEEN SYSTEMATICALLY TESTED. THE
#   PRESENCE OF BUGS CANNOT BE RULED OUT. ALSO, ALTHOUGH WRITTEN GENERICALLY
#   FOR WORKING IN DIFFERENT SCENARIOS AND DATA, THE FUNCTION HAS NOT BEEN
#   TESTED IN CONTEXTS DIFFERENT THAN THE EXAMPLE INCLUDED IN THE PAPER.
#   IT IS RESPONSIBILITY OF THE USER TO CHECK THE RELIABILITY OF THE RESULTS IN
#   DIFFERENT APPLICATIONS.
#
# Version: 25 January 2017
# AN UPDATED VERSION CAN BE FOUND AT:
#   https://github.com/gasparrini/2014_gasparrini_BMCmrm_Rcodedata
#
################################################################################
# SEE THE PDF WITH A DETAILED DOCUMENTATION AT www.ag-myresearch.com
#
#   - x: AN EXPOSURE VECTOR OR (ONLY FOR dir="back") A MATRIX OF LAGGED EXPOSURES
#   - basis: THE CROSS-BASIS COMPUTED FROM x
#   - cases: THE CASES VECTOR OR (ONLY FOR dir="forw") THE MATRIX OF FUTURE CASES
#   - model: THE FITTED MODEL
#   - coef, vcov: COEF AND VCOV FOR basis IF model IS NOT PROVIDED
#   - model.link: LINK FUNCTION IF model IS NOT PROVIDED
#   - type: EITHER "an" OR "af" FOR ATTRIBUTABLE NUMBER OR FRACTION
#   - dir: EITHER "back" OR "forw" FOR BACKWARD OR FORWARD PERSPECTIVES
#   - tot: IF TRUE, THE TOTAL ATTRIBUTABLE RISK IS COMPUTED
#   - cen: THE REFERENCE VALUE USED AS COUNTERFACTUAL SCENARIO
#   - range: THE RANGE OF EXPOSURE. IF NULL, THE WHOLE RANGE IS USED
#   - sim: IF SIMULATION SAMPLES SHOULD BE RETURNED. ONLY FOR tot=TRUE
#   - nsim: NUMBER OF SIMULATION SAMPLES
################################################################################
attrdl <- function(x,
                   basis,
                   cases,
                   model = NULL,
                   coef = NULL,
                   vcov = NULL,
                   model.link = NULL,
                   type = "af",
                   dir = "back",
                   tot = TRUE,
                   cen,
                   range = NULL,
                   sim = FALSE,
                   nsim=5000
                   ){
  ################################################################################
  #
  # CHECK VERSION OF THE DLNM PACKAGE
  if(packageVersion("dlnm") < "2.2.0")
    stop("update dlnm package to version >= 2.2.0")
  #
  # EXTRACT NAME AND CHECK type AND dir
  name <- deparse(substitute(basis))
  type <- match.arg(type, c("an", "af"))
  dir <- match.arg(dir, c("back", "forw"))
  #
  # DEFINE CENTERING
  if(missing(cen) && is.null(cen <- attr(basis, "argvar")$cen))
    stop("'cen' must be provided")
  if(!is.numeric(cen) && length(cen) > 1L) stop("'cen' must be a numeric scalar")
  attributes(basis)$argvar$cen <- NULL
  #
  # SELECT RANGE (FORCE TO CENTERING VALUE OTHERWISE, MEANING NULL RISK)
  if(!is.null(range)) x[x < range[1]|x > range[2]] <- cen
  #
  # COMPUTE THE MATRIX OF
  #   - LAGGED EXPOSURES IF dir="back"
  #   - CONSTANT EXPOSURES ALONG LAGS IF dir="forw"
  lag <- attr(basis, "lag")
  if(NCOL(x) == 1L) {
    at <- if(dir == "back") tsModel:::Lag(x, seq(lag[1], lag[2])) else
      matrix(rep(x, diff(lag) + 1), length(x))
  } else {
    if(dir == "forw") stop("'x' must be a vector when dir='forw'")
    if(ncol(at <- x) != diff(lag) + 1)
      stop("dimension of 'x' not compatible with 'basis'")
  }
  #
  # NUMBER USED FOR THE CONTRIBUTION AT EACH TIME IN FORWARD TYPE
  #   - IF cases PROVIDED AS A MATRIX, TAKE THE ROW AVERAGE
  #   - IF PROVIDED AS A TIME SERIES, COMPUTE THE FORWARD MOVING AVERAGE
  #   - THIS EXCLUDES MISSING ACCORDINGLY
  # ALSO COMPUTE THE DENOMINATOR TO BE USED BELOW
  if(NROW(cases) != NROW(at)) stop("'x' and 'cases' not consistent")
  if(NCOL(cases) > 1L) {
    if(dir == "back") stop("'cases' must be a vector if dir='back'")
    if(ncol(cases) != diff(lag) + 1) stop("dimension of 'cases' not compatible")
    den <- sum(rowMeans(cases, na.rm = TRUE), na.rm = TRUE)
    cases <- rowMeans(cases)
  } else {
    den <- sum(cases, na.rm = TRUE)
    if(dir == "forw")
      cases <- rowMeans(as.matrix(tsModel:::Lag(cases, -seq(lag[1], lag[2]))))
  }
  #
  ################################################################################
  #
  # EXTRACT COEF AND VCOV IF MODEL IS PROVIDED
  if(!is.null(model)) {
    cond <- paste0(name, "[[:print:]]*v[0-9]{1,2}\\.l[0-9]{1,2}")
    if(ncol(basis) == 1L) cond <- name
    model.class <- class(model)
    coef <- dlnm:::getcoef(model, model.class)
    ind <- grep(cond, names(coef))
    coef <- coef[ind]
    vcov <- dlnm:::getvcov(model, model.class)[ind, ind, drop = FALSE]
    model.link <- dlnm:::getlink(model, model.class)
    if(!model.link %in% c("log", "logit"))
      stop("'model' must have a log or logit link function")
  }
  #
  # IF REDUCED ESTIMATES ARE PROVIDED
  typebasis <- ifelse(length(coef) != ncol(basis), "one", "cb")
  #
  ################################################################################
  #
  # PREPARE THE ARGUMENTS FOR TH BASIS TRANSFORMATION
  predvar <- if(typebasis == "one") x else seq(NROW(at))
  predlag <- if(typebasis == "one") 0 else dlnm:::seqlag(lag)
  #
  # CREATE THE MATRIX OF TRANSFORMED CENTRED VARIABLES (dependent_col ON typebasis)
  if(typebasis == "cb") {
    Xpred <- dlnm:::mkXpred(typebasis, basis, at, predvar, predlag, cen)
    Xpredall <- 0
    for (i in seq(length(predlag))) {
      ind <- seq(length(predvar)) + length(predvar) * (i-1)
      Xpredall <- Xpredall + Xpred[ind, , drop = FALSE]
    }
  } else {
    basis <- do.call(onebasis, c(list(x = x), attr(basis, "argvar")))
    Xpredall <- dlnm:::mkXpred(typebasis, basis, x, predvar, predlag, cen)
  }
  #
  # CHECK DIMENSIONS
  if(length(coef) != ncol(Xpredall))
    stop("arguments 'basis' do not match 'model' or 'coef'-'vcov'")
  if(any(dim(vcov) != c(length(coef), length(coef))))
    stop("arguments 'coef' and 'vcov' do no match")
  if(typebasis == "one" && dir == "back")
    stop("only dir='forw' allowed for reduced estimates")
  #
  ################################################################################
  #
  # COMPUTE AF AND AN
  af <- 1 - exp(-drop(as.matrix(Xpredall %*% coef)))
  an <- af * cases
  #
  # TOTAL
  #   - SELECT NON-MISSING OBS CONTRIBUTING TO COMPUTATION
  #   - DERIVE TOTAL AF
  #   - COMPUTE TOTAL AN WITH ADJUSTED DENOMINATOR (OBSERVED TOTAL NUMBER)
  if(tot) {
    isna <- is.na(an)
    af <- sum(an[!isna]) / sum(cases[!isna])
    an <- af * den
  }
  #
  ################################################################################
  #
  # EMPIRICAL CONFIDENCE INTERVALS
  if(!tot && sim) {
    sim <- FALSE
    warning("simulation samples only returned for tot=T")
  }
  if(sim) {
    # SAMPLE COEF
    k <- length(coef)
    eigen <- eigen(vcov)
    X <- matrix(rnorm(length(coef) * nsim), nsim)
    coefsim <- coef + eigen$vectors %*% diag(sqrt(eigen$values), k) %*% t(X)
    # RUN THE LOOP
    # pre_afsim <- (1 - exp(- Xpredall %*% coefsim)) * cases # a matrix
    # afsim <- colSums(pre_afsim,na.rm=TRUE) / sum(cases[!isna],na.rm=TRUE)
    afsim <- apply(coefsim, 2, function(coefi) {
      ani <- (1 - exp(-drop(Xpredall %*% coefi))) * cases
      sum(ani[!is.na(ani)]) / sum(cases[!is.na(ani)])
    })
    ansim <- afsim * den
  }
  #
  ################################################################################
  #
  res <- if(sim) {
    if(type == "an") ansim else afsim
  } else {
    if(type == "an") an else af
  }
  #
  return(res)
}

#
