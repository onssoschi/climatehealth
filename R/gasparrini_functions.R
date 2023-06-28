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
#' @param input_path Path to a CSV containing a daily time series of death and temperature per region.
#' @return
#' \itemize{
#'   \item `df_list_unordered` A list of dataframes for each region comprising dates, deaths, and temperatures.
#'   \item `regions` A character vector with the names of each region.
#'   }
#' @export
load_data <- function(input_path) {

  if(substr(input_path, nchar(input_path) - 3, nchar(input_path)) !=  '.csv') {
    stop("Input path must be a CSV")
  }

  df <- read.csv(input_path, row.names=1)
  df$date <- as.Date(df$date)

  regions <- as.character(unique(df$regnames)) # .distinct() on regnames

  df_list_unordered <- lapply(regions,
                              function(x) df[df$regnames == x, ])

  names(df_list_unordered) <- regions

  return (list(df_list_unordered, regions))

}

#' Get region metadata for analysis
#'
#' @param regions A character vector with the names of each region.
#' **NOTE** Must be same order as input data.
#' @param df_list_unordered A list of dataframes for each region.
#'
#' @return
#' \itemize{
#'   \item `regions_df` A dataframe with two columns. Column 1 is abbreviated
#'   region names. Column 2 is user-specified region names.
#'   \item `df_list` An alphabetically-ordered list of dataframes for each region. Same length as `regions`.
#'   }
#' @export
get_region_metadata <- function(regions, df_list_unordered, region_names = NULL) {

  if (!is.null(region_names)) {

    region_names = region_names

  } else {

    region_names = regions
  }

  regions_df <- data.frame(
    regions = regions,
    region_names = region_names)

  # Order
  ord <- order(regions_df$region_names)
  df_list <- df_list_unordered[ord]
  regions_df <- regions_df[ord,]

  return (list(regions_df, df_list))

}

#' Define and run poisson regression model for each dataframe
#'
#' @param regions_df A dataframe with two columns. Column 1 is abbreviated
#'   region names. Column 2 is user-specified region names.
#' @param df_list An alphabetically-ordered list of dataframes for each region.
#' @return
#' \itemize{
#'   \item `argvar` A list of arguments ($fun, $knots, $degree) for cross-basis function.
#'   \item `coef` A matrix of coefficients for reduced model.
#'   \item `vcov` A list. Co-variance matrices for each region for reduced model.
#'   }
#' @export
run_model <- function(df_list, regions_df) {

  # Loop
  time <- proc.time()[3]

  varper <- c(10, 75, 90)

  # Model formula
  formula <- death ~ cb + dow + ns(date, df = config$dfseas * length(unique(year)))

  # Coefficients and vcov for overall cumulative summary
  coef <- matrix(NA,
                 nrow(regions_df),
                 length(varper) + config$vardegree,
                 dimnames = list(regions_df$regions))

  vcov <- vector("list" ,nrow(regions_df))

  names(vcov) <- regions_df$regions

  for(i in seq(length(df_list))) {

    cat(i,"")

    # Extract data
    data <- df_list[[i]]

    # Define crossbasis
    argvar <- list(fun = config$varfun,
                   knots = quantile(data$tmean, varper/100, na.rm=T),
                   degree = config$vardegree)

    cb <- crossbasis(data$tmean,
                     lag = config$lag,
                     argvar = argvar,
                     arglag = list(knots = logknots(config$lag,config$lagnk)))

    # Run the model and obtain predictions
    model <- glm(formula,
                 data,
                 family = quasipoisson,
                 na.action = "na.exclude")

    cen <- mean(data$tmean, na.rm = T)

    pred <- crosspred(cb, model, cen = cen)

    # Reduction to overall cumulative
    red <- crossreduce(cb, model, cen = cen)

    coef[i,] <- coef(red)
    vcov[[i]] <- vcov(red)

  }

  proc.time()[3]-time

  return (list(argvar, coef, vcov))
}


#' Meta-analysis model
#'
#' Runs meta-analysis model and estimates best linear unbiased predictions
#' (BLUPs) from this model.
#'
#' @param df_list An alphabetically-ordered list of dataframes for each region.
#' @param regions_df A dataframe with two columns. Column 1 is abbreviated
#'   region names. Column 2 is user-specified region names.
#' @param coef A matrix of coefficients for reduced model.
#' @param vcov A list of co-variance matrices for reduced model.
#'
#' @return
#' \itemize{
#'   \item `mvmeta` A model object. A multivariate meta-analysis model.
#'   \item `blup` A list. BLUP (best linear unbiased predictions) from the
#'   meta-analysis model for each region.
#'   }
#' @export
#' @import mvmeta
run_meta_model <- function(df_list, regions_df, coef, vcov) {

  if(!is.list(df_list) | !is.data.frame(df_list[[1]])) {
    stop("Argument 'df_list' must be a list of data frames")
  }

  if(!is.data.frame(regions_df)) {
    stop("Argument 'regions_df' must be a data frame")
  }

  if(!is.matrix(coef) | !is.numeric(coef)) {
    stop("Argument 'coef' must be a numeric matrix")
  }

  if(!is.list(vcov) | !is.matrix(vcov[[1]])) {
    stop("Argument 'vcov' must be a list of matrices")
  }

  # Create average temperature and range as meta-predictors
  avgtmean <- sapply(df_list,
                     function(x) mean(x$tmean, na.rm = TRUE))
  rangetmean <- sapply(df_list,
                       function(x) diff(range(x$tmean, na.rm = TRUE)))

  # Meta-analysis
  # NB: country effects is not included in this example
  mv <- mvmeta(coef ~ avgtmean + rangetmean,
               vcov,
               data = regions_df,
               control = list(showiter = TRUE))

  # Obtain blups
  blup <- blup(mv, vcov = T)

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
#' @return P-values for average and range of temperatures (avgtmean_wald, rangetmean_wald).
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
#' @param regions_df A dataframe with two columns. Column 1 is abbreviated
#'   region names. Column 2 is user-specified region names.
#' @param blup A list of BLUPs (best linear unbiased predictions).
#'
#' @return
#' \itemize{
#'   \item `argvar` An updated list of arguments ($x, $fun, $knots, $degree, $bound) for onebasis function.
#'   \item `bvar` A basis matrix for the predictor vector.
#'   \item `mintempregions` A named numeric vector. Minimum (optimum) mortality temperature per region.
#' }
#'
#' @export
calculate_min_mortality_temp <-  function(df_list, regions_df, blup) {

  if(!is.list(df_list) | !is.data.frame(df_list[[1]])) {
    stop("Argument 'df_list' must be a list of data frames")
  }

  if(!is.data.frame(regions_df)) {
    stop("Argument 'regions_df' must be a data frame")
  }

  if(!is.list(blup)) {
    stop("Argument 'blup' must be a list")
  }

  # Re-centering
  # Generate the matrix for storing results
  minpercregions <- mintempregions <- rep(NA, length(df_list))
  names(mintempregions) <- names(minpercregions) <- regions_df$regions

  varper <- c(10, 75, 90)

  # Define minimum mortality values: exclude low and very hot temperatures
  for(i in seq(length(df_list))) {

    data <- df_list[[i]]
    predvar <- quantile(data$tmean, 1:99/100, na.rm = T)

    # Redefine the function using all arguments (boundary knots included)
    argvar <- list(x = predvar, fun = config$varfun,
                   knots = quantile(data$tmean, varper/100, na.rm = TRUE),
                   degree = config$vardegree,
                   Bound = range(data$tmean, na.rm = T))

    bvar <- do.call(onebasis, argvar)

    minpercregions[i] <- (1:99)[which.min((bvar %*% blup[[i]]$blup))]
    mintempregions[i] <- quantile(data$tmean, minpercregions[i]/100,
                                  na.rm = TRUE)
  }

  # Country-specific points of minimum mortality
  (minperccountry <- median(minpercregions))

  return(list(argvar = argvar, bvar = bvar,
              mintempregions = mintempregions))

}

#' Compute attributable deaths
#'
#' Compute the attributable deaths for each regions,
#' with empirical CI estimated using the re-centered bases.
#'
#' @param df_list An alphabetically-ordered list of dataframes for each region.
#' @param regions_df A dataframe with two columns. Column 1 is abbreviated
#'   region names. Column 2 is user-specified region names.
#' @param coef A matrix of coefficients for reduced model.
#' @param vcov A co-variance matrix for reduced model.
#' @param argvar An updated list of arguments ($x, $fun, $knots, $degree, $bound) for cross-basis function.
#' @param bvar A basis matrix for the predictor vector.
#' @param blup A list of BLUPs (best linear unbiased predictions).
#' @param mintempregions A named numeric vector. Minimum (optimum) mortality temperature per region.
#'
#' @return A list of variables
#' \itemize{
#'   \item `totdeath` A named vector of integers. Total observed mortality per region.
#'   \item `arraysim` An array (numeric). Total (glob), cold and heat-attributable deaths per region for 1000 simulations.
#'   Used to derive confidence intervals.
#'   \item `matsim` A matrix (numeric). Total (glob), cold and heat-attributable deaths per region from reduced coefficients.
#' }
#' @export
compute_attributable_deaths <- function(df_list, regions_df, coef, vcov,
                                        argvar, bvar, blup,
                                        mintempregions) {

  if (file.exists('R/attrdl.R')) {
    source('R/attrdl.R')
  } else {
    source('testdata/attrdl.R')
  }

  # Create the vectors to store the total mortality (accounting for missing)
  totdeath <- rep(NA, nrow(regions_df))
  names(totdeath) <- regions_df$regions

  # Create the matrix to store the attributable deaths
  matsim <- matrix(NA, nrow(regions_df), 3,
                   dimnames = list(regions_df$regions,
                                   c("glob","cold","heat")))

  # Number of simulation runs for computing empirical CI
  nsim <- 1000

  # Create the array to store the CI of attributable deaths
  arraysim <- array(NA, dim = c(nrow(regions_df),3,nsim),
                    dimnames = list(regions_df$regions,
                                    c("glob","cold","heat")))

  varper <- c(10, 75, 90)

  # Run the loop
  for(i in seq(df_list)){

    # Print
    cat(i, "")

    # Extract the data
    data <- df_list[[i]]

    # Derive the cross-basis
    # NB: Centering point different than original choice of 75th
    argvar <- list(x=data$tmean,
                   fun = config$varfun,
                   knots = quantile(data$tmean, varper/100,na.rm=T),
                   degree = config$vardegree)

    cb <- crossbasis(data$tmean,
                     lag = config$lag,
                     argvar = argvar,
                     arglag = list(knots = logknots(config$lag, config$lagnk)))

    # Compute the attributable deaths
    # NB: The reduced coefficients are used here
    matsim[i, "glob"] <- attrdl(data$tmean, cb, data$death,
                               coef=blup[[i]]$blup,
                               vcov = blup[[i]]$vcov,
                               type="an",
                               dir = "forw",
                               cen = mintempregions[i])

    matsim[i, "cold"] <- attrdl(data$tmean, cb, data$death,
                               coef = blup[[i]]$blup,
                               vcov = blup[[i]]$vcov,
                               type = "an",
                               dir = "forw",
                               cen = mintempregions[i],
                               range = c(-100,mintempregions[i]))

    matsim[i, "heat" ] <- attrdl(data$tmean, cb, data$death,
                               coef = blup[[i]]$blup,
                               vcov = blup[[i]]$vcov,
                               type="an",
                               dir = "forw",
                               cen = mintempregions[i],
                               range = c(mintempregions[i],100))

    # Compute empirical occurrences of the attributable deaths
    # Used to derive confidence intervals
    arraysim[i, "glob", ] <- attrdl(data$tmean, cb, data$death,
                                  coef = blup[[i]]$blup,
                                  vcov = blup[[i]]$vcov,
                                  type = "an",
                                  dir = "forw",
                                  cen = mintempregions[i],
                                  sim = T, nsim = nsim)

    arraysim[i, "cold", ] <- attrdl(data$tmean, cb, data$death,
                                  coef=blup[[i]]$blup,
                                  vcov = blup[[i]]$vcov,
                                  type = "an",
                                  dir = "forw",
                                  cen = mintempregions[i],
                                  range = c(-100,mintempregions[i]),
                                  sim = T ,nsim = nsim)

    arraysim[i, "heat", ] <- attrdl(data$tmean, cb, data$death,
                                  coef=blup[[i]]$blup,
                                  vcov = blup[[i]]$vcov,
                                  type = "an",
                                  dir= "forw",
                                  cen = mintempregions[i],
                                  range = c(mintempregions[i],100),
                                  sim = T, nsim = nsim)

    # Store the denominator of attributable deaths, i.e. total observed mortality
    # Correct denominator to compute the attributable fraction later, as in attrdl
    totdeath[i] <- sum(data$death,na.rm=T)

  }

  return (list(totdeath, arraysim, matsim))

}

#' Write outputs to csv
#'
#' @description Write the attributable deaths and temperature for each regions,
#' with empirical CI estimated using the re-centered bases.
#'
#' @param df_list An alphabetically-ordered list of dataframes for each region.
#' @param regions_df A dataframe with two columns. Column 1 is abbreviated
#'   region names. Column 2 is user-specified region names.
#' @param totdeath A named vector of integers. Total observed mortality per region.
#' @param arraysim An array (numeric). Total (glob), cold and heat-attributable deaths per region for 1000 simulations.
#'   Used to derive confidence intervals.
#' @param matsim A matrix (numeric). Total (glob), cold and heat-attributable deaths per region from reduced coefficients.
#' @param output_folder_path Path to folder for storing outputs.
#'
#' @export
#'
#' @return None
#' @examples output_folder_path = 'myfolder/output/'
write_attributable_deaths <- function(df_list, regions_df, matsim, arraysim,
                                      totdeath, output_folder_path = NULL) {

  # Attributable numbers
  # regions-specific
  anregions <- matsim
  anregionslow <- apply(arraysim,c(1,2),quantile,0.025)
  anregionshigh <- apply(arraysim,c(1,2),quantile,0.975)
  rownames(anregions) <- rownames(anregionslow) <- rownames(anregionshigh) <- regions_df$region_names

  # Total
  # NB: first sum through regions_df
  antot <- colSums(matsim)
  antotlow <- apply(apply(arraysim,c(2,3),sum),1,quantile,0.025)
  antothigh <- apply(apply(arraysim,c(2,3),sum),1,quantile,0.975)

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

  # # Temperature
  # tmean_uk <- sapply(df_list, function(region) mean(region$tmean, na.rm = T))
  #
  # total <- c(Country = "UK",
  #   Period = paste(range(df_list[[1]]$year), collapse = "-"),
  #   Deaths = totdeathtot,
  #   Temperature = paste0(formatC(mean(tmean_uk), dig = 1,
  #                              format = "f")," (",
  #                      paste(formatC(range(tmean_uk), dig = 1, format="f"),
  #                                                     collapse = "-"),")"))

  if (!is.null(output_folder_path)) {

    write.csv(anregions_bind, file = paste(output_folder_path, 'attributable_deaths_regions.csv',  sep = ""))
    write.csv(antot_bind, file = paste(output_folder_path, 'attributable_deaths_total.csv',  sep = ""))
    write.csv(afregions_bind, file = paste(output_folder_path, 'attributable_fraction_regions.csv', sep = ""))
    write.csv(aftot_bind, file = paste(output_folder_path, 'attributable_fraction_total.csv',  sep = ""))
    # write.csv(total, file = paste(output_folder_path, 'death_temp_total.csv',  sep = ""))

  } else {

    write.csv(anregions_bind, 'attributable_deaths_regions.csv')
    write.csv(antot_bind, 'attributable_deaths_total.csv')
    write.csv(afregions_bind, 'attributable_fraction_regions.csv')
    write.csv(aftot_bind, 'attributable_fraction_total.csv')
    # write.csv(total, 'death_temp_total')

  }

  return(list(antot, totdeathtot, aftot, afregions))

}

#' Plot and write results of analysis
#'
#' @param df_list An alphabetically-ordered list of dataframes for each region.
#' @param argvar An updated list of arguments ($x, $fun, $knots, $degree, $bound) for onebasis function.
#' @param bvar A basis matrix for the predictor vector.
#' @param blup A list of BLUPs (best linear unbiased predictions).
#' @param regions_df A dataframe with two columns. Column 1 is abbreviated
#'   region names. Column 2 is user-specified region names.
#' @param mintempregions A named numeric vector. Minimum (optimum) mortality temperature per region.
#' @param output_folder_path Path to folder for storing outputs.
#'
#' @export
#'
#' @return A PDF containing a line plot of temperature versus relative risk per region,
#' and histogram of temperatures per region. A CSV of relative risk per temperature per region.
#' @examples output_folder_path = 'myfolder/output/'
plot_and_write_relative_risk <- function(df_list, argvar,
                         bvar, blup, regions_df, mintempregions,
                         output_folder_path){

  if (!is.null(output_folder_path)) {

    pdf(paste(output_folder_path, "output_all_regions_plot.pdf", sep = ''), width = 8, height = 9)

  } else {

    pdf("output_all_regions_plot.pdf", width = 8, height = 9)

  }

  layout(matrix(c(0,1,1,2,2,0, rep(3:8, each = 2),0,9,9,10,10,0),
                ncol = 6, byrow = T))
  par(mar = c(4,3.8,3,2.4), mgp = c(2.5,1,0),las=1)

  per <- t(sapply(df_list,function(x)
    quantile(x$tmean, c(2.5,10,25,50,75,90,97.5)/100, na.rm=T)))

  xlab <- expression(paste("Temperature (",degree,"C)"))

  varper <- c(10, 75, 90)

  region_vector <- c()
  temperature <- c()
  relative_risk <- c()

  for(i in seq(length(df_list))) {

    data <- df_list[[i]]

    # NB: Centering point different than original choice of 75th
    argvar <- list(x = data$tmean,
                   fun = config$varfun,
                   degree = config$vardegree,
                   knots=quantile(data$tmean, varper/100, na.rm=T))

    bvar <- do.call(onebasis, argvar)

    pred <- crosspred(bvar,
                      coef = blup[[i]]$blup,
                      vcov=blup[[i]]$vcov,
                      model.link="log",
                      by=0.1,
                      cen=mintempregions[i])

    plot(pred, type = "n",
         ylim = c(0,2.5), yaxt = "n",
         lab = c(6,5,7), xlab = xlab,
         ylab = "RR",
         main = regions_df$region_names[i])

    ind1 <- pred$predvar <= mintempregions[i]
    ind2 <- pred$predvar >= mintempregions[i]

    lines(pred$predvar[ind1], pred$allRRfit[ind1], col=4, lwd=1.5)
    lines(pred$predvar[ind2], pred$allRRfit[ind2], col=2, lwd=1.5)
    mtext(regions_df$region_names[i], cex=0.7, line=0)

    axis(2,at = 1:5*0.5)

    breaks <- c(min(data$tmean, na.rm = T) - 1,
                seq(pred$predvar[1],
                    pred$predvar[length(pred$predvar)], length = 30),
                max(data$tmean, na.rm = T) + 1)

    hist <- hist(data$tmean, breaks = breaks, plot = F)
    hist$density <- hist$density / max(hist$density) * 0.7
    prop <- max(hist$density) / max(hist$counts)
    counts <- pretty(hist$count, 3)

    plot(hist,
         ylim = c(0,max(hist$density)*3.5),
         axes = F, ann = F, col = grey(0.95),
         breaks = breaks, freq = F, add = T)

    axis(4, at = counts*prop, labels = counts, cex.axis = 0.7)
    mtext("N",4,line=-0.5,at=mean(counts*prop),cex=0.5)

    abline(v = mintempregions[i], lty = 3)
    abline(v = c(per[i,c("2.5%","97.5%")]), lty = 2)

    region_vector <- append(region_vector,
                            rep(regions_df$region_names[i],
                                length(pred$predvar)))
    temperature <- append(temperature, pred$predvar)
    relative_risk <- append(relative_risk, pred$allRRfit)
    }

  dev.off()

  output_df <- data.frame(regions = region_vector,
                          temperature = temperature,
                          relative_risk = relative_risk)

  write.csv(output_df,
            paste(output_folder_path,
                  'output_all_regions_data.csv', sep = ''),
            row.names=FALSE)

  # Output for testing
  output_df_test <- data.frame(regnames = rep(unique(data$regnames),
                                         length(pred$predvar)),
                          temperature = pred$predvar,
                          relative_risk = pred$allRRfit)
  # Output for testing
  write.csv(output_df_test,
            paste(output_folder_path,
                  'output_one_region_data_new.csv', sep = ''),
            row.names=FALSE)

}

#' Do full DLNM analysis
#'
#' @description Runs a sequence of functions to carry out heat-related mortality analysis.
#'
#' @details Modified from Gasparrini A et al. (2015) The Lancet. 2015;386(9991):369-375.
#'
#' @param input_csv_path Path to a CSV contain daily time series of death and temperature per region.
#' @param output_folder_path Path to folder for storing outputs.
#'
#' @return A PDF containing a line plot of temperature versus relative risk per region,
#' and histogram of temperatures per region. A CSV of relative risk per temperature per region.
#'
#' @seealso [dlnm] package
#'
#' @export
do_analysis <- function(input_csv_path, output_folder_path_){

  c(df_list_unordered_, regions_) %<-%
    load_data(input_path = input_csv_path)

  c(regions_df_, df_list_) %<-%
    get_region_metadata(regions = regions_,
                        df_list_unordered = df_list_unordered_,
                        region_names = c("North East","North West",
                                         "Yorkshire & Humber","East Midlands",
                                         "West Midlands","East","London",
                                         "South East","South West", "Wales"))

  c(argvar_, coef_, vcov_) %<-%
    run_model(df_list = df_list_,
              regions_df = regions_df_)

  c(mv_, blup_) %<-%
    run_meta_model(df_list = df_list_,
                   regions_df = regions_df_,
                   coef = coef_,
                   vcov = vcov_)

  c(avgtmean_wald, rangetmean_wald) %<-%
    wald_results(mv = mv_)

  c(argvar_, bvar_, mintempregions_) %<-%
    calculate_min_mortality_temp(df_list = df_list_,
                  regions_df = regions_df_,
                  blup = blup_)

  c(totdeath_, arraysim_, matsim_) %<-%
    compute_attributable_deaths(df_list = df_list_,
                                regions_df = regions_df_,
                                coef = coef_,
                                vcov = vcov_, argvar = argvar_,
                                bvar = bvar_, blup = blup_,
                                mintempregions = mintempregions_)

  c(antot, totdeathtot, aftot, afregions) %<-%
    write_attributable_deaths(df_list = df_list_,
                         regions_df = regions_df_,
                         matsim = matsim_,
                         arraysim = arraysim_,
                         totdeath = totdeath_,
                         output_folder_path = output_folder_path_)

  plot_and_write_relative_risk(df_list = df_list_,
               argvar = argvar_,
               bvar = bvar_,
               blup = blup_,
               regions_df = regions_df_,
               mintempregions = mintempregions_,
               output_folder_path = output_folder_path_)
}

