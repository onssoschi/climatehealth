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
#' @param input_path a path to CSV

#' @return
#' \itemize{
#'   \item df_list_unordered - a list of dataframes for each region
#'   \item regions - a list of strings of the names of each region
#' @examples
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
#' @param regions list of strings of region names
#' @param df_list_unordered list of dataframes for each region
#'
#' @return
#' \itemize{
#'   \item regions - A dataframe with two columns. Column 1 is abbreviated
#'   region names. Column 2 is full region names.
#'   \item df_list - list of dataframes for each region
#' @examples
get_region_metadata <- function(regions, df_list_unordered, region_names = NULL) {

  # if(!is.list(df_list_unordered) | !is.data.frame(df_list_unordered[[1]])) {
  #   stop("Argument 'df_list_unordered' must be a list of data frames")
  #   }
  #
  # if(!is.list(regions) | !is.data.frame(regions[[1]])) {
  #   stop("Argument 'regions' must be a list")
  #   }

  if (!is.null(region_names)) {
    region_names = region_names
  } else {
    region_names = regions
  }

  regions_df <- data.frame(
    regions = regions,
    region_names = region_names
  )

  # Order
  ord <- order(regions_df$region_names)
  df_list <- df_list_unordered[ord]
  regions_df <- regions_df[ord,]

  return (list(regions_df, df_list))

}

#' Define and run regression model for each dataframe
#'
#' @param regions_df A dataframe with two columns. Column 1 is abbreviated
#' region names. Column 2 is full region names.
#' @param df_list_unordered list of dataframes for each region
#' @return
#' \itemize{
#'   \item argvar arguments ($fun, $knots, $degree) for cross-basis function
#'   \item coef matrix of coefficients for reduced model
#'   \item vcov co-variance matrix for reduced model
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
    data <- df_list[[1]]

    # Define crossbasis
    argvar <- list(fun = config$varfun,
                   knots = quantile(data$tmean, (varper)/100, na.rm=T),
                   degree = config$vardegree)

    cb <- crossbasis(data$tmean,
                     lag = config$lag,
                     argvar = argvar,
                     arglag = list(knots = logknots(config$lag,config$lagnk)))

    # Run the model and obtain predictions
    model <- glm(formula,
                 data,
                 family = quasipoisson,na.action = "na.exclude")
    cen <- mean(data$tmean, na.rm = T)
    pred <- crosspred(cb, model, cen = cen)

    # Reduction to overall cumulative
    red <- crossreduce(cb, model, cen=cen)

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
#' @param df_list A list of dataframes for each region
#' @param regions_df A dataframe with two columns. Column 1 is abbreviated
#' region names. Column 2 is full region names.
#' @param coef Matrix of coefficients for reduced model
#' @param vcov A list. Co-variance matrix for reduced model.
#'
#' @return
#' \itemize{
#'   \item mvmeta model (multivariante meta-analysis)
#'   \item blup BLUP (best linear unbiased predictions) for an mvmeta model
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
#' @param model A model object
#' @param var A character. The name of the variable in the model to calculate
#' p-values for.
#'
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
#' @param mv A model object
#'
#' @return P-values for avgtmean and rangetmean
wald_results <- function(mv) {

  avgtmean_wald <- fwald(mv, "avgtmean")
  rangetmean_wald <- fwald(mv, "rangetmean")

  return(list(avgtmean_wald, rangetmean_wald))

}

#' Calculate minimum mortality values
#'
#' ???
#'
#' @param df_list A list of dataframes for each region
#' @param regions_df A dataframe with two columns.
#' Column 1 is abbreviated region names.
#' Column 2 is full region names.
#' @param blup BLUP (best linear unbiased predictions) for an mvmeta model.
#'
#' @return
#' \itemize{
#'   \item argvar: arguments ($fun, $knots, $degree) for cross-basis function
#'   \item bvar:
#'   \item mintempregions: optimum temperature per region
#' }
min_mortality <-  function(df_list, regions_df, blup) {

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
#' Compute the attributable deaths for each regions,
#' with empirical CI estimated using the re-centred bases
#' @param df_list
#' @param regions_df
#' @param coef
#' @param vcov
#' @param argvar
#' @param bvar
#' @param blup
#' @param mintempregions
#'
#' @return A list of variables
#' \itemize{
#'   \item totdeath
#'   \item arraysim
#'   \item matsim
#' }
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
    cat(i,"")

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
    matsim[i,"glob"] <- attrdl(data$tmean, cb, data$death,
                               coef=blup[[i]]$blup,
                               vcov = blup[[i]]$vcov,
                               type="an",
                               dir = "forw",
                               cen = mintempregions[i])

    matsim[i,"cold"] <- attrdl(data$tmean, cb, data$death,
                               coef = blup[[i]]$blup,
                               vcov = blup[[i]]$vcov,
                               type = "an",
                               dir = "forw",
                               cen = mintempregions[i],
                               range = c(-100,mintempregions[i]))

    matsim[i,"heat"] <- attrdl(data$tmean, cb, data$death,
                               coef = blup[[i]]$blup,
                               vcov = blup[[i]]$vcov,
                               type="an",
                               dir = "forw",
                               cen = mintempregions[i],
                               range = c(mintempregions[i],100))

    # Compute empirical occurences of the attributable deaths
    # Uused to derive confidence intervals
    arraysim[i,"glob",] <- attrdl(data$tmean, cb, data$death,
                                  coef = blup[[i]]$blup,
                                  vcov = blup[[i]]$vcov,
                                  type = "an",
                                  dir = "forw",
                                  cen = mintempregions[i],
                                  sim = T, nsim = nsim)

    arraysim[i,"cold",] <- attrdl(data$tmean, cb, data$death,
                                  coef=blup[[i]]$blup,
                                  vcov = blup[[i]]$vcov,
                                  type = "an",
                                  dir = "forw",
                                  cen = mintempregions[i],
                                  range = c(-100,mintempregions[i]),
                                  sim = T ,nsim = nsim)

    arraysim[i,"heat",] <- attrdl(data$tmean, cb, data$death,
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
#' Write the attributable deaths and temperature for each regions,
#' with empirical CI estimated using the re-centred bases
#' @param regions_df
#' @param matsim
#' @param arraysim
#' @param totdeath
#' @param output_folder_path
#'
#' @return None
#' @examples output_folder_path = 'myfolder/output/'
write_outputs_to_csv <- function(regions_df, matsim, arraysim,
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
  # tmeanuk <- sapply(df_list,function(regions) mean(regions$tmean,na.rm=T)), c(Country="UK",
  #   Period=paste(range(df_list[[1]]$year),collapse="-"),Deaths=totdeathtot,
  #   Temperature=paste0(formatC(mean(tmeanuk),dig=1,
  #                              format="f")," (",paste(formatC(range(tmeanuk),dig=1,format="f"),
  #                                                     collapse="-"),")"))

  if (!is.null(output_folder_path)) {

    write.csv(anregions_bind, file = paste(output_folder_path, 'attributable_deaths_regions.csv',  sep = ""))
    write.csv(antot_bind, file = paste(output_folder_path, 'attributable_deaths_total.csv',  sep = ""))
    write.csv(afregions_bind, file = paste(output_folder_path, 'attributable_fraction_regions.csv', sep = ""))
    write.csv(aftot_bind, file = paste(output_folder_path, 'attributable_fraction_total.csv',  sep = ""))
    # write.csv(tmean_uk, output_folder_path, paste(output_folder_path, 'death_temp_total.csv'))


  } else {

    write.csv(anregions_bind, 'attributable_deaths_regions.csv')
    write.csv(antot_bind, 'attributable_deaths_total.csv')
    write.csv(afregions_bind, 'attributable_fraction_regions.csv')
    write.csv(aftot_bind, 'attributable_fraction_total.csv')
    # write.csv(tmean_uk, output_folder_path, paste(output_folder_path, 'death_temp_total'))

  }

  return(list(antot, totdeathtot, aftot, afregions))

}

#' Plot results of analysis
#'
#' @param df_list
#' @param argvar
#' @param bvar
#' @param blup
#' @param regions_df
#' @param mintempregions
#' @param output_folder_path
#'
#' @return a plot
#' @examples output_folder_path = 'myfolder/output/'
plot_results <- function(df_list, argvar,
                         bvar, blup, regions_df, mintempregions,
                         output_folder_path){

  if (!is.null(output_folder_path)) {

    pdf(paste(output_folder_path, "output_all_regions_plot.pdf"), width=8, height=9)

  } else {

    pdf("output_all_regions_plot.pdf", width=8, height=9)

  }

  per <- t(sapply(df_list,function(x)
    quantile(x$tmean, c(2.5,10,25,50,75,90,97.5)/100, na.rm=T)))

  xlab <- expression(paste("Temperature (",degree,"C)"))

  layout(matrix(c(0,1,1,2,2,0, rep(3:8, each = 2),0,9,9,10,10,0),
                ncol = 6, byrow = T))

  par(mar = c(4,3.8,3,2.4), mgp = c(2.5,1,0),las=1)

  varper <- c(10, 75, 90)

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
    mtext(regions_df$countryname[i], cex=0.7, line=0)

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
    #mtext("N",4,line=-0.5,at=mean(counts*prop),cex=0.5)

    abline(v = mintempregions[i], lty = 3)
    abline(v = c(per[i,c("2.5%","97.5%")]), lty = 2)
    }

  # Output for testing
  output_df <- data.frame(regnames = rep(unique(data$regnames),
                                         length(pred$predvar)),
                          temperature = pred$predvar,
                          relative_risk = pred$allRRfit)
  # Output for testing
  write.csv(output_df,
            paste(output_folder_path, 'output_one_region_data_new.csv'),
            row.names=FALSE)

}

#' Do full Gasparrini analysis
#'
#' @param input_csv_path
#' @param output_csv_path
#'
#' @return
#'
#' @examples
do_analysis <- function(input_csv_path, output_csv_path){

  c(df_list_unordered, regions) %<-%
    load_data(input_path = input_csv_path)

  c(regions_df, df_list) %<-%
    get_region_metadata(regions = regions,
                        df_list_unordered = df_list_unordered)

  c(argvar, coef, vcov) %<-%
    run_model(df_list = df_list,
              regions_df = regions_df)

  c(mv, blup) %<-%
    run_meta_model(df_list = df_list,
                   regions_df = regions_df,
                   coef = coef,
                   vcov = vcov)

  c(avgtmean_wald, rangetmean_wald) %<-%
    wald_results(mv = mv)

  c(argvar, bvar, mintempregions) %<-%
    min_mortality(df_list = df_list,
                  regions_df = regions_df,
                  blup = blup)

  c(totdeath, arraysim, matsim) %<-%
    compute_attributable_deaths(df_list = df_list,
                                regions_df = regions_df,
                                coef = coef,
                                vcov = vcov, argvar = argvar,
                                bvar = bvar, blup = blup,
                                mintempregions = mintempregions)

  c(antot, totdeathtot, aftot, afregions) %<-%
    write_outputs_to_csv(regions_df = regions_df,
                         matsim = matsim,
                         arraysim = arraysim,
                         totdeath = totdeath,
                         output_folder_path = output_csv_path)

  plot_results(df_list = df_list,
               argvar = argvar,
               bvar = bvar,
               blup = blup,
               regions_df = regions_df,
               mintempregions = mintempregions,
               output_folder_path = output_csv_path)
}

