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

#' @return dlist_unordered - a list of dataframes for each region
#' @return regions - a list of strings of the names of each region
#'
#' @examples
load_data <- function(input_path) {

  if(substr(input_path, nchar(input_path) - 3, nchar(input_path)) !=  '.csv') {
    stop("Input path must be a CSV")
  }

  df_eng_wales <- read.csv(input_path, row.names=1)
  df_eng_wales$date <- as.Date(df_eng_wales$date)

  regions <- as.character(unique(df_eng_wales$regnames)) # .distinct() on regnames
  dlist_unordered <- lapply(regions, function(x) df_eng_wales[df_eng_wales$regnames == x, ])
  names(dlist_unordered) <- regions

  return (list(dlist_unordered, regions))

}

#' Get region metadata for analysis
#'
#' @param regions list of strings of region names
#' @param dlist_unordered list of dataframes for each region
#'
#' @return regions - an alphabetically-ordered dataframe of region names
#' @return dlist - an alphabetically-ordered list of dataframes for each region
#'
#' @examples
get_region_metadata <- function(regions, dlist_unordered) {

  # if(!is.list(dlist_unordered) | !is.data.frame(dlist_unordered)) {
  #   stop("Argument 'dlist_unordered' must be a list of data frames")
  # }

  # if(!is.list(regions) | !is.data.frame(dlist_unordered)) {
  #   stop("Argument 'regions' must be a list")
  # }

  cities <- data.frame(
    city = regions,
    # The following line can be deleted. It just allows the user to assign
    # the region/city names to a new string and could introduce errors
    # cityname = c("North East", "North West", "Yorkshire & Humber",
    #              "East Midlands", "West Midlands", "East", "London",
    #              "South East", "South West", "Wales")
    cityname = regions
  )

  # Order
  ord <- order(cities$cityname)
  dlist <- dlist_unordered[ord]
  cities <- cities[ord,]

  return (list(cities, dlist))

}

# This function causes the regression to fail in run_model. Not sure why.
# Have moved define_module into run_model and it works.
# define_model <- function(cities) {
#
#   varper <- c(10, 75, 90)
#
#   # Model formula
#   formula <- death ~ cb + dow + ns(date,df = config$dfseas * length(unique(year)))
#
#   # Coefficients and vcov for overall cumulative summary
#   coef <- matrix(NA,
#                  nrow(cities),
#                  length(varper) + config$vardegree,
#                  dimnames = list(cities$city))
#   vcov <- vector("list", nrow(cities))
#   names(vcov) <- cities$city
#
#   return(list(formula, coef, vcov))
#
# }

#' Define and run regression model for each dataframe
#'
#' @param regions list of strings of region names
#' @param dlist_unordered list of dataframes for each region
#'
#' @return red
#' @return coev
#' @return vcov
#' @examples
run_model <- function(dlist, cities) {

  # Loop
  time <- proc.time()[3]

  varper <- c(10, 75, 90)

  # Model formula
  formula <- death ~ cb + dow + ns(date, df = config$dfseas * length(unique(year)))

  # Coefficients and vcov for overall cumulative summary
  coef <- matrix(NA,
                 nrow(cities),
                 length(varper) + config$vardegree,
                 dimnames = list(cities$city))
  vcov <- vector("list" ,nrow(cities))
  names(vcov) <- cities$city

  varper <- c(10, 75, 90)

  for(i in seq(length(dlist))) {

    cat(i,"")

    # Extract data
    data <- dlist[[1]]

    # Define crossbasis
    argvar <- list(fun = config$varfun,
                   knots = quantile(data$tmean, (varper)/100, na.rm=T),
                   degree = config$vardegree)
    cb <- crossbasis(data$tmean,
                     lag = config$lag,
                     argvar = argvar,
                     arglag = list(knots = logknots(config$lag,config$lagnk)))

    # Run the model and obtain predictions
    model <- glm(formula, data, family = quasipoisson,na.action = "na.exclude")
    cen <- mean(data$tmean, na.rm = T)
    pred <- crosspred(cb, model, cen = cen)

    # Reduction to overall cumulative
    red <- crossreduce(cb ,model, cen=cen)
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
#' @param dlist a list of dataframes for each region
#' @param cities A dataframe with two columns. Column 1 is abbreviated region
#' names. Column 2 is full region names.
#' @param coef A matrix populated with estimated model coefficients from a model of
#' ???
#' @param vcov A list. Variance-covariance matrices required for dlnm model.
#'
#' @return an mvmeta model
#' @return BLUP estimates for an mvmeta model
#' @import mvmeta
run_meta_model <- function(dlist, cities, coef, vcov) {

  if(!is.list(dlist) | !is.data.frame(dlist[[1]])) {
    stop("Argument 'dlist' must be a list of data frames")
  }

  if(!is.data.frame(cities)) {
    stop("Argument 'cities' must be a data frame")
  }

  if(!is.matrix(coef) | !is.numeric(coef)) {
    stop("Argument 'coef' must be a numeric matrix")
  }

  if(!is.list(vcov) | !is.matrix(vcov[[1]])) {
    stop("Argument 'vcov' must be a list of matrices")
  }

  # Create average temperature and range as meta-predictors
  avgtmean <- sapply(dlist, function(x) mean(x$tmean, na.rm = TRUE))
  rangetmean <- sapply(dlist,function(x) diff(range(x$tmean, na.rm = TRUE)))

  # Meta-analysis
  # NB: country effects is not included in this example
  mv <- mvmeta(coef ~ avgtmean + rangetmean, vcov, data = cities,
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
#' @param dlist A list of dataframes for each region
#' @param cities A dataframe with two columns.
#' Column 1 is abbreviated region names.
#' Column 2 is full region names.
#' @param blup BLUP estimates for an mvmeta model.
#'
#' @return A list. \cr
#' argvar: A list of redefined arguments\cr
#' bvar: ??? \cr
#' mintempcity: ??? \cr
#' minperccountry: ??? \cr
min_mortality <-  function(dlist, cities, blup) {

  if(!is.list(dlist) | !is.data.frame(dlist[[1]])) {
    stop("Argument 'dlist' must be a list of data frames")
  }

  if(!is.data.frame(cities)) {
    stop("Argument 'cities' must be a data frame")
  }

  if(!is.list(blup)) {
    stop("Argument 'blup' must be a list")
  }

  # Re-centering
  # Generate the matrix for storing results
  minperccity <- mintempcity <- rep(NA, length(dlist))
  names(mintempcity) <- names(minperccity) <- cities$city

  varper <- c(10, 75, 90)

  # Define minimum mortality values: exclude low and very hot temperatures
  for(i in seq(length(dlist))) {
    data <- dlist[[i]]
    predvar <- quantile(data$tmean, 1:99/100, na.rm = T)
    # Redefine the function using all arguments (boundary knots included)
    argvar <- list(x = predvar, fun = config$varfun,
                   knots = quantile(data$tmean, varper/100, na.rm = TRUE),
                   degree = config$vardegree,
                   Bound = range(data$tmean, na.rm = T))
    bvar <- do.call(onebasis, argvar)
    minperccity[i] <- (1:99)[which.min((bvar %*% blup[[i]]$blup))]
    mintempcity[i] <- quantile(data$tmean, minperccity[i]/100, na.rm = TRUE)
  }

  # Country-specific points of minimum mortality
  (minperccountry <- median(minperccity))

  return(list(argvar = argvar, bvar = bvar, mintempcity = mintempcity,
              minperccountry = minperccountry))

}


#' Attrdl
#' Function for computing attributble measures from dlnm
#  requires dlnm v.2.2.0 >
#'
#' @param x An exposure vector or (only for dir="back") a matrix of lagged exposures
#' @param basis: The cross-basis computed from x
#' @param cases: The cases vector or (only for dir="forw") the matrix of future cases
#' @param model: The fitted model
#' @param coef, vcov: coef and vcov for basis if model is not provided
#' @param model.link: Link function if model is not provided
#' @param type: Either "an" or "af" for attributable number or fraction
#' @param dir: Either "back" or "forw" for backward or forward perspectives
#' @param tot: If true, the total attributable risk is computed
#' @param cen: The reference value used as counterfactual scenario
#' @param range: The range of exposure. if null, the whole range is used
#' @param sim: If simulation samples should be returned. only for tot=true
#' @param nsim: Number of simulation samples
#'
#' @return res
#' @examples
attrdl <- function(x,basis,cases,model=NULL,coef=NULL,vcov=NULL,model.link=NULL,
                   type="af",dir="back",tot=TRUE,cen,range=NULL,sim=FALSE,nsim=5000) {

  # Check version of the dlnm package
  if(packageVersion("dlnm")<"2.2.0")
    stop("update dlnm package to version >= 2.2.0")

  # Extract name and check type and dir
  name <- deparse(substitute(basis))
  type <- match.arg(type,c("an","af"))
  dir <- match.arg(dir,c("back","forw"))

  # Define centering
  if(missing(cen) && is.null(cen <- attr(basis,"argvar")$cen))
    stop("'cen' must be provided")
  if(!is.numeric(cen) && length(cen)>1L) stop("'cen' must be a numeric scalar")
  attributes(basis)$argvar$cen <- NULL

  # Select range (force to centering value otherwise, meaning null risk)
  if(!is.null(range)) x[x<range[1]|x>range[2]] <- cen

  # Compute the matrix of
  #   - Lagged exposures if dir="back"
  #   - Constant exposures along lags if dir="forw"
  lag <- attr(basis,"lag")
  if(NCOL(x)==1L) {
    at <- if(dir=="back") tsModel:::Lag(x,seq(lag[1],lag[2])) else
      matrix(rep(x,diff(lag)+1),length(x))
  } else {
    if(dir=="forw") stop("'x' must be a vector when dir='forw'")
    if(ncol(at <- x)!=diff(lag)+1)
      stop("dimension of 'x' not compatible with 'basis'")
  }

  # Number used for the contribution at each time in forward type
  #   - If cases provided as a matrix, take the row average
  #   - If provided as a time series, compute the forward moving average
  #   - This excludes missing accordingly
  # Also compute the denominator to be used below
  if(NROW(cases)!=NROW(at)) stop("'x' and 'cases' not consistent")
  if(NCOL(cases)>1L) {
    if(dir=="back") stop("'cases' must be a vector if dir='back'")
    if(ncol(cases)!=diff(lag)+1) stop("dimension of 'cases' not compatible")
    den <- sum(rowMeans(cases,na.rm=TRUE),na.rm=TRUE)
    cases <- rowMeans(cases)
  } else {
    den <- sum(cases,na.rm=TRUE)
    if(dir=="forw")
      cases <- rowMeans(as.matrix(tsModel:::Lag(cases,-seq(lag[1],lag[2]))))
  }

  # Extract coef and vcov if model is provided
  if(!is.null(model)) {
    cond <- paste0(name,"[[:print:]]*v[0-9]{1,2}\\.l[0-9]{1,2}")
    if(ncol(basis)==1L) cond <- name
    model.class <- class(model)
    coef <- dlnm:::getcoef(model,model.class)
    ind <- grep(cond,names(coef))
    coef <- coef[ind]
    vcov <- dlnm:::getvcov(model,model.class)[ind,ind,drop=FALSE]
    model.link <- dlnm:::getlink(model,model.class)
    if(!model.link %in% c("log","logit"))
      stop("'model' must have a log or logit link function")
  }

  # If reduced estimates are provided
  typebasis <- ifelse(length(coef)!=ncol(basis),"one","cb")

  # Prepare the arguments for th basis transformation
  predvar <- if(typebasis=="one") x else seq(NROW(at))
  predlag <- if(typebasis=="one") 0 else dlnm:::seqlag(lag)

  # Create the matrix of transformed centred variables (dependent on typebasis)
  if(typebasis=="cb") {
    Xpred <- dlnm:::mkXpred(typebasis,basis,at,predvar,predlag,cen)
    Xpredall <- 0
    for (i in seq(length(predlag))) {
      ind <- seq(length(predvar))+length(predvar)*(i-1)
      Xpredall <- Xpredall + Xpred[ind,,drop=FALSE]
    }
  } else {
    basis <- do.call(onebasis,c(list(x=x),attr(basis,"argvar")))
    Xpredall <- dlnm:::mkXpred(typebasis,basis,x,predvar,predlag,cen)
  }

  # Check dimensions
  if(length(coef)!=ncol(Xpredall))
    stop("arguments 'basis' do not match 'model' or 'coef'-'vcov'")
  if(any(dim(vcov)!=c(length(coef),length(coef))))
    stop("arguments 'coef' and 'vcov' do no match")
  if(typebasis=="one" && dir=="back")
    stop("only dir='forw' allowed for reduced estimates")

}

#' Compute attributable deaths
#' Compute the attributable deaths for each city,
#' with empirical CI estimated using the re-centred bases
#' @param dlist
#' @param cities
#' @param coef
#' @param vcov
#' @param varfun
#' @param argvar
#' @param bvar
#' @param blup
#' @param mintempcity
#'
#' @return A list of variables
#' \itemize{
#'   \item totdeath
#'   \item arraysim
#'   \item matsim
#' }
compute_attributable_deaths <- function(dlist, cities, coef, vcov,
                                        varfun, argvar, bvar, blup,
                                        mintempcity) {

  if (file.exists('R/attrdl.R')) {
    source('R/attrdl.R')
  } else {
    source('testdata/attrdl.R')
  }

  # Create the vectors to store the total mortality (accounting for missing)
  totdeath <- rep(NA,nrow(cities))
  names(totdeath) <- cities$city

  # Create the matrix to store the attributable deaths
  matsim <- matrix(NA,nrow(cities),3,dimnames=list(cities$city,
                                                   c("glob","cold","heat")))

  # Number of simulation runs for computing empirical CI
  nsim <- 1000

  # Create the array to store the CI of attributable deaths
  arraysim <- array(NA,dim=c(nrow(cities),3,nsim),dimnames=list(cities$city,
                                                                c("glob","cold","heat")))

  varper <- c(10, 75, 90)

  # Run the loop
  for(i in seq(dlist)){

    # Print
    cat(i,"")

    # Extract the data
    data <- dlist[[i]]

    # Derive the cross-basis
    # NB: Centering point different than original choice of 75th
    argvar <- list(x=data$tmean,fun=config$varfun,knots=quantile(data$tmean,
                                                          varper/100,na.rm=T),degree=config$vardegree)
    cb <- crossbasis(data$tmean,lag=lag,argvar=argvar,
                     arglag=list(knots=logknots(lag,lagnk)))

    # Compute the attributable deaths
    # NB: The reduced coefficients are used here
    matsim[i,"glob"] <- attrdl(data$tmean,cb,data$death,coef=blup[[i]]$blup,
                               vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempcity[i])
    matsim[i,"cold"] <- attrdl(data$tmean,cb,data$death,coef=blup[[i]]$blup,
                               vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempcity[i],
                               range=c(-100,mintempcity[i]))
    matsim[i,"heat"] <- attrdl(data$tmean,cb,data$death,coef=blup[[i]]$blup,
                               vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempcity[i],
                               range=c(mintempcity[i],100))

    # Compute empirical occurences of the attributable deaths
    # Uused to derive confidence intervals
    arraysim[i,"glob",] <- attrdl(data$tmean,cb,data$death,coef=blup[[i]]$blup,
                                  vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempcity[i],sim=T,nsim=nsim)
    arraysim[i,"cold",] <- attrdl(data$tmean,cb,data$death,coef=blup[[i]]$blup,
                                  vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempcity[i],
                                  range=c(-100,mintempcity[i]),sim=T,nsim=nsim)
    arraysim[i,"heat",] <- attrdl(data$tmean,cb,data$death,coef=blup[[i]]$blup,
                                  vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempcity[i],
                                  range=c(mintempcity[i],100),sim=T,nsim=nsim)

    # Store the denominator of attributable deaths, i.e. total observed mortality
    # Correct denominator to compute the attributable fraction later, as in attrdl
    totdeath[i] <- sum(data$death,na.rm=T)

  }

  return (list(totdeath, arraysim, matsim))

}

#' Write outputs to csv
#' Write the attributable deaths and temperature for each city,
#' with empirical CI estimated using the re-centred bases
#' @param cities
#' @param matsim
#' @param arraysim
#' @param totdeath
#' @param output_folder_path
#'
#' @return None
#' @examples output_folder_path = 'myfolder/output/'
write_outputs_to_csv <- function(cities, matsim, arraysim,
                                      totdeath, output_folder_path = NULL) {

  # Attributable numbers
  # City-specific
  ancity <- matsim
  ancitylow <- apply(arraysim,c(1,2),quantile,0.025)
  ancityhigh <- apply(arraysim,c(1,2),quantile,0.975)
  rownames(ancity) <- rownames(ancitylow) <- rownames(ancityhigh) <- cities$cityname

  # Total
  # NB: first sum through cities
  antot <- colSums(matsim)
  antotlow <- apply(apply(arraysim,c(2,3),sum),1,quantile,0.025)
  antothigh <- apply(apply(arraysim,c(2,3),sum),1,quantile,0.975)

  # Total mortality
  # By country
  totdeathtot <- sum(totdeath)

  # Attributable fractions
  # City-specific
  afcity <- ancity/totdeath*100
  afcitylow <- ancitylow/totdeath*100
  afcityhigh <- ancityhigh/totdeath*100

  # Total
  aftot <- antot/totdeathtot*100
  aftotlow <- antotlow/totdeathtot*100
  aftothigh <- antothigh/totdeathtot*100

  # Bind datasets
  ancity_bind <- t(cbind(ancity, ancitylow, ancityhigh))
  antot_bind <- t(cbind(antot, antotlow, antothigh))
  afcity_bind <- t(cbind(afcity, afcitylow, afcityhigh))
  aftot_bind <- t(cbind(aftot, aftotlow, aftothigh))

  # # Temperature
  # tmeanuk <- sapply(dlist,function(city) mean(city$tmean,na.rm=T)), c(Country="UK",
  #   Period=paste(range(dlist[[1]]$year),collapse="-"),Deaths=totdeathtot,
  #   Temperature=paste0(formatC(mean(tmeanuk),dig=1,
  #                              format="f")," (",paste(formatC(range(tmeanuk),dig=1,format="f"),
  #                                                     collapse="-"),")"))

  if (!is.null(output_folder_path)) {

    write.csv(ancity_bind, paste(output_folder_path, 'attributable_deaths_city.csv',  sep = ""))
    write.csv(antot_bind, paste(output_folder_path, 'attributable_deaths_total.csv',  sep = ""))
    write.csv(afcity_bind, paste(output_folder_path, 'attributable_fraction_city.csv', sep = ""))
    write.csv(aftot_bind, paste(output_folder_path, 'attributable_fraction_total.csv',  sep = ""))
    # write.csv(tmean_uk, output_folder_path, paste(output_folder_path, 'death_temp_total.csv'))


  } else {

    write.csv(ancity_bind, 'attributable_deaths_city.csv')
    write.csv(antot_bind, 'attributable_deaths_total.csv')
    write.csv(afcity_bind, 'attributable_fraction_city.csv')
    write.csv(aftot_bind, 'attributable_fraction_total.csv')
    # write.csv(tmean_uk, output_folder_path, paste(output_folder_path, 'death_temp_total'))

  }

  return(c(antot, totdeathtot, aftot, afcity))

}

#' Plot results of analysis
#'
#' @param dlist
#' @param argvar
#' @param bvar
#' @param blup
#' @param cities
#' @param mintempcity
#' @param output_folder_path
#'
#' @return a plot
#' @examples output_folder_path = 'myfolder/output/'
plot_results <- function(dlist, argvar,
                         bvar, blup, cities, mintempcity,
                         output_folder_path){

  # if (output_folder_path) {
  #
  #   pdf(paste(output_folder_path, "output_all_regions_plot.pdf"), width=8, height=9)
  #
  # } else {
  #
  #   pdf("output_all_regions_plot.pdf", width=8, height=9)
  #
  # }

  per <- t(sapply(dlist,function(x)
    quantile(x$tmean,c(2.5,10,25,50,75,90,97.5)/100,na.rm=T)))

  xlab <- expression(paste("Temperature (",degree,"C)"))

  layout(matrix(c(0,1,1,2,2,0,rep(3:8,each=2),0,9,9,10,10,0),ncol=6,byrow=T))
  par(mar=c(4,3.8,3,2.4),mgp=c(2.5,1,0),las=1)

  varper <- c(10, 75, 90)

  for(i in seq(length(dlist))) {

    data <- dlist[[i]]

    # NB: Centering point different than original choice of 75th
    argvar <- list(x=data$tmean,fun=config$varfun,degree=config$vardegree,
                   knots=quantile(data$tmean,varper/100,na.rm=T))
    bvar <- do.call(onebasis,argvar)
    pred <- crosspred(bvar,coef=blup[[i]]$blup,vcov=blup[[i]]$vcov,
                      model.link="log",by=0.1,cen=mintempcity[i])

    plot(pred,type="n",ylim=c(0,2.5),yaxt="n",lab=c(6,5,7),xlab=xlab,ylab="RR",
         main=cities$cityname[i])
    ind1 <- pred$predvar<=mintempcity[i]
    ind2 <- pred$predvar>=mintempcity[i]
    lines(pred$predvar[ind1],pred$allRRfit[ind1],col=4,lwd=1.5)
    lines(pred$predvar[ind2],pred$allRRfit[ind2],col=2,lwd=1.5)
    mtext(cities$countryname[i],cex=0.7,line=0)
    #axis(1,at=-8:8*5)
    axis(2,at=1:5*0.5)

    breaks <- c(min(data$tmean,na.rm=T)-1,seq(pred$predvar[1],
                                              pred$predvar[length(pred$predvar)],length=30),max(data$tmean,na.rm=T)+1)
    hist <- hist(data$tmean,breaks=breaks,plot=F)
    hist$density <- hist$density/max(hist$density)*0.7
    prop <- max(hist$density)/max(hist$counts)
    counts <- pretty(hist$count,3)

    plot(hist,ylim=c(0,max(hist$density)*3.5),axes=F,ann=F,col=grey(0.95),
         breaks=breaks,freq=F,add=T)
    axis(4,at=counts*prop,labels=counts,cex.axis=0.7)
    #mtext("N",4,line=-0.5,at=mean(counts*prop),cex=0.5)
    abline(v=mintempcity[i],lty=3)
    abline(v=c(per[i,c("2.5%","97.5%")]),lty=2)

    }

  # Output for testing
  output_df <- data.frame(regnames = rep(unique(data$regnames), length(pred$predvar)),
                          temperature = pred$predvar,
                          relative_risk = pred$allRRfit)
  # Output for testing
  write.csv(output_df, paste(output_folder_path, 'output_one_region_data_new.csv'), row.names=FALSE)

}

#' Do Gasparrini analysis
#'
#' @param input_csv_path
#' @param output_csv_path
#'
#' @return
#'
#' @examples
do_analysis <- function(input_csv_path, output_csv_path){

  c(dlist_unordered, regions) %<-% load_data(input_path = input_csv_path)

  c(cities, dlist) %<-% get_region_metadata(regions = regions,
                                            dlist_unordered = dlist_unordered)

  c(argvar, coef, vcov) %<-% run_model(dlist = dlist, cities = cities)

  c(mv, blup) %<-% run_meta_model(dlist = dlist, cities = cities, coef = coef,
                                  vcov = vcov)

  c(avgtmean_wald, rangetmean_wald) %<-% wald_results(mv = mv)

  c(argvar, bvar, mintempcity, minperccountry) %<-%
    min_mortality(dlist = dlist, cities = cities, blup = blup)

  # c(totdeath, arraysim, matsim) %<-%
  #   compute_attributable_deaths(dlist = dlist, cities = cities, coef = coef,
  #                               vcov = vcov, varfun = config$varfun, argvar = argvar,
  #                               bvar = bvar, blup = blup,
  #                               mintempcity = mintempcity)
  #
  # c(antot, totdeathtot, aftot, afcity) %<-%
  #   write_outputs_to_csv(cities = cities, matsim = matsim, arraysim = arraysim,
  #                        totdeath = totdeath,
  #                        output_folder_path = output_csv_path)

  plot_results(dlist = dlist,
               cities = cities,
               argvar = argvar,
               bvar = bvar,
               blup = blup,
               mintempcity = mintempcity,
               output_folder_path = output_csv_path)
}

do_analysis(input_csv_path = config$input_csv_path, output_csv_path = config$output_csv_path)

