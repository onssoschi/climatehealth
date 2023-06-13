#' library(dlnm)
#' library(mvmeta)
#' library(splines)
#' library(tsModel)
#' library(config)
#' library(zeallot)
#'
#' # Load config file
#' config <- config::get()
#'
#' # Input data
#' input_csv_path <- config$input_csv_path
#'
#' # Output data
#' output_csv_path <- config$output_csv_path
#'
#' # Specification of the exposure function
#' varfun <- config$varfun
#' vardegree <- config$vardegree
#' varper <- c(10,75,90)
#'
#' # Specification of the lag function
#' lag <- config$lag
#' lagnk <- config$lagnk
#'
#' # Degree of freedom for seasonality
#' dfseas <- config$dfseas
#'
#' #' First-stage analysis
#' #' Run the model in each city/region, reduce and save.
#' #' Create objects to store the results.
#' #'
#' #' @param input_csv_path A character.
#' #' The file path for a csv file of the input data.
#' #'
#' #' @return A list of the following: \cr
#' #' dlist: a list of dataframes for each region \cr
#' #' argvar: A list of arguments to pass to [dlnm::crossbasis()] function \cr
#' #' regions: A character vector.
#' #' Contains names of ten regions; the 9 English regions and Wales. \cr
#' #' cities: A dataframe with two columns.
#' #' Column 1 is abbreviated region names.
#' #' Column 2 is full region names \cr
#' #' coef: A matrix populated with estimated model coefficients from a model of
#' #' ??? \cr
#' #' vcov: A list. Variance-covariance matrices required for [dlnm] model. \cr
#' #'
#' #' @examples prep_and_first_step('data/regEngWales.csv')
#' prep_and_first_step <- function(input_csv_path) {
#'
#'   # Load dataset
#'   regEngWales <- read.csv(input_csv_path,row.names=1)
#'   regEngWales$date <- as.Date(regEngWales$date)
#'
#'   # Arrange the data as a list of data sets
#'   regions <- as.character(unique(regEngWales$regnames))
#'   dlist <- lapply(regions,function(x) regEngWales[regEngWales$regnames==x,])
#'   names(dlist) <- regions
#'
#'   # Metadata for locations
#'   cities <- data.frame(
#'     city = regions,
#'     cityname = c("North East","North West","Yorkshire & Humber","East Midlands",
#'                  "West Midlands","East","London","South East","South West", "Wales")
#'   )
#'
#'   # Order
#'   ord <- order(cities$cityname)
#'   dlist <- dlist[ord]
#'   cities <- cities[ord,]
#'
#'   # Model formula
#'   formula <- death~cb+dow+ns(date,df=dfseas*length(unique(year)))
#'
#'   # Coefficients and vcov for overall cumulative summary
#'   coef <- matrix(NA,nrow(cities),length(varper)+vardegree,
#'                  dimnames=list(cities$city))
#'   vcov <- vector("list",nrow(cities))
#'   names(vcov) <- cities$city
#'
#'   # Loop
#'   time <- proc.time()[3]
#'
#'   for(i in seq(length(dlist))) {
#'
#'     cat(i,"")
#'
#'     # Extract data
#'     data <- dlist[[i]]
#'
#'     # Define crossbasis
#'     argvar <- list(fun=varfun,knots=quantile(data$tmean,varper/100,na.rm=T),
#'                    degree=vardegree)
#'     cb <- crossbasis(data$tmean,lag=lag,argvar=argvar,
#'                      arglag=list(knots=logknots(lag,lagnk)))
#'
#'     #summary(cb)
#'
#'     # Run the model and obtain predictions
#'     model <- glm(formula,data,family=quasipoisson,na.action="na.exclude")
#'     cen <- mean(data$tmean,na.rm=T)
#'     pred <- crosspred(cb,model,cen=cen)
#'
#'     # Reduction to overall cumulative
#'     red <- crossreduce(cb,model,cen=cen)
#'     coef[i,] <- coef(red)
#'     vcov[[i]] <- vcov(red)
#'
#'     }
#'
#'   proc.time()[3]-time
#'
#'   return (list(dlist, argvar, regions, cities, coef, vcov))
#'
#' }
#'
#' #' Second-stage analysis
#' #' Multivariate meta-analysis of the reduced
#' #' coef and then computation of blup
#' #'
#' #' @param dlist a list of dataframes for each region
#' #' @param cities A dataframe with two columns.
#' #' Column 1 is abbreviated region names.
#' #' Column 2 is full region names.
#' #' @param argvar A list of arguments to pass to [dlnm::crossbasis()] function
#' #' @param coef A matrix populated with estimated model coefficients from a
#' #' model of ???
#' #' @param vcov A list. Variance-covariance matrices required for [dlnm] model.
#' #'
#' #' @return A list of the following: \cr
#' #' blup: A list of blup (best linear unbiased predictions) for each region.
#' #' argvar:
#' #' bvar: A basis matrix...
#' #' mintempcity: A named vector...
#' #'
#' #' @examples second_stage(dlist = dlist, cities = cities, argvar = argvar,
#' #' coef = coef, vcov = vcov)
#' second_stage <- function(dlist, cities, argvar, coef, vcov) {
#'
#' # Create average temperature and range as meta-predictors
#' avgtmean <- sapply(dlist,function(x) mean(x$tmean,na.rm=T))
#' rangetmean <- sapply(dlist,function(x) diff(range(x$tmean,na.rm=T)))
#'
#' # Meta-analysis
#' # NB: country effects is not included in this example
#' mv <- mvmeta(coef~avgtmean+rangetmean,vcov,data=cities,control=list(showiter=T))
#' summary(mv)
#'
#' # Function for computing the p-value of Wald test
#' fwald <- function(model,var) {
#'   ind <- grep(var,names(coef(model)))
#'   coef <- coef(model)[ind]
#'   vcov <- vcov(model)[ind,ind]
#'   waldstat <- coef%*%solve(vcov)%*%coef
#'   df <- length(coef)
#'   return(1-pchisq(waldstat,df))
#'   }
#'
#' # Test the effects
#' fwald(mv,"avgtmean")
#' fwald(mv,"rangetmean")
#'
#' # Obtain blups
#' blup <- blup(mv,vcov=T)
#'
#' # Re-centering
#' # Generate the matrix for storing results
#' minperccity <- mintempcity <- rep(NA,length(dlist))
#' names(mintempcity) <- names(minperccity) <- cities$city
#'
#' # Define minimum mortality values: exclude low and very hot temperatures
#' for(i in seq(length(dlist))) {
#'   data <- dlist[[i]]
#'   predvar <- quantile(data$tmean,1:99/100,na.rm=T)
#'   # Redefine the function using all arguments (boundary knots included)
#'   argvar <- list(x=predvar,fun=varfun,
#'                  knots=quantile(data$tmean,varper/100,na.rm=T),degree=vardegree,
#'                  Bound=range(data$tmean,na.rm=T))
#'   bvar <- do.call(onebasis,argvar)
#'   minperccity[i] <- (1:99)[which.min((bvar%*%blup[[i]]$blup))]
#'   mintempcity[i] <- quantile(data$tmean,minperccity[i]/100,na.rm=T)
#'   }
#'
#' # Country-specific points of minimum mortality
#' (minperccountry <- median(minperccity))
#'
#' return (list(blup = blup, argvar = argvar,
#'         bvar = bvar, mintempcity = mintempcity))
#'
#' }


#' Compute attributable deaths
#' Compute the attributable deaths for each city,
#' with empirical CI estimated using the re-centred bases
#' @param dlist
#' @param regions
#' @param cities
#' @param coef
#' @param vcov
#' @param varfun
#' @param argvar
#' @param bvar
#' @param blup
#' @param mintempcity
#'
#' @return
#' @examplesthird_stage (dlist = dlist, cities = cities,regions = regions,
#' argvar = argvar, coef = coef, vcov = vcov, bvar = bvar, blup = blup,
#' varfun = varfun, mintempcity = mintempcity)
compute_attributable_deaths <- function(dlist, cities, coef, vcov,
                                        varfun, argvar, bvar, blup,
                                        mintempcity) {

  # Create the vectors to store the total mortality (accounting for missing)
  totdeath <- rep(NA,nrow(cities))
  names(totdeath) <- cities$city

  # Create the matrix to store the attributanle deaths
  matsim <- matrix(NA,nrow(cities),3,dimnames=list(cities$city,
                                                   c("glob","cold","heat")))

  # Number of simulation runs for computing empirical CI
  nsim <- 1000

  # Create the array to store the CI of attributable deaths
  arraysim <- array(NA,dim=c(nrow(cities),3,nsim),dimnames=list(cities$city,
                                                                c("glob","cold","heat")))

  # Run the loop
  for(i in seq(dlist)){

    # Print
    cat(i,"")

    # Extract the data
    data <- dlist[[i]]

    # Derive the cross-basis
    # NB: Centering point different than original choice of 75th
    argvar <- list(x=data$tmean,fun=varfun,knots=quantile(data$tmean,
                                                          varper/100,na.rm=T),degree=vardegree)
    cb <- crossbasis(data$tmean,lag=lag,argvar=argvar,
                     arglag=list(knots=logknots(lag,lagnk)))

    # Compute the attributable deaths
    # NB: The reduced coefficients are used here
    matsim[i,"glob"] <- dlnm::attrdl(data$tmean,cb,data$death,coef=blup[[i]]$blup,
                               vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempcity[i])
    matsim[i,"cold"] <- dlnm::attrdl(data$tmean,cb,data$death,coef=blup[[i]]$blup,
                               vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempcity[i],
                               range=c(-100,mintempcity[i]))
    matsim[i,"heat"] <- dlnm::attrdl(data$tmean,cb,data$death,coef=blup[[i]]$blup,
                               vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempcity[i],
                               range=c(mintempcity[i],100))

    # Compute empirical occurences of the attributable deaths
    # Uused to derive confidence intervals
    arraysim[i,"glob",] <- dlnm::attrdl(data$tmean,cb,data$death,coef=blup[[i]]$blup,
                                  vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempcity[i],sim=T,nsim=nsim)
    arraysim[i,"cold",] <- dlnm::attrdl(data$tmean,cb,data$death,coef=blup[[i]]$blup,
                                  vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempcity[i],
                                  range=c(-100,mintempcity[i]),sim=T,nsim=nsim)
    arraysim[i,"heat",] <- dlnm::attrdl(data$tmean,cb,data$death,coef=blup[[i]]$blup,
                                  vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempcity[i],
                                  range=c(mintempcity[i],100),sim=T,nsim=nsim)

    # Store the denominator of attributable deaths, i.e. total observed mortality
    # Correct denominator to compute the attributable fraction later, as in attrdl
    totdeath[i] <- sum(data$death,na.rm=T)

  }

  return(totdeath, arraysim, matsim)

}

#' Write attributable deaths
#' Write the attributable deaths for each city,
#' with empirical CI estimated using the re-centred bases

#' @param cities
#' @param matsim
#' @param arraysim
#' @param totdeath
#' @param attr_output_folder_path
#'
#' @return
#' @examples cities, matsim, arraysim, totdeath, attr_output_folder_path = 'myfolder/output/')
#'
write_attributable_deaths <- function(cities, matsim, arraysim,
                                      totdeath, attr_output_folder_path = NULL) {

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

  if (attr_output_folder_path) {

    write.csv(ancity, paste(attr_output_folder_path, 'attributable_deaths_city'))
    write.csv(antot, paste(attr_output_folder_path, 'attributable_deaths_total'))
    write.csv(afcity, paste(attr_output_folder_path, 'attributable_fraction_city'))
    write.csv(antot, paste(attr_output_folder_path, 'attributable_fraction_total'))

  } else {

    write.csv(ancity, paste('output/', 'attributable_deaths_city'))
    write.csv(antot, paste('output/', 'attributable_deaths_total'))
    write.csv(afcity, paste('output/', 'attributable_fraction_city'))
    write.csv(antot, paste('output/', 'attributable_fraction_total'))
  }

  return(c(antot, totdeathdot, aftot, afcity))

}

#'
#' #' Plot results
#' #' Compute the attributable deaths for each city,
#' #' with emprical CI estimated using the re-centred bases
#' #'
#' #' @param dlist
#' #' @param argvar
#' #' @param bvar
#' #' @param blup
#' #' @param cities
#' #' @param mintempcity
#' #' @param output_csv_path
#' #'
#' #' @return A number.
#' #' @examples
#' plot_results <- function(dlist, argvar,
#'                          bvar, blup, cities, mintempcity,
#'                          output_csv_path){
#'
#'   per <- t(sapply(dlist,function(x)
#'     quantile(x$tmean,c(2.5,10,25,50,75,90,97.5)/100,na.rm=T)))
#'
#'   xlab <- expression(paste("Temperature (",degree,"C)"))
#'
#'   # pdf("gasparrini/output/output_all_regions_plot.pdf",width=8,height=9)
#'   layout(matrix(c(0,1,1,2,2,0,rep(3:8,each=2),0,9,9,10,10,0),ncol=6,byrow=T))
#'   par(mar=c(4,3.8,3,2.4),mgp=c(2.5,1,0),las=1)
#'
#'   for(i in seq(length(dlist))) {
#'
#'     data <- dlist[[i]]
#'
#'     # NB: Centering point different than original choice of 75th
#'     argvar <- list(x=data$tmean,fun=varfun,degree=vardegree,
#'                    knots=quantile(data$tmean,varper/100,na.rm=T))
#'     bvar <- do.call(onebasis,argvar)
#'     pred <- crosspred(bvar,coef=blup[[i]]$blup,vcov=blup[[i]]$vcov,
#'                       model.link="log",by=0.1,cen=mintempcity[i])
#'
#'     plot(pred,type="n",ylim=c(0,2.5),yaxt="n",lab=c(6,5,7),xlab=xlab,ylab="RR",
#'          main=cities$cityname[i])
#'     ind1 <- pred$predvar<=mintempcity[i]
#'     ind2 <- pred$predvar>=mintempcity[i]
#'     lines(pred$predvar[ind1],pred$allRRfit[ind1],col=4,lwd=1.5)
#'     lines(pred$predvar[ind2],pred$allRRfit[ind2],col=2,lwd=1.5)
#'     mtext(cities$countryname[i],cex=0.7,line=0)
#'     #axis(1,at=-8:8*5)
#'     axis(2,at=1:5*0.5)
#'
#'     breaks <- c(min(data$tmean,na.rm=T)-1,seq(pred$predvar[1],
#'                                               pred$predvar[length(pred$predvar)],length=30),max(data$tmean,na.rm=T)+1)
#'     hist <- hist(data$tmean,breaks=breaks,plot=F)
#'     hist$density <- hist$density/max(hist$density)*0.7
#'     prop <- max(hist$density)/max(hist$counts)
#'     counts <- pretty(hist$count,3)
#'
#'     plot(hist,ylim=c(0,max(hist$density)*3.5),axes=F,ann=F,col=grey(0.95),
#'          breaks=breaks,freq=F,add=T)
#'     axis(4,at=counts*prop,labels=counts,cex.axis=0.7)
#'     #mtext("N",4,line=-0.5,at=mean(counts*prop),cex=0.5)
#'     abline(v=mintempcity[i],lty=3)
#'     abline(v=c(per[i,c("2.5%","97.5%")]),lty=2)
#'
#'     }
#'
#'   output_df <- data.frame(regnames = rep(unique(data$regnames), length(pred$predvar)),
#'                           temperature = pred$predvar,
#'                           relative_risk = pred$allRRfit)
#'
#'   write.csv(output_df, output_csv_path, row.names=FALSE)
#'
#' }
#'
#'
#' #' Write tables
#' #'
#' #' @return
#' #' @examples
#' tables <- function(){
#'
#'   # Related part of table 1
#'   tmeanuk <- sapply(dlist,function(city) mean(city$tmean,na.rm=T))
#'   c(Country="UK",
#'     Period=paste(range(dlist[[1]]$year),collapse="-"),Deaths=totdeathtot,
#'     Temperature=paste0(formatC(mean(tmeanuk),dig=1,
#'                                format="f")," (",paste(formatC(range(tmeanuk),dig=1,format="f"),
#'                                                       collapse="-"),")"))
#'
#'   # Related part of table 2
#'   # MMP
#'   minperccountry
#'
#'   # Attributable fraction
#'   t(cbind(aftot,aftotlow,aftothigh))
#'
#'   # Related part of table S4
#'   # Deaths
#'   totdeath
#'
#'   # Minimum mortality temperature percentile and absolute temperature
#'   minperccity
#'   mintempcity
#'
#'   # attributable fraction
#'   afcity
#'
#'   return (c(tmeanuk = tmeanuk, minperccountry = minperccountry,
#'           totdeath = totdeath, minperccity = minperccity,
#'           mintempcity = mintempcity, afcity = afcity))
#'
#' }
#'
#' #' Do Gasparrini analysis
#' #'
#' #' @param input_csv_path
#' #' @param output_csv_path
#' #'
#' #' @return
#' #'
#' #' @examples
#' do_analysis <- function(input_csv_path, output_csv_path){
#'
#'   c(dlist, argvar, regions, cities, coef, vcov) %<-% prep_and_first_step(input_csv_path)
#'
#'   c(blup, argvar, bvar, mintempcity) %<-% second_stage(dlist = dlist,
#'                                                        cities = cities,
#'                                                        coef = coef,
#'                                                        vcov = vcov)
#'
#'   third_stage(dlist = dlist,
#'               cities = cities,
#'               regions = regions,
#'               argvar = argvar,
#'               coef = coef,
#'               vcov = vcov,
#'               bvar = bvar,
#'               blup = blup,
#'               varfun = varfun,
#'               mintempcity = mintempcity)
#'
#'   plot_results(dlist = dlist,
#'                cities = cities,
#'                argvar = argvar,
#'                bvar = bvar,
#'                blup = blup,
#'                mintempcity = mintempcity,
#'                output_csv_path = output_csv_path)
#' }
#'
#' do_analysis(input_csv_path = input_csv_path, output_csv_path = output_csv_path)
#'
