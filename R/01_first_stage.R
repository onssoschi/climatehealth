#' First-stage analysis
#' Run the model in each city/region, reduce and save.
#' Create objects to store the results.
#'
#' @param x A number.
#' @param y A number.
#' @return A number.
#' @examples
#' add(1, 1)
#' add(10, 1)
first_step <- function() {

  # Model formula
  formula <- death~cb+dow+ns(date,df=dfseas*length(unique(year)))

  # Coefficients and vcov for overall cumulative summary
  coef <- matrix(NA,nrow(cities),length(varper)+vardegree,
                 dimnames=list(cities$city))
  vcov <- vector("list",nrow(cities))
  names(vcov) <- cities$city

  # Loop
  time <- proc.time()[3]

  if (dlist > 1) {

    for(i in seq(length(dlist))) {

      cat(i,"")

      # Extract daa
      data <- dlist[[i]]

      # Define crossbasis
      argvar <- list(fun=varfun,knots=quantile(data$tmean,varper/100,na.rm=T),
                     degree=vardegree)
      cb <- crossbasis(data$tmean,lag=lag,argvar=argvar,
                       arglag=list(knots=logknots(lag,lagnk)))

      #summary(cb)

      # Run the model and obtain predictions
      model <- glm(formula,data,family=quasipoisson,na.action="na.exclude")
      cen <- mean(data$tmean,na.rm=T)
      pred <- crosspred(cb,model,cen=cen)

      # Reduction to overall cumulative
      red <- crossreduce(cb,model,cen=cen)
      coef[i,] <- coef(red)
      vcov[[i]] <- vcov(red)

    }

  } else {

    # Print
    cat(1,"")

    # Extract data
    data <- dlist[[1]]

    # Define crossbasis
    argvar <- list(fun=varfun,knots=quantile(data$tmean,varper/100,na.rm=T),
                   degree=vardegree)
    cb <- crossbasis(data$tmean,lag=lag,argvar=argvar,
                     arglag=list(knots=logknots(lag,lagnk)))

    #summary(cb)

    # Run the model and obtain predictions
    model <- glm(formula,data,family=quasipoisson,na.action="na.exclude")
    cen <- mean(data$tmean,na.rm=T)
    pred <- crosspred(cb,model,cen=cen)

    # Reduction to overall cumulative
    red <- crossreduce(cb,model,cen=cen)
    coef[1,] <- coef(red)
    vcov[[1]] <- vcov(red)

    }

  proc.time()[3]-time

}


