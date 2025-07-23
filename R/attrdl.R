an_attrdl <- function(x,basis,cases,coef=NULL,vcov=NULL,model.link=NULL,
                      dir="back",tot=TRUE,cen,range=NULL,nsim=5000) {
  ################################################################################

  # CHECK type AND dir
  dir <- match.arg(dir,c("back","forw"))
  #
  # Remove centering value from basis
  attributes(basis)$argvar$cen <- NULL
  #
  # SELECT RANGE (FORCE TO CENTERING VALUE OTHERWISE, MEANING NULL RISK)
  if(!is.null(range)) x[x<range[1]|x>range[2]] <- cen
  #
  # COMPUTE THE MATRIX OF
  #   - LAGGED EXPOSURES IF dir="back"
  #   - CONSTANT EXPOSURES ALONG LAGS IF dir="forw"
  lag <- attr(basis,"lag")
  at <- if(dir=="back") tsModel:::Lag(x,seq(lag[1],lag[2])) else
    matrix(rep(x,diff(lag)+1),length(x))
  #
  # NUMBER USED FOR THE CONTRIBUTION AT EACH TIME IN FORWARD TYPE
  #   - IF PROVIDED AS A TIME SERIES, COMPUTE THE FORWARD MOVING AVERAGE
  #   - THIS EXCLUDES MISSING ACCORDINGLY
  # ALSO COMPUTE THE DENOMINATOR TO BE USED BELOW
  if(NROW(cases)!=NROW(at)) stop("'x' and 'cases' not consistent")
  den <- sum(cases,na.rm=TRUE)
  if(dir=="forw"){
    cases <- rowMeans(as.matrix(tsModel:::Lag(cases,-seq(lag[1],lag[2]))))
  }
  #

  # IF REDUCED ESTIMATES ARE PROVIDED
  typebasis <- ifelse(length(coef)!=ncol(basis),"one","cb")
  #
  ################################################################################
  #
  # PREPARE THE ARGUMENTS FOR TH BASIS TRANSFORMATION
  predvar <- if(typebasis=="one") x else seq(NROW(at))
  predlag <- if(typebasis=="one") 0 else dlnm:::seqlag(lag)
  #
  # CREATE THE MATRIX OF TRANSFORMED CENTRED VARIABLES (DEPENDENT ON typebasis)
  if(typebasis=="cb") {
    Xpred <- dlnm:::mkXpred(typebasis,basis,at,predvar,predlag,cen)
    Xpredall <- 0
    for (i in seq(length(predlag))) {
      ind <- seq(length(predvar))+length(predvar)*(i-1)
      Xpredall <- Xpredall + Xpred[ind,,drop=FALSE]
    }
  } else {
    basis <- do.call(dlnm::onebasis,c(list(x=x),attr(basis,"argvar")))
    Xpredall <- dlnm:::mkXpred(typebasis,basis,x,predvar,predlag,cen)
  }
  #
  # CHECK DIMENSIONS
  if(length(coef)!=ncol(Xpredall))
    stop("arguments 'basis' do not match 'model' or 'coef'-'vcov'")
  if(any(dim(vcov)!=c(length(coef),length(coef))))
    stop("arguments 'coef' and 'vcov' do no match")
  if(typebasis=="one" && dir=="back")
    stop("only dir='forw' allowed for reduced estimates")
  #
  ################################################################################
  #
  # COMPUTE AN
  af <- 1-exp(-drop(as.matrix(Xpredall%*%coef)))
  an <- (1-exp(-drop(as.matrix(Xpredall%*%coef)))) * cases

  ################################################################################
  #

  # SAMPLE COEF
  k <- length(coef)
  eigen <- eigen(vcov)
  X <- matrix(rnorm(k*nsim),nsim)
  coefsim <- coef + eigen$vectors %*% diag(sqrt(eigen$values),k) %*% t(X)

  ansim_mat <- apply(coefsim,2, function(coefi) {
    (1-exp(-drop(Xpredall%*%coefi))) * cases
  })
  #
  ################################################################################
  #
  an_lower_ci <- apply(ansim_mat, 1, quantile, probs = 0.025, na.rm = TRUE)
  an_upper_ci <- apply(ansim_mat, 1, quantile, probs = 0.975, na.rm = TRUE)
  #
  return(list(cases, an, an_lower_ci, an_upper_ci))
}
