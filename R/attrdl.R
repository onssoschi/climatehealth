################################################################################
# FUNCTION FOR COMPUTING ATTRIBUTABLE MEASURES FROM DLNM
#   RE-IMPLEMENTED TO INCLUDE INTERNAL LOGIC FROM DLNM:
#     - getcoef
#     - getvcov
#     - getlink
#     - seqlag
#     - mkXpred
#
# (c) Antonio Gasparrini 2015–2017
# Adapted and extended for standalone use without ::: calls
# Original sources:
#   - https://github.com/gasparrini/2014_gasparrini_BMCmrm_Rcodedata
#   - https://cran.r-project.org/package=dlnm
################################################################################

getcoef <- function(model, class) {
  # EXTRACT COEF
  # NB: gam, gee AND geeglm HAVE CLASS glm AS WELL
  coef <- if(any(class%in%c("glm","gam","coxph"))) coef(model) else
    if(any(class%in%c("lme","lmerMod","glmerMod","lmerModLmerTest"))) lme4::fixef(model) else
      tryCatch(coef(model),error=function(w) "error")
  if(identical(coef,"error")) stop("methods for coef() and vcov() must ",
    "exist for the class of object 'model'. If not, extract them manually and ",
    "use the arguments 'coef' and 'vcov'")
  return(coef)
}

getvcov <- function(model, class) {
  # EXTRACT VCOV
  # NB: gam, gee AND geeglm HAVE CLASS glm AS WELL
  vcov <- if(any(class%in%c("lm","glm","lme","coxph")) &&
      !identical(class,c("gee","glm"))) vcov(model) else if(identical(class,c("gee","glm")))
        model$robust.variance else if(any(class%in%c("lmerMod","glmerMod","lmerModLmerTest")))
            as.matrix(vcov(model)) else tryCatch(vcov(model),error=function(w) "error")
  if(identical(vcov,"error")) stop("methods for coef() and vcov() must ",
    "exist for the class of object 'model'. If not, extract them manually and ",
    "use the arguments 'coef' and 'vcov'")
  return(vcov)
}

getlink <- function(model, class, model.link=NULL) {
  # IF PROVIDED, JUST RETURN
  if(!is.null(model.link)) return(model.link)
  # OTHERWISE, EXTRACT FROM MODEL (IF AVAILABLE)
  link <- if(all(class%in%c("lm")) || all(class%in%c("lme")) ||
    any(class%in%"nlme") || any(class%in%"lmerMod")) "identity" else
    if(any(class %in% c("clogit"))) "logit" else
    if(any(class %in% c("coxph"))) "log" else
    if(any(class %in% c("glm")) || any(class %in% c("glmmPQL")))
    model$family$link else if(any(class %in% c("glmerMod")))
    model@resp$family$link else NA
  return(link)
}

seqlag <- function(lag, by=1) seq(from=lag[1], to=lag[2], by=by)

mkXpred <- function(type, basis, at, predvar, predlag, cen) {
  # CREATE THE MATRIX OF TRANSFORMED CENTRED VARIABLES (DEPENDENT ON TYPE)
  # CREATE VECTORIZED LAGGED VALUES
  varvec <- if(is.matrix(at)) as.numeric(at) else rep(at,length(predlag))
  lagvec <- rep(predlag,each=length(predvar))
  if(type=="cb") {
    # IF STANDARD CROSS-BASIS, CREATE MARGINAL BASIS AND CALL TENSOR
    # NB: ORDER OF BASIS MATRICES IN TENSOR CHANGED SINCE VERSION 2.2.4
    # CENTERING APPLIED ONLY MARGINALLY TO VAR DIMENSION
    basisvar <- do.call(dlnm::onebasis,c(list(x=varvec),attr(basis,"argvar")))
    basislag <- do.call(dlnm::onebasis,c(list(x=lagvec),attr(basis,"arglag")))
    if(!is.null(cen)) {
      basiscen <- do.call(dlnm::onebasis,c(list(x=cen),attr(basis,"argvar")))
      basisvar <- scale(basisvar,center=basiscen,scale=FALSE)
    }
    Xpred <- mgcv::tensor.prod.model.matrix(list(basisvar,basislag))
  } else if(type=="one") {
    # IF ONEBASIS, SIMPLY CALL THE FUNCTION WITH PROPER ARGUMENTS
    ind <- match(c("fun",names(formals(attr(basis,"fun")))),
      names(attributes(basis)),nomatch=0)
    basisvar <- do.call(dlnm::onebasis,c(list(x=varvec),attributes(basis)[ind]))
    if(!is.null(cen)) {
      basiscen <- do.call(dlnm::onebasis,c(list(x=cen),attributes(basis)[ind]))
      basisvar <- scale(basisvar,center=basiscen,scale=FALSE)
    }
    Xpred <- basisvar
  } else {
    # FINALLY, IF GAM, CALL PredictMat WITH PROPER DATA
    # CENTERING APPLIED TO THE TENSOR PRODUCT (NOT EFFICIENT BUT EASIER)
    data <- list(varvec, lagvec)
    names(data) <- basis$term
    Xpred <- mgcv::PredictMat(basis, data, n=length(varvec))
    if(!is.null(cen)) {
      data[[1]] <- rep(cen,length(varvec))
      cbcen <- mgcv::PredictMat(basis,data,n=length(varvec))
      Xpred <- Xpred-cbcen
    }
  }
  return(Xpred)
}

#' FUNCTION FOR COMPUTING ATTRIBUTABLE MEASURES FROM DLNM
#'
#' @description
#' A function to calculate attributable numbers and fractions derived from
#' (c) Antonio Gasparrini 2015-2017.
#'
#'
#' @param x AN EXPOSURE VECTOR OR (ONLY FOR dir="back") A MATRIX OF LAGGED EXPOSURES
#' @param basis THE CROSS-BASIS COMPUTED FROM x
#' @param cases THE CASES VECTOR OR (ONLY FOR dir="forw") THE MATRIX OF FUTURE CASES
#' @param model THE FITTED MODEL
#' @param coef COEF FOR basis IF model IS NOT PROVIDED
#' @param vcov VCOV FOR basis IF model IS NOT PROVIDED
#' @param model.link LINK FUNCTION IF model IS NOT PROVIDED
#' @param type EITHER "an" OR "af" FOR ATTRIBUTABLE NUMBER OR FRACTION
#' @param dir EITHER "back" OR "forw" FOR BACKWARD OR FORWARD PERSPECTIVES
#' @param tot IF TRUE, THE TOTAL ATTRIBUTABLE RISK IS COMPUTED
#' @param cen THE REFERENCE VALUE USED AS COUNTERFACTUAL SCENARIO
#' @param range THE RANGE OF EXPOSURE. IF NULL, THE WHOLE RANGE IS USED
#' @param sim IF SIMULATION SAMPLES SHOULD BE RETURNED. ONLY FOR tot=TRUE
#' @param nsim NUMBER OF SIMULATION SAMPLES
#'
#' @return Attributable Numbers and Fractions
#'
#' @export
attrdl <- function(x,basis,cases,model=NULL,coef=NULL,vcov=NULL,model.link=NULL,
                   type="af",dir="back",tot=TRUE,cen,range=NULL,sim=FALSE,nsim=5000) {
  ################################################################################
  #
  # CHECK VERSION OF THE DLNM PACKAGE
  if(packageVersion("dlnm")<"2.2.0")
    stop("update dlnm package to version >= 2.2.0")
  #
  # EXTRACT NAME AND CHECK type AND dir
  name <- deparse(substitute(basis))
  type <- match.arg(type,c("an","af"))
  dir <- match.arg(dir,c("back","forw"))
  #
  # DEFINE CENTERING
  if(missing(cen) && is.null(cen1 <- attr(basis,"argvar")$cen))
    stop("'cen' must be provided")
  if(!is.numeric(cen) && length(cen)>1L) stop("'cen' must be a numeric scalar")
  attributes(basis)$argvar$cen <- NULL
  #
  # SELECT RANGE (FORCE TO CENTERING VALUE OTHERWISE, MEANING NULL RISK)
  if(!is.null(range)) x[x<range[1]|x>range[2]] <- cen
  #
  # COMPUTE THE MATRIX OF
  #   - LAGGED EXPOSURES IF dir="back"
  #   - CONSTANT EXPOSURES ALONG LAGS IF dir="forw"
  lag <- attr(basis,"lag")
  if(NCOL(x)==1L) {
    at <- if(dir=="back") tsModel::Lag(x,seq(lag[1],lag[2])) else
      matrix(rep(x,diff(lag)+1),length(x))
  } else {
    if(dir=="forw") stop("'x' must be a vector when dir='forw'")
    if(ncol(at <- x)!=diff(lag)+1)
      stop("dimension of 'x' not compatible with 'basis'")
  }
  #
  # NUMBER USED FOR THE CONTRIBUTION AT EACH TIME IN FORWARD TYPE
  #   - IF cases PROVIDED AS A MATRIX, TAKE THE ROW AVERAGE
  #   - IF PROVIDED AS A TIME SERIES, COMPUTE THE FORWARD MOVING AVERAGE
  #   - THIS EXCLUDES MISSING ACCORDINGLY
  # ALSO COMPUTE THE DENOMINATOR TO BE USED BELOW
  if(NROW(cases)!=NROW(at)) stop("'x' and 'cases' not consistent")
  if(NCOL(cases)>1L) {
    if(dir=="back") stop("'cases' must be a vector if dir='back'")
    if(ncol(cases)!=diff(lag)+1) stop("dimension of 'cases' not compatible")
    den <- sum(rowMeans(cases,na.rm=TRUE),na.rm=TRUE)
    cases <- rowMeans(cases)
  } else {
    den <- sum(cases,na.rm=TRUE)
    if(dir=="forw")
      cases <- rowMeans(as.matrix(tsModel::Lag(cases,-seq(lag[1],lag[2]))))
  }

  #
  ################################################################################
  #
  # EXTRACT COEF AND VCOV IF MODEL IS PROVIDED
  if(!is.null(model)) {
    cond <- paste0(name,"[[:print:]]*v[0-9]{1,2}\\.l[0-9]{1,2}")
    if(ncol(basis)==1L) cond <- name
    model.class <- class(model)
    coef <- getcoef(model,model.class)
    ind <- grep(cond,names(coef))
    coef <- coef[ind]
    vcov <- getvcov(model,model.class)[ind,ind,drop=FALSE]
    model.link <- getlink(model,model.class)
    if(!model.link %in% c("log","logit"))
      stop("'model' must have a log or logit link function")
  }
  #
  # IF REDUCED ESTIMATES ARE PROVIDED
  typebasis <- ifelse(length(coef)!=ncol(basis),"one","cb")
  #
  ################################################################################
  #
  # PREPARE THE ARGUMENTS FOR TH BASIS TRANSFORMATION
  predvar <- if(typebasis=="one") x else seq(NROW(at))
  predlag <- if(typebasis=="one") 0 else seqlag(lag)
  #
  # CREATE THE MATRIX OF TRANSFORMED CENTRED VARIABLES (DEPENDENT ON typebasis)
  if(typebasis=="cb") {
    Xpred <- mkXpred(typebasis,basis,at,predvar,predlag,cen)
    Xpredall <- 0
    for (i in seq(length(predlag))) {
      ind <- seq(length(predvar))+length(predvar)*(i-1)
      Xpredall <- Xpredall + Xpred[ind,,drop=FALSE]
    }
  } else {
    basis <- do.call(dlnm::onebasis,c(list(x=x),attr(basis,"argvar")))
    Xpredall <- mkXpred(typebasis,basis,x,predvar,predlag,cen)
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
  # COMPUTE AF AND AN
  af <- 1-exp(-drop(as.matrix(Xpredall%*%coef)))
  an <- af*cases
  #
  # TOTAL
  #   - SELECT NON-MISSING OBS CONTRIBUTING TO COMPUTATION
  #   - DERIVE TOTAL AF
  #   - COMPUTE TOTAL AN WITH ADJUSTED DENOMINATOR (OBSERVED TOTAL NUMBER)
  if(tot) {
    isna <- is.na(an)
    af <- sum(an[!isna])/sum(cases[!isna])
    an <- af*den
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
    X <- matrix(rnorm(length(coef)*nsim),nsim)
    coefsim <- coef + eigen$vectors %*% diag(sqrt(eigen$values),k) %*% t(X)
    # RUN THE LOOP
    # pre_afsim <- (1 - exp(- Xpredall %*% coefsim)) * cases # a matrix
    # afsim <- colSums(pre_afsim,na.rm=TRUE) / sum(cases[!isna],na.rm=TRUE)
    afsim <- apply(coefsim,2, function(coefi) {
      ani <- (1-exp(-drop(Xpredall%*%coefi)))*cases
      sum(ani[!is.na(ani)])/sum(cases[!is.na(ani)])
    })
    ansim <- afsim*den
  }
  #
  ################################################################################
  #
  res <- if(sim) {
    if(type=="an") ansim else afsim
  } else {
    if(type=="an") an else af
  }
  #
  return(res)
}

#' FUNCTION FOR COMPUTING ATTRIBUTABLE MEASURES FROM DLNM
#'
#' @description
#' A function to calculate attributable numbers and fractions derived from
#' (c) Antonio Gasparrini 2015-2017. Modifications to produce daily values with
#' confidence intervals.
#'
#'
#' @param x AN EXPOSURE VECTOR OR (ONLY FOR dir="back") A MATRIX OF LAGGED EXPOSURES
#' @param basis THE CROSS-BASIS COMPUTED FROM x
#' @param cases THE CASES VECTOR OR (ONLY FOR dir="forw") THE MATRIX OF FUTURE CASES
#' @param coef COEF FOR basis IF model IS NOT PROVIDED
#' @param vcov VCOV FOR basis IF model IS NOT PROVIDED
#' @param model.link LINK FUNCTION IF model IS NOT PROVIDED
#' @param dir EITHER "back" OR "forw" FOR BACKWARD OR FORWARD PERSPECTIVES
#' @param tot IF TRUE, THE TOTAL ATTRIBUTABLE RISK IS COMPUTED
#' @param cen THE REFERENCE VALUE USED AS COUNTERFACTUAL SCENARIO
#' @param range THE RANGE OF EXPOSURE. IF NULL, THE WHOLE RANGE IS USED
#' @param nsim NUMBER OF SIMULATION SAMPLES
#'
#' @return
#' \itemize{
#'  \item Attributable Fraction
#'  \item Attributable Fraction lower confidence intervals
#'  \item Attributable Fraction upper confidence intervals
#'  \item Attributable Numbers
#'  \item Attributable Numbers lower confidence intervals
#'  \item Attributable Numbers upper confidence intervals
#'  }
#' @export
#'
an_attrdl <- function(
    x,
    basis,
    cases,
    coef=NULL,
    vcov=NULL,
    model.link=NULL,
    dir="back",
    tot=TRUE,
    cen,
    range=NULL,
    nsim=5000
) {

  # CHECK type AND dir
  dir <- match.arg(dir,c("back","forw"))

  # Remove centering value from basis
  attributes(basis)$argvar$cen <- NULL
  # SELECT RANGE (FORCE TO CENTERING VALUE OTHERWISE, MEANING NULL RISK)
  if(!is.null(range)) x[x<range[1]|x>range[2]] <- cen
  # COMPUTE THE MATRIX OF
  #   - LAGGED EXPOSURES IF dir="back"
  #   - CONSTANT EXPOSURES ALONG LAGS IF dir="forw"
  lag <- attr(basis,"lag")
  at <- if(dir=="back") tsModel::Lag(x,seq(lag[1],lag[2])) else
    matrix(rep(x,diff(lag)+1),length(x))
  # NUMBER USED FOR THE CONTRIBUTION AT EACH TIME IN FORWARD TYPE
  #   - IF PROVIDED AS A TIME SERIES, COMPUTE THE FORWARD MOVING AVERAGE
  #   - THIS EXCLUDES MISSING ACCORDINGLY
  # ALSO COMPUTE THE DENOMINATOR TO BE USED BELOW
  if(NROW(cases)!=NROW(at)) stop("'x' and 'cases' not consistent")
  den <- sum(cases,na.rm=TRUE)
  if(dir=="forw"){
    cases <- rowMeans(as.matrix(tsModel::Lag(cases,-seq(lag[1],lag[2]))))
  }
  #

  # IF REDUCED ESTIMATES ARE PROVIDED
  typebasis <- ifelse(length(coef)!=ncol(basis),"one","cb")

  # PREPARE THE ARGUMENTS FOR TH BASIS TRANSFORMATION
  predvar <- if(typebasis=="one") x else seq(NROW(at))
  predlag <- if(typebasis=="one") 0 else seqlag(lag)

  # CREATE THE MATRIX OF TRANSFORMED CENTRED VARIABLES (DEPENDENT ON typebasis)
  if(typebasis=="cb") {
    Xpred <- mkXpred(typebasis,basis,at,predvar,predlag,cen)
    Xpredall <- 0
    for (i in seq(length(predlag))) {
      ind <- seq(length(predvar))+length(predvar)*(i-1)
      Xpredall <- Xpredall + Xpred[ind,,drop=FALSE]
    }
  } else {
    basis <- do.call(dlnm::onebasis,c(list(x=x),attr(basis,"argvar")))
    Xpredall <- mkXpred(typebasis,basis,x,predvar,predlag,cen)
  }

  # CHECK DIMENSIONS
  if(length(coef)!=ncol(Xpredall))
    stop("arguments 'basis' do not match 'model' or 'coef'-'vcov'")
  if(any(dim(vcov)!=c(length(coef),length(coef))))
    stop("arguments 'coef' and 'vcov' do no match")
  if(typebasis=="one" && dir=="back")
    stop("only dir='forw' allowed for reduced estimates")
  # COMPUTE AN
  an <- (1-exp(-drop(as.matrix(Xpredall%*%coef)))) * cases
  af <- an / cases

  # SAMPLE COEF
  k <- length(coef)
  eigen <- eigen(vcov)
  X <- matrix(rnorm(k*nsim),nsim)
  coefsim <- coef + eigen$vectors %*% diag(sqrt(eigen$values),k) %*% t(X)

  ansim_mat <- apply(coefsim,2, function(coefi) {
    (1-exp(-drop(Xpredall%*%coefi))) * cases
  })

  an_lower_ci <- apply(ansim_mat, 1, quantile, probs = 0.025, na.rm = TRUE)
  an_upper_ci <- apply(ansim_mat, 1, quantile, probs = 0.975, na.rm = TRUE)
  af_lower_ci <- an_lower_ci / cases
  af_upper_ci <- an_upper_ci / cases

  return(list(af, af_lower_ci, af_upper_ci, an, an_lower_ci, an_upper_ci))
}


