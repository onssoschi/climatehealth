
#' Run a minimal smoke check post-install
#'
#' Verifies that critical dependencies are available and that a minimal,
#' representative modelling path executes successfully.
#'
#' @param verbose Logical. If TRUE, print status messages.
#'   Defaults to FALSE.
#'
#' @return Invisibly returns TRUE on success.
#' @export
pkg_health_check <- function(verbose = FALSE) {

  # Helper for controlled messaging
  vmsg <- function(...) {
    if (isTRUE(verbose)) {
      message(...)
    }
  }

  # --------------------------------------------------------------------------
  # 1) Check presence of critical dependencies (no installation attempted)
  # --------------------------------------------------------------------------
  critical_pkgs <- c(
    "dlnm",
    "dplyr",
    "splines",
    "stats"
  )

  missing <- critical_pkgs[
    !vapply(critical_pkgs, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(missing)) {
    msg <- sprintf(
      "[climatehealth] Smoke check failed: missing critical dependencies: %s",
      paste(missing, collapse = ", ")
    )
    if (verbose) message(msg)
    stop(msg, call. = FALSE)
  }

  dep_versions <- vapply(
    critical_pkgs,
    function(p) as.character(utils::packageVersion(p)),
    character(1)
  )

  vmsg(sprintf(
    "[climatehealth] Dependencies OK: %s",
    paste(sprintf("%s@%s", names(dep_versions), dep_versions), collapse = ", ")
  ))

  # --------------------------------------------------------------------------
  # 2) Execute a minimal representative modelling path
  # --------------------------------------------------------------------------
  set.seed(42)

  df <- data.frame(
    dependent = stats::rpois(30, lambda = 8),
    temp      = rnorm(30, mean = 20, sd = 2.5),
    date      = seq.Date(as.Date("2023-01-01"), by = "day", length.out = 30),
    year      = rep(2023, 30)
  )

  res <- tryCatch(
    define_model(
      dataset          = df,
      independent_cols = NULL,
      varfun           = "bs",
      varper           = 50,  # single internal knot
      vardegree        = 1,
      lag              = 2,
      lagnk            = 1,
      dfseas           = 2
    ),
    error = function(e) {
      msg <- sprintf(
        "[climatehealth] Smoke check failed during model definition: %s",
        conditionMessage(e)
      )
      if (verbose) message(msg)
      stop(msg, call. = FALSE)
    }
  )

  stopifnot(is.list(res), length(res) == 2)
  model <- res[[1]]
  cb    <- res[[2]]

  stopifnot(inherits(model, "glm"))
  stopifnot(inherits(cb, "crossbasis"))

  cen  <- mean(df$temp, na.rm = TRUE)
  pred <- dlnm::crossreduce(cb, model, cen = cen)

  stopifnot(
    is.numeric(stats::coef(pred)),
    is.matrix(stats::vcov(pred))
  )

  # --------------------------------------------------------------------------
  # 3) Successful completion
  # --------------------------------------------------------------------------
  vmsg("[climatehealth] Smoke check passed successfully.")

  invisible(TRUE)
}
