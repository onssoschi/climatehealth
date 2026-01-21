
#' pkg_health_check() is an internal smoke test intended for CI / developer
#' validation run after package installation.
#'
#' Verifies that critical dependencies are available and that a minimal,
#' representative DLNM modelling path executes successfully.
#'
#' @param verbose Logical. If TRUE, print status messages.
#'   Defaults to FALSE.
#'
#' @return Invisibly returns TRUE on success.
#' @keywords internal
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

  critical_pkgs <- c("dlnm", "dplyr")

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
  # 2) Execute a minimal DLNM modelling path (single region, stable configuration)
  # --------------------------------------------------------------------------
  set.seed(123)

  n_days <- 50

  df <- data.frame(
    temp    = stats::rnorm(n_days, mean = 20, sd = 5),
    outcome = stats::rpois(n_days, lambda = 10)
  )

  df_list <- list(region1 = df)

  cb_list <- list(
    region1 = dlnm::crossbasis(
      df$temp,
      lag = 0,
      argvar = list(
        fun    = "bs",
        knots  = stats::quantile(df$temp, c(0.25, 0.5, 0.75)),
        degree = 2
      ),
      arglag = list(fun = "lin")
    )
  )

  model_list <- list(
    region1 = stats::glm(
      outcome ~ cb_list$region1,
      family = stats::poisson(),
      data   = df
    )
  )

  reduce_fn <- get("dlnm_reduce_cumulative", envir = asNamespace("climatehealth"))

  res <- reduce_fn(
    df_list    = df_list,
    cb_list    = cb_list,
    model_list = model_list
  )

  stopifnot(
    is.list(res),
    length(res) == 2,
    is.matrix(res[[1]]),
    is.list(res[[2]]),
    is.matrix(res[[2]][[1]])
  )

  # --------------------------------------------------------------------------
  # 3) Successful completion
  # --------------------------------------------------------------------------

  vmsg("[climatehealth] Smoke check passed successfully.")

  invisible(TRUE)
}
