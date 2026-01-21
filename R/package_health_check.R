
#' pkg_health_check() is an internal smoke test intended for CI / developer validation run after package installation.
#'
#' Verifies that critical dependencies are available and that a minimal,
#' representative modelling path executes successfully.
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

  critical_pkgs <- c(
    "dlnm",
    "dplyr"
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
  # 2) Execute a minimal DLNM modelling path (single region)
  # --------------------------------------------------------------------------
  set.seed(42)

  df <- data.frame(
    dependent = stats::rpois(20, lambda = 10),
    temp      = stats::rnorm(20, mean = 20, sd = 3),
    date      = seq.Date(as.Date("2023-01-01"), by = "day", length.out = 20),
    year      = rep(2023, 20)
  )

  df_list <- list(region1 = df)

  cb <- dlnm::crossbasis(
    df$temp,
    lag = 2,
    argvar = list(
      fun   = "bs",
      knots = stats::quantile(df$temp, c(0.25, 0.5, 0.75))
    ),
    arglag = list(
      knots = dlnm::logknots(2, 1)
    )
  )

  cb_list <- list(region1 = cb)

  model <- stats::glm(
    dependent ~ cb,
    data = df,
    family = stats::quasipoisson(),
    na.action = stats::na.exclude
  )

  model_list <- list(region1 = model)

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
