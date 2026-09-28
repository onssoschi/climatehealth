# Central control for "API mode": the headless, request-serving configuration
# used by the plumber router. Every *_do_analysis() entrypoint routes through
# apply_api_mode() so the rule lives in one place rather than in six
# hand-maintained copies.

#' Is the package running in API mode?
#'
#' @description API mode is switched on by the plumber entrypoint
#' (`inst/plumber/run_api.R`) and reasserted for every request by the
#' `api_mode` filter in `inst/plumber/plumber.R`. In API mode an analysis must
#' neither draw nor write to disk: the calling client renders its own charts
#' from the returned data, and there is no durable filesystem to write to.
#'
#' @returns Boolean. `TRUE` when the `climatehealth.api_mode` option is set.
#'
#' @keywords internal
api_mode_enabled <- function() {
  isTRUE(getOption("climatehealth.api_mode", FALSE))
}

#' Force output-producing arguments off in API mode
#'
#' @description Overrides the named arguments of the calling function when
#' API mode is active: `flags` are set to `FALSE` and `paths` to `NULL`. Call
#' it once at the top of each `*_do_analysis()` entrypoint.
#'
#' Argument names are validated against the calling frame on every call,
#' including when API mode is off, so a typo fails loudly during normal use
#' instead of silently leaving a flag switched on in production.
#'
#' @param flags Character vector. Names of arguments to set to `FALSE`.
#' @param paths Character vector. Names of arguments to set to `NULL`.
#' @param envir Environment to modify. Defaults to the calling frame.
#'
#' @returns Invisibly, `TRUE` when API mode was active and the overrides were
#' applied, otherwise `FALSE`. Assign it to `api_mode` where the caller needs
#' to branch on the mode later.
#'
#' @keywords internal
apply_api_mode <- function(flags = character(),
                           paths = character(),
                           envir = parent.frame()) {
  targets <- c(flags, paths)

  unknown <- targets[!vapply(
    targets,
    exists,
    logical(1),
    envir = envir,
    inherits = FALSE
  )]

  if (length(unknown) > 0) {
    stop(
      "apply_api_mode() was asked to override arguments that do not exist ",
      "in the calling function: ",
      paste(unknown, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  if (!api_mode_enabled()) {
    return(invisible(FALSE))
  }

  for (nm in flags) {
    assign(nm, FALSE, envir = envir)
  }

  for (nm in paths) {
    assign(nm, NULL, envir = envir)
  }

  invisible(TRUE)
}
