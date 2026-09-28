# Helpers for testing inst/plumber/plumber.R without starting a router.
#
# Mirrors helper-throttle_modules.R, which loads the throttling modules the
# same way. Used by test_plumber_disease_endpoints.R and by the API-payload
# tests in test_diarrhea.R / test_malaria.R.

# Locate inst/plumber, whether the package is installed or loaded from source.
# Mirrors load_throttle_modules() in helper-throttle_modules.R.
plumber_api_root <- function() {
  root <- system.file("plumber", package = "climatehealth")
  if (nzchar(root) && file.exists(file.path(root, "plumber.R"))) {
    return(root)
  }
  candidates <- c(
    file.path("..", "..", "inst", "plumber"), # cwd is tests/testthat/
    file.path("inst", "plumber")              # cwd is the package root
  )
  for (candidate in candidates) {
    if (file.exists(file.path(candidate, "plumber.R"))) {
      return(candidate)
    }
  }
  NA_character_
}

# Source plumber.R and return the environment holding its definitions.
#
# plumber.R is plain R -- the `#*` annotations are comments -- so it can be
# sourced without starting a router. Two side effects need containing:
#   * it sets options(climatehealth.api_mode = TRUE) at the top of the file,
#     which setup-api_mode.R deliberately forces to FALSE for the test run, so
#     we register a restore of the caller's value before sourcing;
#   * it defines the endpoint handler objects, which stay in the returned
#     environment rather than leaking into the global environment.
load_plumber_env <- function(.local_envir = parent.frame()) {
  skip_if_not_installed("jsonlite")

  api_root <- plumber_api_root()
  skip_if(
    is.na(api_root),
    "Could not locate inst/plumber/plumber.R from the test working directory."
  )

  # Same restore idiom as setup-api_mode.R, scoped to the calling test.
  previous_api_mode <- getOption("climatehealth.api_mode")
  withr::defer(
    options(climatehealth.api_mode = previous_api_mode),
    envir = .local_envir
  )

  env <- new.env(parent = globalenv())
  source(file.path(api_root, "plumber.R"), local = env)
  env
}
