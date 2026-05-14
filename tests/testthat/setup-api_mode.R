# Force climatehealth.api_mode = FALSE for the whole test session.
#
# `inst/plumber/plumber.R` sets `options(climatehealth.api_mode = TRUE)` at
# the top of the file so every request through the plumber API short-circuits
# the plotting / file-writing branches inside `*_do_analysis()`. That option
# is process-global, so if you've ever sourced or run plumber.R in the same R
# session (e.g. from RStudio while iterating on the API), the option leaks
# into subsequent `testthat::test()` runs and breaks every test that asserts
# on `save_fig=TRUE` / `save_csv=TRUE` / `create_run_subdir=TRUE` behaviour.
#
# This setup file resets the option for the duration of the test run and
# restores whatever the caller had configured when testthat tears down.

local({
  prev <- getOption("climatehealth.api_mode")
  options(climatehealth.api_mode = FALSE)
  withr::defer(
    options(climatehealth.api_mode = prev),
    teardown_env()
  )
})
