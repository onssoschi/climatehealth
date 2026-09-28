# Tests for the shared API-mode guard.
#
# Note: setup-api_mode.R forces climatehealth.api_mode = FALSE for the whole
# test session, so every test here that needs API mode opts back in explicitly
# with withr::local_options(). That is deliberate -- it keeps the rest of the
# suite free to assert on save_fig = TRUE behaviour.
#
# Per-indicator behavioural coverage lives beside each module's fixtures, in
# test_temp_mortality.R, test_mental_health.R, test_wildfire.R,
# test_air_pollution.R, test_malaria.R and test_diarrhea.R.


# --- api_mode_enabled() ------------------------------------------------------

test_that("api_mode_enabled reflects the climatehealth.api_mode option", {
  withr::local_options(list(climatehealth.api_mode = TRUE))
  expect_true(api_mode_enabled())

  withr::local_options(list(climatehealth.api_mode = FALSE))
  expect_false(api_mode_enabled())

  withr::local_options(list(climatehealth.api_mode = NULL))
  expect_false(api_mode_enabled())
})


# --- apply_api_mode() --------------------------------------------------------

# Stand-in for a *_do_analysis() entrypoint, so the overrides are exercised
# against a real calling frame rather than a synthetic environment.
fake_entrypoint <- function(save_fig = TRUE,
                            save_csv = TRUE,
                            output_folder_path = "/tmp/output") {
  api_mode <- apply_api_mode(
    flags = c("save_fig", "save_csv"),
    paths = "output_folder_path"
  )

  list(
    api_mode = api_mode,
    save_fig = save_fig,
    save_csv = save_csv,
    output_folder_path = output_folder_path
  )
}


test_that("apply_api_mode leaves arguments untouched when API mode is off", {
  withr::local_options(list(climatehealth.api_mode = FALSE))

  result <- fake_entrypoint()

  expect_false(result$api_mode)
  expect_true(result$save_fig)
  expect_true(result$save_csv)
  expect_equal(result$output_folder_path, "/tmp/output")
})


test_that("apply_api_mode forces flags off and paths to NULL in API mode", {
  withr::local_options(list(climatehealth.api_mode = TRUE))

  result <- fake_entrypoint()

  expect_true(result$api_mode)
  expect_false(result$save_fig)
  expect_false(result$save_csv)
  expect_null(result$output_folder_path)
})


test_that("apply_api_mode rejects names that the caller does not have", {
  # A typo must fail loudly rather than silently creating a new variable and
  # leaving the real flag switched on. Checked in both modes so the mistake
  # surfaces during ordinary development, not only in production.
  typo_entrypoint <- function(save_fig = TRUE) {
    apply_api_mode(flags = c("save_fig", "save_fgi"))
    save_fig
  }

  withr::local_options(list(climatehealth.api_mode = TRUE))
  expect_error(typo_entrypoint(), "save_fgi")

  withr::local_options(list(climatehealth.api_mode = FALSE))
  expect_error(typo_entrypoint(), "save_fgi")
})


test_that("apply_api_mode accepts an empty override set", {
  withr::local_options(list(climatehealth.api_mode = TRUE))

  noop_entrypoint <- function() apply_api_mode()

  expect_true(noop_entrypoint())
})


# --- completeness -----------------------------------------------------------

test_that("every analysis entrypoint routes through apply_api_mode", {
  # This is a completeness check, not a correctness one: it cannot show that
  # the guard runs at the right point, only that no entrypoint is missing it
  # entirely. That is the gap this codebase actually fell through --
  # temp_mortality got its guard in a separate commit from mental_health and
  # the difference went unnoticed for months. Behavioural coverage for each
  # module lives in that module's own test file.
  entrypoints <- c(
    "temp_mortality_do_analysis",
    "suicides_heat_do_analysis",
    "wildfire_do_analysis",
    "air_pollution_do_analysis",
    "malaria_do_analysis",
    "diarrhea_do_analysis"
  )

  missing_guard <- entrypoints[!vapply(
    entrypoints,
    function(nm) {
      source_text <- paste(deparse(body(get(nm))), collapse = "\n")
      grepl("apply_api_mode(", source_text, fixed = TRUE)
    },
    logical(1)
  )]

  expect_equal(missing_guard, character(0))
})


test_that("the plumber router reasserts API mode on every request", {
  # Static check on the shipped router: the filter is what stops a package
  # reload mid-session from restoring interactive plotting behaviour.
  plumber_file <- system.file("plumber", "plumber.R", package = "climatehealth")
  skip_if(
    !nzchar(plumber_file) || !file.exists(plumber_file),
    "plumber assets are not available in this installation"
  )

  router_source <- paste(readLines(plumber_file, warn = FALSE), collapse = "\n")

  expect_match(router_source, "#* @filter api_mode", fixed = TRUE)
  expect_match(
    router_source,
    "options(climatehealth.api_mode = TRUE)",
    fixed = TRUE
  )
})


# --- diagnostics must not draw when figures are disabled ---------------------

test_that("hc_model_validation does no plotting when figures are disabled", {
  # hc_model_validation() is where the outage started: before the May guards
  # its four diagnostic plot loops ran unconditionally, so save_fig = FALSE
  # still drew to whatever device happened to be current.
  qaic <- data.frame(
    region = "Region1",
    formula = "deaths ~ cb",
    disp = 1,
    qaic = 2
  )

  local_mocked_bindings(
    hc_model_combo_res = function(...) {
      list(qaic, list(Region1 = list()))
    },
    hc_adf = function(...) {
      list(Region1 = data.frame(statistic = -5, p_value = 0.01))
    }
  )

  result <- expect_no_plotting(
    hc_model_validation(
      df_list = list(Region1 = data.frame()),
      cb_list = list(Region1 = matrix(numeric())),
      save_fig = FALSE,
      save_csv = FALSE
    )
  )

  expect_equal(result[[1]], qaic)
  expect_length(result, 5)
})
