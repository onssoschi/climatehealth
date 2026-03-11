# Tests for structured error handling (abort.R)
#
# Note: rlang::abort() stores metadata as named list elements accessible
# via $, not as attributes accessible via attr().

if (!"package:climatehealth" %in% search()) {
  pkgload::load_all(".", export_all = TRUE, helpers = FALSE, quiet = TRUE)
}

test_that("abort_climate raises error with correct class hierarchy", {
  err <- tryCatch(
    abort_climate("Test error", "test_type"),
    error = function(e) e
  )

  expect_s3_class(err, "test_type")
  expect_s3_class(err, "climate_error")
  expect_s3_class(err, "error")
  expect_s3_class(err, "condition")
  expect_match(conditionMessage(err), "Test error")
})


test_that("abort_climate includes custom metadata", {
  err <- tryCatch(
    abort_climate(
      "Error with metadata",
      "validation_error",
      param = "nlag",
      value = -1,
      expected = "non-negative"
    ),
    error = function(e) e
  )

  # rlang stores metadata as named list elements
  expect_equal(err$param, "nlag")
  expect_equal(err$value, -1)
  expect_equal(err$expected, "non-negative")
  expect_equal(err$type, "validation_error")
})


test_that("abort_validation raises validation_error class", {
  err <- tryCatch(
    abort_validation("Invalid input", param = "test"),
    error = function(e) e
  )

  expect_s3_class(err, "validation_error")
  expect_s3_class(err, "climate_error")
  expect_equal(err$type, "validation_error")
})


test_that("abort_column_not_found includes column and available columns", {
  available_cols <- c("date", "region", "temperature", "death_count")

  err <- tryCatch(
    abort_column_not_found("tmean", available_cols),
    error = function(e) e
  )

  expect_s3_class(err, "column_not_found")
  expect_s3_class(err, "climate_error")
  expect_equal(err$column, "tmean")
  expect_equal(err$available, available_cols)
  expect_match(conditionMessage(err), "tmean")
  expect_match(conditionMessage(err), "not found")
})


test_that("abort_column_not_found suggests similar column names", {
  # Test with a typo that should match "temperature"
  available_cols <- c("date", "region", "temperature", "death_count")

  err <- tryCatch(
    abort_column_not_found("tmperature", available_cols),  # typo
    error = function(e) e
  )

  # Should suggest "temperature" as a match
  expect_equal(err$suggestion, "temperature")
})


test_that("abort_column_not_found returns NULL suggestion for no close match", {
  available_cols <- c("date", "region", "population")

  err <- tryCatch(
    abort_column_not_found("xyz_totally_different", available_cols),
    error = function(e) e
  )

  expect_null(err$suggestion)
})


test_that("abort_model_error includes model_type", {
  err <- tryCatch(
    abort_model_error(
      "Convergence failed",
      model_type = "dlnm",
      iterations = 100
    ),
    error = function(e) e
  )

  expect_s3_class(err, "model_error")
  expect_s3_class(err, "climate_error")
  expect_equal(err$model_type, "dlnm")
  expect_equal(err$iterations, 100)
  expect_equal(err$type, "model_error")
})


test_that("is_climate_error correctly identifies typed errors", {
  climate_err <- tryCatch(
    abort_climate("test", "test_type"),
    error = function(e) e
  )

  regular_err <- tryCatch(
    stop("regular error"),
    error = function(e) e
  )

  expect_true(is_climate_error(climate_err))
  expect_false(is_climate_error(regular_err))
})


test_that("extract_error_metadata returns structured data for climate errors", {
  err <- tryCatch(
    abort_column_not_found("tmean", c("date", "temp")),
    error = function(e) e
  )

  metadata <- extract_error_metadata(err)

  expect_true(is.list(metadata))
  expect_equal(metadata$type, "column_not_found")
  expect_equal(metadata$column, "tmean")
  expect_equal(metadata$available, c("date", "temp"))
  expect_match(metadata$message, "tmean")
})


test_that("extract_error_metadata handles regular errors gracefully", {
  regular_err <- tryCatch(
    stop("regular error"),
    error = function(e) e
  )

  metadata <- extract_error_metadata(regular_err)

  expect_true(is.list(metadata))
  expect_equal(metadata$type, "generic_error")
  expect_match(metadata$message, "regular error")
})


test_that("suggest_column_match finds close matches", {
  available <- c("date", "region", "temperature", "death_count", "tmean")

  # Test various typos
  expect_equal(
    suggest_column_match("tmperature", available),
    "temperature"
  )

  expect_equal(
    suggest_column_match("daeth_count", available),
    "death_count"
  )
})


test_that("suggest_column_match returns NULL for no match", {
  available <- c("date", "region", "temperature")

  result <- suggest_column_match("xyz_completely_different", available)
  expect_null(result)
})


test_that("suggest_column_match handles empty inputs", {
  expect_null(suggest_column_match("test", character(0)))
})


test_that("suggest_column_match handles NULL input", {
  available <- c("date", "region")
  expect_null(suggest_column_match(NULL, available))
  expect_null(suggest_column_match("", available))
})


test_that("typed errors can be caught at different class levels", {
  # Test catching at specific level
  result <- tryCatch(
    abort_column_not_found("test", c("a", "b")),
    column_not_found = function(e) "caught_specific",
    climate_error = function(e) "caught_category",
    error = function(e) "caught_base"
  )
  expect_equal(result, "caught_specific")

  # Test catching at category level
  result <- tryCatch(
    abort_validation("test"),
    climate_error = function(e) "caught_category",
    error = function(e) "caught_base"
  )
  expect_equal(result, "caught_category")

  # Test catching at base level
  result <- tryCatch(
    abort_model_error("test"),
    error = function(e) "caught_base"
  )
  expect_equal(result, "caught_base")
})


test_that("errors still work when caught generically", {
  # Ensure backwards compatibility - errors caught with generic handler
  # should still contain the message
  err_message <- NULL

  tryCatch(
    abort_column_not_found("missing_col", c("a", "b", "c")),
    error = function(e) {
      err_message <<- conditionMessage(e)
    }
  )

  expect_match(err_message, "missing_col")
  expect_match(err_message, "not found")
})
