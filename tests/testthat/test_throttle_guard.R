#' Unit Tests for throttle_guard.R
#'
#' Tests admission control logic, bypass handling, and response generation.

if (!exists("load_throttle_modules")) {
  source("tests/testthat/helper-throttle_modules.R", local = FALSE)
}

context("Throttle Guard")

if (!exists("check_admission")) {
  load_throttle_modules(include_executor = FALSE)
}

test_that("create_decision returns structured decision object", {
  decision <- create_decision(TRUE, "test_reason", list(foo = "bar"))
  expect_true(is.list(decision))
  expect_true(decision$admitted)
  expect_equal(decision$reason, "test_reason")
  expect_equal(decision$details$foo, "bar")
  expect_true(inherits(decision$timestamp, "POSIXct"))
})

test_that("check_bypass returns TRUE when throttling disabled", {
  req <- list()
  bypass <- check_bypass(req, throttle_enabled = FALSE)
  expect_true(bypass$bypass)
  expect_equal(bypass$reason, "throttling_disabled")
})

test_that("check_bypass returns TRUE for X-Bypass-Throttle header", {
  req <- list(HTTP_X_BYPASS_THROTTLE = "true")
  bypass <- check_bypass(req, throttle_enabled = TRUE)
  expect_true(bypass$bypass)
  expect_equal(bypass$reason, "bypass_header")
})

test_that("check_bypass returns TRUE for various header formats", {
  req1 <- list(HTTP_X_BYPASS_THROTTLE = "1")
  expect_true(check_bypass(req1, TRUE)$bypass)

  req2 <- list(headers = list(`x-bypass-throttle` = "yes"))
  expect_true(check_bypass(req2, TRUE)$bypass)

  req3 <- list(headers = list(`X-Bypass-Throttle` = "true"))
  expect_true(check_bypass(req3, TRUE)$bypass)
})

test_that("check_bypass returns FALSE when no bypass conditions", {
  req <- list()
  bypass <- check_bypass(req, throttle_enabled = TRUE)
  expect_false(bypass$bypass)
  expect_null(bypass$reason)
})

test_that("check_admission rejects when memory insufficient", {
  cost <- list(estimated_mb = 1000)
  memory <- list(success = TRUE, available_mb = 500)

  decision <- check_admission(cost, memory, min_buffer_mb = 100)

  expect_false(decision$admitted)
  expect_equal(decision$reason, "insufficient_memory")
  expect_equal(decision$details$shortfall_mb, 600)
})

test_that("check_admission admits when memory sufficient", {
  cost <- list(estimated_mb = 500)
  memory <- list(success = TRUE, available_mb = 2000)

  decision <- check_admission(cost, memory, min_buffer_mb = 100)

  expect_true(decision$admitted)
  expect_equal(decision$reason, "sufficient_memory")
  expect_equal(decision$details$headroom_mb, 1400)
})

test_that("check_admission admits when memory exactly sufficient", {
  cost <- list(estimated_mb = 500)
  memory <- list(success = TRUE, available_mb = 600)

  decision <- check_admission(cost, memory, min_buffer_mb = 100)

  expect_true(decision$admitted)
  expect_equal(decision$details$headroom_mb, 0)
})

test_that("check_admission admits when memory detection fails (fail-open)", {
  cost <- list(estimated_mb = 10000)
  memory <- list(success = FALSE, available_mb = NULL, source = "unknown")

  decision <- check_admission(cost, memory, min_buffer_mb = 100)

  expect_true(decision$admitted)
  expect_equal(decision$reason, "memory_detection_unavailable")
})

test_that("reject_request returns 503 for temporary insufficient memory", {
  decision <- create_decision(
    admitted = FALSE,
    reason = "insufficient_memory",
    details = list(available_mb = 500, required_mb = 1500, total_mb = 4000, shortfall_mb = 1000)
  )

  res <- new.env()
  res$status <- NULL
  res$headers <- list()
  res$setHeader <- function(name, value) {
    res$headers[[name]] <<- value
  }

  result <- reject_request(decision, res)

  expect_equal(res$status, 503)
  expect_equal(res$headers[["Retry-After"]], "60")
  expect_equal(res$headers[["X-Throttle-Reason"]], "insufficient_memory")
  expect_equal(result$reason, "insufficient_memory")
  expect_equal(result$retry_after_seconds, 60)
  expect_true(grepl("retry", result$message, ignore.case = TRUE))
})

test_that("reject_request returns 413 for file too large (permanent)", {
  decision <- create_decision(
    admitted = FALSE,
    reason = "insufficient_memory",
    details = list(available_mb = 2000, required_mb = 5000, total_mb = 4000, shortfall_mb = 3000)
  )

  res <- new.env()
  res$status <- NULL
  res$headers <- list()
  res$setHeader <- function(name, value) {
    res$headers[[name]] <<- value
  }

  result <- reject_request(decision, res)

  expect_equal(res$status, 413)
  expect_equal(res$headers[["X-Throttle-Reason"]], "file_too_large")
  expect_null(res$headers[["Retry-After"]])
  expect_equal(result$reason, "file_too_large")
  expect_null(result$retry_after_seconds)
  expect_equal(result$details$excess_mb, 1000)
  expect_true(grepl("reduce", result$message, ignore.case = TRUE))
})

test_that("reject_request falls back to 503 when total_mb is unknown", {
  decision <- create_decision(
    admitted = FALSE,
    reason = "insufficient_memory",
    details = list(available_mb = 500, required_mb = 1500, total_mb = 0, shortfall_mb = 1000)
  )

  res <- new.env()
  res$status <- NULL
  res$headers <- list()
  res$setHeader <- function(name, value) {
    res$headers[[name]] <<- value
  }

  result <- reject_request(decision, res)

  expect_equal(res$status, 503)
  expect_equal(result$reason, "insufficient_memory")
})

test_that("create_throttle_guard creates working guard function", {
  mock_memory <- function() list(success = TRUE, available_mb = 8000, total_mb = 16000, source = "mock")

  config <- list(
    enabled = TRUE,
    min_system_buffer_mb = 512,
    default_base_mb = 500,
    endpoints = list(
      temperature = list(base_mb = 800, multiplier = 25, arg_name = "data_path", arg_type = "filepath")
    )
  )

  guard <- create_throttle_guard(config, memory_fn = mock_memory)

  expect_true(is.function(guard))

  req <- list(PATH_INFO = "/temperature", args = list(), body = list())
  decision <- guard("/temperature", req)

  expect_true(decision$admitted)
})

test_that("create_throttle_guard rejects when memory low", {
  mock_memory <- function() list(success = TRUE, available_mb = 200, source = "mock")

  config <- list(
    enabled = TRUE,
    min_system_buffer_mb = 512,
    endpoints = list(
      malaria = list(base_mb = 2000, multiplier = 40, arg_name = "geo_zip_file", arg_type = "upload")
    )
  )

  guard <- create_throttle_guard(config, memory_fn = mock_memory)

  req <- list(PATH_INFO = "/malaria", args = list(), body = list())
  decision <- guard("/malaria", req)

  expect_false(decision$admitted)
})

test_that("create_throttle_guard respects bypass when disabled", {
  config <- list(enabled = FALSE)

  guard <- create_throttle_guard(config)

  req <- list()
  decision <- guard("/anything", req)

  expect_true(decision$admitted)
  expect_true(grepl("bypass", decision$reason))
})
