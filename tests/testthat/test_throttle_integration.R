#' Integration Tests for Throttle System
#'
#' Tests the full throttle pipeline including:
#' - Guard creation with injected dependencies
#' - Full admit/reject flow
#' - Path normalization
#' - Bypass scenarios

context("Throttle Integration")

if (!exists("create_throttle_guard")) {
  load_throttle_modules(include_executor = TRUE)
}

create_test_config <- function(enabled = TRUE, buffer_mb = 512) {
  list(
    enabled = enabled,
    min_system_buffer_mb = buffer_mb,
    default_base_mb = 500,
    default_multiplier = 20,
    endpoints = list(
      temperature = list(base_mb = 800, multiplier = 25, arg_name = "data_path", arg_type = "filepath"),
      malaria = list(base_mb = 2000, multiplier = 40, arg_name = "geo_zip_file", arg_type = "upload"),
      descriptive_stats = list(base_mb = 100, multiplier = 5, arg_name = "data", arg_type = "filepath")
    )
  )
}

test_that("full pipeline admits requests with sufficient memory", {
  mock_high_memory <- function() list(success = TRUE, available_mb = 8000, source = "mock")
  config <- create_test_config()

  guard <- create_throttle_guard(config, memory_fn = mock_high_memory)

  req <- list(PATH_INFO = "/temperature", args = list(), body = list())
  decision <- guard("/temperature", req)

  expect_true(decision$admitted)
  expect_equal(decision$reason, "sufficient_memory")
  expect_true(decision$details$headroom_mb > 0)
})

test_that("full pipeline rejects requests with insufficient memory", {
  mock_low_memory <- function() list(success = TRUE, available_mb = 200, source = "mock")
  config <- create_test_config()

  guard <- create_throttle_guard(config, memory_fn = mock_low_memory)

  req <- list(PATH_INFO = "/malaria", args = list(), body = list())
  decision <- guard("/malaria", req)

  expect_false(decision$admitted)
  expect_equal(decision$reason, "insufficient_memory")
  expect_true(decision$details$shortfall_mb > 0)
})

test_that("full pipeline handles memory detection failure (fail-open)", {
  mock_failed_memory <- function() list(success = FALSE, available_mb = NULL, source = "unknown")
  config <- create_test_config()

  guard <- create_throttle_guard(config, memory_fn = mock_failed_memory)

  req <- list(PATH_INFO = "/malaria", args = list(), body = list())
  decision <- guard("/malaria", req)

  expect_true(decision$admitted)
  expect_equal(decision$reason, "memory_detection_unavailable")
})

test_that("guard handles endpoint with leading slash", {
  mock_memory <- function() list(success = TRUE, available_mb = 8000, source = "mock")
  config <- create_test_config()
  guard <- create_throttle_guard(config, memory_fn = mock_memory)

  req <- list(args = list(), body = list())

  d1 <- guard("/temperature", req)
  expect_true(d1$admitted)

  d2 <- guard("temperature", req)
  expect_true(d2$admitted)
})

test_that("guard uses default config for unknown endpoints", {
  mock_memory <- function() list(success = TRUE, available_mb = 8000, source = "mock")
  config <- create_test_config()
  guard <- create_throttle_guard(config, memory_fn = mock_memory)

  req <- list(args = list(), body = list())
  decision <- guard("/some_unknown_endpoint", req)

  expect_true(decision$admitted)
  expect_equal(decision$details$estimated_mb, 500)
})

test_that("filter-level skip logic for lightweight endpoints", {
  skip_paths <- c("", "/", "/health", "/version")

  normalize_path <- function(path) {
    clean <- sub("/$", "", path)
    clean <- sub("\\?.*$", "", clean)
    clean
  }

  expect_true(normalize_path("/health") %in% skip_paths)
  expect_true(normalize_path("/health/") %in% skip_paths)
  expect_true(normalize_path("/") %in% skip_paths)
  expect_true(normalize_path("") %in% skip_paths)
  expect_true(normalize_path("/version") %in% skip_paths)
  expect_false(normalize_path("/temperature") %in% skip_paths)
  expect_false(normalize_path("/malaria") %in% skip_paths)
})

test_that("query params stripped for path matching", {
  normalize_path <- function(path) {
    clean <- sub("/$", "", path)
    clean <- sub("\\?.*$", "", clean)
    clean
  }

  expect_equal(normalize_path("/temperature?foo=bar"), "/temperature")
  expect_equal(normalize_path("/health?check=1"), "/health")
  expect_equal(normalize_path("/malaria?verbose=true&limit=10"), "/malaria")
})

test_that("bypass header allows request even with low memory", {
  mock_low_memory <- function() list(success = TRUE, available_mb = 100, source = "mock")
  config <- create_test_config()
  guard <- create_throttle_guard(config, memory_fn = mock_low_memory)

  req <- list(
    PATH_INFO = "/malaria",
    HTTP_X_BYPASS_THROTTLE = "true",
    args = list(),
    body = list()
  )
  decision <- guard("/malaria", req)

  expect_true(decision$admitted)
  expect_true(grepl("bypass", decision$reason))
})

test_that("disabled throttling allows all requests", {
  mock_low_memory <- function() list(success = TRUE, available_mb = 1, source = "mock")
  config <- create_test_config(enabled = FALSE)
  guard <- create_throttle_guard(config, memory_fn = mock_low_memory)

  req <- list(args = list(), body = list())
  decision <- guard("/malaria", req)

  expect_true(decision$admitted)
  expect_true(grepl("bypass", decision$reason) || grepl("disabled", decision$reason))
})

test_that("guard correctly estimates cost from file size", {
  mock_memory <- function() list(success = TRUE, available_mb = 5000, source = "mock")
  config <- create_test_config()
  guard <- create_throttle_guard(config, memory_fn = mock_memory)

  tmp_file <- tempfile(fileext = ".csv")
  writeBin(raw(10 * 1024 * 1024), tmp_file)
  on.exit(unlink(tmp_file), add = TRUE)

  req <- list(args = list(data_path = tmp_file), body = list())
  decision <- guard("/temperature", req)

  expect_true(decision$admitted)
  expect_equal(decision$details$estimated_mb, 1050, tolerance = 10)
})

test_that("guard correctly estimates cost from upload object", {
  mock_memory <- function() list(success = TRUE, available_mb = 5000, source = "mock")
  config <- create_test_config()
  guard <- create_throttle_guard(config, memory_fn = mock_memory)

  mock_upload <- list(
    filename = "test.zip",
    size = 5 * 1024 * 1024
  )

  req <- list(args = list(geo_zip_file = mock_upload), body = list())
  decision <- guard("/malaria", req)

  expect_true(decision$admitted)
  expect_equal(decision$details$estimated_mb, 2200, tolerance = 10)
})

test_that("admission decision contains all required fields", {
  mock_memory <- function() list(success = TRUE, available_mb = 8000, source = "mock")
  config <- create_test_config()
  guard <- create_throttle_guard(config, memory_fn = mock_memory)

  req <- list(args = list(), body = list())
  decision <- guard("/temperature", req)

  expect_true(is.list(decision))
  expect_true("admitted" %in% names(decision))
  expect_true("reason" %in% names(decision))
  expect_true("details" %in% names(decision))
  expect_true("timestamp" %in% names(decision))

  expect_true("available_mb" %in% names(decision$details))
  expect_true("required_mb" %in% names(decision$details))
  expect_true("estimated_mb" %in% names(decision$details))
})

test_that("rejection decision contains shortfall information", {
  mock_low_memory <- function() list(success = TRUE, available_mb = 500, source = "mock")
  config <- create_test_config()
  guard <- create_throttle_guard(config, memory_fn = mock_low_memory)

  req <- list(args = list(), body = list())
  decision <- guard("/malaria", req)

  expect_false(decision$admitted)
  expect_true("shortfall_mb" %in% names(decision$details))
  expect_true(decision$details$shortfall_mb > 0)
})

test_that("guard uses custom logger function when provided", {
  mock_memory <- function() list(success = TRUE, available_mb = 8000, source = "mock")
  config <- create_test_config()

  log_messages <- c()
  mock_logger <- function(msg) {
    log_messages <<- c(log_messages, msg)
  }

  guard <- create_throttle_guard(config, memory_fn = mock_memory, logger_fn = mock_logger)

  req <- list(args = list(), body = list())
  guard("/temperature", req)

  expect_true(length(log_messages) > 0)
})

test_that("guard handles empty request gracefully", {
  mock_memory <- function() list(success = TRUE, available_mb = 8000, source = "mock")
  config <- create_test_config()
  guard <- create_throttle_guard(config, memory_fn = mock_memory)

  expect_error(guard("/temperature", list()), NA)
  expect_error(guard("/temperature", NULL), NA)
})

test_that("guard handles very high memory requirements", {
  mock_memory <- function() list(success = TRUE, available_mb = 100000, source = "mock")
  config <- list(
    enabled = TRUE,
    min_system_buffer_mb = 512,
    endpoints = list(
      huge = list(base_mb = 50000, multiplier = 100, arg_name = "data", arg_type = "filepath")
    )
  )
  guard <- create_throttle_guard(config, memory_fn = mock_memory)

  req <- list(args = list(), body = list())
  decision <- guard("/huge", req)

  expect_true(decision$admitted)
})
