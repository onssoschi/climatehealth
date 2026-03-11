#' Unit Tests for throttle_estimator.R
#'
#' Tests type-aware cost estimation for different argument types:
#' - filepath: String path to file on server
#' - upload: Plumber multipart upload object
#' - json_field: Path nested inside JSON payload

if (!exists("load_throttle_modules")) {
  source("tests/testthat/helper-throttle_modules.R", local = FALSE)
}

context("Throttle Estimator")

if (!exists("create_cost_estimator")) {
  load_throttle_modules(include_executor = FALSE)
}

test_that("create_cost_estimator returns a function", {
  config <- list(endpoints = list())
  estimator <- create_cost_estimator(config)
  expect_true(is.function(estimator))
})

test_that("estimator returns default fallback for unknown endpoint", {
  config <- list(
    default_base_mb = 500,
    default_multiplier = 20,
    endpoints = list()
  )
  estimator <- create_cost_estimator(config)

  cost <- estimator("/unknown_endpoint", list(args = list(), body = list()))

  expect_equal(cost$estimated_mb, 500)
  expect_equal(cost$calculation_method, "default_fallback")
  expect_equal(cost$data_size_mb, 0)
})

test_that("estimator uses config default_base_mb for fallback", {
  config <- list(
    default_base_mb = 777,
    endpoints = list()
  )
  estimator <- create_cost_estimator(config)

  cost <- estimator("/anything", list(args = list(), body = list()))

  expect_equal(cost$estimated_mb, 777)
})

test_that("estimator returns base_mb when no file provided", {
  config <- list(
    endpoints = list(
      temperature = list(base_mb = 800, multiplier = 25, arg_name = "data_path", arg_type = "filepath")
    )
  )
  estimator <- create_cost_estimator(config)

  req <- list(args = list(), body = list())
  cost <- estimator("/temperature", req)

  expect_equal(cost$estimated_mb, 800)
  expect_equal(cost$calculation_method, "base_only")
  expect_equal(cost$data_size_mb, 0)
})

test_that("estimator strips leading slash from endpoint name", {
  config <- list(
    endpoints = list(
      malaria = list(base_mb = 2000, multiplier = 40, arg_name = "geo_zip_file", arg_type = "upload")
    )
  )
  estimator <- create_cost_estimator(config)

  cost1 <- estimator("/malaria", list(args = list(), body = list()))
  cost2 <- estimator("malaria", list(args = list(), body = list()))

  expect_equal(cost1$estimated_mb, 2000)
  expect_equal(cost2$estimated_mb, 2000)
})

test_that("estimator calculates cost from filepath with linear model", {
  config <- list(
    endpoints = list(
      temperature = list(base_mb = 100, multiplier = 10, arg_name = "data_path", arg_type = "filepath")
    )
  )
  estimator <- create_cost_estimator(config)

  tmp_file <- tempfile(fileext = ".csv")
  writeBin(raw(1024 * 1024), tmp_file)
  on.exit(unlink(tmp_file), add = TRUE)

  req <- list(args = list(data_path = tmp_file), body = list())
  cost <- estimator("/temperature", req)

  expect_equal(cost$estimated_mb, 110, tolerance = 1)
  expect_equal(cost$calculation_method, "linear_model")
  expect_equal(cost$data_size_mb, 1, tolerance = 0.1)
})

test_that("estimator returns base_only for non-existent filepath", {
  config <- list(
    endpoints = list(
      temperature = list(base_mb = 800, multiplier = 25, arg_name = "data_path", arg_type = "filepath")
    )
  )
  estimator <- create_cost_estimator(config)

  req <- list(args = list(data_path = "/nonexistent/path/file.csv"), body = list())
  cost <- estimator("/temperature", req)

  expect_equal(cost$estimated_mb, 800)
  expect_equal(cost$calculation_method, "base_only")
})

test_that("estimator finds filepath in req$body if not in req$args", {
  config <- list(
    endpoints = list(
      temperature = list(base_mb = 100, multiplier = 10, arg_name = "data_path", arg_type = "filepath")
    )
  )
  estimator <- create_cost_estimator(config)

  tmp_file <- tempfile(fileext = ".csv")
  writeBin(raw(1024 * 1024), tmp_file)
  on.exit(unlink(tmp_file), add = TRUE)

  req <- list(args = list(), body = list(data_path = tmp_file))
  cost <- estimator("/temperature", req)

  expect_equal(cost$calculation_method, "linear_model")
  expect_equal(cost$data_size_mb, 1, tolerance = 0.1)
})

test_that("estimator handles upload type with size attribute", {
  config <- list(
    endpoints = list(
      malaria = list(base_mb = 500, multiplier = 20, arg_name = "geo_zip_file", arg_type = "upload")
    )
  )
  estimator <- create_cost_estimator(config)

  mock_upload <- list(
    filename = "test.zip",
    size = 5 * 1024 * 1024,
    datapath = tempfile()
  )

  req <- list(args = list(geo_zip_file = mock_upload), body = list())
  cost <- estimator("/malaria", req)

  expect_equal(cost$estimated_mb, 600, tolerance = 1)
  expect_equal(cost$calculation_method, "linear_model")
  expect_equal(cost$data_size_mb, 5, tolerance = 0.1)
})

test_that("estimator handles upload type with datapath fallback", {
  config <- list(
    endpoints = list(
      malaria = list(base_mb = 500, multiplier = 20, arg_name = "geo_zip_file", arg_type = "upload")
    )
  )
  estimator <- create_cost_estimator(config)

  tmp_file <- tempfile(fileext = ".zip")
  writeBin(raw(2 * 1024 * 1024), tmp_file)
  on.exit(unlink(tmp_file), add = TRUE)

  mock_upload <- list(
    filename = "test.zip",
    datapath = tmp_file
  )

  req <- list(args = list(geo_zip_file = mock_upload), body = list())
  cost <- estimator("/malaria", req)

  expect_equal(cost$estimated_mb, 540, tolerance = 1)
  expect_equal(cost$calculation_method, "linear_model")
})

test_that("estimator returns base_only for upload with invalid object", {
  config <- list(
    endpoints = list(
      malaria = list(base_mb = 2000, multiplier = 40, arg_name = "geo_zip_file", arg_type = "upload")
    )
  )
  estimator <- create_cost_estimator(config)

  req <- list(args = list(geo_zip_file = "not_a_list"), body = list())
  cost <- estimator("/malaria", req)

  expect_equal(cost$estimated_mb, 2000)
  expect_equal(cost$calculation_method, "base_only")
})

test_that("estimator handles json_field type with nested path", {
  config <- list(
    endpoints = list(
      diarrhea = list(
        base_mb = 500,
        multiplier = 20,
        arg_name = "payload",
        arg_type = "json_field",
        payload_field = "health_data"
      )
    )
  )
  estimator <- create_cost_estimator(config)

  tmp_file <- tempfile(fileext = ".csv")
  writeBin(raw(3 * 1024 * 1024), tmp_file)
  on.exit(unlink(tmp_file), add = TRUE)

  req <- list(
    args = list(),
    body = list(
      payload = list(
        health_data = tmp_file
      )
    )
  )
  cost <- estimator("/diarrhea", req)

  expect_equal(cost$estimated_mb, 560, tolerance = 1)
  expect_equal(cost$calculation_method, "linear_model")
})

test_that("estimator handles json_field with JSON string payload", {
  config <- list(
    endpoints = list(
      diarrhea = list(
        base_mb = 500,
        multiplier = 20,
        arg_name = "payload",
        arg_type = "json_field",
        payload_field = "health_data"
      )
    )
  )
  estimator <- create_cost_estimator(config)

  tmp_file <- tempfile(fileext = ".csv")
  writeBin(raw(2 * 1024 * 1024), tmp_file)
  on.exit(unlink(tmp_file), add = TRUE)

  json_payload <- sprintf('{"health_data": "%s"}', tmp_file)
  req <- list(
    args = list(),
    body = list(
      payload = json_payload
    )
  )
  cost <- estimator("/diarrhea", req)

  expect_equal(cost$estimated_mb, 540, tolerance = 1)
})

test_that("estimator returns base_only for json_field with missing payload_field config", {
  config <- list(
    endpoints = list(
      diarrhea = list(
        base_mb = 500,
        multiplier = 20,
        arg_name = "payload",
        arg_type = "json_field"
      )
    )
  )
  estimator <- create_cost_estimator(config)

  req <- list(args = list(), body = list(payload = list(health_data = "/some/path")))
  cost <- estimator("/diarrhea", req)

  expect_equal(cost$estimated_mb, 500)
  expect_equal(cost$calculation_method, "base_only")
})

test_that("estimator returns config_used in result", {
  config <- list(
    endpoints = list(
      temperature = list(base_mb = 800, multiplier = 25, arg_name = "data_path", arg_type = "filepath")
    )
  )
  estimator <- create_cost_estimator(config)

  cost <- estimator("/temperature", list(args = list(), body = list()))

  expect_true("config_used" %in% names(cost))
  expect_equal(cost$config_used$base, 800)
  expect_equal(cost$config_used$mult, 25)
})

test_that("estimator handles errors gracefully without crashing", {
  config <- list(
    endpoints = list(
      test = list(base_mb = 100, multiplier = 10, arg_name = "data", arg_type = "filepath")
    )
  )
  estimator <- create_cost_estimator(config)

  expect_error(estimator("/test", NULL), NA)
  expect_error(estimator("/test", list()), NA)
  expect_error(estimator("/test", list(args = NULL)), NA)
})
