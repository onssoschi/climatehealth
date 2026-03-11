#' Unit Tests for throttle_config.R
#'
#' Tests configuration loading and environment variable override behavior.

context("Throttle Config")

if (!exists("load_throttle_config")) {
  load_throttle_modules(include_executor = FALSE)
}

test_that("get_env_typed returns default when env var not set", {
  withr::with_envvar(c(TEST_VAR_UNSET = NA), {
    expect_equal(get_env_typed("TEST_VAR_UNSET", default = 42, type = "numeric"), 42)
    expect_equal(get_env_typed("TEST_VAR_UNSET", default = "hello", type = "character"), "hello")
    expect_equal(get_env_typed("TEST_VAR_UNSET", default = TRUE, type = "logical"), TRUE)
  })
})

test_that("get_env_typed parses logical values correctly", {
  withr::with_envvar(c(TEST_BOOL = "true"), {
    expect_true(get_env_typed("TEST_BOOL", type = "logical"))
  })
  withr::with_envvar(c(TEST_BOOL = "false"), {
    expect_false(get_env_typed("TEST_BOOL", type = "logical"))
  })
  withr::with_envvar(c(TEST_BOOL = "1"), {
    expect_true(get_env_typed("TEST_BOOL", type = "logical"))
  })
  withr::with_envvar(c(TEST_BOOL = "yes"), {
    expect_true(get_env_typed("TEST_BOOL", type = "logical"))
  })
})

test_that("get_env_typed parses numeric values correctly", {
  withr::with_envvar(c(TEST_NUM = "123"), {
    expect_equal(get_env_typed("TEST_NUM", type = "numeric"), 123)
  })
  withr::with_envvar(c(TEST_NUM = "45.67"), {
    expect_equal(get_env_typed("TEST_NUM", type = "numeric"), 45.67)
  })
})

test_that("load_throttle_config uses YAML defaults when no env vars set", {
  withr::with_envvar(c(THROTTLE_ENABLED = NA, THROTTLE_MIN_BUFFER_MB = NA), {
    yaml_config <- list(enabled = TRUE, min_system_buffer_mb = 512)
    config <- load_throttle_config(yaml_config)
    expect_true(config$enabled)
    expect_equal(config$min_system_buffer_mb, 512)
  })
})

test_that("THROTTLE_ENABLED=false overrides YAML", {
  withr::with_envvar(c(THROTTLE_ENABLED = "false"), {
    yaml_config <- list(enabled = TRUE)
    config <- load_throttle_config(yaml_config)
    expect_false(config$enabled)
  })
})

test_that("THROTTLE_MIN_BUFFER_MB overrides YAML", {
  withr::with_envvar(c(THROTTLE_MIN_BUFFER_MB = "1024"), {
    yaml_config <- list(min_system_buffer_mb = 512)
    config <- load_throttle_config(yaml_config)
    expect_equal(config$min_system_buffer_mb, 1024)
  })
})

test_that("per-endpoint env vars override YAML", {
  withr::with_envvar(c(
    THROTTLE_MALARIA_BASE_MB = "5000",
    THROTTLE_MALARIA_MULTIPLIER = "100"
  ), {
    yaml_config <- list(
      endpoints = list(
        malaria = list(base_mb = 2000, multiplier = 40)
      )
    )
    config <- load_throttle_config(yaml_config)
    expect_equal(config$endpoints$malaria$base_mb, 5000)
    expect_equal(config$endpoints$malaria$multiplier, 100)
  })
})

test_that("load_throttle_config handles empty config", {
  config <- load_throttle_config(list())
  expect_true(config$enabled)
  expect_equal(config$min_system_buffer_mb, 512)
})

test_that("get_endpoint_config returns configured endpoint", {
  config <- list(
    endpoints = list(
      temperature = list(base_mb = 800, multiplier = 25, arg_type = "filepath")
    ),
    default_base_mb = 500,
    default_multiplier = 20
  )
  ep_config <- get_endpoint_config(config, "temperature")
  expect_equal(ep_config$base_mb, 800)
  expect_equal(ep_config$multiplier, 25)
  expect_equal(ep_config$arg_type, "filepath")
})

test_that("get_endpoint_config returns defaults for unknown endpoints", {
  config <- list(
    endpoints = list(),
    default_base_mb = 500,
    default_multiplier = 20
  )
  ep_config <- get_endpoint_config(config, "/unknown_endpoint")
  expect_equal(ep_config$base_mb, 500)
  expect_equal(ep_config$multiplier, 20)
})

test_that("get_endpoint_config normalizes endpoint name (removes leading slash)", {
  config <- list(
    endpoints = list(
      malaria = list(base_mb = 2000, multiplier = 40)
    )
  )
  expect_equal(get_endpoint_config(config, "/malaria")$base_mb, 2000)
  expect_equal(get_endpoint_config(config, "malaria")$base_mb, 2000)
})
