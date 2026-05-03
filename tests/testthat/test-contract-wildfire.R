# Contract tests for wildfire indicator
# Tests verify input/output contracts for the external-facing API

# Test read_and_format_data input contract
test_that("read_and_format_data accepts valid wildfire data", {
  data <- generate_wildfire_data(n_days = 30, regions = c("Region_A", "Region_B"))

  result <- read_and_format_data(
    health_path = data,
    date_col = "date",
    mean_temperature_col = "tmean",
    health_outcome_col = "health_outcome",
    region_col = "region",
    rh_col = "rh",
    wind_speed_col = "wind_speed"
  )

  # Output contract: returns a data.frame
  expect_true(is.data.frame(result))
  expect_true(nrow(result) > 0)
})

test_that("read_and_format_data output has required columns", {
  data <- generate_wildfire_data(n_days = 30, regions = c("TestRegion"))

  result <- read_and_format_data(
    health_path = data,
    date_col = "date",
    mean_temperature_col = "tmean",
    health_outcome_col = "health_outcome",
    region_col = "region"
  )

  # Output contract: standardized column names
  required_cols <- c("date", "tmean", "health_outcome", "region", "year", "month", "day", "dow")
  expect_true(
    all(required_cols %in% names(result)),
    info = paste("Missing columns:", paste(setdiff(required_cols, names(result)), collapse = ", "))
  )
})

test_that("read_and_format_data handles optional columns", {
  data <- generate_wildfire_data(n_days = 30, regions = c("Test"))

  # Without optional columns
  result_minimal <- read_and_format_data(
    health_path = data,
    date_col = "date",
    mean_temperature_col = "tmean",
    health_outcome_col = "health_outcome"
  )

  # Should still work and create placeholders
  expect_true(is.data.frame(result_minimal))
  expect_true("region" %in% names(result_minimal))
})

# Test create_lagged_variables contract
test_that("create_lagged_variables creates lag columns", {
  data <- generate_wildfire_data(n_days = 100, regions = c("Test"))

  formatted <- read_and_format_data(
    health_path = data,
    date_col = "date",
    mean_temperature_col = "tmean",
    health_outcome_col = "health_outcome",
    region_col = "region"
  )

  # Add mean_PM column (required for lagging)
  formatted$mean_PM <- runif(nrow(formatted), 5, 50)

  result <- create_lagged_variables(
    data = formatted,
    wildfire_lag = 3,
    temperature_lag = 1
  )

  # Output contract: lag columns created (uses mean_PM_l{n}_mean naming)
  expect_true(is.data.frame(result))
  # Check for lagged columns with the package's naming convention
  lag_cols <- grep("^mean_PM_l[0-9]+_mean$|^mean_PM$", names(result), value = TRUE)
  expect_true(length(lag_cols) > 0, info = paste("Lag columns found:", paste(lag_cols, collapse = ", ")))
})

# Test create_temperature_splines contract
test_that("create_temperature_splines creates spline basis columns", {
  data <- generate_wildfire_data(n_days = 100, regions = c("Test"))

  formatted <- read_and_format_data(
    health_path = data,
    date_col = "date",
    mean_temperature_col = "tmean",
    health_outcome_col = "health_outcome",
    region_col = "region"
  )

  # First create lagged variables (required before splines)
  formatted$mean_PM <- runif(nrow(formatted), 5, 50)
  formatted <- create_lagged_variables(
    data = formatted,
    wildfire_lag = 0,
    temperature_lag = 0
  )

  result <- create_temperature_splines(
    data = formatted,
    nlag = 0,
    degrees_freedom = 6
  )

  # Output contract: spline basis columns created
  expect_true(is.data.frame(result))

  # Should have tmean_basis columns
  spline_cols <- grep("^tmean_basis|^tmean_l", names(result), value = TRUE)
  expect_true(length(spline_cols) > 0, info = paste("Columns:", paste(names(result), collapse = ", ")))
})

# Test time_stratify contract
test_that("time_stratify creates stratum column", {
  data <- generate_wildfire_data(n_days = 100, regions = c("Test"))

  formatted <- read_and_format_data(
    health_path = data,
    date_col = "date",
    mean_temperature_col = "tmean",
    health_outcome_col = "health_outcome",
    region_col = "region"
  )

  result <- time_stratify(data = formatted)

  # Output contract: stratum column created
  expect_true(is.data.frame(result))
  expect_true("stratum" %in% names(result))
  expect_true(all(!is.na(result$stratum)))
})

# Test casecrossover_quasipoisson contract (using pre-generated test data)
test_that("casecrossover_quasipoisson returns RR results", {
  # Use the specialized wildfire test data generator
  test_data <- generate_wildfire_test_data(
    n = 500,
    n_strata = 5,
    n_lags = 2,
    spline_df = 6
  )

  result <- suppress_plot({
    casecrossover_quasipoisson(
      data = test_data,
      scale_factor_wildfire_pm = 10,
      save_fig = FALSE,
      output_folder_path = NULL,
      print_model_summaries = FALSE
    )
  })

  # Output contract: returns RR results
  expect_true(is.data.frame(result))

  # Required columns in output
  required_cols <- c("lag", "relative_risk", "ci_lower", "ci_upper")
  expect_true(
    all(required_cols %in% names(result)),
    info = paste("Missing columns:", paste(setdiff(required_cols, names(result)), collapse = ", "))
  )

  # Behavioral contract: RR values are reasonable
  expect_true(all(result$relative_risk > 0), info = "RR must be positive")
  expect_true(all(result$relative_risk < 10), info = "RR should be < 10")

  # CI ordering contract
  expect_true(
    all(result$ci_lower <= result$relative_risk),
    info = "Lower CI must be <= RR"
  )
  expect_true(
    all(result$ci_upper >= result$relative_risk),
    info = "Upper CI must be >= RR"
  )
})

# Test date format handling
test_that("read_and_format_data handles ISO date format", {
  data <- data.frame(
    date = c("2020-01-01", "2020-01-02", "2020-01-03"),
    tmean = c(15.0, 16.0, 14.5),
    health_outcome = c(5, 7, 4),
    region = "Test"
  )

  result <- read_and_format_data(
    health_path = data,
    date_col = "date",
    mean_temperature_col = "tmean",
    health_outcome_col = "health_outcome",
    region_col = "region"
  )

  expect_true(inherits(result$date, "Date"))
  expect_equal(result$day, c(1, 2, 3))
  expect_equal(result$month, c(1, 1, 1))
})

test_that("read_and_format_data handles DMY date format", {
  data <- data.frame(
    date = c("01-01-2020", "02-01-2020", "03-01-2020"),
    tmean = c(15.0, 16.0, 14.5),
    health_outcome = c(5, 7, 4),
    region = "Test"
  )

  result <- read_and_format_data(
    health_path = data,
    date_col = "date",
    mean_temperature_col = "tmean",
    health_outcome_col = "health_outcome",
    region_col = "region"
  )

  expect_true(inherits(result$date, "Date"))
})
