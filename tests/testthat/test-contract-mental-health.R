# Contract tests for mental health / suicides indicator
# Tests verify input/output contracts for the external-facing API

# Test mh_read_and_format_data input contract
test_that("mh_read_and_format_data accepts valid mental health data", {
  data <- generate_mental_health_data(n_days = 30, regions = c("National"))

  result <- mh_read_and_format_data(
    data_path = data,
    date_col = "date",
    region_col = "region",
    temperature_col = "temp",
    health_outcome_col = "suicides",
    population_col = "population"
  )

  # Output contract: returns a list
  expect_true(is.list(result))
  expect_true(length(result) > 0)
})

test_that("mh_read_and_format_data output has required columns", {
  data <- generate_mental_health_data(n_days = 30, regions = c("TestRegion"))

  result <- mh_read_and_format_data(
    data_path = data,
    date_col = "date",
    region_col = "region",
    temperature_col = "temp",
    health_outcome_col = "suicides",
    population_col = "population"
  )

  # Output contract: each region has standardized columns
  for (geog in names(result)) {
    df <- result[[geog]]
    expect_true(is.data.frame(df))

    # Required columns after processing
    required_cols <- c("suicides", "temp", "population", "stratum", "ind")
    expect_true(
      all(required_cols %in% names(df)),
      info = paste("Missing columns:", paste(setdiff(required_cols, names(df)), collapse = ", "))
    )
  }
})

test_that("mh_read_and_format_data handles multiple regions", {
  data <- generate_mental_health_data(n_days = 30, regions = c("Region1", "Region2", "Region3"))

  result <- mh_read_and_format_data(
    data_path = data,
    date_col = "date",
    region_col = "region",
    temperature_col = "temp",
    health_outcome_col = "suicides",
    population_col = "population"
  )

  # Output contract: one entry per region
  expect_equal(length(result), 3)
  expect_true(all(c("Region1", "Region2", "Region3") %in% names(result)))
})

# Test mh_create_crossbasis contract
test_that("mh_create_crossbasis produces valid crossbasis objects", {
  data <- generate_mental_health_data(n_days = 100, regions = c("National"))

  df_list <- mh_read_and_format_data(
    data_path = data,
    date_col = "date",
    region_col = "region",
    temperature_col = "temp",
    health_outcome_col = "suicides",
    population_col = "population"
  )

  cb_list <- mh_create_crossbasis(
    df_list = df_list,
    lag_days = 2
  )

  # Output contract: returns a list of crossbasis objects
  expect_true(is.list(cb_list))
  expect_equal(length(cb_list), length(df_list))

  for (cb in cb_list) {
    expect_true(inherits(cb, "crossbasis"))
    expect_true(nrow(cb) > 0)
  }
})

# Test dlnm_pop_totals with mental health data
test_that("dlnm_pop_totals works with mental health data", {
  data <- generate_mental_health_data(n_days = 100, regions = c("Region1", "Region2"))

  df_list <- mh_read_and_format_data(
    data_path = data,
    date_col = "date",
    region_col = "region",
    temperature_col = "temp",
    health_outcome_col = "suicides",
    population_col = "population"
  )

  pop_list <- dlnm_pop_totals(
    df_list = df_list,
    country = "National",
    meta_analysis = TRUE
  )

  # Output contract: returns population data
  expect_true(is.list(pop_list))
  expect_true(length(pop_list) > 0)

  for (pop in pop_list) {
    expect_true(is.data.frame(pop))
    expect_true("population" %in% names(pop))
  }
})

# Test stratum creation contract
test_that("mh_read_and_format_data creates valid strata", {
  data <- generate_mental_health_data(n_days = 100, regions = c("National"))

  result <- mh_read_and_format_data(
    data_path = data,
    date_col = "date",
    region_col = "region",
    temperature_col = "temp",
    health_outcome_col = "suicides",
    population_col = "population"
  )

  df <- result[[1]]

  # Behavioral contract: strata should be valid
  expect_true("stratum" %in% names(df))
  expect_true(all(!is.na(df$stratum)))
  expect_true(length(unique(df$stratum)) > 1)
})

# Test ind column creation contract
test_that("mh_read_and_format_data creates ind column", {
  data <- generate_mental_health_data(n_days = 100, regions = c("National"))

  result <- mh_read_and_format_data(
    data_path = data,
    date_col = "date",
    region_col = "region",
    temperature_col = "temp",
    health_outcome_col = "suicides",
    population_col = "population"
  )

  df <- result[[1]]

  # Output contract: ind column exists
  expect_true("ind" %in% names(df))
  expect_true(is.numeric(df$ind))
})

# Test data preservation contract
test_that("mh_read_and_format_data preserves numeric values", {
  data <- data.frame(
    date = as.Date("2020-01-01") + 0:4,
    suicides = c(5, 7, 4, 6, 5),
    temp = c(20.0, 22.5, 18.0, 21.0, 19.5),
    region = "Test",
    population = 5000000
  )

  result <- mh_read_and_format_data(
    data_path = data,
    date_col = "date",
    region_col = "region",
    temperature_col = "temp",
    health_outcome_col = "suicides",
    population_col = "population"
  )

  df <- result[[1]]

  # Behavioral contract: values preserved
  expect_equal(df$suicides, c(5, 7, 4, 6, 5))
  expect_equal(df$temp, c(20.0, 22.5, 18.0, 21.0, 19.5))
  expect_equal(unique(df$population), 5000000)
})

# Test column rename handling (tidyselect compliance)
test_that("mh_read_and_format_data handles column renaming correctly", {
  # Use non-standard column names
  data <- data.frame(
    my_date = as.Date("2020-01-01") + 0:9,
    my_outcome = rpois(10, 5),
    my_temp = rnorm(10, 20, 5),
    my_region = "TestRegion",
    my_pop = 1000000
  )

  result <- mh_read_and_format_data(
    data_path = data,
    date_col = "my_date",
    region_col = "my_region",
    temperature_col = "my_temp",
    health_outcome_col = "my_outcome",
    population_col = "my_pop"
  )

  expect_true(is.list(result))
  expect_true(length(result) == 1)

  df <- result[[1]]
  expect_true("suicides" %in% names(df))
  expect_true("temp" %in% names(df))
  expect_true("population" %in% names(df))
})
