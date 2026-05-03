# Contract tests for temperature mortality indicator
# Tests verify input/output contracts for the external-facing API

# Test hc_read_data input contract
test_that("hc_read_data accepts valid temperature mortality data", {
  data <- generate_temp_mortality_data(n_days = 30, regions = c("North", "South"))

  result <- hc_read_data(
    input_csv_path = data,
    dependent_col = "deaths",
    date_col = "date",
    region_col = "region",
    temperature_col = "tmean",
    population_col = "population"
  )

  # Output contract: returns a list

  expect_true(is.list(result))
  expect_equal(length(result), 2)  # Two regions
})

test_that("hc_read_data output has required columns", {
  data <- generate_temp_mortality_data(n_days = 30, regions = c("TestRegion"))

  result <- hc_read_data(
    input_csv_path = data,
    dependent_col = "deaths",
    date_col = "date",
    region_col = "region",
    temperature_col = "tmean",
    population_col = "population"
  )

  # Output contract: each region has standardized columns
  for (geog in names(result)) {
    df <- result[[geog]]
    expect_true(is.data.frame(df))

    # Required columns after processing
    required_cols <- c("dependent", "date", "region", "temp", "population", "year", "month", "dow")
    expect_true(
      all(required_cols %in% names(df)),
      info = paste("Missing columns:", paste(setdiff(required_cols, names(df)), collapse = ", "))
    )
  }
})

test_that("hc_read_data rejects data with missing required columns", {
  # Data missing temperature column
  incomplete_data <- data.frame(
    date = as.Date("2020-01-01") + 0:9,
    deaths = rpois(10, 100),
    region = "Test",
    population = 100000
  )

  expect_error(
    hc_read_data(
      input_csv_path = incomplete_data,
      dependent_col = "deaths",
      date_col = "date",
      region_col = "region",
      temperature_col = "tmean",  # This column doesn't exist
      population_col = "population"
    )
  )
})

# Test hc_create_crossbasis contract
test_that("hc_create_crossbasis produces valid crossbasis objects", {
  data <- generate_temp_mortality_data(n_days = 100, regions = c("Region1"))

  df_list <- hc_read_data(
    input_csv_path = data,
    dependent_col = "deaths",
    date_col = "date",
    region_col = "region",
    temperature_col = "tmean",
    population_col = "population"
  )

  cb_list <- hc_create_crossbasis(
    df_list = df_list,
    lagn = 21,
    lagnk = 3
  )

  # Output contract: returns a list of crossbasis objects
  expect_true(is.list(cb_list))
  expect_equal(length(cb_list), length(df_list))

  for (cb in cb_list) {
    expect_true(inherits(cb, "crossbasis"))
    expect_true(nrow(cb) > 0)
  }
})

# Test dlnm_pop_totals contract
test_that("dlnm_pop_totals returns population by region", {
  data <- generate_temp_mortality_data(n_days = 100, regions = c("North", "South"))

  df_list <- hc_read_data(
    input_csv_path = data,
    dependent_col = "deaths",
    date_col = "date",
    region_col = "region",
    temperature_col = "tmean",
    population_col = "population"
  )

  pop_list <- dlnm_pop_totals(
    df_list = df_list,
    country = "National",
    meta_analysis = FALSE
  )

  # Output contract: returns a list with population data
  expect_true(is.list(pop_list))
  expect_true(length(pop_list) > 0)

  for (pop in pop_list) {
    expect_true(is.data.frame(pop))
    expect_true("population" %in% names(pop))
    expect_true(all(pop$population > 0))
  }
})

# Test date format handling contract
test_that("hc_read_data handles multiple date formats", {
  # ISO format
  data_iso <- data.frame(
    date = c("2020-01-01", "2020-01-02", "2020-01-03"),
    deaths = c(100, 105, 98),
    tmean = c(5.0, 6.0, 4.5),
    region = "Test",
    population = 500000
  )

  result_iso <- hc_read_data(
    input_csv_path = data_iso,
    dependent_col = "deaths",
    date_col = "date",
    region_col = "region",
    temperature_col = "tmean",
    population_col = "population"
  )

  expect_true(is.list(result_iso))
  expect_true(inherits(result_iso[[1]]$date, "Date"))
})

# Test behavioral contract: data preservation
test_that("hc_read_data preserves numeric values correctly", {
  data <- data.frame(
    date = as.Date("2020-01-01") + 0:4,
    deaths = c(100, 105, 98, 102, 99),
    tmean = c(5.0, 6.0, 4.5, 5.5, 5.2),
    region = "Test",
    population = 500000
  )

  result <- hc_read_data(
    input_csv_path = data,
    dependent_col = "deaths",
    date_col = "date",
    region_col = "region",
    temperature_col = "tmean",
    population_col = "population"
  )

  df <- result[[1]]

  # Behavioral contract: values should be preserved
  expect_equal(df$dependent, c(100, 105, 98, 102, 99))
  expect_equal(df$temp, c(5.0, 6.0, 4.5, 5.5, 5.2))
  expect_equal(unique(df$population), 500000)
})
