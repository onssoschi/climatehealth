library(testthat)
library(climatehealth)
library(config)

test_that('Test data loads correctly', {

  config <- config::get()

  c(df_list_) %<-%
    load_data(
      input_csv_path = config$input_csv_path,
      dependent_col = config$dependent_col,
      time_col = config$time_col,
      region_col = config$region_col,
      temp_col = config$temp_col,
      time_range = config$time_range,
      region_names = config$region_names
    )

  test_list <- list("a", "b", "c")

  expect_equal(typeof(test_list), typeof(df_list_))

  test_list <- vector(mode = "list", length = length(config$region_names))

  expect_equal(typeof(test_list), typeof(df_list_))

})

test_that('Test regions sorted correctly', {

  config <- config::get()

  c(df_list_) %<-%
    load_data(
      input_csv_path = config$input_csv_path,
      dependent_col = config$dependent_col,
      time_col = config$time_col,
      region_col = config$region_col,
      temp_col = config$temp_col,
      time_range = config$time_range,
      region_names = config$region_names
    )

  config_names_random <- sample(config$region_names)

  config_names_ordered <- sort(config_names_random)

  df_list_names <- names(df_list_)

  expect_equal(df_list_names, config_names_ordered)

})

