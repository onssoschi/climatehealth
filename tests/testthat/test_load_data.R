library(testthat)
library(climatehealth)
library(config)

test_that('Test data loads correctly', {

  config <- config::get()

  varper_ <- c(10, 75, 90)

  c(df_list_) %<-%
    load_data(
      input_csv_path = config$input_csv_path,
      dependent_col = config$dependent_col,
      time_col = config$time_col,
      region_col = config$region_col,
      temp_col = config$temp_col,
      population_col = config$population_col,
      time_range_start = config$time_range_start,
      time_range_end = config$time_range_end
    )

  test_list <- list("a", "b", "c")

  expect_equal(typeof(test_list), typeof(df_list_))

  df <- read.csv(config$input_csv_path, row.names = 1)

  df <- unique(df$regnames)

  test_list <- vector(mode = "list", length = length(df))

  expect_equal(length(test_list), length(df_list_))

})

