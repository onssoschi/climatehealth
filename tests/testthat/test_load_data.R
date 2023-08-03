library(testthat)
library(climatehealth)
library(config)

test_that('Test data loads correctly', {

  config <- config::get()

  c(df_list_unordered_, regions_) %<-%
    load_data(
      input_csv_path = config$input_csv_path,
      dependent_col = config$dependent_col,
      time_col = config$time_col,
      region_col = config$region_col,
      temp_col = config$temp_col
    )

  expect_that(regions_, is_a("character"))
  expect_equal(length(regions_), length(df_list_unordered_))

})
