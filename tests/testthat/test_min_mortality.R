library(testthat)
library(climatehealth)
library(config)
library(zeallot)

context("Test errors for incorrect inputs")
test_that('Test min_mortality() produces appropriate errors', {

  # df_list not a list
  expect_error(
    calculate_min_mortality_temp(
      df_list = data.frame(x = c(1, 2, 3), y = c(1, 2, 3)),
      blup = list(1, 2)
      ),
    regexp = "Argument 'df_list' must be a list of data frames",
    fixed = TRUE)

  # blup not a list
  expect_error(
    calculate_min_mortality_temp(
      df_list = list(a <- data.frame(x = c(1, 2, 3), y = c(1, 2, 3)),
                     b <- data.frame(x = c(4, 5, 6), y = c(4, 5, 6))),
      blup = c(1, 2)
      ),
    regexp = "Argument 'blup' must be a list",
    fixed = TRUE)

})

context("Test output data types")
test_that('Test min_mortality() returns correct data types', {

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

  if (config$meta_analysis == TRUE) {

    c(coef_, vcov_) %<-%
      run_model(df_list = df_list_,
                independent_col1 = config$independent_col1,
                independent_col2 = config$independent_col2,
                independent_col3 = config$independent_col3,
                varfun = config$varfun,
                varper = varper_,
                vardegree = config$vardegree,
                lag = config$lag,
                lagnk = config$lagnk,
                dfseas = config$dfseas
      )

    c(mv_, blup_) %<-%
      run_meta_model(
        df_list = df_list_,
        coef = coef_,
        vcov = vcov_
      )

    c(avgtmean_wald, rangetmean_wald) %<-%
      wald_results(
        mv = mv_
      )

  } else {

    blup_ <- NULL

  }

  c(mintempregions_) %<-%
    calculate_min_mortality_temp(
      df_list = df_list_,
      blup = blup_,
      independent_col1 = config$independent_col1,
      independent_col2 = config$independent_col2,
      independent_col3 = config$independent_col3,
      varfun = config$varfun,
      varper = varper_,
      vardegree = config$vardegree,
      lag = config$lag,
      lagnk = config$lagnk,
      dfseas = config$dfseas
    )

  # mintempcity
  expect_equal(typeof(mintempregions_), "double")
  expect_equal(is.vector(mintempregions_), TRUE)
  expect_equal(is.numeric(mintempregions_), TRUE)

})

