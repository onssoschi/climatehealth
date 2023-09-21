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

  c(df_list_) %<-%
    load_data(
      input_csv_path = config$input_csv_path,
      dependent_col = config$dependent_col,
      time_col = config$time_col,
      region_col = config$region_col,
      temp_col = config$temp_col,
      time_range = config$time_range
    )

  if (config$meta_analysis == TRUE) {

    c(coef_, vcov_) %<-%
      run_model(df_list = df_list_,
                independent_col = config$independent_col,
                varfun = config$varfun,
                varper = config$varper,
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
      independent_col = config$independent_col,
      varfun = config$varfun,
      varper = config$varper,
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

