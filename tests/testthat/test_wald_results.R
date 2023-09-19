library(testthat)
library(climatehealth)
library(config)
library(zeallot)

context("Test output data types")
test_that('Test wald_results() returns correct data type', {

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

  }

  expect_equal(typeof(wald_results(mv_)), "list")
  expect_equal(typeof(wald_results(mv_)[[1]]), "double")
  expect_equal(typeof(wald_results(mv_)[[2]]), "double")
  expect_equal(is.numeric(wald_results(mv_)[[1]]), TRUE)
  expect_equal(is.numeric(wald_results(mv_)[[2]]), TRUE)

})

context("Test output length")
test_that('Test wald_results() returns list of correct length', {

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

  }

  expect_equal(length(wald_results(mv_)), 2)
  expect_equal(length(wald_results(mv_)[1]), 1)
  expect_equal(length(wald_results(mv_)[2]), 1)

})

context("Test output range")
test_that('Test wald_results() returns p-values between 0 and 1', {

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

  }

  expect_lte(wald_results(mv_)[[1]], 1)
  expect_gte(wald_results(mv_)[[1]], 0)

  expect_lte(wald_results(mv_)[[2]], 1)
  expect_gte(wald_results(mv_)[[2]], 0)

})

