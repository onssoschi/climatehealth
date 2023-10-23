library(testthat)
library(climatehealth)
library(config)

test_that('Test compute_attributable_deaths() returns correct data types and
          lengths', {

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

  c(totdeath_, arraysim_, matsim_, attrdl_yr_all,
    attr_fractions_yr) %<-%
    compute_attributable_deaths(
      df_list = df_list_,
      blup = blup_,
      mintempregions = mintempregions_,
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

  # totdeath
  expected_output <- rep(5L, length(names(df_list_)))

  expect_equal(typeof(totdeath_), typeof(expected_output))
  expect_equal(length(totdeath_), length(expected_output))
  expect_equal(is.vector(totdeath_), TRUE)

  # arraysim
  expect_equal(typeof(arraysim_), "double")
  expect_equal(class(arraysim_), "array")
  expect_equal(is.numeric(arraysim_), TRUE)
  expect_equal(is.numeric(arraysim_[1]), TRUE)
  expect_equal(nrow(arraysim_[, , 1]), length(names(df_list_)))

  # matsim
  expect_equal(typeof(matsim_), "double")
  expect_equal(class(matsim_)[1], "matrix")
  expect_equal(is.numeric(matsim_), TRUE)
  expect_equal(is.numeric(matsim_[1]), TRUE)
  expect_equal(nrow(matsim_), length(names(df_list_)))

  # attrdl_yr_all
  expect_equal(is.data.frame(attrdl_yr_all), TRUE)
  expect_equal(length(unique(attrdl_yr_all$region)), length(names(df_list_)))
})
