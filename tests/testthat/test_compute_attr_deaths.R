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
      output_year = config$output_year,
      RR_distribution_length = config$RR_distribution_length
    )

  if (config$meta_analysis == TRUE) {

    c(coef_, vcov_) %<-%
      run_model(df_list = df_list_,
                independent_col1 = config$independent_col1,
                independent_col2 = config$independent_col2,
                independent_col3 = config$independent_col3,
                independent_col4 = config$independent_col4,
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

    c(avgtmean_wald_, rangetmean_wald_) %<-%
      wald_results(
        mv = mv_
      )

  } else {

    blup_ <- NULL
    avgtmean_wald_ <- NULL
    rangetmean_wald_ <- NULL

  }

  c(mintempregions_, an_thresholds_) %<-%
    calculate_min_mortality_temp(
      df_list = df_list_,
      blup = blup_,
      independent_col1 = config$independent_col1,
      independent_col2 = config$independent_col2,
      independent_col3 = config$independent_col3,
      independent_col4 = config$independent_col4,
      varfun = config$varfun,
      varper = varper_,
      vardegree = config$vardegree,
      lag = config$lag,
      lagnk = config$lagnk,
      dfseas = config$dfseas
    )

  c(arraysim_, matsim_) %<-%
    compute_attributable_deaths(
      df_list = df_list_,
      output_year = config$output_year,
      blup = blup_,
      mintempregions = mintempregions_,
      an_thresholds = an_thresholds_,
      independent_col1 = config$independent_col1,
      independent_col2 = config$independent_col2,
      independent_col3 = config$independent_col3,
      independent_col4 = config$independent_col4,
      varfun = config$varfun,
      varper = varper_,
      vardegree = config$vardegree,
      lag = config$lag,
      lagnk = config$lagnk,
      dfseas = config$dfseas
    )

  # totdeath
  expected_output <- rep(5L, length(names(df_list_)))

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

})
