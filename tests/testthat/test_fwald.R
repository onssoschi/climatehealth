library(testthat)
library(climatehealth)
library(config)
library(zeallot)

context("Test errors for incorrect inputs")
test_that('Test that fwald() arguments are correct type', {

  expect_error(fwald(mv, 1),
               "Argument 'var' must be a character", fixed = TRUE)

})

context("Test output data types")
test_that('Test fwald() returns correct data type', {

  config <- config::get()

  varper_ <- c(10, 75, 90)

  c(df_list_) %<-%
    load_temperature_data(
      input_csv_path = config$input_csv_path,
      dependent_col = config$dependent_col,
      time_col = config$time_col,
      region_col = config$region_col,
      temp_col = config$temp_col,
      population_col = config$population_col,
      output_year = config$output_year,
      RR_distribution_length = config$RR_distribution_length
    )

  c(coef_, vcov_, cb_, model_) %<-%
    run_model(df_list = df_list_,
              independent_cols = config$independent_cols,
              varfun = config$varfun,
              varper = varper_,
              vardegree = config$vardegree,
              lag = config$lag,
              lagnk = config$lagnk,
              dfseas = config$dfseas
    )

  if (config$meta_analysis == TRUE) {

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
  }

  expect_lte(fwald(mv_, "avgtmean"), 1)
  expect_gte(fwald(mv_, "avgtmean"), 0)

})
