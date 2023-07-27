library(testthat)
library(climatehealth)
library(config)

test_that('Test output CSVs are written and of correct length', {

  config <- config::get()

  c(df_list_unordered_, regions_) %<-%
    load_data(
      input_csv_path = config$input_csv_path,
      dependent_col = config$dependent_col,
      time_col = config$time_col,
      region_col = config$region_col,
      temp_col = config$temp_col
    )

  c(regions_df_, df_list_) %<-%
    get_region_metadata(
      regions = regions_,
      df_list_unordered = df_list_unordered_,
      region_names = NULL
    )

  if (config$meta_analysis == TRUE) {

    c(coef_, vcov_) %<-%
      run_model(df_list = df_list_,
                regions_df = regions_df_,
                dependent_col = config$dependent_col,
                independent_col = config$independent_col,
                varfun = config$varfun,
                varper = config$varper,
                vardegree = config$vardegree,
                lag = config$lag,
                lagnk = config$lagnk)

    c(mv_, blup_) %<-%
      run_meta_model(
        df_list = df_list_,
        regions_df = regions_df_,
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
      regions_df = regions_df_,
      blup = blup_,
      varfun = config$varfun,
      varper = config$varper,
      vardegree = config$vardegree,
      lag = config$lag,
      lagnk = config$lagnk
    )

  c(totdeath_, arraysim_, matsim_) %<-%
    compute_attributable_deaths(
      df_list = df_list_,
      regions_df = regions_df_,
      blup = blup_,
      mintempregions = mintempregions_,
      dependent_col = config$dependent_col,
      independent_col = config$independent_col,
      varfun = config$varfun,
      varper = config$varper,
      vardegree = config$vardegree,
      lag = config$lag,
      lagnk = config$lagnk
    )

  c(antot, totdeathtot, aftot, afregions) %<-%
    write_attributable_deaths(
      df_list = df_list_,
      regions_df = regions_df_,
      matsim = matsim_,
      arraysim = arraysim_,
      totdeath = totdeath_,
      output_folder_path = config$output_folder_path
    )

  actual_output <- read.csv('testdata/attributable_deaths_regions.csv')

  expected_output <- data.frame(matrix(NA, nrow = 1, ncol = length(regions_) + 1))

  expect_equal(typeof(actual_output), typeof(expected_output))
  expect_equal(length(actual_output), length(expected_output))

})
