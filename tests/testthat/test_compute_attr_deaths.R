library(testthat)
library(climatehealth)

test_that('Test total deaths is an integer of correct length', {

  config <- config::get()

  c(df_list_unordered_, regions_) %<-%
    load_data(
      input_csv_path = config$input_csv_path,
      dependent_col = config$dependent_col,
      time_col = config$time_col,
      region_col = config$region_col,
      temp_col = config$temp_col,
      time_range = config$time_range
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
      independent_col = config$independent_col,
      varfun = config$varfun,
      varper = config$varper,
      vardegree = config$vardegree,
      lag = config$lag,
      lagnk = config$lagnk,
      dfseas = config$dfseas
    )

  c(totdeath_, arraysim_, matsim_, attrdl_yr_all_) %<-%
    compute_attributable_deaths(
      df_list = df_list_,
      regions_df = regions_df_,
      blup = blup_,
      mintempregions = mintempregions_,
      independent_col = config$independent_col,
      varfun = config$varfun,
      varper = config$varper,
      vardegree = config$vardegree,
      lag = config$lag,
      lagnk = config$lagnk,
      dfseas = config$dfseas
    )

  expected_output <- rep(5L, nrow(regions_df_))

  expect_equal(typeof(totdeath_), typeof(expected_output))
  expect_equal(length(totdeath_), length(expected_output))

})
