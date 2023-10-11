library(testthat)
library(climatehealth)
library(config)

test_that('Test output CSVs are written and of correct length', {

  config <- config::get()

  varper_ <- c(10, 75, 90)

  by_region <- TRUE

  c(df_list_) %<-%
    load_data(
      input_csv_path = config$input_csv_path,
      dependent_col = config$dependent_col,
      time_col = config$time_col,
      region_col = config$region_col,
      temp_col = config$temp_col,
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
  if (by_region == FALSE) {

    c(output_df, tmean_df) %<-%
      plot_and_write_relative_risk_all(
        df_list = df_list_,
        mintempregions = mintempregions_,
        save_fig = config$save_fig,
        save_csv = config$save_csv,
        dependent_col = config$dependent_col,
        varfun = config$varfun,
        varper = varper_,
        vardegree = config$vardegree,
        coef = coef_,
        vcov = vcov_,
        output_folder_path = config$output_folder_path
      )

  } else {

    c(output_df, tmean_df) %<-%
      plot_and_write_relative_risk(
        df_list = df_list_,
        blup = blup_,
        mintempregions = mintempregions_,
        save_fig = config$save_fig,
        save_csv = config$save_csv,
        independent_col1 = config$independent_col1,
        independent_col2 = config$independent_col2,
        independent_col3 = config$independent_col3,
        varfun = config$varfun,
        varper = varper_,
        vardegree = config$vardegree,
        lag = config$lag,
        lagnk = config$lagnk,
        dfseas = config$dfseas,
        output_folder_path = config$output_folder_path
      )

  }

  actual_output <- read.csv('testdata/attributable_deaths_regions.csv')

  expected_output <- data.frame(matrix(NA, nrow = 1, ncol = length(names(df_list_)) + 1))

  expect_equal(typeof(actual_output), typeof(expected_output))
  expect_equal(length(actual_output), length(expected_output))

})
