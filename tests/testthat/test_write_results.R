library(testthat)
library(climatehealth)
library(config)

test_that('Test output CSVs are written and of correct length', {

  config <- config::get()

  varper_ <- c(10, 75, 90)

  by_region <- TRUE

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

  } else {

    blup_ <- NULL
    avgtmean_wald_ <- NULL
    rangetmean_wald_ <- NULL

  }

  c(mintempregions_, an_thresholds_) %<-%
    calculate_min_mortality_temp(
      df_list = df_list_,
      blup = blup_,
      independent_cols = config$independent_cols,
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
      independent_cols = config$independent_cols,
      varfun = config$varfun,
      varper = varper_,
      vardegree = config$vardegree,
      lag = config$lag,
      lagnk = config$lagnk,
      dfseas = config$dfseas
    )

  c(anregions_bind_,antot_bind_, arregions_bind_, artot_bind_) %<-%
    compute_attributable_rates(df_list = df_list_,
                               output_year = config$output_year,
                               matsim = matsim_,
                               arraysim = arraysim_)

  c(wald_publication_, anregions_publication_, antot_bind_, arregions_publication_, artot_bind_) %<-%
    write_attributable_deaths(
      avgtmean_wald = avgtmean_wald_,
      rangetmean_wald = rangetmean_wald_,
      anregions_bind = anregions_bind_,
      antot_bind = antot_bind_,
      arregions_bind = arregions_bind_,
      artot_bind = artot_bind_,
      output_folder_path = config$output_folder_path
    )

  if (by_region == FALSE) {

    c(output_df, tmean_df) %<-%
      plot_and_write(
        output_name = "output_all_regions",
        output_all = TRUE,
        df_list = df_list_,
        mintempregions = mintempregions_,
        save_fig = config$save_fig,
        save_csv = config$save_csv,
        cb = cb_,
        model = model_,
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
      plot_and_write(
        output_name = "output_all",
        output_all = FALSE,
        df_list = df_list_,
        blup = blup_,
        mintempregions = mintempregions_,
        an_thresholds = an_thresholds_,
        save_fig = config$save_fig,
        save_csv = config$save_csv,
        independent_cols = config$independent_cols,
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
  expected_output <- data.frame(matrix(NA, nrow = length(names(df_list_))), ncol = 1)

  print(expected_output)
  expect_equal(typeof(actual_output), typeof(expected_output))
  expect_equal(nrow(actual_output), nrow(expected_output))

})
