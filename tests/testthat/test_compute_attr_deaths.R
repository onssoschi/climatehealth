library(testthat)
library(indicatorfunctions)

test_that('Test total deaths is an integer of correct length', {

  c(df_list_unordered_, regions_) %<-%
    load_data(
      input_path =  'testdata/regEngWales.csv'
    )

  c(regions_df_, df_list_) %<-%
    get_region_metadata(
      regions = regions_,
      df_list_unordered = df_list_unordered_,
      region_names = c("North East","North West",
                       "Yorkshire & Humber","East Midlands",
                       "West Midlands","East","London",
                       "South East","South West", "Wales")
    )

  c(coef_, vcov_) %<-%
    run_model(df_list = df_list_,
              regions_df = regions_df_)

  c(mv_, blup_) %<-%
    run_meta_model(
      df_list = df_list_,
      regions_df = regions_df_,
      coef = coef_,
      vcov = vcov_
    )

  c(mintempregions_) %<-%
    calculate_min_mortality_temp(
      df_list = df_list_,
      regions_df = regions_df_,
      blup = blup_
    )

  c(totdeath_, arraysim_, matsim_) %<-%
    compute_attributable_deaths(
      df_list = df_list_,
      regions_df = regions_df_,
      blup = blup_,
      mintempregions = mintempregions_
    )

  expected_output <- rep(5L, nrow(regions_df_))

  expect_equal(typeof(totdeath_), typeof(expected_output))
  expect_equal(length(totdeath_), length(expected_output))

})
