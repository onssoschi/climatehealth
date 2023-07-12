library(testthat)
library(indicatorfunctions)

test_that('Test output CSVs are written and of correct length', {

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

  c(antot, totdeathtot, aftot, afregions) %<-%
    write_attributable_deaths(
      df_list = df_list_,
      regions_df = regions_df_,
      matsim = matsim_,
      arraysim = arraysim_,
      totdeath = totdeath_,
      output_folder_path = 'testdata/'
    )

  actual_output <- read.csv('testdata/attributable_deaths_regions.csv')

  expected_output <- data.frame(matrix(NA, nrow = 1, ncol = length(regions_) + 1))

  expect_equal(typeof(actual_output), typeof(expected_output))
  expect_equal(length(actual_output), length(expected_output))

})
