library(testthat)
library(indicatorfunctions)
library(zeallot)

context("Test errors for incorrect inputs")
test_that('Test that fwald() arguments are correct type', {

  expect_error(fwald(mv, 1),
               "Argument 'var' must be a character", fixed = TRUE)

})

context("Test output data types")
test_that('Test fwald() returns correct data type', {

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

  expect_equal(typeof(fwald(mv_, "avgtmean")), "double")
  expect_equal(is.numeric(fwald(mv_, "avgtmean")), TRUE)

})

context("Test output within expected range")
test_that('Test fwald() returns p-value between 0 and 1', {

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

  expect_lte(fwald(mv_, "avgtmean"), 1)
  expect_gte(fwald(mv_, "avgtmean"), 0)

})
