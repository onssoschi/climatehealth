library(testthat)
library(indicatorfunctions)
library(zeallot)

context("Test output data types")
test_that('Test wald_results() returns correct data type', {

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

  c(avgtmean_wald, rangetmean_wald) %<-%
    wald_results(
      mv = mv_
      )

  expect_equal(typeof(wald_results(mv_)), "list")
  expect_equal(typeof(wald_results(mv_)[[1]]), "double")
  expect_equal(typeof(wald_results(mv_)[[2]]), "double")
  expect_equal(is.numeric(wald_results(mv_)[[1]]), TRUE)
  expect_equal(is.numeric(wald_results(mv_)[[2]]), TRUE)

})

context("Test output length")
test_that('Test wald_results() returns list of correct length', {

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

  c(avgtmean_wald, rangetmean_wald) %<-%
    wald_results(
      mv = mv_
    )

  expect_equal(length(wald_results(mv_)), 2)
  expect_equal(length(wald_results(mv_)[1]), 1)
  expect_equal(length(wald_results(mv_)[2]), 1)

})

context("Test output range")
test_that('Test wald_results() returns p-values between 0 and 1', {

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

  c(avgtmean_wald, rangetmean_wald) %<-%
    wald_results(
      mv = mv_
    )


  expect_lte(wald_results(mv_)[[1]], 1)
  expect_gte(wald_results(mv_)[[1]], 0)

  expect_lte(wald_results(mv_)[[2]], 1)
  expect_gte(wald_results(mv_)[[2]], 0)

})

