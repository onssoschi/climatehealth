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

  c(df_list_unordered, regions) %<-% load_data(input_path = 'testdata/regEngWales.csv')

  c(regions_df, df_list) %<-% get_region_metadata(regions = regions,
                                                  df_list_unordered = df_list_unordered)

  c(argvar, coef, vcov) %<-% run_model(df_list = df_list, regions_df = regions_df)

  c(mv, blup) %<-% run_meta_model(df_list = df_list, regions_df = regions_df, coef = coef,
                                  vcov = vcov)

  expect_equal(typeof(fwald(mv, "avgtmean")), "double")
  expect_equal(is.numeric(fwald(mv, "avgtmean")), TRUE)

})

context("Test output within expected range")
test_that('Test fwald() returns p-value between 0 and 1', {

  c(df_list_unordered, regions) %<-% load_data(input_path = 'testdata/regEngWales.csv')

  c(regions_df, df_list) %<-% get_region_metadata(regions = regions,
                                                  df_list_unordered = df_list_unordered)

  c(argvar, coef, vcov) %<-% run_model(df_list = df_list, regions_df = regions_df)

  c(mv, blup) %<-% run_meta_model(df_list = df_list, regions_df = regions_df, coef = coef,
                                  vcov = vcov)

  expect_lte(fwald(mv, "avgtmean"), 1)
  expect_gte(fwald(mv, "avgtmean"), 0)

})
