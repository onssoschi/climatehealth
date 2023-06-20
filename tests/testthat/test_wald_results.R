library(testthat)
library(indicatorfunctions)
library(zeallot)

context("Test output data types")
test_that('Test wald_results() returns correct data type', {

  c(df_list_unordered, regions) %<-% load_data(input_path = 'testdata/regEngWales.csv')

  c(regions_df, df_list) %<-% get_region_metadata(regions = regions,
                                                  df_list_unordered = df_list_unordered)

  c(argvar, coef, vcov) %<-% run_model(df_list = df_list, regions_df = regions_df)

  c(mv, blup) %<-% run_meta_model(df_list = df_list, regions_df = regions_df, coef = coef,
                                  vcov = vcov)

  expect_equal(typeof(wald_results(mv)), "list")
  expect_equal(typeof(wald_results(mv)[[1]]), "double")
  expect_equal(typeof(wald_results(mv)[[2]]), "double")
  expect_equal(is.numeric(wald_results(mv)[[1]]), TRUE)
  expect_equal(is.numeric(wald_results(mv)[[2]]), TRUE)

})

context("Test output length")
test_that('Test wald_results() returns list of correct length', {

  c(df_list_unordered, regions) %<-% load_data(input_path = 'testdata/regEngWales.csv')

  c(regions_df, df_list) %<-% get_region_metadata(regions = regions,
                                                  df_list_unordered = df_list_unordered)

  c(argvar, coef, vcov) %<-% run_model(df_list = df_list, regions_df = regions_df)

  c(mv, blup) %<-% run_meta_model(df_list = df_list, regions_df = regions_df, coef = coef,
                                  vcov = vcov)

  expect_equal(length(wald_results(mv)), 2)
  expect_equal(length(wald_results(mv)[1]), 1)
  expect_equal(length(wald_results(mv)[2]), 1)

})

context("Test output range")
test_that('Test wald_results() returns p-values between 0 and 1', {

  c(df_list_unordered, regions) %<-% load_data(input_path = 'testdata/regEngWales.csv')

  c(regions_df, df_list) %<-% get_region_metadata(regions = regions,
                                                  df_list_unordered = df_list_unordered)

  c(argvar, coef, vcov) %<-% run_model(df_list = df_list, regions_df = regions_df)

  c(mv, blup) %<-% run_meta_model(df_list = df_list, regions_df = regions_df, coef = coef,
                                  vcov = vcov)


  expect_lte(wald_results(mv)[[1]], 1)
  expect_gte(wald_results(mv)[[1]], 0)

  expect_lte(wald_results(mv)[[2]], 1)
  expect_gte(wald_results(mv)[[2]], 0)

})

