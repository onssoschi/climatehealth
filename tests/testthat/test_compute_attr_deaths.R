library(testthat)
library(indicatorfunctions)

test_that('Test total deaths is an integer of correct length', {

  c(df_list_unordered, regions) %<-% load_data(input_path = 'testdata/regEngWales.csv')

  c(regions_df, df_list) %<-% get_region_metadata(regions = regions,
                                                  df_list_unordered = df_list_unordered)

  c(argvar, coef, vcov) %<-% run_model(df_list = df_list, regions_df = regions_df)

  c(mv, blup) %<-% run_meta_model(df_list = df_list, regions_df = regions_df, coef = coef,
                                  vcov = vcov)

  c(argvar, bvar, mintempregions) %<-%
    calculate_min_mortality_temp(df_list = df_list, regions_df = regions_df, blup = blup)

  c(totdeath, arraysim, matsim) %<-%
    compute_attributable_deaths(df_list = df_list, regions_df = regions_df, coef = coef,
                                vcov = vcov, argvar = argvar,
                                bvar = bvar, blup = blup,
                                mintempregions = mintempregions)

  expected_output <- rep(5L, nrow(regions_df))

  expect_equal(typeof(totdeath), typeof(expected_output))
  expect_equal(length(totdeath), length(expected_output))

})
