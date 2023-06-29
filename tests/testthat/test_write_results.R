library(testthat)
library(indicatorfunctions)

test_that('Test output CSVs are written and of correct length', {

  c(df_list_unordered, regions) %<-% load_data(input_path = 'testdata/regEngWales.csv')

  c(regions_df, df_list) %<-% get_region_metadata(regions = regions,
                                                  df_list_unordered = df_list_unordered)

  c(argvar, coef, vcov) %<-% run_model(df_list = df_list, regions_df = regions_df)

  c(mv, blup) %<-% run_meta_model(df_list = df_list, regions_df = regions_df, coef = coef,
                                  vcov = vcov)

  c(avgtmean_wald, rangetmean_wald) %<-% wald_results(mv = mv)

  c(argvar, bvar, mintempregions) %<-%
    calculate_min_mortality_temp(df_list = df_list, regions_df = regions_df, blup = blup)

  c(totdeath, arraysim, matsim) %<-%
    compute_attributable_deaths(df_list = df_list, regions_df = regions_df, coef = coef,
                                vcov = vcov, blup = blup,
                                mintempregions = mintempregions)

  c(antot, totdeathtot, aftot, afregions) %<-%
    write_attributable_deaths(regions_df = regions_df, matsim = matsim, arraysim = arraysim,
                         totdeath = totdeath,
                         output_folder_path = 'testdata/')

  actual_output <- read.csv('testdata/attributable_deaths_regions.csv')

  expected_output <- data.frame(matrix(NA, nrow = 1, ncol = length(regions) + 1))

  expect_equal(typeof(actual_output), typeof(expected_output))
  expect_equal(length(actual_output), length(expected_output))

})
