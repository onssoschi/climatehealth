library(testthat)
library(indicatorfunctions)

test_that('Test new output matches original output', {

  c(dlist, argvar, regions, cities, coef, vcov) %<-%
    prep_and_first_step('testdata/regEngWales.csv')

  c(totdeath, arraysim, matsim) %<-%
    compute_attributable_deaths(dlist, cities,
                                coef, vcov,
                                varfun, argvar,
                                bvar, blup,
                                mintempcity)

  expected_output <- read.csv('testdata/output_one_region_data_original.csv')

  actual_output <- read.csv('testdata/output_one_region_data_new.csv')

  expect_equal(actual_output, expected_output)

})
