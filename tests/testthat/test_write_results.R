library(testthat)
library(indicatorfunctions)

test_that('Test output CSVs are written and of correct length', {

  c(dlist, argvar, regions, cities, coef, vcov) %<-%
    prep_and_first_step('testdata/regEngWales.csv')

  c(blup, argvar, bvarr, mintempcity) %<-%
    second_stage(dlist, cities, argvar, coef, vcov)

  c(totdeath, arraysim, matsim) %<-%
    compute_attributable_deaths(dlist, cities,
                                coef, vcov,
                                varfun, argvar,
                                bvar, blup,
                                mintempcity)

  write_outputs_to_csv(cities, matsim, arraysim,
                       totdeath, output_folder_path = 'testdata/')

  actual_output <- read.csv('testdata/attributable_deaths_city.csv')

  expected_output <- data.frame(matrix(NA, nrow = 1, ncol = length(regions) + 1))

  expect_equal(typeof(actual_output), typeof(expected_output))
  expect_equal(length(actual_output), length(expected_output))

})
