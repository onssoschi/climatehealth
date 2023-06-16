library(testthat)
library(indicatorfunctions)

test_that('Test total deaths is an integer of correct length', {

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

  expected_output <- rep(5L, length(regions))

  expect_equal(typeof(totdeath), typeof(expected_output))
  expect_equal(length(totdeath), length(expected_output))

})
