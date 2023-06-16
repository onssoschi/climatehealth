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

  c(dlist, argvar, regions, cities, coef, vcov) %<-%
    prep_and_first_step("testdata/regEngWales.csv")

  c(mv, blup) %<-% run_meta_model(dlist = dlist, cities = cities, coef = coef,
                                  vcov = vcov)

  expect_equal(typeof(fwald(mv, "avgtmean")), "double")
  expect_equal(is.numeric(fwald(mv, "avgtmean")), TRUE)

})

context("Test output within expected range")
test_that('Test fwald() returns p-value between 0 and 1', {

  c(dlist, argvar, regions, cities, coef, vcov) %<-%
    prep_and_first_step("testdata/regEngWales.csv")

  c(mv, blup) %<-% run_meta_model(dlist = dlist, cities = cities, coef = coef,
                                  vcov = vcov)

  expect_lte(fwald(mv, "avgtmean"), 1)
  expect_gte(fwald(mv, "avgtmean"), 0)

})
