library(testthat)
library(indicatorfunctions)
library(zeallot)

source("./R/gasparrini_functions.R")

context("Test output data types")
test_that('Test wald_results() returns correct data type', {

  c(dlist, argvar, regions, cities, coef, vcov) %<-%
    prep_and_first_step(input_csv_path)

  c(mv, blup) %<-% run_meta_model(dlist = dlist, cities = cities, coef = coef,
                                  vcov = vcov)

  expect_equal(typeof(wald_results(mv)), "list")
  expect_equal(typeof(wald_results(mv)[[1]]), "double")
  expect_equal(typeof(wald_results(mv)[[2]]), "double")

})

context("Test output length")
test_that('Test wald_results() returns list of correct length', {

  c(dlist, argvar, regions, cities, coef, vcov) %<-%
    prep_and_first_step(input_csv_path)

  c(mv, blup) %<-% run_meta_model(dlist = dlist, cities = cities, coef = coef,
                                  vcov = vcov)

  expect_equal(length(wald_results(mv)), 2)
})

context("Test output range")
test_that('Test wald_results() returns p-values between 0 and 1', {

  c(dlist, argvar, regions, cities, coef, vcov) %<-%
    prep_and_first_step(input_csv_path)

  c(mv, blup) %<-% run_meta_model(dlist = dlist, cities = cities, coef = coef,
                                  vcov = vcov)

  expect_lte(wald_results(mv)[[1]], 1)
  expect_gte(wald_results(mv)[[1]], 0)

  expect_lte(wald_results(mv)[[2]], 1)
  expect_gte(wald_results(mv)[[2]], 0)
})

