library(testthat)
library(indicatorfunctions)
library(zeallot)

context("Test errors for incorrect inputs")
test_that('Test min_mortality() produces appropriate errors', {

  # dlist not a list
  expect_error(min_mortality(
    dlist = data.frame(x = c(1, 2, 3), y = c(1,2,3)),
    cities = data.frame(x = 1, y = 1),
    blup = list(1, 2),
    "Argument 'dlist' must be a list of data frames", fixed = TRUE))

  # cities not a data frame
  expect_error(min_mortality(
    dlist = list(a <- data.frame(x = c(1, 2, 3), y = c(1, 2, 3)),
                 b <- data.frame(x = c(4, 5, 6), y = c(4, 5, 6))),
    cities = c(1:10),
    blup = list(1, 2),
    "Argument 'cities' must be a data frame", fixed = TRUE))

  # blup not a list
  expect_error(min_mortality(
    dlist = list(a <- data.frame(x = c(1, 2, 3), y = c(1, 2, 3)),
                 b <- data.frame(x = c(4, 5, 6), y = c(4, 5, 6))),
    cities = data.frame(x = 1, y = 1),
    blup = c(1, 2),
    "Argument 'blup' must be a list", fixed = TRUE))

})

context("Test output data types")
test_that('Test min_mortality() returns correct data types', {

  c(dlist, argvar, regions, cities, coef, vcov) %<-%
    prep_and_first_step("testdata/regEngWales.csv")

  c(mv, blup) %<-% run_meta_model(dlist = dlist, cities = cities, coef = coef,
                                  vcov = vcov)

  c(avgtmean_wald, rangetmean_wald) %<-% wald_results(mv = mv)

  c(argvar, bvar, mintempcity, minperccountry) %<-%
    min_mortality(dlist = dlist, cities = cities, blup = blup)

  # argvar
  expect_equal(typeof(argvar), "list")
  expect_equal(length(argvar), 5)

  # bvar
  expect_equal(typeof(bvar), "double")
  expect_equal(is.numeric(bvar), TRUE)
  expect_equal(class(bvar)[1], "onebasis")
  expect_equal(class(bvar)[2], "matrix")

  # mintempcity
  expect_equal(typeof(mintempcity), "double")
  expect_equal(is.vector(mintempcity), TRUE)
  expect_equal(is.numeric(mintempcity), TRUE)

  # minperccountry
  expect_equal(typeof(minperccountry), "double")
  expect_equal(is.numeric(minperccountry), TRUE)
  expect_equal(length(minperccountry), 1)

})

