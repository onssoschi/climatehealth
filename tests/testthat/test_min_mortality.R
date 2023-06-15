library(testthat)
library(indicatorfunctions)
library(zeallot)

source("./R/gasparrini_functions.R")

context("Test errors for incorrect inputs")
test_that('Test min_mortality() produces appropriate errors', {

  c(dlist, argvar, regions, cities, coef, vcov) %<-% prep_and_first_step(input_csv_path)

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

