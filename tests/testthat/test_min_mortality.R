library(testthat)
library(indicatorfunctions)
library(zeallot)

context("Test errors for incorrect inputs")
test_that('Test min_mortality() produces appropriate errors', {

  # dlist not a list
  expect_error(min_mortality(
    df_list = data.frame(x = c(1, 2, 3), y = c(1,2,3)),
    regions_df = data.frame(x = 1, y = 1),
    blup = list(1, 2),
    "Argument 'dlist' must be a list of data frames", fixed = TRUE))

  # cities not a data frame
  expect_error(min_mortality(
    df_list = list(a <- data.frame(x = c(1, 2, 3), y = c(1, 2, 3)),
                 b <- data.frame(x = c(4, 5, 6), y = c(4, 5, 6))),
    regions_df = c(1:10),
    blup = list(1, 2),
    "Argument 'cities' must be a data frame", fixed = TRUE))

  # blup not a list
  expect_error(min_mortality(
    df_list = list(a <- data.frame(x = c(1, 2, 3), y = c(1, 2, 3)),
                 b <- data.frame(x = c(4, 5, 6), y = c(4, 5, 6))),
    regions_df = data.frame(x = 1, y = 1),
    blup = c(1, 2),
    "Argument 'blup' must be a list", fixed = TRUE))

})

context("Test output data types")
test_that('Test min_mortality() returns correct data types', {


  c(df_list_unordered, regions) %<-% load_data(input_path = 'testdata/regEngWales.csv')

  c(regions_df, df_list) %<-% get_region_metadata(regions = regions,
                                                  df_list_unordered = df_list_unordered)

  c(argvar, coef, vcov) %<-% run_model(df_list = df_list, regions_df = regions_df)

  c(mv, blup) %<-% run_meta_model(df_list = df_list, regions_df = regions_df, coef = coef,
                                  vcov = vcov)

  c(argvar, bvar, mintempregions, minperccountry) %<-%
    min_mortality(df_list = df_list, regions_df = regions_df, blup = blup)

  # argvar
  expect_equal(typeof(argvar), "list")
  expect_equal(length(argvar), 5)

  # bvar
  expect_equal(typeof(bvar), "double")
  expect_equal(is.numeric(bvar), TRUE)
  expect_equal(class(bvar)[1], "onebasis")
  expect_equal(class(bvar)[2], "matrix")

  # mintempcity
  expect_equal(typeof(mintempregions), "double")
  expect_equal(is.vector(mintempregions), TRUE)
  expect_equal(is.numeric(mintempregions), TRUE)

  # minperccountry
  expect_equal(typeof(minperccountry), "double")
  expect_equal(is.numeric(minperccountry), TRUE)
  expect_equal(length(minperccountry), 1)

})

