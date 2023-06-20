library(testthat)
library(indicatorfunctions)
library(zeallot)

context("Test errors for incorrect inputs")
test_that('Test run_meta_model() produces appropriate errors', {

  # df_list not a list
  expect_error(run_meta_model(
    df_list = data.frame(x = c(1, 2, 3), y = c(1,2,3)),
    regions_df = data.frame(x = 1, y = 1),
    coef = as.matrix(1:10),
    vcov = list(as.matrix(1:10), as.matrix(11:20))),
    "Argument 'df_list' must be a list of data frames", fixed = TRUE)

  # df_list elements not data frames
  expect_error(run_meta_model(
    df_list = list(1, 2, 3),
    regions_df = data.frame(x = 1, y = 1),
    coef = as.matrix(1:10),
    vcov = list(as.matrix(1:10), as.matrix(11:20))),
    "Argument 'df_list' must be a list of data frames", fixed = TRUE)

  # regions_df not a dataframe
  expect_error(run_meta_model(
    df_list = list(a <- data.frame(x = c(1, 2, 3), y = c(1, 2, 3)),
                 b <- data.frame(x = c(4, 5, 6), y = c(4, 5, 6))),
    regions_df = c(1:10),
    coef = as.matrix(1:10),
    vcov = list(as.matrix(1:10), as.matrix(11:20))),
    "Argument 'regions_df' must be a data frame", fixed = TRUE)

  # coef not a matrix
  expect_error(run_meta_model(
    df_list = list(a <- data.frame(x = c(1, 2, 3), y = c(1, 2, 3)),
                 b <- data.frame(x = c(4, 5, 6), y = c(4, 5, 6))),
    regions_df = data.frame(x = 1, y = 1),
    coef = c(1:10),
    vcov = list(as.matrix(1:10), as.matrix(11:20))),
    "Argument 'coef' must be a numeric matrix", fixed = TRUE)

  # coef not numeric
  expect_error(run_meta_model(
    df_list = list(a <- data.frame(x = c(1, 2, 3), y = c(1, 2, 3)),
                 b <- data.frame(x = c(4, 5, 6), y = c(4, 5, 6))),
    regions_df = data.frame(x = 1, y = 1),
    coef = matrix(c("a", "b", "c", "d"), nrow = 2),
    vcov = list(as.matrix(1:10), as.matrix(11:20))),
    "Argument 'coef' must be a numeric matrix", fixed = TRUE)

  # vcov not a list
  expect_error(run_meta_model(
    df_list = list(a <- data.frame(x = c(1, 2, 3), y = c(1, 2, 3)),
                 b <- data.frame(x = c(4, 5, 6), y = c(4, 5, 6))),
    regions_df = data.frame(x = 1, y = 1),
    coef = as.matrix(1:10),
    vcov = as.matrix(1:10)),
    "Argument 'vcov' must be a list of matrices", fixed = TRUE)

  # vcov elements not matrices
  expect_error(run_meta_model(
    df_list = list(a <- data.frame(x = c(1, 2, 3), y = c(1, 2, 3)),
                 b <- data.frame(x = c(4, 5, 6), y = c(4, 5, 6))),
    regions_df = data.frame(x = 1, y = 1),
    coef = as.matrix(1:10),
    vcov = list(1, 2, 3)),
    "Argument 'vcov' must be a list of matrices", fixed = TRUE)
})

context("Test output data types")
test_that('Test run_meta_model() returns correct data types', {

  # blup
  c(df_list_unordered, regions) %<-%
    load_data(input_path = 'testdata/regEngWales.csv')

  c(regions_df, df_list) %<-%
    get_region_metadata(regions = regions,
                        df_list_unordered = df_list_unordered)

  c(mv, blup) %<-%
    run_meta_model(df_list = df_list,
                   regions_df = regions_df,
                   coef = coef,
                   vcov = vcov)

  expect_equal(typeof(blup), "list")
  expect_equal(typeof(blup[[1]]), "list")
  expect_equal(typeof(blup[[2]]), "list")
  expect_equal(typeof(blup[[1]][[1]]), "double")
  expect_equal(is.numeric(blup[[1]][[2]]), TRUE)
  expect_equal(is.numeric(blup[[1]][[2]]), TRUE)

  # mv
  expect_equal(typeof(mv), "list")
  expect_equal(class(mv), "mvmeta")

})
