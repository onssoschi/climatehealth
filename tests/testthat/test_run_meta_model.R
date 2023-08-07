library(testthat)
library(climatehealth)
library(config)
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

  config <- config::get()

  c(df_list_unordered_, regions_) %<-%
    load_data(
      input_csv_path = config$input_csv_path,
      dependent_col = config$dependent_col,
      time_col = config$time_col,
      region_col = config$region_col,
      temp_col = config$temp_col
    )

  c(regions_df_, df_list_) %<-%
    get_region_metadata(
      regions = regions_,
      df_list_unordered = df_list_unordered_,
      region_names = NULL
    )

  if (config$meta_analysis == TRUE) {

    c(coef_, vcov_) %<-%
      run_model(df_list = df_list_,
                regions_df = regions_df_,
                independent_col = config$independent_col,
                varfun = config$varfun,
                varper = config$varper,
                vardegree = config$vardegree,
                lag = config$lag,
                lagnk = config$lagnk,
                dfseas = config$dfseas)

    c(mv_, blup_) %<-%
      run_meta_model(
        df_list = df_list_,
        regions_df = regions_df_,
        coef = coef_,
        vcov = vcov_
      )

  }

  expect_equal(typeof(blup_), "list")
  expect_equal(typeof(blup_[[1]]), "list")
  expect_equal(typeof(blup_[[2]]), "list")
  expect_equal(typeof(blup_[[1]][[1]]), "double")
  expect_equal(is.numeric(blup_[[1]][[2]]), TRUE)
  expect_equal(is.numeric(blup_[[1]][[2]]), TRUE)

  # mv
  expect_equal(typeof(mv_), "list")
  expect_equal(class(mv_), "mvmeta")

})
