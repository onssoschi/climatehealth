library(testthat)
library(climatehealth)
library(config)
library(zeallot)

context("Test errors for incorrect inputs")
test_that('Test run_meta_model() produces appropriate errors', {

  # df_list not a list
  expect_error(
    run_meta_model(
      df_list = "tester",
      coef = as.matrix(1:10),
      vcov = list(as.matrix(1:10),
                  as.matrix(11:20))),
    regexp = "df_list expected a list. Got character",
    fixed = TRUE)

  # df_list elements not data frames
  expect_error(
    run_meta_model(
      df_list = list(1, 2, 3),
      coef = as.matrix(1:10),
      vcov = list(as.matrix(1:10),
                  as.matrix(11:20))),
    regexp = paste(
      "df_list expected a list of dataframes. List contains item of",
      "type double"
    ),
    fixed = TRUE)

  # coef not a matrix
  expect_error(
    run_meta_model(
      df_list = list(a <- data.frame(x = c(1, 2, 3),
                                     y = c(1, 2, 3)),
                     b <- data.frame(x = c(4, 5, 6),
                                     y = c(4, 5, 6))),
      coef = c(1:10),
      vcov = c(1:10)),
    regexp = "Argument 'coef' must be a numeric matrix",
    fixed = TRUE)

  # coef not numeric
  expect_error(
    run_meta_model(
      df_list = list(a <- data.frame(x = c(1, 2, 3),
                                     y = c(1, 2, 3)),
                     b <- data.frame(x = c(4, 5, 6),
                                     y = c(4, 5, 6))),
      coef = matrix(c("a", "b", "c", "d"), nrow = 2),
      vcov = list(as.matrix(1:10), as.matrix(11:20))),
    regexp = "Argument 'coef' must be a numeric matrix",
    fixed = TRUE)

  # vcov not a list
  expect_error(
    run_meta_model(
      df_list = list(a <- data.frame(x = c(1, 2, 3),
                                     y = c(1, 2, 3)),
                     b <- data.frame(x = c(4, 5, 6),
                                     y = c(4, 5, 6))),
      coef = as.matrix(1:10),
      vcov = 2),
    regexp = "'vcov' expected a list. Got double",
    fixed = TRUE)

  # vcov elements not matrices
  expect_error(
    run_meta_model(
      df_list = list(a <- data.frame(x = c(1, 2, 3),
                                     y = c(1, 2, 3)),
                     b <- data.frame(x = c(4, 5, 6),
                                     y = c(4, 5, 6))),
      coef = as.matrix(1:10),
      vcov = list(1, 2, 3)),
    regexp = paste(
      "'vcov' expected a list of matrices. List contains item of",
      "type double"
    ),
    fixed = TRUE)

  })

context("Test output data types")
test_that('Test run_meta_model() returns correct data types', {

  config <- config::get()

  varper_ <- c(10, 75, 90)

  c(df_list_) %<-%
    load_temperature_data(
      input_csv_path = config$input_csv_path,
      dependent_col = config$dependent_col,
      time_col = config$time_col,
      region_col = config$region_col,
      temp_col = config$temp_col,
      population_col = config$population_col,
      output_year = config$output_year,
      RR_distribution_length = config$RR_distribution_length
    )

  if (config$meta_analysis == TRUE) {

    c(coef_, vcov_) %<-%
      run_model(df_list = df_list_,
                independent_cols = config$independent_cols,
                varfun = config$varfun,
                varper = varper_,
                vardegree = config$vardegree,
                lag = config$lag,
                lagnk = config$lagnk,
                dfseas = config$dfseas
      )

    c(mv_, blup_) %<-%
      run_meta_model(
        df_list = df_list_,
        coef = coef_,
        vcov = vcov_
      )

    c(avgtmean_wald_, rangetmean_wald_) %<-%
      wald_results(
        mv = mv_
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
