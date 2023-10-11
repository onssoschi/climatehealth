library(testthat)
library(climatehealth)
library(config)
library(zeallot)

test_that('Test run_model() returns correct data types', {

  config <- config::get()

  varper_ <- c(10, 75, 90)

  c(df_list_) %<-%
    load_data(
      input_csv_path = config$input_csv_path,
      dependent_col = config$dependent_col,
      time_col = config$time_col,
      region_col = config$region_col,
      temp_col = config$temp_col,
      time_range_start = config$time_range_start,
      time_range_end = config$time_range_end
    )

  if (config$meta_analysis == TRUE) {

    c(coef_, vcov_) %<-%
      run_model(df_list = df_list_,
                independent_col1 = config$independent_col1,
                independent_col2 = config$independent_col2,
                independent_col3 = config$independent_col3,
                varfun = config$varfun,
                varper = varper_,
                vardegree = config$vardegree,
                lag = config$lag,
                lagnk = config$lagnk,
                dfseas = config$dfseas
      )

      # coef
      expect_equal(typeof(coef_), "double")
      expect_equal(class(coef_)[1], "matrix")
      expect_equal(is.numeric(coef_), TRUE)
      expect_equal(is.numeric(coef_[1]), TRUE)
      expect_equal(nrow(coef_), length(names(df_list_)))

      # vcov
      expect_equal(typeof(vcov_), "list")
      expect_equal(typeof(vcov_[[1]]), "double")
      expect_equal(is.numeric(vcov_[[1]]), TRUE)
      expect_equal(length(vcov_), length(names(df_list_)))

  }


})
