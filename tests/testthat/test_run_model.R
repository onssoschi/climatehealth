library(testthat)
library(climatehealth)
library(config)
library(zeallot)

test_that('Test run_model() returns correct data types', {

            config <- config::get()

            c(df_list_) %<-%
              load_data(
                input_csv_path = config$input_csv_path,
                dependent_col = config$dependent_col,
                time_col = config$time_col,
                region_col = config$region_col,
                temp_col = config$temp_col,
                time_range = config$time_range
                )

            c(coef_, vcov_) %<-%
              run_model(
                df_list = df_list_,
                independent_col = config$independent_col,
                varfun = config$varfun,
                varper = config$varper,
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


})
