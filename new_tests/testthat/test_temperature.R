# Tests for temperature.R
library(zeallot)
library(patrick)

# Tests for filter_on_rr_distribution

rr_dist_test_df <- data.frame(
  ind = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  year = c(2004, 2005, 2006, 2006, 2007, 2004, 2008, 2009, 2010, 2008),
  rand_val = c(T, T, F, T, F, F, F, F, T, T)
)

test_that(
  "Test for the case when output_year=0.",
  {
    output <- filter_on_rr_distribution(rr_dist_test_df,
                                        5,
                                        5,
                                        15,
                                        0)
    # expect func to create a dist of 5 years from output year (2010-max)
    # case - 2006, 2007, 2008, 2009, 2010 (5 years)
    expected_output <- dplyr::filter(.data=rr_dist_test_df, year > 2005)
    expect_equal(output, expected_output)
  }
)

test_that(
  "Test for the case when RR_distribution_length=0.",
  {
    output <- filter_on_rr_distribution(rr_dist_test_df,
                                        0,
                                        5,
                                        15,
                                        2010)
    # expect rr_dist of 6 (=exclude 2004)
    expected_output <- dplyr::filter(.data=rr_dist_test_df, year > 2004)
    expect_equal(output, expected_output)
  }
)

test_that(
  "Test for the case where the lower range has been exceeded.",
  {
    expect_error(filter_on_rr_distribution(
      rr_dist_test_df,
      5,
      7,
      15,
      0), regexp = "Timeseries to calculate the RR is less than 7 years.*")
  }
)

test_that(
  "Test for the case where the upper range has been exceeded.",
  {
    expect_error(filter_on_rr_distribution(
      rr_dist_test_df,
      14,
      7,
      13,
      0), regexp = "Timeseries to calculate the RR is more than 13 years.*")
  }
)
# TODO: review whether we also need a case for both arguments being 0.

# Tests for load_temperature_data

TEST_DATA_PATH <- "testdata/temperature_test_data.csv"

test_that(
  "Test that the dataframe is returned as expected (using testdata).",
  {
    resultant <- load_temperature_data(TEST_DATA_PATH,
                                       "death",
                                       "date",
                                       "regnames",
                                       "tmean",
                                       "pop",
                                       0,
                                       0)[[1]]
    # check list length is correct
    expect(length(resultant[[1]]), 2)
    # check shape of first df
    rows <- dim(resultant[1][[1]])[1]
    cols <- dim(resultant[1][[1]])[2]
    expect_equal(rows, 4748, info  = "load_temperature_data expected to return y rows")
    expect_equal(cols, 18, info  = "load_temperature_data expected to return x column")
    # check colnames are correct - checks renaming functionality
    expected_columns<-c("X.1","X","date","year","month","day","time",
                    "yday","dow","region","regnames","temp","tmin","tmax",
                    "dewp","rh","dependent","pop_col")
    expect_equal(colnames(resultant[1][[1]]), expected_columns)
  }
)

test_that(
  "Test that columns are filled automatically (population_col, region_col).",
  {
    resultant <- load_temperature_data(TEST_DATA_PATH,
                                       "death",
                                       "date",
                                       NULL,
                                       "tmean",
                                       NULL,
                                       0,
                                       0)[[1]]
    df <- resultant[1][[1]]
    expect_equal(unique(df$pop_col), c("NONE"))
    expect_equal(unique(df$regnames), c("aggregated"))
  }
)


# Tests for define_model

get_test_input_data <- function(path) {
  data <- load_temperature_data(
    path,
    "death",
    "date",
    "regnames",
    "tmean",
    "pop",
    0,
    0
  )
  return(data)
}

test_that(
  "Test that define_model returns a list of 2 items - model+crossbasis",
  {
    data <- get_test_input_data(TEST_DATA_PATH)[[1]][1][[1]]
    returned <- define_model(
      data,
      independent_cols = NULL,
      varfun = "bs",
      varper = c(10, 75, 90),
      vardegree = 2,
      lag = 21,
      lagnk = 3,
      dfseas = 8
    )
    expect_type(returned, "list")
    expect_equal(length(returned), 2, info = "define_model expected to return a list of 2 items.")
    expect_type(returned[[1]], "list")
    expect_type(returned[[2]], "double")
  }
)

test_that(
  "Test that 'model' has the expected attributes.",
  {
    data <- get_test_input_data(TEST_DATA_PATH)[[1]][1][[1]]
    model <- define_model(
      data,
      independent_cols = NULL,
      varfun = "bs",
      varper = c(10, 75, 90),
      vardegree = 2,
      lag = 21,
      lagnk = 3,
      dfseas = 8
    )[[1]]
    exp_family <- "quasipoisson"
    exp_rank <- 130
    exp_method <- "glm.fit"
    exp_y_50 <- 217
    expect_equal(exp_family, model$family$family)
    expect_equal(exp_rank, model$rank)
    expect_equal(exp_method, model$method)
    expect_equal(exp_y_50, model$y[[50]])
  }
)

test_that(
  "Test that 'crossbasis' has the expected attributes",
  {
    data <- get_test_input_data(TEST_DATA_PATH)[[1]][1][[1]]
    cb <- define_model(
      data,
      independent_cols = NULL,
      varfun = "bs",
      varper = c(10, 75, 90),
      vardegree = 2,
      lag = 21,
      lagnk = 3,
      dfseas = 8
    )[[2]]
    exp_class_1 <- "crossbasis"
    exp_class_2 <- "matrix"
    exp_fun <- "ns"
    exp_knots <-  c(1.011, 2.779, 7.640)
    expect_equal(attr(cb, "class")[[1]], exp_class_1)
    expect_equal(attr(cb, "class")[[2]], exp_class_2)
    arglag <- attr(cb, "arglag")
    expect_equal(arglag$fun, exp_fun)
    expect_equal(round(arglag$knots[1:3], 3), exp_knots)
  }
)

test_that(
  "Test that define_model raises an error when a vector of non-strings is passed.",
  {
    data <- get_test_input_data(TEST_DATA_PATH)[[1]][1][[1]]
    expect_error(
      define_model(
          data,
          independent_cols = c(1, 2, 3),
          varfun = "bs",
          varper = c(10, 75, 90),
          vardegree = 2,
          lag = 21,
          lagnk = 3,
          dfseas = 8
      ),
      regexp = ".*independent_cols.*expected a vector of strings"
    )
  }
)

# Tests for run_model

test_that(
  "Test that run_model returns the expected data.",
  {
    # load input data
    data <- get_test_input_data(TEST_DATA_PATH)[[1]]
    # use run_model func to obtain results
    returned <- run_model(df_list = data,
                          independent_cols = NULL,
                          varfun = "bs",
                          varper = c(10, 75, 90),
                          vardegree = 2,
                          lag = 21,
                          lagnk = 3,
                          dfseas = 8
                          )
    # assert return type is correct
    add_info <- paste("run_model expected to return 4 items. Got", length(returned))
    expect_equal(length(returned), 4, info = add_info)
    # return 1 - coef
    expect_type(returned[1], "list")
    exp_coef_names <- c("North West", "South East", "Wales")
    exp_coef <- c(-0.863, -0.863, -0.669)
    expect_equal(names(returned[[1]][,1]), exp_coef_names)
    expect_equal(round(as.vector(returned[[1]][,1]), 3), exp_coef)
    # return 2 - vcov
    expect_type(returned[2], "list")
    exp_wal_vcov <- c(0.0109, 0.0083, 0.0099)
    expect_equal(round(returned[2][[1]]$Wales[1:3], 4), exp_wal_vcov)
    # return 3 and 4 not tested since it is the same return as define_model
  }
)

# Tests for run_meta_model (meta-analysis)

test_that(
  "Test that run_meta_model raises when coef is not a numeric matrix.",
  {
    dummy_df_list <- list(data.frame(), data.frame())
    error_msg <- "Argument .*coef.* must be a numeric matrix"
    expect_error(run_meta_model(dummy_df_list, 2L, matrix(0, 3, 3)), regexp = error_msg)
    expect_error(run_meta_model(dummy_df_list, matrix("T", 3, 3), matrix(0, 3, 3)), regexp = error_msg)
  }
)

test_that(
  "Test that run_meta_model raises when vcov is not a list of matrices.",
  {
    dummy_df_list <- list(data.frame(), data.frame())
    not_list_msg <- ".*vcov.* expected a list. Got.*double.*"
    expect_error(run_meta_model(dummy_df_list, matrix(0, 3, 3), 2L, regexp = not_list_msg))
    not_matrix_msg <- ".*vcov.* expected a list of matrices. List contains item.*"
    expect_error(run_meta_model(dummy_df_list, matrix(0, 3, 3), list(3, 3, "a")), regexp = not_matrix_msg)
  }
)

test_that(
  "Test that run_meta_model has the correct return values (using test data).",
  {
    data <- get_test_input_data(TEST_DATA_PATH)[[1]]
    c(coef_, vcov_, cb_, model_) %<-%
      run_model(df_list = data,
                independent_cols = NULL,
                varfun = "bs",
                varper = c(10, 75, 90),
                vardegree = 2,
                lag = 21,
                lagnk = 3,
                dfseas = 8)
    returned <- run_meta_model(data, coef_, vcov_)
    expect_true(is.list(returned))
    expect_equal(length(returned), 2)
    # mv
    mv <- returned[[1]]
    exp_wales_fitted_vals <- c(-0.669, -0.722, -0.862, -0.789, -0.680)
    expect_equal(
      round(as.vector(mv$fitted.values["Wales",]), 3), exp_wales_fitted_vals
    )
    # blup
    blup <- returned[[2]]
    exp_blup <- c(-0.863, -0.837, -0.988, -0.938, -0.693)
    expect_equal(
      round(as.vector(blup[[1]]$blup), 3), exp_blup
    )
  }
)

# test fwald

test_that(
  "fwald raises an error when 'var' is not of type character.",
  {
    error_msg <- "Argument .*var.* must be a character"
    expect_error(fwald(2L, 2L), regexp = error_msg)
  }
)

test_that(
  "fwald calculates p-values for the explanatory variable.",
  {
    data <- get_test_input_data(TEST_DATA_PATH)[[1]]
    c(coef_, vcov_, cb_, model_) %<-%
      run_model(df_list = data,
                independent_cols = NULL,
                varfun = "bs",
                varper = c(10, 75, 90),
                vardegree = 2,
                lag = 21,
                lagnk = 3,
                dfseas = 8)
    returned <- run_meta_model(data, coef_, vcov_)
    results <- fwald(returned[[1]], "avgtmean")
    exp_fwald <- 0.498
    expect_equal(round(results[1], 4), exp_fwald, tolerance = 1e-04)
  }
)

# wald_results tests skipped since the function only utilises fwald

# test define_and_validate_optimal_tempts

optimal_temps_dummy = c(1.01, 1.045, 1.09, 1.1, 1.034)
names(optimal_temps_dummy) <- c("10", "11", "12", "13", "14")
optimal_temps_dummy <- list("allRRfit"=optimal_temps_dummy)

test_that(
  "define_and_validate_optimal_temps works as intended.",
  {
    OTR <- data.frame(lower = NA, upper = NA, row.names = "aggregated")
    exp_OTR <- data.frame(lower = 10, upper = 14, row.names = "aggregated")
    returned <- define_and_validate_optimal_temps(OTR, optimal_temps_dummy, index="aggregated")
    expect_equal(returned, exp_OTR)
  }
)

# Test for warnings
test_that(
  "define_and_validate_optimal_temps raises the correct warning (below 0).",
  {
    pred <- optimal_temps_dummy
    pred$allRRfit[["15"]] <- 0.9
    OTR <- data.frame(lower = NA, upper = NA, row.names = "aggregated")
    expect_warning(define_and_validate_optimal_temps(OTR, pred, index="aggregated"))
  }
)

# Tests for calculate_min_mortality_temp
test_that(
  "Test that an error is raised when blup is not a list.",
  {
    expect_error(
      calculate_min_mortality_temp(
        df_list = list(data.frame()),
        blup = 2,
        independent_cols = NULL,
        varfun = NULL,
        varper = NULL,
        vardegree = NULL,
        lag = NULL,
        lagnk = NULL,
        dfseas = NULL
      ), regexp = "Argument 'blup' must be a list"
    )
  }
)



