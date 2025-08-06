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

# Tests for load_temperature_data

TEST_DATA_PATH <- system.file("testdata", package="climatehealth")

test_that(
  "Test that the dataframe is returned as expected (using testdata).",
  {
    resultant <- load_temperature_data(file.path(TEST_DATA_PATH, "temperature_test_data.csv"),
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
    expect_equal(cols, 19, info  = "load_temperature_data expected to return x column")
    # check colnames are correct - checks renaming functionality
    expected_columns<-c("X.1","...1", "X","date","year","month","day","time",
                    "yday","dow","region","regnames","temp","tmin","tmax",
                    "dewp","rh","dependent","pop_col")
    expect_equal(colnames(resultant[1][[1]]), expected_columns)
  }
)

test_that(
  "Test that columns are filled automatically (population_col, region_col).",
  {
    resultant <- load_temperature_data(file.path(TEST_DATA_PATH, "temperature_test_data.csv"),
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
    data <- get_test_input_data(file.path(TEST_DATA_PATH, "temperature_test_data.csv"))[[1]][1][[1]]
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
    data <- get_test_input_data(file.path(TEST_DATA_PATH, "temperature_test_data.csv"))[[1]][1][[1]]
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
    data <- get_test_input_data(file.path(TEST_DATA_PATH, "temperature_test_data.csv"))[[1]][1][[1]]
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
    data <- get_test_input_data(file.path(TEST_DATA_PATH, "temperature_test_data.csv"))[[1]][1][[1]]
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
    data <- get_test_input_data(file.path(TEST_DATA_PATH, "temperature_test_data.csv"))[[1]]
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
    data <- get_test_input_data(file.path(TEST_DATA_PATH, "temperature_test_data.csv"))[[1]]
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
    data <- get_test_input_data(file.path(TEST_DATA_PATH, "temperature_test_data.csv"))[[1]]
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
  "Test that calculate_min_mortality_temp returns an error when blup is not a list.",
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

test_that(
  "Test for calculate_min_mortality_temp when blup=NULL.",
  {
    # define my expected values
    expected_thresholds <- data.frame(
      min_high_cold = c(-100, -100, -100),
      high_moderate_cold = c(1.070975, 1.090602, 1.497259),
      moderate_cold_OTR = c(10.5, 11.0, 10.5),
      moderate_heat_OTR = c(20.5, 21.0, 21.0),
      high_moderate_heat = c(20.5, 21.0, 21.0),
      max_high_heat = c(100, 100, 100)
    )
    rownames(expected_thresholds) <- c("North West", "South East", "Wales")
    expected_mintempregions <- c("North West" = 16, "South East" = 18, "Wales" = 16)
    # obtain my results
    data <- get_test_input_data(file.path(TEST_DATA_PATH, "temperature_test_data.csv"))[[1]]
    test_blup = readRDS(file.path(TEST_DATA_PATH, "temperature_blup.rds"))
    returned <- calculate_min_mortality_temp(
      df_list = data,
      blup = NULL,
      independent_cols = NULL,
      varfun = "bs",
      varper = c(10, 75, 90),
      vardegree = 2L,
      lag = 21L,
      lagnk = 3L,
      dfseas = 8L
    )
    # test that the returned results are correct
    expect_equal(returned[[1]], expected_mintempregions, tolerance = 1e-6)
    expect_equal(returned[[2]], expected_thresholds, tolerance = 1e-6)
  }
)

test_that(
  "Test for calculate_min_mortality_temp when blup is not NULL.",
  {
    # define my expected values
    expected_thresholds <- data.frame(
      min_high_cold = c(-100, -100, -100),
      high_moderate_cold = c(1.070975, 1.090602, 1.497259),
      moderate_cold_OTR = c(10.1, 10.3, 10.2),
      moderate_heat_OTR = c(21.0, 21.4, 21.3),
      high_moderate_heat = c(21.0, 21.4, 21.3),
      max_high_heat = c(100, 100, 100)
    )
    rownames(expected_thresholds) <- c("North West", "South East", "Wales")
    expected_mintempregions <- c("North West" = 16.10869, "South East" = 18.00545, "Wales" = 15.79192)
    # obtain my results
    data <- get_test_input_data(file.path(TEST_DATA_PATH, "temperature_test_data.csv"))[[1]]
    test_blup = readRDS(file.path(TEST_DATA_PATH, "temperature_blup.rds"))
    returned <- calculate_min_mortality_temp(
      df_list = data,
      blup = test_blup,
      independent_cols = NULL,
      varfun = "bs",
      varper = c(10, 75, 90),
      vardegree = 2L,
      lag = 21L,
      lagnk = 3L,
      dfseas = 8L
    )
    # test that the returned results are correct
    expect_equal(returned[[1]], expected_mintempregions, tolerance = 1e-6)
    expect_equal(returned[[2]], expected_thresholds, tolerance = 1e-5)
  }
)

compute_rates <- function(data, o_year){
  # get attributable deaths
  attr_deaths <- readRDS(file.path(TEST_DATA_PATH, "temperature_attributable_deaths.rds"))
  arraysim <- attr_deaths[[1]]
  matsim <- attr_deaths[[2]]
  # compute rates
  returned <- compute_attributable_rates(data, o_year, matsim, arraysim)
  return(returned)
}

test_that(
  "Test that compute_attributable_rates produces the correct results.",
  {
    # read in test data
    data <- get_test_input_data(file.path(TEST_DATA_PATH, "temperature_test_data.csv"))[[1]]
    # compute rates
    returned <- compute_rates(data, 0)
    # test return size
    expect_equal(
      length(returned),
      4,
      info = "compute_attributable_rates returned an unexpected number of items"
    )
    anregions <- returned[[1]]
    antot <- returned[[2]]
    arregions <- returned[[3]]
    artot <- returned[[4]]
    # test anregions values
    expect_equal(dim(anregions), c(21, 3), info = "The size of anregions is not as expected")
    exp_anregions <- data.frame(
      `North West` = c(3904.2489, 252.8935),
      `South East` = c(4630.8474, 400.5894),
      `Wales` = c(1976.08402, 61.04349)
    )
    rownames(exp_anregions) <- c("glob_cold", "glob_heat")
    inf = "anregions from compute_attributable_rates not as expected"
    expect_equal(data.frame(exp_anregions), data.frame(anregions[1:2, ]), tolerance = 1e-7, info = inf)
    # test antot values
    expect_equal(dim(antot), c(3, 7), info = "The size of antot is not as expected")
    exp_antot <- data.frame(
      glob_cold = c(10511.180, 9422.625, 12915.442),
      glob_heat = c(714.5264, 536.9729, 965.1201),
      moderate_cold = c(9373.979, 8386.406, 11663.161),
      moderate_heat = c(0, 0, 0),
      high_cold = c(1137.201, 1110.339, 1313.597),
      high_heat = c(714.5264, 556.4483, 973.8864),
      heatwave = c(633.2973, 487.6599, 868.8256)
    )
    rownames(exp_antot) <- c("antot", "antotlow", "antothigh")
    inf = "antot from compute_attributable_rates not as expected"
    expect_equal(data.frame(exp_antot), data.frame(antot), tolerance = 1e-6, info = inf)
    # test arregions values
    expect_equal(dim(arregions), c(21, 3), info = "The size of arregions is not as expected")
    exp_arregions <- data.frame(
      `North West` = c(260.28326, 16.85957),
      `South East` = c(308.72316, 26.70596),
      `Wales` = c(131.738935, 4.069566)
    )
    rownames(exp_arregions) <- c("glob_cold", "glob_heat")
    inf = "arregions from compute_attributable_rates not as expected"
    expect_equal(data.frame(exp_arregions), data.frame(arregions[1:2, ]), tolerance = 1e-7, info = inf)
    # test artot values
    expect_equal(dim(artot), c(3, 7), info = "The size of artot is not as expected")
    exp_artot <- data.frame(
      glob_cold = c(233.5818, 209.3917, 287.0098),
      glob_heat = c(15.87836, 11.93273, 21.44711),
      moderate_cold = c(208.3106, 186.3646, 259.1814),
      moderate_heat = c(0, 0, 0),
      high_cold = c(25.27114, 24.67420, 29.19104),
      high_heat = c(15.87836, 12.36552, 21.64192),
      heatwave = c(14.07327, 10.83689, 19.30724)
    )
    rownames(exp_artot) <- c("artot", "artotlow", "artothigh")
    inf = "artot from compute_attributable_rates not as expected"
    expect_equal(data.frame(exp_artot), data.frame(artot), tolerance = 1e-6, info = inf)
  }
)

# Tests for write_attributable_deaths

test_that(
  "Test that write_attributable_deaths writes to file correctly.",
  {
    # setup a temp directory for outputs
    temp_dir = base::tempdir()
    # load data
    data <- get_test_input_data(file.path(TEST_DATA_PATH, "temperature_test_data.csv"))[[1]]
    rates <- compute_rates(data, 0)
    # write results
    write_attributable_deaths(
      avgtmean_wald = NULL,
      rangetmean_wald = NULL,
      anregions_bind = rates[[1]],
      antot_bind = rates[[2]],
      arregions_bind = rates[[3]],
      artot_bind = rates[[4]],
      save_csv = TRUE,
      output_folder_path = temp_dir
    )
    # assert files were correctly written
    filenames <- c(
      "heat_and_cold_attributable_deaths_regions.csv",
      "heat_and_cold_attributable_deaths_total.csv",
      "heat_and_cold_attributable_rates_regions.csv",
      "heat_and_cold_attributable_rates_total.csv",
      "heat_and_cold_wald_test_results.csv"
    )
    sizes <- list(c(3, 22), c(3, 8), c(3, 22), c(3, 8), c())
    for (i in 1:5){
      fpath <- file.path(temp_dir, filenames[i])
      expect_true(file.exists(fpath), info = paste(fpath, "does not exist"))
      if (i==5){
        # empty wald file
        invisible()
      } else {
        expect_equal(dim(read.csv(fpath)), sizes[[i]], info = "Outputted csv file not as expected")
      }
    }

  }
)

# Tests for plot_and_write_relative_risk_all

get_plot_dummy_data <- function(){
  df_list <- get_test_input_data(file.path(TEST_DATA_PATH, "temperature_test_data.csv"))[[1]]
  cb <- readRDS(file.path(TEST_DATA_PATH, "temperature_cb.rds"))
  model <- readRDS(file.path(TEST_DATA_PATH, "temperature_model.rds"))
  mintempregions <- c("North West" = 16.10869, "South East" = 17.79293, "Wales" = 15.79192)
  varfun <- "bs"
  varper <- c(10, 75, 90)
  vardegree <- 2L
  data <- list(df_list, cb, model, mintempregions, varfun, varper, vardegree)
  return(data)
}

test_that(
  "Test that plot_and_write_relative_risk_all behaves as expected when nothing is saved.",
  {
    # create temp save directory
    temp_dir = tempdir()
    data <- get_plot_dummy_data()
    plot_and_write_relative_risk_all(
      df_list = data[[1]],
      cb = data[[2]],
      model = data[[3]],
      mintempregions = data[[4]],
      save_fig = FALSE,
      save_csv = FALSE,
      csv_output_path = file.path(temp_dir, "temperature_test.csv"),
      dependent_col = "death",
      varfun = data[[5]],
      varper = data[[6]],
      vardegree = data[[7]]
    )
    # assert nothing is saved
    expect_false(file.exists(file.path(temp_dir, "temperature_test.csv")), info = "file saved unexpectedly")
    # clear dir
    unlink(paste0(temp_dir, "/*"), recursive = TRUE)
  }
)


test_that(
  "Test that plot_and_write_relative_risk_all behaves as expected when data/figures are saved.",
  {
    # get data
    data <- get_plot_dummy_data()
    df_list <- data[[1]]
    temp_dir = tempdir()
    # setup figure
    grid <- create_grid(length(df_list))
    fig_fpath <- file.path(temp_dir, "temperature_fig.pdf")
    pdf(paste(fig_fpath, sep = ''),
        width=grid[1]*4, height=grid[2]*4)

    par(mfrow=c(grid[1],  grid[2]))
    # write plots and csv's
    plot_and_write_relative_risk_all(
      df_list = data[[1]],
      cb = data[[2]],
      model = data[[3]],
      mintempregions = data[[4]],
      save_fig = TRUE,
      save_csv = TRUE,
      csv_output_path = file.path(temp_dir, "temperature_test.csv"),
      dependent_col = "death",
      varfun = data[[5]],
      varper = data[[6]],
      vardegree = data[[7]]
    )
    # assert that the data is saved
    expect_true(file.exists(file.path(temp_dir, "temperature_test.csv")), info = "csv file was not saved")
    expect_true(file.exists(fig_fpath), info = "plots were not saved")
    unlink(paste0(temp_dir, "/*"), recursive = TRUE)
  }
)

# Tests for plot_and_write_relative_risk

get_plot_dummy_data <- function(){
  # dataset
  df_list <- get_test_input_data(file.path(TEST_DATA_PATH, "temperature_test_data.csv"))[[1]]
  # model vars
  varfun <- "bs"
  varper <- c(10, 75, 90)
  vardegree <- 2L
  lag <- 21L
  lagnk <- 3
  dfseas <- 8L
  # calculated variables
  min_mortality_temp <- calculate_min_mortality_temp(
    df_list = df_list,
    blup = NULL,
    independent_cols = NULL,
    varfun = varfun,
    varper = varper,
    vardegree = vardegree,
    lag = lag,
    lagnk = lagnk,
    dfseas = dfseas
  )
  mintempregions <- min_mortality_temp[[1]]
  an_thresholds <- min_mortality_temp[[2]]
  data <- list(df_list, varfun, varper, vardegree, lag, lagnk, dfseas, mintempregions, an_thresholds)
  return(data)
}

test_that(
  "Test that plot_and_write_relative_risk behaves as expected when nothing is saved.",
  {
    # create temp save directory
    temp_dir = tempdir()
    data <- get_plot_dummy_data()
    plot_and_write_relative_risk(
      df_list = data[[1]],
      blup = NULL,
      mintempregions = data[[8]],
      an_thresholds = data[[9]],
      save_fig = FALSE,
      save_csv = FALSE,
      csv_output_path = file.path(temp_dir, "temperature_test_2.csv"),
      independent_cols = NULL,
      varfun = data[[2]],
      varper = data[[3]],
      vardegree = data[[4]],
      lag = data[[5]],
      lagnk = data[[6]],
      dfseas = data[[7]]
    )
    # assert nothing is saved
    expect_false(file.exists(file.path(temp_dir, "temperature_test_2.csv")), info = "file saved unexpectedly")
    # clear dir
    unlink(paste0(temp_dir, "/*"), recursive = TRUE)
  }
)


test_that(
  "Test that plot_and_write_relative_risk behaves as expected when data/figures are saved.",
  {
    # get data
    data <- get_plot_dummy_data()
    df_list <- data[[1]]
    temp_dir = tempdir()
    # setup figure
    grid <- create_grid(length(df_list))
    fig_fpath <- file.path(temp_dir, "temperature_fig_2.pdf")
    pdf(paste(fig_fpath, sep = ''),
        width=grid[1]*4, height=grid[2]*4)

    par(mfrow=c(grid[1],  grid[2]))
    # write plots and csv's
    plot_and_write_relative_risk(
      df_list = data[[1]],
      blup = NULL,
      mintempregions = data[[8]],
      an_thresholds = data[[9]],
      save_fig = TRUE,
      save_csv = TRUE,
      csv_output_path = file.path(temp_dir, "temperature_test_2.csv"),
      independent_cols = NULL,
      varfun = data[[2]],
      varper = data[[3]],
      vardegree = data[[4]],
      lag = data[[5]],
      lagnk = data[[6]],
      dfseas = data[[7]]
    )
    # assert that the data is saved
    expect_true(file.exists(file.path(temp_dir, "temperature_test_2.csv")), info = "csv file was not saved")
    expect_true(file.exists(fig_fpath), info = "plots were not saved")
    unlink(paste0(temp_dir, "/*"), recursive = TRUE)
  }
)
