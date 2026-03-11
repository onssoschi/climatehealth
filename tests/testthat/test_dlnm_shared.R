# Unit tests for dlnm_shared

if (!"package:climatehealth" %in% search()) {
  pkgload::load_all(".", export_all = TRUE, helpers = FALSE, quiet = TRUE)
}

# Create temp_dir to be used by all MH tests
temp_dir <- tempdir()
temp_dir <- file.path(temp_dir, "dlnm_shared_tests")
if (!file.exists(temp_dir)) dir.create(temp_dir)

test_that("Test dlnm_pop_totals", {
  # Define test data
  dlnm_pop_totals_df <- data.frame(
    region = structure(c(1L, 1L, 3L, 2L, 3L, 1L, 2L, 1L), levels = c("Central","East", "North"), class = "factor"),
    population = c(1050L, 950L, 1000L, 1200L, 1100L, 850L, 1150L, 1300L),
    year = structure(c(1L, 1L, 2L, 3L, 2L, 3L, 1L, 2L), levels = c("2022", "2024", "2023"), class = "factor"))
  dlnm_pop_totals_list <- aggregate_by_column(dlnm_pop_totals_df, "region")


  dlnm_pop_totals_control_list <- list(
    Central = structure(list(
      year = structure(1:3, levels = c("2022", "2024", "2023"), class = "factor"),
      population = c(1000, 1300, 850)), row.names = c(NA, -3L), class = "data.frame"),
    East = structure(list(
      year = structure(c(1L, 3L), levels = c("2022", "2024", "2023"), class = "factor"),
      population = c(1150, 1200)), row.names = c(NA, -2L), class = "data.frame"),
    North = structure(list(
      year = structure(2L, levels = c("2022", "2024", "2023"), class = "factor"),
      population = 1050), row.names = c(NA, -1L), class = "data.frame"))

  # Call function
  dlnm_pop_totals_national_control_list <- append(dlnm_pop_totals_control_list, list(
    National = structure(list(
      year = structure(1:3, levels = c("2022", "2024", "2023"), class = "factor"),
      population = c(2150, 2350, 2050)), row.names = c(NA, -3L), class = "data.frame")))

  # Assert equality
  expect_equal(dlnm_pop_totals(dlnm_pop_totals_list, meta_analysis = FALSE),
               dlnm_pop_totals_control_list,
               label = "dlnm_pop_totals(dlnm_pop_totals_list, meta_analysis = FALSE)")
  expect_equal(dlnm_pop_totals(dlnm_pop_totals_list, meta_analysis = TRUE),
               dlnm_pop_totals_national_control_list,
               label = "dlnm_pop_totals(dlnm_pop_totals_list, meta_analysis = TRUE)")
})


test_that("dlnm_vif calculates variance inflation factors correctly", {
  # Create sample data with known correlations
  set.seed(123)  # For reproducibility
  n <- 30
  temp <- rnorm(n, mean = 23, sd = 2)

  sample_df <- data.frame(
    date = seq.Date(from = as.Date("2023-01-01"),
                    to = as.Date("2023-01-30"),
                    by = "day"),
    temp = temp,
    suicides = rpois(n, lambda = 10),
    # Create variables with known relationships to temperature
    humidity = temp * 0.5 + rnorm(n, mean = 70, sd = 5),  # Moderate correlation
    pollution = rnorm(n, mean = 50, sd = 15),             # Low correlation
    wind = temp * 0.8 + rnorm(n, mean = 20, sd = 3)      # Strong correlation
  )

  # Create df_list
  df_list <- list(
    "Region1" = sample_df,
    "Region2" = sample_df
  )

  # Test with multiple independent variables
  independent_vars <- c("humidity", "pollution", "wind")
  results <- dlnm_vif(df_list, independent_vars)

  # Test structure
  expect_type(results, "list")
  expect_named(results, c("Region1", "Region2"))

  # Test each region's results
  for (reg in names(results)) {
    region_vif <- results[[reg]]

    # Check structure
    expect_s3_class(region_vif, "data.frame")
    expect_named(region_vif, c("region", "variable_combo", "variable", "vif"))
    expect_equal(ncol(region_vif), 4)  # temp + 3 independent vars

    # Check all variables are present
    expected_vars <- c("temp", independent_vars)
    expect_setequal(region_vif$variable, expected_vars)

    # Check VIF values are numeric and positive
    expect_true(all(is.numeric(region_vif$vif)))
    expect_true(all(region_vif$vif > 0))

    # Check that wind (strongly correlated) has higher VIF than pollution (weakly correlated)
    wind_vif <- region_vif$vif[region_vif$variable == "wind"]
    pollution_vif <- region_vif$vif[region_vif$variable == "pollution"]
    expect_true(all(wind_vif > pollution_vif))
  }

  # Test with single independent variable
  single_var_results <- dlnm_vif(df_list, "pollution")
  for (reg in names(single_var_results)) {
    expect_equal(nrow(single_var_results[[reg]]), 2)  # temp and pollution
    expect_setequal(single_var_results[[reg]]$variable, c("temp", "pollution"))
  }
})


test_that("dlnm_reduce_cumulative produces expected output with valid inputs", {
  # Set seed for reproducibility
  set.seed(123)

  # Create sample data for two regions
  n_days <- 100
  temp_data1 <- rnorm(n_days, mean = 20, sd = 5)
  temp_data2 <- rnorm(n_days, mean = 22, sd = 4)

  df_list <- list(
    region1 = data.frame(temp = temp_data1,
                         outcome = rpois(n_days, lambda = 10)),
    region2 = data.frame(temp = temp_data2,
                         outcome = rpois(n_days, lambda = 12))
  )

  # Create crossbasis objects with simplified structure
  cb_list <- list()
  model_list <- list()

  # Specify knots positions based on var_per
  var_per <- c(25, 50, 75)
  var_degree <- 2

  for(reg in names(df_list)) {
    # Create crossbasis with specific structure
    cb_list[[reg]] <- dlnm::crossbasis(
      df_list[[reg]]$temp,
      lag = 0,  # No lag for simplicity
      argvar = list(
        fun = "bs",  # Using bs (B-spline) instead of ns as it accepts degree parameter
        knots = quantile(df_list[[reg]]$temp, var_per/100),
        degree = var_degree
      ),
      arglag = list(fun = "lin")  # Simple linear lag
    )

    # Fit simple model
    model_list[[reg]] <- glm(
      outcome ~ cb_list[[reg]],
      family = poisson(),
      data = df_list[[reg]]
    )
  }

  # Test the function
  result <- dlnm_reduce_cumulative(
    df_list = df_list,
    var_per = var_per,
    var_degree = var_degree,
    cenper = 50,
    cb_list = cb_list,
    model_list = model_list
  )

  # Structure tests
  expect_type(result, "list")
  expect_length(result, 2)
  expect_true(is.matrix(result[[1]]))
  expect_true(is.list(result[[2]]))

  # Dimension tests
  expect_equal(nrow(result[[1]]), length(df_list))
  expect_equal(ncol(result[[1]]), length(var_per) + var_degree)
  expect_equal(names(result[[2]]), names(df_list))

  # Content tests
  expect_true(all(!is.na(result[[1]])))  # No NA in coefficients
  expect_true(all(sapply(result[[2]], is.matrix)))  # All vcov elements are matrices

  # Test with missing values
  df_list$region1$temp[1] <- NA
  expect_no_error(
    dlnm_reduce_cumulative(df_list, cb_list = cb_list, model_list = model_list)
  )
})


test_that("dlnm_meta_analysis performs meta-analysis correctly", {
  # Set seed for reproducibility
  set.seed(123)

  # Create test data
  n_regions <- 3
  n_days <- 100
  n_coef <- 4

  # Generate df_list with known properties
  df_list <- lapply(1:n_regions, function(i) {
    data.frame(
      temp = rnorm(n_days, mean = 20 + i*2, sd = 3),  # Different means for each region
      outcome = rpois(n_days, lambda = 10 + i)
    )
  })
  names(df_list) <- paste0("region", 1:n_regions)

  # Generate coefficient matrix with known structure
  coef_ <- matrix(
    rnorm(n_regions * n_coef, mean = 0, sd = 0.5),
    nrow = n_regions,
    ncol = n_coef,
    dimnames = list(names(df_list))
  )

  # Generate vcov list with positive definite matrices
  vcov_ <- lapply(1:n_regions, function(i) {
    m <- matrix(runif(n_coef^2), n_coef, n_coef)
    # Make symmetric positive definite
    m <- m %*% t(m)
    m
  })
  names(vcov_) <- names(df_list)

  # Create temporary directory for CSV output test
  output_csv <- file.path(temp_dir, "meta_model_stat_test_results.csv")
  on.exit(unlink(output_csv), add = TRUE)

  # Run function with various configurations
  result_basic <- dlnm_meta_analysis(df_list, coef_, vcov_)
  result_with_csv <- dlnm_meta_analysis(
    df_list, coef_, vcov_,
    save_csv = TRUE,
    output_folder_path = temp_dir
  )

  # Structure tests
  expect_type(result_basic, "list")
  expect_length(result_basic, 3)
  expect_s3_class(result_basic[[1]], "mixmeta")
  expect_type(result_basic[[2]], "list")
  expect_s3_class(result_basic[[3]], "data.frame")

  # Content tests
  expect_equal(names(result_basic[[2]]), names(df_list))
  expect_equal(nrow(result_basic[[3]]), 5)  # 5 test results
  expect_true(all(result_basic[[3]]$test == c(
    "temp_avg Wald p-value",
    "temp_range Wald p-value",
    "Cochran's Q test p-value",
    "I2 (%)",
    "AIC"
  )))

  # CSV output test
  expect_true(file.exists(output_csv))
})


test_that("dlnm_meta_analysis errors when save_csv = TRUE and output path is missing", {
  # Set seed for reproducibility
  set.seed(123)

  # Create test data
  n_regions <- 5
  n_days <- 80
  n_coef <- 3

  df_list <- lapply(seq_len(n_regions), function(i) {
    data.frame(
      temp = rnorm(n_days, mean = 20 + i, sd = 2),
      outcome = rpois(n_days, lambda = 8 + i)
    )
  })
  names(df_list) <- paste0("region", seq_len(n_regions))

  # Coefficient matrix
  coef_ <- matrix(
    rnorm(n_regions * n_coef, mean = 0, sd = 0.3),
    nrow = n_regions,
    ncol = n_coef,
    dimnames = list(names(df_list))
  )

  # Generate vcov list with positive definite matrices
  vcov_ <- lapply(seq_len(n_regions), function(i) {
    M <- matrix(rnorm(n_coef^2), n_coef, n_coef)
    crossprod(M) + diag(n_coef) * 0.5  # add diagonal for conditioning
  })
  names(vcov_) <- names(df_list)

  # Expect error due to output path not being specified
  expect_error(
    dlnm_meta_analysis(df_list, coef_, vcov_, save_csv = TRUE, output_folder_path = NULL),
    "Output path not specified",
    fixed = TRUE
  )
})


# Define blup for specific conditions of dlnm_min_suicide_temp (see below) to be used for testing other meta_analysis functions at a later date
blup <- list(region1 = list(blup = c(y1 = 1.07682186619024, y2 = -0.00124519149119406,
                                     y3 = 0.12735288433524, y4 = 0.458084787928137, y5 = -0.422585618213722
), vcov = c(2.0232841917723, 0.754492831583736, 1.74159115208854,
            1.87466328377305, 2.04230007103516, 0.754492831583736, 0.410519023044737,
            0.557550516102308, 0.51036725151191, 0.845289713293512, 1.74159115208854,
            0.557550516102308, 1.71140995311799, 1.62848871042226, 1.66517808066849,
            1.87466328377305, 0.51036725151191, 1.62848871042226, 2.31772250883277,
            1.60661695703015, 2.04230007103516, 0.845289713293512, 1.66517808066849,
            1.60661695703015, 2.34708542365521)),
region2 = list(blup = c(y1 = 0.161699455934293,
                        y2 = 0.016578758841848, y3 = 1.2861402850592, y4 = -0.29625496475898,
                        y5 = 0.450368244681151), vcov = c(1.42370486932474,
                                                          1.17642246467664, 1.54378824677196, 1.76216535256413, 1.53882330735721,
                                                          1.17642246467664, 1.651257618267, 1.5020680530722, 1.50233695780163,
                                                          1.41398180793844, 1.54378824677196, 1.5020680530722, 1.89737329128652,
                                                          1.81459335815769, 1.56672475939414, 1.76216535256413, 1.50233695780163,
                                                          1.81459335815769, 2.73157533502173, 2.31015628790682, 1.53882330735721,
                                                          1.41398180793844, 1.56672475939414, 2.31015628790682, 2.06260608467254
                        )),
region3 = list(blup = c(y1 = -0.198397612864354,
                        y2 = 0.539619576074286, y3 = -0.00293663579066807, y4 = -0.251856410739526,
                        y5 = -0.461415405436472), vcov = c(2.13504552128293,
                                                           1.49587297376017, 1.69179507509888, 1.55488388134613, 1.9602054436117,
                                                           1.49587297376017, 1.5916186371976, 1.38395985557859, 0.854041196449995,
                                                           1.63116434508889, 1.69179507509888, 1.38395985557859, 1.63075827687899,
                                                           1.14365623555706, 1.47896009671971, 1.55488388134613, 0.854041196449995,
                                                           1.14365623555706, 1.3810451389053, 1.2775932649805, 1.9602054436117,
                                                           1.63116434508889, 1.47896009671971, 1.2775932649805, 2.12470587378516
                        )
)
)


test_that("dlnm_min_mortality_temp correctly generates minpercreg", {
  # Set seed for reproducibility
  set.seed(3728)

  # Create test data
  n_regions <- 3
  n_days <- 100
  n_coef <- 5

  # Generate df_list with known properties
  df_list <- lapply(1:n_regions, function(i) {
    data.frame(
      temp = rnorm(n_days, mean = 20 + i*2, sd = 3),  # Different means for each region
      outcome = rpois(n_days, lambda = 10 + i)
    )
  })
  names(df_list) <- paste0("region", 1:n_regions)

  # Generate coefficient matrix with known structure
  coef_ <- matrix(
    rnorm(n_regions * n_coef, mean = 0, sd = 0.5),
    nrow = n_regions,
    ncol = n_coef,
    dimnames = list(names(df_list))
  )

  # Generate vcov list with positive definite matrices
  vcov_ <- lapply(1:n_regions, function(i) {
    m <- matrix(runif(n_coef^2), n_coef, n_coef)
    # Make symmetric positive definite
    m <- m %*% t(m)
    return(m)
  })
  names(vcov_) <- names(df_list)

  control_minpercreg <- c(region1 = 46L, region2 = 26L, region3 = 3L)

  # Test without meta_analysis
  test_minpercreg <- dlnm_min_mortality_temp(df_list,
                                         var_fun = "bs",
                                         var_per = c(25,50,75),
                                         var_degree = 2,
                                         blup = NULL,
                                         coef_,
                                         meta_analysis = FALSE,
                                         outcome_type = "suicide")
  expect_identical(test_minpercreg, control_minpercreg)

  # Test with meta_analysis
  test_minpercreg_meta <- dlnm_min_mortality_temp(df_list,
                                              var_fun = "bs",
                                              var_per = c(25,50,75),
                                              var_degree = 2,
                                              blup = blup,
                                              coef_,
                                              meta_analysis = TRUE,
                                              outcome_type = "suicide")
  expect_identical(test_minpercreg_meta, control_minpercreg)

})


test_that("dlnm_predict_nat produces expected output", {
  # Set seed for reproducibility
  set.seed(123)

  # Create mock national data
  n_days <- 10
  national_df <- data.frame(
    date = as.Date("2000-01-01") + 0:(n_days - 1),
    region = rep("National", n_days),
    temp = rnorm(n_days, mean = 15, sd = 3),
    hum = runif(n_days, 70, 90),
    sun = runif(n_days, 2, 4),
    rainfall = runif(n_days, 0, 10),
    population = rep(2600000, n_days),
    suicides = rpois(n_days, lambda = 1),
    year = rep(2000, n_days),
    month = rep(1, n_days),
    dow = c("Sat", "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun", "Mon"),
    stratum = paste("National:2000:1", c("Sat","Sun","Mon","Tue","Wed","Thu","Fri","Sat","Sun","Mon"), sep=":"),
    ind = 1:n_days
  )

  # df_list with National data
  df_list <- list(National = national_df)

  # Mock mmpredall
  mmpredall <- list(
    fit = c(y1 = -0.02, y2 = 0.12, y3 = 0.22, y4 = -0.52, y5 = 1.57),
    vcov = {
      m <- matrix(runif(25, 8, 20), nrow = 5)
      dimnames(m) <- list(paste0("y", 1:5), paste0("y", 1:5))
      m
    }
  )

  # Mock minpercreg
  minpercreg <- c(National = 37L)

  # Empty pred_list
  pred_list <- list()

  # Run function
  result <- suppressWarnings(dlnm_predict_nat(
    df_list = df_list,
    var_fun = "bs",
    var_per = c(25, 50, 75),
    var_degree = 2,
    minpercreg = minpercreg,
    mmpredall = mmpredall,
    pred_list = pred_list,
    country = "National"
  ))

  # Structure tests
  expect_type(result, "list")
  expect_named(result, "National")
  expect_s3_class(result$National, "crosspred")

  # Check required components in crosspred object
  required_components <- c("predvar", "cen", "lag", "bylag", "coefficients", "vcov", "matfit", "matse",
                           "allfit", "allse", "matRRfit", "matRRlow", "matRRhigh", "allRRfit", "allRRlow", "allRRhigh",
                           "ci.level", "model.class", "model.link")
  expect_true(all(required_components %in% names(result$National)))

  # Check prediction range matches input temperature range (within tolerance)
  expect_equal(min(result$National$predvar), min(national_df$temp, na.rm = TRUE), tolerance = 0.1)
  expect_equal(max(result$National$predvar), max(national_df$temp, na.rm = TRUE), tolerance = 0.1)

  # Check centering value
  expected_cen <- stats::quantile(national_df$temp, minpercreg["National"] / 100, na.rm = TRUE)
  expect_equal(result$National$cen, expected_cen)

  # Check lengths of prediction vectors
  expect_equal(length(result$National$allfit), length(result$National$predvar))
  expect_equal(length(result$National$allse), length(result$National$predvar))
})


test_that("dlnm_power_list produces expected output", {
  # Create sample data for two regions
  set.seed(123)
  n_regions <- 2
  n_days <- 10

  # Create df_list with temperature and outcome data
  df_list <- lapply(1:n_regions, function(i) {
    data.frame(
      temp = rnorm(n_days, mean = 20 + i * 2, sd = 3),
      outcome = rpois(n_days, lambda = 10 + i)
    )
  })
  names(df_list) <- paste0("region", 1:n_regions)

  # Create pred_list with prediction values for each region
  pred_list <- lapply(1:n_regions, function(i) {
    data.frame(
      predvar = seq(15, 30, length.out = 5),
      allfit = rnorm(5, mean = 0.2 * i, sd = 0.05),
      allse = runif(5, 0.03, 0.07)
    )
  })
  names(pred_list) <- names(df_list)

  # Create minpercreg vector for percentile thresholds
  minpercreg <- setNames(sample(20:60, n_regions, replace = TRUE), names(df_list))

  # Run the function
  result <- dlnm_power_list(df_list, pred_list, minpercreg, compute_low = FALSE)

  # Tests
  expect_type(result, "list")
  expect_named(result, names(df_list))
  expect_true(all(sapply(result, is.data.frame)))

  # Check required columns exist in each region's output
  required_cols <- c("region", "temperature", "cen", "log_rr", "se", "power")
  for (region in names(result)) {
    df <- result[[region]]
    expect_true(all(required_cols %in% names(df)))
    expect_true(all(df$power >= 0 & df$power <= 100))  # Power should be between 0 and 100
    expect_true(all(df$temperature >= round(quantile(df_list[[region]]$temp, 97.5 / 100), 1)))  # Threshold check
  }

  # Check that 'cen' matches minpercreg percentile (ignore names)
  for (region in names(df_list)) {
    expected_cen <- unname(round(quantile(df_list[[region]]$temp, minpercreg[region] / 100, na.rm = TRUE), 1))
    actual_cen <- unique(result[[region]]$cen)
    expect_equal(actual_cen, expected_cen)
  }

  # Check rounding of log_rr and se
  for (region in names(result)) {
    df <- result[[region]]
    expect_true(all(df$log_rr == round(df$log_rr, 2)))
    expect_true(all(df$se == round(df$se, 2)))
  }

  # Check power is rounded to 1 decimal place
  for (region in names(result)) {
    df <- result[[region]]
    expect_true(all(df$power == round(df$power, 1)))
  }
})


test_that("dlnm_power_list handles edge cases correctly", {
  # test Single region
  set.seed(456)
  df_list_single <- list(region1 = data.frame(temp = rnorm(10, 20, 2)))
  pred_list_single <- list(region1 = data.frame(predvar = seq(18, 25, length.out = 3),
                                                allfit = c(0.1, 0.2, 0.3),
                                                allse = c(0.05, 0.06, 0.07)))
  minpercreg_single <- c(region1 = 50)

  # Run function for single region
  result_single <- dlnm_power_list(df_list_single, pred_list_single, minpercreg_single, compute_low = FALSE)
  expect_type(result_single, "list")
  expect_length(result_single, 1)
  expect_named(result_single, "region1")
  expect_true(all(c("region", "temperature", "cen", "log_rr", "se", "power") %in% names(result_single$region1)))

  # Case 2: Missing values in temperature
  df_list_na <- list(region1 = data.frame(temp = c(NA, 20, 22, NA, 25)))
  pred_list_na <- list(region1 = data.frame(predvar = c(20, 22, 25),
                                            allfit = c(0.1, 0.15, 0.2),
                                            allse = c(0.05, 0.06, 0.07)))
  minpercreg_na <- c(region1 = 50)

  # Run function with NA values
  result_na <- dlnm_power_list(df_list_na, pred_list_na, minpercreg_na, compute_low = FALSE)
  expect_type(result_na, "list")
  expect_true(all(!is.na(result_na$region1$cen)))  # cen should compute despite NA values

  # Empty lists
  df_list_empty <- list()
  pred_list_empty <- list()
  minpercreg_empty <- c()

  # Run function with empty inputs
  result_empty <- dlnm_power_list(df_list_empty, pred_list_empty, minpercreg_empty, compute_low = FALSE)
  expect_type(result_empty, "list")
  expect_length(result_empty, 0)  # Should return empty list without error
})
