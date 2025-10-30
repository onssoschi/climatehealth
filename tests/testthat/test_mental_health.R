# Tests for diseases_shared.R
# source("../../R/mental_health.R")

# Test mh_read_and_format_data
test_that("Test mh_read_and_format_data", {

  mock_check_file_extension <- function(data_path, extension, param_nm) {
    identical(tools::file_ext(data_path), extension)
  }

  mock_read_input_data <- function(data_path) {
    df <- data.frame(
      date_column = c("2023-01-01", "02-01-2023", "2023-01-03", "04-01-2023", "2023-01-05"),
      region = c("North", "South", "East", "West", "Central"),
      temp = c(23.5, 25.1, 22.8, 24.3, 21.9),
      health_outcomes = c(10.2, 12.5, 9.8, 11.1, 8.7),
      population = c(1000L, 1200L, 950L, 1100L, 1050L)
    )
  }

  local_mocked_bindings(
    check_file_extension = mock_check_file_extension,
    read_input_data = mock_read_input_data
    )
  {
    control_df <- data.frame(
      date = structure(c(19362, 19360, 19358, -718778, -718048), class = "Date"),
      region = structure(c(1L, 2L, 3L, 4L, 5L), levels = c("Central","East", "North", "South", "West"), class = "factor"),
      temp = c(21.9, 22.8, 23.5, 25.1, 24.3),
      suicides = c(8.7, 9.8, 10.2, 12.5, 11.1),
      population = c(1050L, 950L, 1000L, 1200L, 1100L),
      year = structure(c(3L, 3L, 3L, 1L, 2L), levels = c("2", "4", "2023"), class = "factor"),
      month = structure(c(1L, 1L, 1L, 1L, 1L), levels = "1", class = "factor"),
      dow = structure(c(5L, 3L, 1L, 1L, 3L), levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"), class = c("ordered", "factor")),
      stratum = structure(c(19L, 38L, 57L, 64L, 94L),
                          levels = c("Central:2:1:Sun","Central:2:1:Mon", "Central:2:1:Tue",
                                     "Central:2:1:Wed","Central:2:1:Thu", "Central:2:1:Fri",
                                     "Central:2:1:Sat", "Central:4:1:Sun", "Central:4:1:Mon",
                                     "Central:4:1:Tue", "Central:4:1:Wed", "Central:4:1:Thu",
                                     "Central:4:1:Fri", "Central:4:1:Sat", "Central:2023:1:Sun",
                                     "Central:2023:1:Mon", "Central:2023:1:Tue", "Central:2023:1:Wed",
                                     "Central:2023:1:Thu", "Central:2023:1:Fri", "Central:2023:1:Sat",
                                     "East:2:1:Sun", "East:2:1:Mon", "East:2:1:Tue", "East:2:1:Wed",
                                     "East:2:1:Thu", "East:2:1:Fri", "East:2:1:Sat", "East:4:1:Sun",
                                     "East:4:1:Mon", "East:4:1:Tue", "East:4:1:Wed", "East:4:1:Thu",
                                     "East:4:1:Fri", "East:4:1:Sat", "East:2023:1:Sun",
                                     "East:2023:1:Mon", "East:2023:1:Tue", "East:2023:1:Wed",
                                     "East:2023:1:Thu", "East:2023:1:Fri", "East:2023:1:Sat",
                                     "North:2:1:Sun", "North:2:1:Mon", "North:2:1:Tue",
                                     "North:2:1:Wed", "North:2:1:Thu", "North:2:1:Fri",
                                     "North:2:1:Sat", "North:4:1:Sun", "North:4:1:Mon",
                                     "North:4:1:Tue", "North:4:1:Wed", "North:4:1:Thu",
                                     "North:4:1:Fri", "North:4:1:Sat", "North:2023:1:Sun",
                                     "North:2023:1:Mon", "North:2023:1:Tue", "North:2023:1:Wed",
                                     "North:2023:1:Thu", "North:2023:1:Fri", "North:2023:1:Sat",
                                     "South:2:1:Sun", "South:2:1:Mon", "South:2:1:Tue",
                                     "South:2:1:Wed", "South:2:1:Thu", "South:2:1:Fri",
                                     "South:2:1:Sat", "South:4:1:Sun", "South:4:1:Mon",
                                     "South:4:1:Tue", "South:4:1:Wed", "South:4:1:Thu",
                                     "South:4:1:Fri", "South:4:1:Sat", "South:2023:1:Sun",
                                     "South:2023:1:Mon", "South:2023:1:Tue", "South:2023:1:Wed",
                                     "South:2023:1:Thu", "South:2023:1:Fri", "South:2023:1:Sat",
                                     "West:2:1:Sun", "West:2:1:Mon", "West:2:1:Tue", "West:2:1:Wed",
                                     "West:2:1:Thu", "West:2:1:Fri", "West:2:1:Sat", "West:4:1:Sun",
                                     "West:4:1:Mon", "West:4:1:Tue", "West:4:1:Wed", "West:4:1:Thu",
                                     "West:4:1:Fri", "West:4:1:Sat", "West:2023:1:Sun",
                                     "West:2023:1:Mon", "West:2023:1:Tue", "West:2023:1:Wed",
                                     "West:2023:1:Thu", "West:2023:1:Fri", "West:2023:1:Sat"),
                          class = "factor")
    )

    control_df <- control_df %>% dplyr::mutate(ind = tapply(.data$suicides, .data$stratum, sum)[.data$stratum])

    control_test_list <- aggregate_by_column(control_df, "region")

    mh_test_list <- mh_read_and_format_data("mock_file.csv",
                              "date_column",
                              "region",
                              "temp",
                              "health_outcomes",
                              "pop")
    }

    expect_identical(mh_test_list, control_test_list)
  })



# Test mh_pop_totals
test_that("Test mh_pop_totals", {
  mh_pop_totals_df <- data.frame(
    region = structure(c(1L, 1L, 3L, 2L, 3L, 1L, 2L, 1L), levels = c("Central","East", "North"), class = "factor"),
    population = c(1050L, 950L, 1000L, 1200L, 1100L, 850L, 1150L, 1300L),
    year = structure(c(1L, 1L, 2L, 3L, 2L, 3L, 1L, 2L), levels = c("2022", "2024", "2023"), class = "factor"))
  mh_pop_totals_list <- aggregate_by_column(mh_pop_totals_df, "region")


  mh_pop_totals_control_list <- list(
    Central = structure(list(
      year = structure(1:3, levels = c("2022", "2024", "2023"), class = "factor"),
      population = c(1000, 1300, 850)), row.names = c(NA, -3L), class = "data.frame"),
    East = structure(list(
      year = structure(c(1L, 3L), levels = c("2022", "2024", "2023"), class = "factor"),
      population = c(1150, 1200)), row.names = c(NA, -2L), class = "data.frame"),
    North = structure(list(
      year = structure(2L, levels = c("2022", "2024", "2023"), class = "factor"),
      population = 1050), row.names = c(NA, -1L), class = "data.frame"))

  mh_pop_totals_national_control_list <- append(mh_pop_totals_control_list, list(
    National = structure(list(
      year = structure(1:3, levels = c("2022", "2024", "2023"), class = "factor"),
      population = c(2150, 2350, 2050)), row.names = c(NA, -3L), class = "data.frame")))

  expect_equal(mh_pop_totals(mh_pop_totals_list, meta_analysis = FALSE), mh_pop_totals_control_list, label = "mh_pop_totals(mh_pop_totals_list, meta_analysis = FALSE)")
  expect_equal(mh_pop_totals(mh_pop_totals_list, meta_analysis = TRUE), mh_pop_totals_national_control_list, label = "mh_pop_totals(mh_pop_totals_list, meta_analysis = TRUE)")
})

test_that("mh_create_crossbasis creates correct cross-basis matrices", {
  # Create sample data with sufficient observations for lag
  sample_df <- data.frame(
    date = seq.Date(from = as.Date("2023-01-01"),
                    to = as.Date("2023-01-10"),
                    by = "day"),
    temp = c(20, 22, 25, 23, 21, 24, 26, 22, 20, 23),
    suicides = c(8, 9, 10, 9, 8, 11, 10, 9, 8, 10)
  )

  # Create sample dataframe list with two regions
  sample_df_list <- list(
    "Region1" = sample_df,
    "Region2" = sample_df
  )

  # Run the function
  result <- mh_create_crossbasis(sample_df_list)

  # Tests
  expect_type(result, "list")
  expect_equal(length(result), 2)  # Should have two regions
  expect_equal(names(result), c("Region1", "Region2"))

  # Check structure of cross-basis objects
  for(reg in names(result)) {
    expect_s3_class(result[[reg]], "crossbasis")
    expect_equal(nrow(result[[reg]]), nrow(sample_df_list[[reg]]))
    expect_equal(attr(result[[reg]], "df"),
                 c(length(attr(result[[reg]], "argvar")$knots) + attr(result[[reg]], "argvar")$degree,
                   length(attr(result[[reg]], "arglag")$breaks) + 1))
  }

  # Test with custom parameters
  custom_result <- mh_create_crossbasis(
    sample_df_list,
    var_fun = "bs",
    var_degree = 3,
    var_per = c(10, 50, 90),
    lag_fun = "strata",
    lag_breaks = 2,
    lag_days = 3
  )

  expect_equal(length(custom_result), 2)
  expect_equal(attr(custom_result$Region1, "argvar")$degree, 3)
  expect_equal(attr(custom_result$Region1, "lag")[2], 3)
})

test_that("mh_model_combo_res creates correct model combinations and diagnostics", {
  # Create sample data
  sample_df <- data.frame(
    date = seq.Date(from = as.Date("2023-01-01"),
                    to = as.Date("2023-01-30"),
                    by = "day"),
    temp = rnorm(30, mean = 23, sd = 2),
    suicides = rpois(30, lambda = 10),
    humidity = rnorm(30, mean = 70, sd = 10),
    pollution = rnorm(30, mean = 50, sd = 15),
    stratum = factor(rep(1:6, each = 5)),
    ind = rep(1, 30)
  )

  # Create input lists
  df_list <- list(
    "Region1" = sample_df,
    "Region2" = sample_df
  )

  # Create mock cross-basis matrix directly
  mock_cb <- matrix(rnorm(30 * 6), nrow = 30)  # 30 rows (days) x 6 cols (basis functions)
  colnames(mock_cb) <- paste0("v", 1:6)
  class(mock_cb) <- c("crossbasis", "matrix")

  # Add necessary attributes that gnm might use
  attr(mock_cb, "df") <- c(3, 2)  # Degrees of freedom for var and lag
  attr(mock_cb, "range") <- c(min(sample_df$temp), max(sample_df$temp))
  attr(mock_cb, "lag") <- c(0, 3)

  # Create mock cb_list
  cb_list <- list(
    "Region1" = mock_cb,
    "Region2" = mock_cb
  )

  independent_vars <- c("humidity", "pollution")

  # Run function
  results <- mh_model_combo_res(df_list, cb_list, independent_vars)

  # Test structure
  expect_type(results, "list")
  expect_length(results, 2)
  expect_s3_class(results[[1]], "data.frame")
  expect_type(results[[2]], "list")

  # Test QAIC results
  qaic_results <- results[[1]]
  expect_equal(nrow(qaic_results), 8)  # 2 regions * 4 combinations (none, hum, pol, both)
  expect_named(qaic_results, c("region", "formula", "disp", "qaic"))

  # Test formulas generated - trim whitespace and verify all expected combinations exist
  expected_formulas <- c(
    "suicides ~ cb",  # Base case (equivalent to NULL independent_cols)
    "suicides ~ cb + humidity_ns",
    "suicides ~ cb + pollution_ns",
    "suicides ~ cb + humidity_ns + pollution_ns"
  )
  actual_formulas <- unique(trimws(qaic_results$formula))
  expect_setequal(actual_formulas, expected_formulas)

  # Verify base formula exists for each region
  region_base_formulas <- qaic_results[trimws(qaic_results$formula) == "suicides ~ cb", ]
  expect_equal(nrow(region_base_formulas), 2)  # Should have base formula for both regions
  expect_setequal(region_base_formulas$region, c("Region1", "Region2"))

  # Test residuals structure
  residuals_list <- results[[2]]
  expect_named(residuals_list, c("Region1", "Region2"))

  # Test first region's residuals
  region1_residuals <- residuals_list[["Region1"]]
  expect_type(region1_residuals, "list")
  expect_length(region1_residuals, 4)  # 4 model combinations

  # Test residuals content
  first_model_residuals <- region1_residuals[[1]]
  expect_s3_class(first_model_residuals, "data.frame")
  expect_named(first_model_residuals,
               c("region", "formula", "fitted", "residuals"))
})

test_that("mh_vif calculates variance inflation factors correctly", {
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
  results <- mh_vif(df_list, independent_vars)

  # Test structure
  expect_type(results, "list")
  expect_named(results, c("Region1", "Region2"))

  # Test each region's results
  for (reg in names(results)) {
    region_vif <- results[[reg]]

    # Check structure
    expect_s3_class(region_vif, "data.frame")
    expect_named(region_vif, c("variable", "vif"))
    expect_equal(nrow(region_vif), 4)  # temp + 3 independent vars

    # Check all variables are present
    expected_vars <- c("temp", independent_vars)
    expect_setequal(region_vif$variable, expected_vars)

    # Check VIF values are numeric and positive
    expect_true(all(is.numeric(region_vif$vif)))
    expect_true(all(region_vif$vif > 0))

    # Check that wind (strongly correlated) has higher VIF than pollution (weakly correlated)
    wind_vif <- region_vif$vif[region_vif$variable == "wind"]
    pollution_vif <- region_vif$vif[region_vif$variable == "pollution"]
    expect_gt(wind_vif, pollution_vif)
  }

  # Test with single independent variable
  single_var_results <- mh_vif(df_list, "pollution")
  for (reg in names(single_var_results)) {
    expect_equal(nrow(single_var_results[[reg]]), 2)  # temp and pollution
    expect_setequal(single_var_results[[reg]]$variable, c("temp", "pollution"))
  }
})

test_that("mh_model_validation performs complete model validation", {
  # Setup test data
  set.seed(123)
  n_days <- 100

  # Create sample data
  sample_df <- data.frame(
    date = seq.Date(from = as.Date("2023-01-01"),
                    length.out = n_days,
                    by = "day"),
    temp = rnorm(n_days, mean = 23, sd = 2),
    suicides = rpois(n_days, lambda = 10),
    humidity = rnorm(n_days, mean = 70, sd = 10),
    pollution = rnorm(n_days, mean = 50, sd = 15),
    stratum = factor(rep(1:20, each = 5)),
    ind = rep(1, n_days)
  )

  # Create df_list
  df_list <- list(
    "Region1" = sample_df,
    "Region2" = sample_df
  )

  # Create cross-basis matrices
  mock_cb <- matrix(rnorm(n_days * 6), nrow = n_days)
  colnames(mock_cb) <- paste0("v", 1:6)
  class(mock_cb) <- c("crossbasis", "matrix")
  attr(mock_cb, "df") <- c(3, 2)
  attr(mock_cb, "range") <- range(sample_df$temp)
  attr(mock_cb, "lag") <- c(0, 3)

  cb_list <- list(
    "Region1" = mock_cb,
    "Region2" = mock_cb
  )

  independent_vars <- c("humidity", "pollution")
  temp_dir <- tempdir()
  validation_dir <- file.path(temp_dir, "model_validation")

  # Ensure cleanup runs even if test fails
  on.exit({
    if (dir.exists(validation_dir)) {
      unlink(validation_dir, recursive = TRUE)
    }
  })

  results_basic <- mh_model_validation(
    df_list = df_list,
    cb_list = cb_list,
    independent_cols = independent_vars,
    save_fig = TRUE,  # Explicitly set to FALSE
    save_csv = TRUE,
    output_folder_path = temp_dir
  )

  # Structure tests
  expect_type(results_basic, "list")
  expect_length(results_basic, 4)

  # QAIC results tests
  expect_s3_class(results_basic[[1]], "data.frame")
  expect_named(results_basic[[1]], c("region", "formula", "disp", "qaic"))

  # QAIC summary tests
  expect_s3_class(results_basic[[2]], "data.frame")
  expect_named(results_basic[[2]], c("formula", "mean_disp", "mean_qaic"))

  # VIF results tests
  expect_s3_class(results_basic[[3]], "data.frame")
  expect_named(results_basic[[3]], c("Region", "variable", "vif"))

  # VIF summary tests
  expect_s3_class(results_basic[[4]], "data.frame")
  expect_named(results_basic[[4]], c("variable", "mean_vif"))

  # Check if files were created
  expect_true(dir.exists(validation_dir))

  expected_files <- c(
    "qaic_results.csv",
    "qaic_summary.csv",
    "vif_results.csv",
    "vif_summary.csv"
  )

  for (file in expected_files) {
    expect_true(
      file.exists(file.path(validation_dir, file)),
      info = paste("Missing file:", file)
    )
  }

  # Check if plot directories were created for each region
  for (reg in names(df_list)) {
    reg_path <- file.path(validation_dir, reg)
    expect_true(dir.exists(reg_path))

    # Check for plot files
    expect_true(file.exists(file.path(reg_path, paste0(reg, "_residuals_timeseries.pdf"))))
    expect_true(file.exists(file.path(reg_path, paste0(reg, "_residuals_fitted.pdf"))))
    expect_true(file.exists(file.path(reg_path, paste0(reg, "_qq_plot.pdf"))))
  }

  #Single region case
  single_region_results <- mh_model_validation(
    df_list = list("Region1" = sample_df),
    cb_list = list("Region1" = mock_cb),
    independent_cols = independent_vars,
    save_fig = TRUE,
    save_csv = TRUE,
    output_folder_path = temp_dir
  )

  expect_null(single_region_results[[2]])  # QAIC summary should be NULL
  expect_null(single_region_results[[4]])  # VIF summary should be NULL
})
