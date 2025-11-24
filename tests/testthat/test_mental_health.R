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
      pop = c(1000L, 1200L, 950L, 1100L, 1050L)
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

  expect_equal(mh_pop_totals(mh_pop_totals_list, meta_analysis = FALSE),
               mh_pop_totals_control_list,
               label = "mh_pop_totals(mh_pop_totals_list, meta_analysis = FALSE)")
  expect_equal(mh_pop_totals(mh_pop_totals_list, meta_analysis = TRUE),
               mh_pop_totals_national_control_list,
               label = "mh_pop_totals(mh_pop_totals_list, meta_analysis = TRUE)")
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
  expect_named(results_basic[[3]], c("region", "variable_combo", "variable", "vif"))

  # VIF summary tests
  expect_s3_class(results_basic[[4]], "data.frame")
  expect_named(results_basic[[4]], c("variable_combo", "variable", "mean_vif"))

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

test_that("mh_casecrossover_dlnm fits case-crossover DLNM models correctly", {
  # Setup test data with sufficient observations
  set.seed(123)
  n_days <- 100

  # Create sample data with relevant variables
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

  # Create regional data structure
  df_list <- list(
    "Region1" = sample_df,
    "Region2" = sample_df
  )

  # Create mock cross-basis matrix for DLNM
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

  # Test 1: Basic functionality with multiple control variables
  control_cols <- c("humidity", "pollution")
  models <- mh_casecrossover_dlnm(df_list, control_cols, cb_list)

  # Check basic structure
  expect_type(models, "list")
  expect_named(models, c("Region1", "Region2"))
  expect_s3_class(models$Region1, "gnm")

  # Check model components for each region
  for (reg in names(models)) {
    model <- models[[reg]]
    formula_str <- paste(deparse(model$formula), collapse = " ")

    # Check formula includes all control variables as splines
    expect_true(all(sapply(control_cols,
                           function(x) grepl(paste0("splines::ns\\(", x, ",\\s*df\\s*=\\s*3"), formula_str))))

    # Verify model family
    expect_equal(model$family$family, "quasipoisson")

    # Check data dimensions
    expect_equal(nrow(model$data), sum(df_list[[reg]]$ind > 0))

    # Verify spline terms in model
    terms <- attr(terms(model), "term.labels")
    expect_true(all(sapply(control_cols,
                           function(x) any(grepl(paste0("ns\\(", x), terms)))))
  }

  # Test 2: Verify NULL control variables case
  models_no_controls <- mh_casecrossover_dlnm(df_list, NULL, cb_list)
  expect_type(models_no_controls, "list")
  for (reg in names(models_no_controls)) {
    model <- models_no_controls[[reg]]
    formula_str <- paste(deparse(model$formula), collapse = " ")
    expect_equal(formula_str, "suicides ~ cb")
  }

  # Test 3: Verify single control variable handling
  single_control <- "humidity"
  models_single <- mh_casecrossover_dlnm(df_list, single_control, cb_list)

  expect_type(models_single, "list")
  for (reg in names(models_single)) {
    model <- models_single[[reg]]
    formula_str <- paste(deparse(model$formula), collapse = " ")
    expect_true(grepl(paste0("splines::ns\\(", single_control, ",\\s*df\\s*=\\s*3"),
                      formula_str))
  }

  # Test 4: Verify error handling for invalid input
  expect_error(
    mh_casecrossover_dlnm(df_list, control_cols = 123, cb_list),
    "'control_cols' expected a vector of strings or a string"
  )

  # Test 5: Verify model fit properties
  for (reg in names(models)) {
    model <- models[[reg]]
    # Check model convergence
    expect_true(!is.null(coef(model)))
    # Verify residuals computation
    expect_true(!is.null(residuals(model)))
    # Check fitted values are valid for Poisson family
    expect_true(all(fitted(model) > 0))
  }
})

test_that("mh_reduce_cumulative produces expected output with valid inputs", {
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
  result <- mh_reduce_cumulative(
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
  expect_error(
    mh_reduce_cumulative(df_list, cb_list = cb_list, model_list = model_list),
    NA
  )
})

test_that("mh_meta_analysis performs meta-analysis correctly", {
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
    return(m)
  })
  names(vcov_) <- names(df_list)

  # Create temporary directory for CSV output test
  temp_dir <- tempdir()

  # Run function with various configurations
  result_basic <- mh_meta_analysis(df_list, coef_, vcov_)
  result_with_csv <- mh_meta_analysis(df_list, coef_, vcov_,
                                      save_csv = TRUE,
                                      output_folder_path = temp_dir)

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
    "Temp_avg Wald p-value",
    "Temp_range Wald p-value",
    "Cochrane Q test p-value",
    "I2 (%)",
    "AIC"
  )))

  # CSV output test
  expect_true(file.exists(file.path(temp_dir, "meta_model_stat_test_results.csv")))

  # Error handling tests
  expect_error(
    mh_meta_analysis(df_list, coef_, vcov_, save_csv = TRUE),
    "Output path not specified"
  )

  # Clean up
  unlink(file.path(temp_dir, "meta_model_stat_test_results.csv"))
})

test_that("mh_min_suicide_temp correctly generates minpercreg", {
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

  test_minpercreg <- mh_min_suicide_temp(df_list,
                                  var_fun = "bs",
                                  var_per = c(25,50,75),
                                  var_degree = 2,
                                  blup = blup,
                                  coef_,
                                  meta_analysis = FALSE)

  expect_identical(test_minpercreg, control_minpercreg)
})

test_that("mh_predict_reg produces expected output", {
  # Create test data
  n_regions <- 2
  n_days <- 10
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

  minpercreg <- c(region1 = 46L, region2 = 26L, region3 = 34L)

  # run function with suppressed warnings, as x values beyond boundary knots not a concern
  test_mh_predict_reg <- suppressWarnings(mh_predict_reg(df_list,
                                           var_fun = "bs",
                                           var_per = c(25,50,75),
                                           var_degree = 2,
                                           minpercreg,
                                           blup,
                                           coef_,
                                           vcov_,
                                           meta_analysis = FALSE)
  )

    #
    expect_type(test_mh_predict_reg, "list")

    expect_named(test_mh_predict_reg, names(df_list))

    expect_true(all(sapply(test_mh_predict_reg, function(x) inherits(x, "crosspred"))))

    required_components <- c("predvar", "cen", "lag", "bylag", "coefficients", "vcov", "matfit", "matse",
                             "allfit", "allse", "matRRfit", "matRRlow", "matRRhigh", "allRRfit", "allRRlow", "allRRhigh",
                             "ci.level", "model.class", "model.link")

    for (i in seq(from = 1, to = length(test_mh_predict_reg), by = 1)) {
      expect_true(all(required_components %in% names(test_mh_predict_reg[[i]])))
    }

    # model only predicts to within 0.1 degrees so tolerance set to that
    for (region in names(df_list)) {
      region_data <- df_list[[region]]
      pred <- test_mh_predict_reg[[region]]
      expect_equal(min(pred$predvar), min(region_data$temp, na.rm = TRUE), tolerance=0.1)
      expect_equal(max(pred$predvar), max(region_data$temp, na.rm = TRUE), tolerance=0.1)
    }

    for (region in names(df_list)) {
      region_data <- df_list[[region]]
      expected_cen <- quantile(region_data$temp, minpercreg[region]/100, na.rm = TRUE)
      actual_cen <- test_mh_predict_reg[[region]]$cen
      expect_equal(actual_cen, expected_cen)
    }

    for (pred in test_mh_predict_reg) {
      expect_equal(length(pred$allfit), length(pred$predvar))
      expect_equal(length(pred$allse), length(pred$predvar))
    }
})

test_that("test mh_add_national_data", {
  # Set seed for reproducibility
  set.seed(123)

  # Parameters
  n_regions <- 3
  n_days <- 30
  start_date <- as.Date("2020-01-01")
  dates <- seq(start_date, by = "day", length.out = n_days)
  years <- lubridate::year(dates)

  # Generate df_list
  df_list <- lapply(1:n_regions, function(i) {
    data.frame(
      date = dates,
      year = years,
      temp = rnorm(n_days, mean = 15 + i * 2, sd = 5),
      suicides = rpois(n_days, lambda = 5 + i),
      population = sample(100000:200000, n_days, replace = TRUE),
      region = paste0("region", i)
    )
  })
  names(df_list) <- paste0("region", 1:n_regions)

  # Generate pop_list
  pop_list <- lapply(names(df_list), function(region) {
    data.frame(
      year = unique(years),
      population = sample(100000:200000, length(unique(years)), replace = TRUE)
    )
  })
  names(pop_list) <- names(df_list)

  # Add national population
  pop_list[["National"]] <- data.frame(
    year = unique(years),
    population = sample(500000:600000, length(unique(years)), replace = TRUE)
  )

  # Create a temporary national dataset to determine basis dimension
  temp_nat <- do.call(rbind, df_list)
  temp_cb <- dlnm::onebasis(
    x = quantile(temp_nat$temp, 1:99 / 100),
    fun = "bs",
    knots = quantile(temp_nat$temp, c(0.25, 0.5, 0.75)),
    degree = 2,
    Boundary.knots = range(temp_nat$temp)
  )
  n_basis <- ncol(temp_cb)

  # Generate synthetic coefficients and vcov for mvmeta
  coef_mat <- matrix(rnorm(n_regions * n_basis), ncol = n_basis)
  vcov_list <- replicate(n_regions, diag(n_basis), simplify = FALSE)
  names(vcov_list) <- names(df_list)

  # Fit mvmeta model
  mm <- mvmeta::mvmeta(coef_mat, vcov_list, method = "reml")

  # Generate cb_list
  cb_list <- lapply(df_list, function(df) {
    dlnm::crossbasis(
      df$temp,
      lag = 2,
      argvar = list(fun = "bs", knots = quantile(df$temp, c(0.25, 0.5, 0.75))),
      arglag = list(fun = "strata", breaks = 1)
    )
  })

  # Generate minpercreg
  minpercreg <- setNames(sample(20:80, n_regions, replace = TRUE), names(df_list))

  result <- mh_add_national_data(df_list, pop_list, cb_list = cb_list, mm = mm, minpercreg = minpercreg)

  expect_type(result, "list")
  expect_length(result, 4)
  expect_named(result, NULL)  # unnamed list

  df_out <- result[[1]]
  expect_true("National" %in% names(df_out))
  nat_df <- df_out[["National"]]

  expect_s3_class(nat_df, "data.frame")
  expect_true(all(c("date", "temp", "suicides", "population", "year", "month", "region") %in% names(nat_df)))
  expect_equal(nrow(nat_df), length(unique(df_list[[1]]$date)))
  expect_true(all(nat_df$region == "National"))

  cb_out <- result[[2]]
  expect_true("National" %in% names(cb_out))
  nat_cb <- cb_out[["National"]]

  expect_s3_class(nat_cb, "crossbasis")
  expect_true(all(c("argvar", "arglag") %in% names(attributes(nat_cb))))
  expect_equal(attr(nat_cb, "argvar")$fun, "bs")
  expect_equal(attr(nat_cb, "arglag")$fun, "strata")

  minperc_out <- result[[3]]
  expect_true("National" %in% names(minperc_out))
  expect_true(is.numeric(minperc_out[["National"]]))
  expect_true(minperc_out[["National"]] >= 1 && minperc_out[["National"]] <= 50)

  mmpredall <- result[[4]]

  expect_type(mmpredall, "list")
  expect_true(all(c("fit", "vcov") %in% names(mmpredall)))
  expect_true(is.numeric(mmpredall$fit))
  expect_true(is.matrix(mmpredall$vcov))
  expect_equal(length(mmpredall$fit), ncol(mmpredall$vcov))

})

test_that("mh_predict_nat produces expected output", {
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
  result <- suppressWarnings(mh_predict_nat(
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

test_that("mh_power_list produces expected output", {
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
  result <- mh_power_list(df_list, pred_list, minpercreg, attr_thr = 97.5)

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

test_that("mh_power_list handles edge cases correctly", {
  # test Single region
  set.seed(456)
  df_list_single <- list(region1 = data.frame(temp = rnorm(10, 20, 2)))
  pred_list_single <- list(region1 = data.frame(predvar = seq(18, 25, length.out = 3),
                                                allfit = c(0.1, 0.2, 0.3),
                                                allse = c(0.05, 0.06, 0.07)))
  minpercreg_single <- c(region1 = 50)

  # Run function for single region
  result_single <- mh_power_list(df_list_single, pred_list_single, minpercreg_single)
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
  result_na <- mh_power_list(df_list_na, pred_list_na, minpercreg_na)
  expect_type(result_na, "list")
  expect_true(all(!is.na(result_na$region1$cen)))  # cen should compute despite NA values

  # Empty lists
  df_list_empty <- list()
  pred_list_empty <- list()
  minpercreg_empty <- c()

  # Run function with empty inputs
  result_empty <- mh_power_list(df_list_empty, pred_list_empty, minpercreg_empty)
  expect_type(result_empty, "list")
  expect_length(result_empty, 0)  # Should return empty list without error
})

test_that("mh_plot_power produces plots correctly", {
  # Create sample power_list with two regions
  power_list <- list(
    region1 = data.frame(
      region = "region1",
      temperature = seq(20, 30, by = 2),
      cen = 22,
      log_rr = c(0.1, 0.15, 0.2, 0.25, 0.3, 0.35),
      se = c(0.05, 0.05, 0.06, 0.06, 0.07, 0.07),
      power = c(70, 75, 80, 85, 90, 95)
    ),
    region2 = data.frame(
      region = "region2",
      temperature = seq(18, 28, by = 2),
      cen = 20,
      log_rr = c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3),
      se = c(0.04, 0.05, 0.05, 0.06, 0.06, 0.07),
      power = c(60, 65, 70, 75, 80, 85)
    )
  )

  # Plot without saving (allow warnings)
  expect_warning(mh_plot_power(power_list, save_fig = FALSE), NA)

  # Plot with saving enabled
  tmp_dir <- tempdir()
  output_folder <- file.path(tmp_dir, "test_plots")
  model_validation_dir <- file.path(output_folder, "model_validation")

  # Cleanup before creating
  if (dir.exists(model_validation_dir)) {
    unlink(model_validation_dir, recursive = TRUE)
  }

  # Create directory only if it doesn't exist
  if (!dir.exists(model_validation_dir)) {
    dir.create(model_validation_dir, recursive = TRUE)
  }

  expect_warning(
    mh_plot_power(power_list, save_fig = TRUE, output_folder_path = output_folder, country = "TestCountry"),
    NA
  )

  # Check that the PDF file was created
  output_path <- file.path(model_validation_dir, "power_vs_temperature.pdf")
  expect_true(file.exists(output_path))
  expect_gt(file.info(output_path)$size, 0)

  # Cleanup after test
  unlink(output_folder, recursive = TRUE)
})

test_that("mh_rr_results produces expected cumulative RR results", {
  # Create sample data for two regions
  set.seed(123)
  n_regions <- 2
  n_days <- 10

  # Create df_list with temperature data
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
      allRRfit = runif(5, 1, 2),
      allRRlow = runif(5, 0.8, 1.2),
      allRRhigh = runif(5, 2, 3)
    )
  })
  names(pred_list) <- names(df_list)

  # Create minpercreg vector for percentile thresholds
  minpercreg <- setNames(sample(20:60, n_regions, replace = TRUE), names(df_list))

  # Run the function
  result <- suppressWarnings(mh_rr_results(pred_list, df_list, attr_thr = 97.5, minpercreg))

  # Tests
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)

  # Check required columns exist
  required_cols <- c("Area", "MinST", "Attr_Threshold_Temp", "Temperature",
                     "Temp_Frequency", "RR", "RR_lower_CI", "RR_upper_CI")
  expect_true(all(required_cols %in% names(result)))

  # Check that Area names match region names
  expect_true(all(result$Area %in% names(df_list)))

  # Check rounding of RR and confidence intervals
  expect_true(all(result$RR == round(result$RR, 2)))
  expect_true(all(result$RR_lower_CI == round(result$RR_lower_CI, 2)))
  expect_true(all(result$RR_upper_CI == round(result$RR_upper_CI, 2)))

  # Check Temp_Frequency is numeric and non-negative
  expect_true(is.numeric(result$Temp_Frequency))
  expect_true(all(result$Temp_Frequency >= 0))

  # Check MinST and Attr_Threshold_Temp are correctly computed (ignore names)
  for (region in names(df_list)) {
    expected_min_st <- unname(round(quantile(df_list[[region]]$temp, minpercreg[region] / 100, na.rm = TRUE), 1))
    actual_min_st <- unique(result[result$Area == region, "MinST"])
    expect_equal(actual_min_st, expected_min_st)

    expected_attr_thr <- unname(round(quantile(df_list[[region]]$temp, 97.5 / 100, na.rm = TRUE), 1))
    actual_attr_thr <- unique(result[result$Area == region, "Attr_Threshold_Temp"])
    expect_equal(actual_attr_thr, expected_attr_thr)
  }
})

test_that("mh_rr_results handles edge cases correctly", {
  # Single region
  df_list_single <- list(region1 = data.frame(temp = rnorm(10, 20, 2)))
  pred_list_single <- list(region1 = data.frame(predvar = seq(18, 25, length.out = 3),
                                                allRRfit = c(1.1, 1.2, 1.3),
                                                allRRlow = c(0.9, 1.0, 1.1),
                                                allRRhigh = c(1.5, 1.6, 1.7)))
  minpercreg_single <- c(region1 = 50)

  result_single <- suppressWarnings(mh_rr_results(pred_list_single, df_list_single, minpercreg = minpercreg_single))
  expect_s3_class(result_single, "data.frame")
  expect_equal(unique(result_single$Area), "region1")

  # Missing values in temperature
  df_list_na <- list(region1 = data.frame(temp = c(NA, 20, 22, NA, 25)))
  pred_list_na <- list(region1 = data.frame(predvar = c(20, 22, 25),
                                            allRRfit = c(1.1, 1.2, 1.3),
                                            allRRlow = c(0.9, 1.0, 1.1),
                                            allRRhigh = c(1.5, 1.6, 1.7)))
  minpercreg_na <- c(region1 = 50)

  result_na <- suppressWarnings(mh_rr_results(pred_list_na, df_list_na, minpercreg = minpercreg_na))
  expect_s3_class(result_na, "data.frame")
  expect_true(all(!is.na(result_na$MinST)))  # MinST should compute despite NA values

  # Empty lists
  df_list_empty <- list()
  pred_list_empty <- list()
  minpercreg_empty <- c()

  result_empty <- mh_rr_results(pred_list_empty, df_list_empty, minpercreg = minpercreg_empty)
  expect_s3_class(result_empty, "data.frame")
  expect_equal(nrow(result_empty), 0)  # Should return empty data frame without error
})

test_that("mh_plot_rr produces plots correctly", {
  # Create sample df_list
  set.seed(123)
  df_list <- list(
    Region1 = data.frame(
      date = seq.Date(as.Date("2023-01-01"), by = "day", length.out = 10),
      region = "Region1",
      temp = rnorm(10, 20, 3),
      suicides = rpois(10, lambda = 5),
      population = rep(1000000, 10),
      year = rep(2023, 10),
      month = rep(1, 10)
    )
  )

  # Generate dummy crosspred object with strong positive effect
  temp_seq <- seq(-6, 6, length.out = 20)
  basis <- onebasis(temp_seq, "lin")
  coef <- 2  # large positive coefficient for RR values
  vcov <- matrix(0.01)
  pred_obj <- crosspred(basis, coef = coef, vcov = vcov, cen = 0, at = temp_seq)

  # Wrap in list for mh_plot_rr
  pred_list <- list(Region1 = pred_obj)

  # minpercreg
  minpercreg <- c(Region1 = 50)

  # Plot without saving (expect warnings)
  expect_warning(
    mh_plot_rr(df_list, pred_list, attr_thr = 0, minpercreg, save_fig = FALSE)
  )

  # Plot with saving enabled (expect warnings)
  tmp_dir <- tempdir()
  output_folder <- file.path(tmp_dir, "test_rr_plots")
  model_validation_dir <- file.path(output_folder, "model_validation")

  if (dir.exists(model_validation_dir)) unlink(model_validation_dir, recursive = TRUE)
  dir.create(model_validation_dir, recursive = TRUE)

  expect_warning(
    mh_plot_rr(df_list, pred_list, attr_thr = 0, minpercreg,
               save_fig = TRUE, output_folder_path = output_folder, country = "TestCountry")
  )

  # Check PDF file exists and is non-empty
  output_path <- file.path(output_folder, "suicides_rr_plot.pdf")
  expect_true(file.exists(output_path))
  expect_gt(file.info(output_path)$size, 0)

  # Cleanup
  unlink(output_folder, recursive = TRUE)
})


test_that("mh_attr produces expected output structure and values", {
  # Create synthetic df_list with enough rows for lag calculations
  set.seed(123)
  df_list <- list(
    Region1 = data.frame(
      date = seq.Date(as.Date("2000-01-01"), by = "day", length.out = 30),
      region = "Region 1",
      temp = rnorm(30, 5, 2),
      suicides = rpois(30, lambda = 2),
      population = rep(2600000, 30),
      year = rep(2000, 30),
      month = rep(1, 30)
    )
  )

  # Create cb_list mimicking crossbasis structure
  cb <- matrix(rnorm(30 * 5), nrow = 30, ncol = 5)
  class(cb) <- c("crossbasis", "matrix")
  attr(cb, "df") <- c(5, 2)
  attr(cb, "range") <- c(-5.9, 23.3)
  attr(cb, "lag") <- c(0, 2)
  attr(cb, "argvar") <- list(fun = "bs", knots = c(3.1, 8.5, 13.9), degree = 2)
  attr(cb, "arglag") <- list(fun = "strata", breaks = 1)
  cb_list <- list(Region1 = cb)

  # Create pred_list with coefficients and vcov
  pred_list <- list(
    Region1 = list(
      coefficients = c(-0.19, -0.30, -0.20, -0.14, -0.36),
      vcov = diag(c(0.18, 0.11, 0.15, 0.12, 0.16))
    )
  )

  # minpercreg
  minpercreg <- c(Region1 = 37)

  # Run mh_attr and check for errors
  expect_error(
    result <- mh_attr(df_list, cb_list, pred_list, minpercreg, attr_thr = 97.5),
    NA
  )

  # Validate output structure
  expect_type(result, "list")
  expect_named(result, names(df_list))
  expect_true(all(sapply(result, is.list)))

  # Check that each region contains 'results' and 'ansim_mat'
  for (region in names(result)) {
    expect_true(all(c("results", "ansim_mat") %in% names(result[[region]])))

    # Validate results data frame columns
    df <- result[[region]]$results
    expected_cols <- c("region", "date", "temp", "year", "month", "suicides", "population",
                       "threshold_temp", "af", "af_lower_ci", "af_upper_ci",
                       "an", "an_lower_ci", "an_upper_ci",
                       "ar", "ar_lower_ci", "ar_upper_ci")
    expect_true(all(expected_cols %in% names(df)))
  }
})


test_that("mh_attr_tables aggregates attributable estimates correctly", {
  # Set seed for reproducibility
  set.seed(123)

  # Create mock attr_list for two regions
  n_sim <- 100
  ansim_mat_region1 <- matrix(stats::rpois(n_sim * 10, lambda = 2), nrow = 10)
  ansim_mat_region2 <- matrix(stats::rpois(n_sim * 8, lambda = 3), nrow = 8)

  results_region1 <- data.frame(
    year = rep(2000:2001, each = 5),
    month = rep(1:5, times = 2),
    region = "region1",
    population = rep(100000, 10),
    temp = rnorm(10, mean = 15, sd = 3),
    threshold_temp = rep(25, 10),
    suicides = rep(10, 10),
    an = rep(2, 10)
  )

  results_region2 <- data.frame(
    year = rep(2000:2001, each = 4),
    month = rep(1:4, times = 2),
    region = "region2",
    population = rep(120000, 8),
    temp = rnorm(8, mean = 20, sd = 3),
    threshold_temp = rep(28, 8),
    suicides = rep(12, 8),
    an = rep(3, 8)
  )

  attr_list <- list(
    region1 = list(results = results_region1, ansim_mat = ansim_mat_region1),
    region2 = list(results = results_region2, ansim_mat = ansim_mat_region2)
  )

  # Run function
  result <- mh_attr_tables(attr_list, country = "National", meta_analysis = FALSE)

  # Structure tests
  expect_type(result, "list")
  expect_length(result, 3)

  res_attr_tot <- result[[1]]
  attr_yr_list <- result[[2]]
  attr_mth_list <- result[[3]]

  # Check overall totals dataframe
  expect_true(is.data.frame(res_attr_tot))
  required_cols_tot <- c("region", "population", "temp", "threshold_temp", "suicides",
                         "an", "an_lower_ci", "an_upper_ci", "af", "af_lower_ci", "af_upper_ci",
                         "ar", "ar_lower_ci", "ar_upper_ci")
  expect_true(all(required_cols_tot %in% names(res_attr_tot)))

  # Check yearly list structure
  expect_type(attr_yr_list, "list")
  expect_named(attr_yr_list, c("region1", "region2"))
  expect_true(all(sapply(attr_yr_list, is.data.frame)))

  # Check monthly list structure
  expect_type(attr_mth_list, "list")
  expect_named(attr_mth_list, c("region1", "region2"))
  expect_true(all(sapply(attr_mth_list, is.data.frame)))

  # Validate CI calculations: upper CI should be >= lower CI
  expect_true(all(res_attr_tot$an_upper_ci >= res_attr_tot$an_lower_ci))

  # Validate AF and AR are positive
  expect_true(all(res_attr_tot$af >= 0))
  expect_true(all(res_attr_tot$ar >= 0))

  # Check that month names are converted correctly in monthly list
  expect_true(all(attr_mth_list$region1$month %in% month.name))
})


test_that("mh_plot_attr_totals generates plots and validates dynamic elements", {
  # Set seed for reproducibility
  set.seed(123)

  # Mock df_list with date column for two regions
  n_days <- 10
  df_list <- list(
    region1 = data.frame(date = seq.Date(as.Date("2000-01-01"), by = "day", length.out = n_days)),
    region2 = data.frame(date = seq.Date(as.Date("2000-01-01"), by = "day", length.out = n_days))
  )

  # Mock res_attr_tot with required columns
  res_attr_tot <- data.frame(
    region = c("region1", "region2"),
    af = c(12.5, 8.3),
    af_lower_ci = c(10.0, 6.0),
    af_upper_ci = c(15.0, 10.0),
    ar = c(5.2, 3.8),
    ar_lower_ci = c(4.0, 3.0),
    ar_upper_ci = c(6.5, 4.5)
  )

  # Capture plot output for dynamic checks without saving
  expect_silent({
    grDevices::pdf(NULL) # Direct to null device for testing
    mh_plot_attr_totals(df_list, res_attr_tot, save_fig = FALSE, country = "region1")
    plot_calls <- recordPlot()
    grDevices::dev.off()
  })

  # Validate sorting by AF and AR
  sorted_af <- order(res_attr_tot$af, decreasing = TRUE)
  expect_equal(res_attr_tot$region[sorted_af][1], "region1") # region1 has highest AF

  sorted_ar <- order(res_attr_tot$ar, decreasing = TRUE)
  expect_equal(res_attr_tot$region[sorted_ar][1], "region1") # region1 has highest AR

  # Validate highlight color logic for AF
  bar_col_af <- rep("#296991", nrow(res_attr_tot))
  nat_ind_af <- which(res_attr_tot$region[sorted_af] == "region1")
  bar_col_af[nat_ind_af] <- "#7a855c"
  expect_true("#7a855c" %in% bar_col_af)

  # Validate CI warning text formatting
  af_ci_range <- c(min(res_attr_tot$af_lower_ci), max(res_attr_tot$af_upper_ci))
  af_warning <- sprintf("Warning: AF CI's range from %.2f%% to %.2f%%", af_ci_range[1], af_ci_range[2])
  expect_match(af_warning, "Warning: AF CI's range from")

  # Validate year range calculation for title
  year_min <- min(sapply(df_list, function(x) min(lubridate::year(x$date), na.rm = TRUE)))
  year_max <- max(sapply(df_list, function(x) max(lubridate::year(x$date), na.rm = TRUE)))
  expect_equal(year_min, 2000)
  expect_equal(year_max, 2000)

  # Validate file saving when save_fig = TRUE
  temp_dir <- tempdir()
  output_path <- file.path(temp_dir, "suicides_total_attr_plot.pdf")

  expect_silent(
    mh_plot_attr_totals(df_list, res_attr_tot, save_fig = TRUE, output_folder_path = temp_dir, country = "region1")
  )

  expect_true(file.exists(output_path))

  # Clean up
  unlink(output_path)
})




test_that("mh_plot_attr_totals saves plot when save_fig = TRUE", {
  # Set seed for reproducibility
  set.seed(456)

  # Create mock df_list with date column
  n_days <- 10
  df_list <- list(
    region1 = data.frame(date = seq.Date(as.Date("2000-01-01"), by = "day", length.out = n_days)),
    region2 = data.frame(date = seq.Date(as.Date("2000-01-01"), by = "day", length.out = n_days))
  )

  # Mock res_attr_tot with required columns
  res_attr_tot <- data.frame(
    region = c("region1", "region2"),
    af = c(12.5, 8.3),
    af_lower_ci = c(10.0, 6.0),
    af_upper_ci = c(15.0, 10.0),
    ar = c(5.2, 3.8),
    ar_lower_ci = c(4.0, 3.0),
    ar_upper_ci = c(6.5, 4.5)
  )

  # Create temporary directory for saving plot
  temp_dir <- tempdir()
  output_path <- file.path(temp_dir, "suicides_total_attr_plot.pdf")

  # Run function with save_fig = TRUE
  expect_silent(
    mh_plot_attr_totals(df_list, res_attr_tot, save_fig = TRUE, output_folder_path = temp_dir, country = "region1")
  )

  # Check that file was created
  expect_true(file.exists(output_path))

  # Clean up
  unlink(output_path)
})


test_that("mh_plot_af_yearly generates plots without errors", {
  # Set seed for reproducibility
  set.seed(123)

  # Create mock attr_yr_list for two regions
  attr_yr_list <- list(
    region1 = data.frame(
      year = 2000:2002,
      af = c(10.5, 12.0, 11.3),
      af_lower_ci = c(9.0, 10.0, 9.5),
      af_upper_ci = c(12.0, 14.0, 13.0)
    ),
    region2 = data.frame(
      year = 2000:2002,
      af = c(8.2, 9.5, 10.1),
      af_lower_ci = c(7.0, 8.0, 8.5),
      af_upper_ci = c(9.5, 11.0, 11.5)
    )
  )

  # Test plotting without saving
  expect_silent(
    mh_plot_af_yearly(attr_yr_list, save_fig = FALSE, country = "region1")
  )
})


test_that("mh_plot_af_yearly generates plots and validates dynamic elements", {
  # Set seed for reproducibility
  set.seed(123)

  # Create mock attr_yr_list for two regions
  attr_yr_list <- list(
    region1 = data.frame(
      year = 2000:2002,
      af = c(10.5, 12.0, 11.3),
      af_lower_ci = c(9.0, 10.0, 9.5),
      af_upper_ci = c(12.0, 14.0, 13.0)
    ),
    region2 = data.frame(
      year = 2000:2002,
      af = c(8.2, 9.5, 10.1),
      af_lower_ci = c(7.0, 8.0, 8.5),
      af_upper_ci = c(9.5, 11.0, 11.5)
    )
  )

  # Capture plot output for dynamic checks without saving
  expect_silent({
    grDevices::pdf(NULL) # Direct to null device for testing
    mh_plot_af_yearly(attr_yr_list, save_fig = FALSE, country = "region1")
    plot_calls <- recordPlot()
    grDevices::dev.off()
  })

  # Validate year range calculation
  year_min <- min(sapply(attr_yr_list, function(x) min(x$year, na.rm = TRUE)))
  year_max <- max(sapply(attr_yr_list, function(x) max(x$year, na.rm = TRUE)))
  expect_equal(year_min, 2000)
  expect_equal(year_max, 2002)

  # Validate ylim calculation includes AF values
  y_min <- min(sapply(attr_yr_list, function(x) min(x$af, na.rm = TRUE)))
  y_max <- max(sapply(attr_yr_list, function(x) max(x$af, na.rm = TRUE)))
  expect_true(y_max >= 12.0)

  # Validate CI polygon coordinates for region1
  region_af <- attr_yr_list$region1[order(attr_yr_list$region1$year), ]
  x_poly <- c(region_af$year, rev(region_af$year))
  y_poly <- c(region_af$af_upper_ci, rev(region_af$af_lower_ci))
  expect_equal(length(x_poly), length(y_poly)) # Polygon coordinates match

  # Validate title text when save_fig = TRUE
  temp_dir <- tempdir()
  output_path <- file.path(temp_dir, "suicides_af_timeseries.pdf")

  expect_silent(
    mh_plot_af_yearly(attr_yr_list, save_fig = TRUE, output_folder_path = temp_dir, country = "region1")
  )

  expect_true(file.exists(output_path))

  # Clean up
  unlink(output_path)
})



test_that("mh_plot_ar_yearly generates plots and validates dynamic elements", {
  # Set seed for reproducibility
  set.seed(123)

  # Create mock attr_yr_list for two regions
  attr_yr_list <- list(
    region1 = data.frame(
      year = 2000:2002,
      ar = c(5.2, 6.0, 5.8),
      ar_lower_ci = c(4.0, 4.5, 4.8),
      ar_upper_ci = c(6.5, 7.0, 6.8)
    ),
    region2 = data.frame(
      year = 2000:2002,
      ar = c(3.8, 4.2, 4.5),
      ar_lower_ci = c(3.0, 3.2, 3.5),
      ar_upper_ci = c(4.5, 4.8, 5.0)
    )
  )

  # Capture plot output for dynamic checks without saving
  expect_silent({
    grDevices::pdf(NULL) # Direct to null device for testing
    mh_plot_ar_yearly(attr_yr_list, save_fig = FALSE, country = "region1")
    plot_calls <- recordPlot()
    grDevices::dev.off()
  })

  # Validate year range calculation
  year_min <- min(sapply(attr_yr_list, function(x) min(x$year, na.rm = TRUE)))
  year_max <- max(sapply(attr_yr_list, function(x) max(x$year, na.rm = TRUE)))
  expect_equal(year_min, 2000)
  expect_equal(year_max, 2002)

  # Validate ylim calculation includes AR values
  y_min <- min(sapply(attr_yr_list, function(x) min(x$ar, na.rm = TRUE)))
  y_max <- max(sapply(attr_yr_list, function(x) max(x$ar, na.rm = TRUE)))
  expect_true(y_max >= 6.0)

  # Validate CI polygon coordinates for region1
  region_ar <- attr_yr_list$region1[order(attr_yr_list$region1$year), ]
  x_poly <- c(region_ar$year, rev(region_ar$year))
  y_poly <- c(region_ar$ar_upper_ci, rev(region_ar$ar_lower_ci))
  expect_equal(length(x_poly), length(y_poly)) # Polygon coordinates match

  # Validate title text when save_fig = TRUE
  temp_dir <- tempdir()
  output_path <- file.path(temp_dir, "suicides_ar_timeseries.pdf")

  expect_silent(
    mh_plot_ar_yearly(attr_yr_list, save_fig = TRUE, output_folder_path = temp_dir, country = "region1")
  )

  expect_true(file.exists(output_path))

  # Clean up
  unlink(output_path)
})

