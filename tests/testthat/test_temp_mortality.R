# Unit tests for temp_mortality.R

# Create temp_dir to be used by all temp_mortality tests
temp_dir <- tempdir()
temp_dir <- file.path(temp_dir, "temp_mortality_tests")
if (!file.exists(temp_dir)) dir.create(temp_dir)

test_that("Test hc_read_data", {

  # Mocking functions to avoid dependency on filesystem and internals
  mock_read_input_data <- function(input_csv_path) {
    data.frame(
      date_col = c("2023-01-01", "02/01/2023", "2023-01-03", "04/01/2023", "2023-01-05"),
      regnames = c("North", "South", "East", "West", "Central"),
      tmean    = c(23.5, 25.1, 22.8, 24.3, 21.9),
      deaths   = c(12L, 15L, 9L, 11L, 8L),
      pop      = c(1000L, 1200L, 950L, 1100L, 1050L),
      stringsAsFactors = FALSE
    )
  }

  # Pass-through reformat (keeps test focused on hc_read_data)
  mock_reformat_data <- function(df, reformat_date = TRUE, fill_na = NULL, year_from_date = TRUE) {
    df
  }

  # Emulate split-by-region and alphabetical ordering
  mock_aggregate_by_column <- function(df, col) {
    stopifnot(col %in% names(df))
    split_list <- split(df[order(df[[col]]), , drop = FALSE], df[[col]])
    split_list[order(names(split_list))]
  }

  # Set mocked bindings
  local_mocked_bindings(
    read_input_data      = mock_read_input_data,
    reformat_data        = mock_reformat_data,
    aggregate_by_column  = mock_aggregate_by_column
  )

  # Create control dataframe
  control_input <- mock_read_input_data("ignored.csv")
  control_df <- control_input %>%
    dplyr::rename(
      dependent  = deaths,
      date       = date_col,
      region     = regnames,
      temp       = tmean,
      population = pop
    ) %>%
    dplyr::mutate(
      date  = as.Date(date, tryFormats = c("%d/%m/%Y", "%Y-%m-%d")),
      year  = as.factor(lubridate::year(date)),
      month = as.factor(lubridate::month(date)),
      dow   = as.factor(lubridate::wday(date, label = TRUE)),
      region = as.factor(.data$region)
    )

  # Create control list (alphabetical by region)
  control_list <- split(control_df[order(control_df$region), , drop = FALSE], control_df$region)
  control_list <- control_list[order(names(control_list))]

  # Call function
  res_list <- hc_read_data(
    input_csv_path   = file.path(temp_dir, "mock.csv"),
    dependent_col    = "deaths",
    date_col         = "date_col",
    region_col       = "regnames",
    temperature_col  = "tmean",
    population_col   = "pop"
  )

  # Test list names/order
  expect_identical(names(res_list), names(control_list))

  # Test each regional dataframe without purrr
  for (nm in names(control_list)) {
    expect_identical(res_list[[nm]], control_list[[nm]])
  }
})


test_that("hc_create_crossbasis creates correct cross-basis matrices", {

  # Create sample data with sufficient observations for lag
  sample_df <- data.frame(
    date = seq.Date(
      from = as.Date("2023-01-01"),
      to   = as.Date("2023-02-15"),
      by   = "day"
    ),
    temp = seq(10, 25, length.out = 46),
    dependent = sample(5:15, 46, replace = TRUE)
  )

  # Create sample dataframe list with two geographies
  sample_df_list <- list(
    "Geog1" = sample_df,
    "Geog2" = sample_df
  )

  # Run the function with defaults
  result <- hc_create_crossbasis(sample_df_list)

  # Basic checks
  expect_type(result, "list")
  expect_equal(length(result), 2)
  expect_equal(names(result), c("Geog1", "Geog2"))

  # Check structure of cross-basis objects
  for (geog in names(result)) {
    cb <- result[[geog]]

    expect_s3_class(cb, "crossbasis")
    expect_equal(nrow(cb), nrow(sample_df_list[[geog]]))

    # Check argvar structure
    expect_equal(attr(cb, "argvar")$fun, "bs")
    expect_equal(attr(cb, "argvar")$degree, 2)
    expect_length(attr(cb, "argvar")$knots, 3)

    # Check lag specification
    expect_equal(attr(cb, "lag")[2], 21)
    expect_length(attr(cb, "arglag")$knots, 3)
  }

  # Test with custom parameters
  custom_result <- hc_create_crossbasis(
    sample_df_list,
    var_fun = "bs",
    var_degree = 3,
    var_per = c(5, 50, 95),
    lagn = 14,
    lagnk = 4,
    dfseas = 6
  )

  expect_equal(length(custom_result), 2)

  cb1 <- custom_result$Geog1

  # Check custom argvar settings
  expect_equal(attr(cb1, "argvar")$degree, 3)
  expect_length(attr(cb1, "argvar")$knots, 3)

  # Check custom lag settings
  expect_equal(attr(cb1, "lag")[2], 14)
  expect_length(attr(cb1, "arglag")$knots, 4)
})


test_that("hc_model_combo_res creates correct model combinations and diagnostics", {

  # Create sample data (single year so length(unique(year)) = 1)
  set.seed(123)
  sample_df <- data.frame(
    date       = seq.Date(from = as.Date("2023-01-01"),
                          to   = as.Date("2023-01-30"),
                          by   = "day"),
    temp       = rnorm(30, mean = 23, sd = 2),
    dependent  = rpois(30, lambda = 10),
    humidity   = rnorm(30, mean = 70, sd = 10),
    pollution  = rnorm(30, mean = 50, sd = 15)
  ) %>%
    dplyr::mutate(
      year = as.factor(lubridate::year(date)),
      dow  = as.factor(lubridate::wday(date, label = TRUE))
    )

  # Create input lists
  df_list <- list(
    "Geog1" = sample_df,
    "Geog2" = sample_df
  )

  # Create mock cross-basis matrix directly (30 rows x 6 cols)
  mock_cb <- matrix(rnorm(30 * 6), nrow = 30)
  colnames(mock_cb) <- paste0("v", 1:6)
  class(mock_cb) <- c("crossbasis", "matrix")

  # Minimal attributes for bookkeeping (not strictly required by glm)
  attr(mock_cb, "df")    <- c(3, 2)
  attr(mock_cb, "range") <- range(sample_df$temp)
  attr(mock_cb, "lag")   <- c(0, 3)

  # Create mock cb_list
  cb_list <- list(
    "Geog1" = mock_cb,
    "Geog2" = mock_cb
  )

  independent_vars <- c("humidity", "pollution")

  # Run function (defaults: dfseas = 8)
  results <- hc_model_combo_res(df_list, cb_list, independent_vars)

  # Test structure
  expect_type(results, "list")
  expect_length(results, 2)
  expect_s3_class(results[[1]], "data.frame")
  expect_type(results[[2]], "list")

  # Test QAIC results
  qaic_results <- results[[1]]
  expect_equal(nrow(qaic_results), 8)  # 2 geogs * 4 combinations (none, hum, pol, both)
  expect_named(qaic_results, c("geography", "formula", "disp", "qaic"))

  # Test formulas generated - trim whitespace and verify all expected combinations exist
  # Base formula includes cb, dow, and the date spline with dfseas * length(unique(year))
  base_formula <- "dependent ~ cb + dow + splines::ns(date, df = 8 * length(unique(year)))"

  expected_formulas <- c(
    base_formula,
    paste0(base_formula, " + humidity_ns"),
    paste0(base_formula, " + pollution_ns"),
    paste0(base_formula, " + humidity_ns + pollution_ns")
  )
  actual_formulas <- unique(trimws(qaic_results$formula))
  expect_setequal(actual_formulas, expected_formulas)

  # Verify base formula exists for each geography
  base_rows <- qaic_results[trimws(qaic_results$formula) == base_formula, ]
  expect_equal(nrow(base_rows), 2)
  expect_setequal(base_rows$geography, c("Geog1", "Geog2"))

  # Test residuals structure
  residuals_list <- results[[2]]
  expect_named(residuals_list, c("Geog1", "Geog2"))

  # Each geography should have residuals for 4 model combinations
  expect_length(residuals_list$Geog1, 4)
  expect_length(residuals_list$Geog2, 4)

  # Test one model residuals content for Geog1
  first_model_residuals <- residuals_list$Geog1[[1]]
  expect_s3_class(first_model_residuals, "data.frame")
  expect_named(first_model_residuals, c("geography", "formula", "fitted", "residuals"))
  expect_equal(nrow(first_model_residuals), nrow(sample_df))
})


test_that("hc_model_combo_res works when independent_cols = NULL (base model only)", {

  # Create sample data (single year)
  set.seed(456)
  sample_df <- data.frame(
    date       = seq.Date(from = as.Date("2023-02-01"),
                          to   = as.Date("2023-03-02"),
                          by   = "day"),
    temp       = rnorm(30, mean = 20, sd = 3),
    dependent  = rpois(30, lambda = 8)
  ) %>%
    dplyr::mutate(
      year = as.factor(lubridate::year(date)),
      dow  = as.factor(lubridate::wday(date, label = TRUE))
    )

  df_list <- list("GeogA" = sample_df, "GeogB" = sample_df)

  mock_cb <- matrix(rnorm(30 * 4), nrow = 30)
  colnames(mock_cb) <- paste0("b", 1:4)
  class(mock_cb) <- c("crossbasis", "matrix")
  attr(mock_cb, "df")    <- c(2, 2)
  attr(mock_cb, "range") <- range(sample_df$temp)
  attr(mock_cb, "lag")   <- c(0, 2)

  cb_list <- list("GeogA" = mock_cb, "GeogB" = mock_cb)

  # Run function with no independents
  results <- hc_model_combo_res(df_list, cb_list, independent_cols = NULL)

  # QAIC results should exist with only the base model per geography
  qaic_results <- results[[1]]
  expect_s3_class(qaic_results, "data.frame")
  expect_equal(nrow(qaic_results), 2)
  expect_setequal(qaic_results$geography, c("GeogA", "GeogB"))

  # Base formula should include date spline with dfseas = 8
  base_formula <- "dependent ~ cb + dow + splines::ns(date, df = 8 * length(unique(year)))"
  expect_true(all(trimws(qaic_results$formula) == base_formula))

  # Residuals list should have one entry per geography (1 model each)
  residuals_list <- results[[2]]
  expect_named(residuals_list, c("GeogA", "GeogB"))
  expect_length(residuals_list$GeogA, 1)
  expect_length(residuals_list$GeogB, 1)
})


test_that("hc_model_combo_res respects custom dfseas in formula construction", {

  # Create sample data (single year)
  set.seed(789)
  sample_df <- data.frame(
    date       = seq.Date(from = as.Date("2023-03-01"),
                          to   = as.Date("2023-03-30"),
                          by   = "day"),
    temp       = rnorm(30, mean = 18, sd = 2),
    dependent  = rpois(30, lambda = 7),
    humidity   = rnorm(30, mean = 65, sd = 8)
  ) %>%
    dplyr::mutate(
      year = as.factor(lubridate::year(date)),
      dow  = as.factor(lubridate::wday(date, label = TRUE))
    )

  df_list <- list("GeoX" = sample_df)

  mock_cb <- matrix(rnorm(30 * 5), nrow = 30)
  class(mock_cb) <- c("crossbasis", "matrix")
  cb_list <- list("GeoX" = mock_cb)

  # Use a custom dfseas value and one independent var
  custom_dfseas <- 6
  results <- hc_model_combo_res(df_list, cb_list, independent_cols = "humidity", dfseas = custom_dfseas)

  qaic_results <- results[[1]]
  expect_s3_class(qaic_results, "data.frame")
  # Should have 2 rows: base and + humidity_ns
  expect_equal(nrow(qaic_results), 2)

  # Check that dfseas value appears in formula construction
  base_formula_custom <- paste0(
    "dependent ~ cb + dow + splines::ns(date, df = ", custom_dfseas, " * length(unique(year)))"
  )
  expected_forms <- c(
    base_formula_custom,
    paste0(base_formula_custom, " + humidity_ns")
  )
  actual_forms <- unique(trimws(qaic_results$formula))
  expect_setequal(actual_forms, expected_forms)
})


test_that("hc_adf runs ADF test and returns expected results by geography", {

  # Create sample data
  set.seed(123)
  sample_df <- data.frame(
    date = seq.Date(from = as.Date("2023-01-01"),
                    to   = as.Date("2023-02-15"),
                    by   = "day"),
    dependent = rnorm(46, mean = 10, sd = 2)
  )

  # Create input dataframe list
  df_list <- list(
    "Geog1" = sample_df,
    "Geog2" = sample_df
  )

  # Run function
  results <- hc_adf(df_list)

  # Test structure
  expect_type(results, "list")
  expect_named(results, c("Geog1", "Geog2"))

  for (geog in names(results)) {
    adf_df <- results[[geog]]

    expect_s3_class(adf_df, "data.frame")
    expect_equal(nrow(adf_df), 1)
    expect_named(adf_df, c("adf_test_stat", "lag_order", "p_value"))

    # Check types
    expect_type(adf_df$adf_test_stat, "double")
    expect_type(adf_df$lag_order, "double")
    expect_type(adf_df$p_value, "double")

    # Sanity checks on values
    expect_true(is.finite(adf_df$adf_test_stat))
    expect_true(adf_df$lag_order >= 0)
    expect_true(adf_df$p_value >= 0 && adf_df$p_value <= 1)
  }
})


test_that("hc_model_validation performs complete model validation", {

  # Setup test data
  set.seed(123)
  n_days <- 60

  sample_df <- data.frame(
    date = seq.Date(
      from = as.Date("2023-01-01"),
      length.out = n_days,
      by = "day"
    ),
    temp       = rnorm(n_days, mean = 22, sd = 2),
    dependent  = rpois(n_days, lambda = 10),
    humidity   = rnorm(n_days, mean = 70, sd = 10),
    pollution  = rnorm(n_days, mean = 50, sd = 15),
    year       = factor(rep(2023, n_days)),
    dow        = factor(lubridate::wday(
      seq.Date(as.Date("2023-01-01"), length.out = n_days, by = "day"),
      label = TRUE
    ))
  )

  df_list <- list(
    "Geo1" = sample_df,
    "Geo2" = sample_df
  )

  # Mock crossbasis
  mock_cb <- matrix(rnorm(n_days * 5), nrow = n_days)
  class(mock_cb) <- c("crossbasis", "matrix")

  cb_list <- list(
    "Geo1" = mock_cb,
    "Geo2" = mock_cb
  )

  independent_vars <- c("humidity", "pollution")

  # Expected QAIC output
  mock_qaic <- data.frame(
    geography = rep(c("Geo1", "Geo2"), each = 2),
    formula   = rep(c("f1", "f2"), times = 2),
    disp      = runif(4, 1, 2),
    qaic      = runif(4, 100, 200)
  )

  # Mock residuals
  mock_residuals <- list(
    "Geo1" = list(
      f1 = data.frame(geography = "Geo1", formula = "f1",
                      fitted = rnorm(n_days), residuals = rnorm(n_days)),
      f2 = data.frame(geography = "Geo1", formula = "f2",
                      fitted = rnorm(n_days), residuals = rnorm(n_days))
    ),
    "Geo2" = list(
      f1 = data.frame(geography = "Geo2", formula = "f1",
                      fitted = rnorm(n_days), residuals = rnorm(n_days)),
      f2 = data.frame(geography = "Geo2", formula = "f2",
                      fitted = rnorm(n_days), residuals = rnorm(n_days))
    )
  )

  # Mock VIF
  mock_vif <- list(
    Geo1 = data.frame(variable = c("humidity", "pollution"), vif = c(1.2, 1.3)),
    Geo2 = data.frame(variable = c("humidity", "pollution"), vif = c(1.1, 1.4))
  )

  # Mock ADF
  mock_adf <- list(
    Geo1 = data.frame(adf_test_stat = -3, lag_order = 2, p_value = 0.01),
    Geo2 = data.frame(adf_test_stat = -2.8, lag_order = 2, p_value = 0.02)
  )

  validation_dir <- file.path(temp_dir, "model_validation")

  on.exit({
    if (dir.exists(validation_dir)) {
      unlink(validation_dir, recursive = TRUE)
    }
  })

  # Mock internal dependencies
  local_mocked_bindings(
    hc_model_combo_res = function(...) list(mock_qaic, mock_residuals),
    dlnm_vif = function(...) mock_vif,
    hc_adf = function(...) mock_adf
  )

  # Run function
  results <- hc_model_validation(
    df_list = df_list,
    cb_list = cb_list,
    independent_cols = independent_vars,
    save_fig = FALSE,
    save_csv = TRUE,
    output_folder_path = temp_dir
  )

  # Returned object structure
  expect_type(results, "list")
  expect_length(results, 5)

  # QAIC results
  expect_s3_class(results[[1]], "data.frame")
  expect_equal(nrow(results[[1]]), 4)

  # QAIC summary
  expect_s3_class(results[[2]], "data.frame")
  expect_named(results[[2]], c("formula", "mean_disp", "mean_qaic"))

  # VIF results
  expect_s3_class(results[[3]], "data.frame")
  expect_named(results[[3]], c("Geography", "variable", "vif"))

  # VIF summary
  expect_s3_class(results[[4]], "data.frame")
  expect_named(results[[4]], c("variable", "mean_vif"))

  # ADF results
  expect_s3_class(results[[5]], "data.frame")
  expect_named(results[[5]], c("Geography", "adf_test_stat", "lag_order", "p_value"))

  # CSV outputs
  expect_true(file.exists(file.path(validation_dir, "qaic_results.csv")))
  expect_true(file.exists(file.path(validation_dir, "vif_results.csv")))
  expect_true(file.exists(file.path(validation_dir, "adf_results.csv")))
  expect_true(file.exists(file.path(validation_dir, "qaic_summary.csv")))
  expect_true(file.exists(file.path(validation_dir, "vif_summary.csv")))
})


test_that("hc_quasipoisson_dlnm fits DLNM GLMs correctly with controls", {

  set.seed(123)
  n_days <- 100

  # Create sample data for two geographies
  sample_df <- data.frame(
    date       = seq.Date(from = as.Date("2023-01-01"), length.out = n_days, by = "day"),
    temp       = rnorm(n_days, mean = 22, sd = 2),   # not used in model here
    dependent  = rpois(n_days, lambda = 10),
    humidity   = rnorm(n_days, mean = 70, sd = 10),
    pollution  = rnorm(n_days, mean = 50, sd = 15)
  ) %>%
    dplyr::mutate(
      year = as.factor(lubridate::year(date)),
      dow  = as.factor(lubridate::wday(date, label = TRUE))
    )

  df_list <- list(
    "Geo1" = sample_df,
    "Geo2" = sample_df
  )

  # Mock cross-basis matrices compatible with glm
  mock_cb <- matrix(rnorm(n_days * 5), nrow = n_days)
  colnames(mock_cb) <- paste0("b", 1:5)
  class(mock_cb) <- c("crossbasis", "matrix")

  cb_list <- list(
    "Geo1" = mock_cb,
    "Geo2" = mock_cb
  )

  control_cols <- c("humidity", "pollution")

  # Run function (default dfseas = 8)
  models <- hc_quasipoisson_dlnm(
    df_list      = df_list,
    control_cols = control_cols,
    cb_list      = cb_list
  )

  # Structure checks
  expect_type(models, "list")
  expect_named(models, c("Geo1", "Geo2"))
  expect_s3_class(models$Geo1, "glm")
  expect_s3_class(models$Geo2, "glm")

  # Model checks for each geography
  for (g in names(models)) {
    m <- models[[g]]

    # Family is quasi-Poisson
    expect_equal(m$family$family, "quasipoisson")

    # Observations match
    expect_equal(nobs(m), n_days)

    # Fitted values should be positive
    expect_true(all(fitted(m) > 0))

    # Formula content (substring checks to avoid whitespace brittleness)
    f_str <- paste(deparse(formula(m)), collapse = " ")
    expect_true(grepl("dependent ~", f_str, fixed = TRUE))
    expect_true(grepl(" cb", f_str, fixed = TRUE))
    expect_true(grepl(" dow", f_str, fixed = TRUE))
    expect_true(grepl("splines::ns\\(date, df = 8 \\* length\\(unique\\(year\\)\\)\\)", f_str))

    # Control variables are included as raw terms (not splines) in this function
    terms_rhs <- attr(terms(m), "term.labels")
    expect_true("humidity" %in% terms_rhs)
    expect_true("pollution" %in% terms_rhs)


    # Terms contain expected labels
    trm <- attr(terms(m), "term.labels")
    expect_true(any(grepl("^cb", trm)))
    expect_true(any(grepl("^dow", trm)))
    expect_true(any(grepl("^splines::ns\\(date", trm)))
    expect_true("humidity" %in% trm)
    expect_true("pollution" %in% trm)
  }
})


test_that("hc_quasipoisson_dlnm respects custom dfseas in the date spline", {

  set.seed(456)
  n_days <- 60

  sample_df <- data.frame(
    date       = seq.Date(from = as.Date("2023-02-01"), length.out = n_days, by = "day"),
    dependent  = rpois(n_days, 9),
    humidity   = rnorm(n_days, 65, 8),
    pollution  = rnorm(n_days, 45, 12)
  ) %>%
    dplyr::mutate(
      year = as.factor(lubridate::year(date)),
      dow  = as.factor(lubridate::wday(date, label = TRUE))
    )

  df_list <- list("Geo" = sample_df)

  mock_cb <- matrix(rnorm(n_days * 4), nrow = n_days)
  class(mock_cb) <- c("crossbasis", "matrix")
  cb_list <- list("Geo" = mock_cb)

  models <- hc_quasipoisson_dlnm(
    df_list      = df_list,
    control_cols = c("humidity"),
    cb_list      = cb_list,
    dfseas       = 6
  )

  m <- models$Geo
  f_str <- paste(deparse(formula(m)), collapse = " ")

  terms_rhs <- attr(terms(m), "term.labels")
  expect_true("humidity" %in% terms_rhs)
  expect_true(grepl("splines::ns\\(date, df = 6 \\* length\\(unique\\(year\\)\\)\\)", f_str))
  expect_equal(m$family$family, "quasipoisson")
})


test_that("hc_quasipoisson_dlnm errors on invalid control_cols types", {

  set.seed(789)
  n_days <- 30

  sample_df <- data.frame(
    date       = seq.Date(from = as.Date("2023-03-01"), length.out = n_days, by = "day"),
    dependent  = rpois(n_days, 8),
    humidity   = rnorm(n_days, 60, 7),
    pollution  = rnorm(n_days, 40, 10)
  ) %>%
    dplyr::mutate(
      year = as.factor(lubridate::year(date)),
      dow  = as.factor(lubridate::wday(date, label = TRUE))
    )

  df_list <- list("Geo" = sample_df)

  mock_cb <- matrix(rnorm(n_days * 3), nrow = n_days)
  class(mock_cb) <- c("crossbasis", "matrix")
  cb_list <- list("Geo" = mock_cb)

  # Non-character control_cols should error
  expect_error(
    hc_quasipoisson_dlnm(df_list, control_cols = 123, cb_list),
    "'control_cols' expected a vector of strings or a string\\."
  )

  # Character vector is fine
  expect_no_error(
    hc_quasipoisson_dlnm(df_list, control_cols = c("humidity"), cb_list)
  )
})


test_that("fwald returns a valid p-value for variables in the model", {

  set.seed(42)
  n_days <- 80

  # Minimal data for glm with 'humidity' so we can test fwald("humidity")
  df <- data.frame(
    date       = seq.Date(from = as.Date("2023-01-01"), length.out = n_days, by = "day"),
    dependent  = rpois(n_days, 10),
    humidity   = rnorm(n_days, 65, 9),
    pollution  = rnorm(n_days, 45, 12)
  ) %>%
    dplyr::mutate(
      year = as.factor(lubridate::year(date)),
      dow  = as.factor(lubridate::wday(date, label = TRUE))
    )

  # Simple 'cb' with 1 column to keep coefficient set small (but realistic)
  cb <- matrix(rnorm(n_days), nrow = n_days)
  class(cb) <- c("crossbasis", "matrix")

  # Fit a model similar to hc_quasipoisson_dlnm outcome
  form <- as.formula("dependent ~ cb + dow + splines::ns(date, df = 8 * length(unique(year))) + humidity")
  m <- glm(formula = form, data = df, family = quasipoisson, na.action = "na.exclude")

  # Get p-value for 'humidity' using fwald
  p <- fwald(m, "humidity")
  expect_type(p, "double")
  expect_true(p >= 0 && p <= 1)
})


test_that("fwald validates inputs and errors when variable not present", {

  set.seed(77)
  n <- 50
  df <- data.frame(
    date      = seq.Date(as.Date("2023-04-01"), by = "day", length.out = n),
    dependent = rpois(n, 7),
    humidity  = rnorm(n, 60, 8)
  ) %>%
    dplyr::mutate(
      year = as.factor(lubridate::year(date)),
      dow  = as.factor(lubridate::wday(date, label = TRUE))
    )

  cb <- matrix(rnorm(n), nrow = n); class(cb) <- c("crossbasis", "matrix")
  m <- glm(dependent ~ cb + dow + splines::ns(date, df = 8 * length(unique(year))) + humidity,
           data = df, family = quasipoisson, na.action = "na.exclude")

  # Invalid 'var' type
  expect_error(fwald(m, 123), "Argument 'var' must be a character")

  # Variable not present in model -> expect error (subscript/selection)
  expect_error(fwald(m, "not_in_model"))
})

test_that("hc_predict_subnat produces expected output (non-meta-analysis path)", {
  # Set seed for reproducibility
  set.seed(3728)

  # Create test data
  n_regions <- 3
  n_days <- 12
  n_coef <- 5  # for bs with length(knots)=3 and degree=2, basis typically has 5 cols

  # Generate df_list with known properties
  df_list <- lapply(1:n_regions, function(i) {
    data.frame(
      temp = rnorm(n_days, mean = 18 + i * 2, sd = 3)  # distinct means per region
    )
  })
  names(df_list) <- paste0("geog", 1:n_regions)

  # Centering temperatures per geography (hc expects actual centering value, not a percentile)
  mintempgeog_ <- vapply(names(df_list), function(nm) {
    stats::quantile(df_list[[nm]]$temp, 0.5, na.rm = TRUE)  # median as example
  }, numeric(1))
  names(mintempgeog_) <- names(df_list)

  # Coefficient matrix (rows = geogs, cols = basis dimension)
  coef_ <- matrix(
    rnorm(n_regions * n_coef, mean = 0, sd = 0.3),
    nrow = n_regions,
    ncol = n_coef,
    dimnames = list(names(df_list), NULL)
  )

  # Positive definite vcov matrices per geography
  vcov_ <- lapply(1:n_regions, function(i) {
    m <- matrix(runif(n_coef^2), n_coef, n_coef)
    m %*% t(m)  # SPD
  })
  names(vcov_) <- names(df_list)

  # Run function with suppressed warnings (boundary warnings are fine here)
  pred_list <- suppressWarnings(
    hc_predict_subnat(
      df_list = df_list,
      var_fun = "bs",
      var_per = c(10, 75, 90),
      var_degree = 2,
      mintempgeog_ = mintempgeog_,
      blup = NULL,
      coef_ = coef_,
      vcov_ = vcov_,
      meta_analysis = FALSE
    )
  )

  # Structure checks
  expect_type(pred_list, "list")
  expect_named(pred_list, names(df_list))
  expect_true(all(sapply(pred_list, function(x) inherits(x, "crosspred"))))

  # Required components in crosspred objects
  required_components <- c(
    "predvar", "cen", "lag", "bylag", "coefficients", "vcov", "matfit", "matse",
    "allfit", "allse", "matRRfit", "matRRlow", "matRRhigh",
    "allRRfit", "allRRlow", "allRRhigh", "ci.level", "model.class", "model.link"
  )

  for (nm in names(pred_list)) {
    pred <- pred_list[[nm]]
    expect_true(all(required_components %in% names(pred)))

    # Range checks (by = 0.1 grid from min(temp) to max(temp))
    expect_equal(min(pred$predvar), min(df_list[[nm]]$temp, na.rm = TRUE), tolerance = 0.11)
    expect_equal(max(pred$predvar), max(df_list[[nm]]$temp, na.rm = TRUE), tolerance = 0.11)

    # Center is passed directly from mintempgeog_
    expect_equal(pred$cen, mintempgeog_[nm])

    # Length consistency
    expect_equal(length(pred$allfit), length(pred$predvar))
    expect_equal(length(pred$allse),  length(pred$predvar))

    # Coefficients length matches basis dimension used
    expect_equal(length(pred$coefficients), n_coef)
    expect_equal(ncol(pred$vcov), n_coef)
    expect_equal(nrow(pred$vcov), n_coef)
  }
})


test_that("hc_predict_subnat supports meta-analysis BLUP input (meta_analysis = TRUE)", {
  set.seed(711)

  n_regions <- 2
  n_days <- 15
  n_coef <- 5

  # df_list
  df_list <- lapply(1:n_regions, function(i) {
    data.frame(
      temp = rnorm(n_days, mean = 15 + i * 3, sd = 2.5)
    )
  })
  names(df_list) <- paste0("geog", 1:n_regions)

  # Centering temps (direct values)
  mintempgeog_ <- vapply(names(df_list), function(nm) {
    stats::quantile(df_list[[nm]]$temp, 0.4, na.rm = TRUE)  # arbitrary choice
  }, numeric(1))
  names(mintempgeog_) <- names(df_list)

  # Construct BLUP list: blup$blup (coef vector), blup$vcov (cov matrix)
  blup <- lapply(names(df_list), function(nm) {
    co <- rnorm(n_coef, 0, 0.25)
    V <- matrix(runif(n_coef^2), n_coef, n_coef)
    V <- V %*% t(V)  # SPD
    list(blup = co, vcov = V)
  })
  names(blup) <- names(df_list)

  # Run with meta_analysis = TRUE
  pred_list <- suppressWarnings(
    hc_predict_subnat(
      df_list         = df_list,
      var_fun         = "bs",
      var_per         = c(10, 75, 90),
      var_degree      = 2,
      mintempgeog_    = mintempgeog_,
      blup            = blup,
      coef_           = NULL,
      vcov_           = NULL,
      meta_analysis   = TRUE
    )
  )

  # Structure checks
  expect_type(pred_list, "list")
  expect_named(pred_list, names(df_list))
  expect_true(all(sapply(pred_list, function(x) inherits(x, "crosspred"))))

  for (nm in names(pred_list)) {
    pred <- pred_list[[nm]]

    # Center equals passed value
    expect_equal(pred$cen, mintempgeog_[nm])

    # Coefficients/VCOV dimensions match
    expect_equal(length(pred$coefficients), n_coef)
    expect_equal(dim(pred$vcov), c(n_coef, n_coef))

    # Grid coverage
    expect_equal(min(pred$predvar), min(df_list[[nm]]$temp, na.rm = TRUE), tolerance = 0.11)
    expect_equal(max(pred$predvar), max(df_list[[nm]]$temp, na.rm = TRUE), tolerance = 0.11)
  }
})


test_that("test hc_add_national_data", {
  skip_if_not_installed("mvmeta")

  # Set seed for reproducibility
  set.seed(123)

  # Parameters
  n_regions <- 3
  n_days <- 30
  start_date <- as.Date("2020-01-01")
  dates <- seq(start_date, by = "day", length.out = n_days)
  years <- lubridate::year(dates)

  # Generate df_list (use 'dependent' to match hc_add_national_data)
  df_list <- lapply(1:n_regions, function(i) {
    data.frame(
      date = dates,
      year = years,
      temp = rnorm(n_days, mean = 15 + i * 2, sd = 5),
      dependent = rpois(n_days, lambda = 5 + i),  # <-- outcome column is 'dependent'
      population = sample(100000:200000, n_days, replace = TRUE),
      geog = paste0("region", i)                   # <-- name consistent with function output
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
    knots = quantile(temp_nat$temp, c(0.10, 0.75, 0.90)),  # align with var_per default
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

  # Generate cb_list for regions (use same lag parameterization as the function)
  lagn <- 21
  lagnk <- 3
  cb_list <- lapply(df_list, function(df) {
    dlnm::crossbasis(
      df$temp,
      lag = lagn,
      argvar = list(
        fun = "bs",
        knots = quantile(df$temp, c(0.10, 0.75, 0.90))
      ),
      arglag = list(
        knots = dlnm::logknots(lagn, lagnk)
      )
    )
  })

  # Generate minperc (vector exists but function overwrites/adds National)
  minpercgeog_ <- setNames(sample(20:80, n_regions, replace = TRUE), names(df_list))

  # Call function under test
  result <- hc_add_national_data(
    df_list = df_list,
    pop_list = pop_list,
    var_fun = "bs",
    var_per = c(10, 75, 90),  # matches function default; OK to change if desired
    var_degree = 2,
    lagn = lagn,
    lagnk = lagnk,
    country = "National",
    cb_list = cb_list,
    mm = mm,
    minpercgeog_ = minpercgeog_
  )

  # Basic structure checks
  expect_type(result, "list")
  expect_length(result, 4)
  expect_named(result, NULL)  # unnamed list

  # National dataframe checks
  df_out <- result[[1]]
  expect_true("National" %in% names(df_out))
  nat_df <- df_out[["National"]]

  expect_s3_class(nat_df, "data.frame")
  expect_true(all(c("date", "temp", "dependent", "population", "year", "month", "geog") %in% names(nat_df)))
  expect_equal(nrow(nat_df), length(unique(df_list[[1]]$date)))
  expect_true(all(nat_df$geog == "National"))

  # National crossbasis checks
  cb_out <- result[[2]]
  expect_true("National" %in% names(cb_out))
  nat_cb <- cb_out[["National"]]

  expect_s3_class(nat_cb, "crossbasis")
  expect_true(all(c("argvar", "arglag") %in% names(attributes(nat_cb))))
  expect_equal(attr(nat_cb, "argvar")$fun, "bs")
  # For arglag we pass knots via logknots; assert presence/length rather than 'fun'
  expect_true("knots" %in% names(attr(nat_cb, "arglag")))
  expect_equal(length(attr(nat_cb, "arglag")$knots), lagnk)

  # National min percentile checks (function uses 1:99 grid)
  minperc_out <- result[[3]]
  expect_true("National" %in% names(minperc_out))
  expect_true(is.numeric(minperc_out[["National"]]))
  expect_true(minperc_out[["National"]] >= 1 && minperc_out[["National"]] <= 99)

  # Predicted national coefficients / vcov checks
  mmpredall <- result[[4]]
  expect_type(mmpredall, "list")
  expect_true(all(c("fit", "vcov") %in% names(mmpredall)))
  expect_true(is.numeric(mmpredall$fit))
  expect_true(is.matrix(mmpredall$vcov))
  expect_equal(length(mmpredall$fit), ncol(mmpredall$vcov))
})


test_that("hc_plot_power produces plots correctly (high and low)", {
  # Create sample power_list_high and power_list_low with two regions each
  power_list_high <- list(
    region1 = data.frame(
      geog = "region1",
      temperature = seq(20, 30, by = 2),
      cen = 22,
      log_rr = c(0.1, 0.15, 0.2, 0.25, 0.3, 0.35),
      se = c(0.05, 0.05, 0.06, 0.06, 0.07, 0.07),
      power = c(70, 75, 80, 85, 90, 95)
    ),
    region2 = data.frame(
      geog = "region2",
      temperature = seq(18, 28, by = 2),
      cen = 20,
      log_rr = c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3),
      se = c(0.04, 0.05, 0.05, 0.06, 0.06, 0.07),
      power = c(60, 65, 70, 75, 80, 85)
    )
  )

  power_list_low <- list(
    region1 = data.frame(
      geog = "region1",
      temperature = seq(0, 10, by = 2),
      cen = 5,
      log_rr = c(0.1, 0.12, 0.15, 0.18, 0.2, 0.22),
      se = c(0.05, 0.05, 0.05, 0.06, 0.06, 0.07),
      power = c(65, 70, 75, 80, 85, 90)
    ),
    region2 = data.frame(
      geog = "region2",
      temperature = seq(-2, 8, by = 2),
      cen = 3,
      log_rr = c(0.08, 0.1, 0.12, 0.14, 0.17, 0.2),
      se = c(0.04, 0.05, 0.05, 0.06, 0.06, 0.07),
      power = c(55, 60, 65, 70, 75, 80)
    )
  )

  # Plot without saving
  suppressWarnings(
    hc_plot_power(power_list_high, power_list_low, save_fig = FALSE)
  )

  # create temporary output folder
  output_folder <- tempfile(pattern = "test_plots_", tmpdir = temp_dir)
  model_validation_dir <- file.path(output_folder, "model_validation")

  # Create directories (no need to pre-clean since we use a unique temp path)
  dir.create(model_validation_dir, recursive = TRUE, showWarnings = FALSE)

  # Schedule cleanup immediately after creation
  on.exit(unlink(output_folder, recursive = TRUE, force = TRUE), add = TRUE)

  # Run plotting with saving enabled
  suppressWarnings(
    hc_plot_power(
      power_list_high = power_list_high,
      power_list_low  = power_list_low,
      save_fig = TRUE,
      output_folder_path = output_folder,
      country = "TestCountry"
    )
  )

  # Validate the PDF files were created and are non-empty
  output_path_high <- file.path(model_validation_dir, "power_vs_high_temperature.pdf")
  output_path_low  <- file.path(model_validation_dir, "power_vs_low_temperature.pdf")

  expect_true(file.exists(output_path_high))
  expect_gt(file.info(output_path_high)$size, 0)

  expect_true(file.exists(output_path_low))
  expect_gt(file.info(output_path_low)$size, 0)
})


test_that("hc_rr_results produces expected cumulative RR results", {
  # Create sample data for two geographies
  set.seed(123)
  n_geog <- 2
  n_days <- 10

  # Create df_list with temperature data
  df_list <- lapply(1:n_geog, function(i) {
    data.frame(
      temp = rnorm(n_days, mean = 20 + i * 2, sd = 3),
      dependent = rpois(n_days, lambda = 10 + i)
    )
  })
  names(df_list) <- paste0("geog", 1:n_geog)

  # Create pred_list with prediction values for each geography
  pred_list <- lapply(1:n_geog, function(i) {
    data.frame(
      predvar   = seq(15, 30, length.out = 5),
      allRRfit  = runif(5, 1, 2),
      allRRlow  = runif(5, 0.8, 1.2),
      allRRhigh = runif(5, 2, 3)
    )
  })
  names(pred_list) <- names(df_list)

  # Create minpercgeog_ vector for percentile thresholds
  minpercgeog_ <- setNames(sample(20:60, n_geog, replace = TRUE), names(df_list))

  # Run the function
  result <- suppressWarnings(
    hc_rr_results(
      pred_list = pred_list,
      df_list = df_list,
      minpercgeog_ = minpercgeog_,
      attr_thr_high = 97.5,
      attr_thr_low = 2.5
    )
  )

  # Tests
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)

  # Check required columns exist
  required_cols <- c(
    "Area",
    "MMT",
    "Attr_Threshold_High_Temp",
    "Attr_Threshold_Ligh_Temp",  # note: Ligh in function output
    "Temperature",
    "Temp_Frequency",
    "RR",
    "RR_lower_CI",
    "RR_upper_CI"
  )
  expect_true(all(required_cols %in% names(result)))

  # Check that Area names match geog names
  expect_true(all(result$Area %in% names(df_list)))

  # Check rounding of RR and confidence intervals
  expect_true(all(result$RR == round(result$RR, 2)))
  expect_true(all(result$RR_lower_CI == round(result$RR_lower_CI, 2)))
  expect_true(all(result$RR_upper_CI == round(result$RR_upper_CI, 2)))

  # Check Temp_Frequency is numeric and non-negative
  expect_true(is.numeric(result$Temp_Frequency))
  expect_true(all(result$Temp_Frequency >= 0))

  # Check MMT and threshold temps are correctly computed (ignore names)
  for (geog in names(df_list)) {
    temps <- df_list[[geog]]$temp

    expected_mmt <- unname(round(quantile(temps, minpercgeog_[geog] / 100, na.rm = TRUE), 1))
    actual_mmt <- unique(result[result$Area == geog, "MMT"])
    expect_equal(actual_mmt, expected_mmt)

    expected_high <- unname(round(quantile(temps, 97.5 / 100, na.rm = TRUE), 1))
    actual_high <- unique(result[result$Area == geog, "Attr_Threshold_High_Temp"])
    expect_equal(actual_high, expected_high)

    expected_low <- unname(round(quantile(temps, 2.5 / 100, na.rm = TRUE), 1))
    actual_low <- unique(result[result$Area == geog, "Attr_Threshold_Ligh_Temp"])
    expect_equal(actual_low, expected_low)
  }
})


test_that("hc_rr_results handles edge cases correctly", {
  # Single geography
  df_list_single <- list(geog1 = data.frame(temp = rnorm(10, 20, 2)))
  pred_list_single <- list(
    geog1 = data.frame(
      predvar   = seq(18, 25, length.out = 3),
      allRRfit  = c(1.1, 1.2, 1.3),
      allRRlow  = c(0.9, 1.0, 1.1),
      allRRhigh = c(1.5, 1.6, 1.7)
    )
  )
  minpercgeog_single <- c(geog1 = 50)

  result_single <- suppressWarnings(
    hc_rr_results(pred_list_single, df_list_single, minpercgeog_ = minpercgeog_single)
  )
  expect_s3_class(result_single, "data.frame")
  expect_equal(unique(result_single$Area), "geog1")

  # Missing values in temperature
  df_list_na <- list(geog1 = data.frame(temp = c(NA, 20, 22, NA, 25)))
  pred_list_na <- list(
    geog1 = data.frame(
      predvar   = c(20, 22, 25),
      allRRfit  = c(1.1, 1.2, 1.3),
      allRRlow  = c(0.9, 1.0, 1.1),
      allRRhigh = c(1.5, 1.6, 1.7)
    )
  )
  minpercgeog_na <- c(geog1 = 50)

  result_na <- suppressWarnings(
    hc_rr_results(pred_list_na, df_list_na, minpercgeog_ = minpercgeog_na)
  )
  expect_s3_class(result_na, "data.frame")
  expect_true(all(!is.na(result_na$MMT)))  # MMT should compute despite NA values

  # Empty lists
  df_list_empty <- list()
  pred_list_empty <- list()
  minpercgeog_empty <- c()

  result_empty <- hc_rr_results(
    pred_list = pred_list_empty,
    df_list = df_list_empty,
    minpercgeog_ = minpercgeog_empty
  )
  expect_s3_class(result_empty, "data.frame")
  expect_equal(nrow(result_empty), 0)  # Should return empty data frame without error
})


test_that("hc_plot_rr produces plots correctly", {

  # Create sample df_list
  set.seed(123)
  df_list <- list(
    Geog1 = data.frame(
      date = seq.Date(as.Date("2023-01-01"), by = "day", length.out = 10),
      geog = "Geog1",
      temp = rnorm(10, 20, 3),
      dependent = rpois(10, lambda = 5),
      population = rep(1000000, 10),
      year = rep(2023, 10),
      month = rep(1, 10)
    )
  )

  # Generate dummy crosspred object with strong positive effect
  temp_seq <- seq(-6, 6, length.out = 20)
  basis <- dlnm::onebasis(temp_seq, "lin")
  coef <- 2  # large positive coefficient for RR values
  vcov <- matrix(0.01)
  pred_obj <- dlnm::crosspred(basis, coef = coef, vcov = vcov, cen = 0, at = temp_seq)

  # Wrap in list for hc_plot_rr
  pred_list <- list(Geog1 = pred_obj)

  # minpercgeog_
  minpercgeog_ <- c(Geog1 = 50)

  # Plot without saving
  suppressWarnings(
    hc_plot_rr(
      df_list = df_list,
      pred_list = pred_list,
      attr_thr_high = 0,
      attr_thr_low = 0,
      minpercgeog_ = minpercgeog_,
      save_fig = FALSE
    )
  )

  # Plot with saving enabled (expect warnings)
  output_folder <- file.path(temp_dir, "test_temp_mortality_rr_plots")
  on.exit(unlink(output_folder, recursive = TRUE), add = TRUE)

  if (dir.exists(output_folder)) unlink(output_folder, recursive = TRUE)
  dir.create(output_folder, recursive = TRUE)

  suppressWarnings(
    hc_plot_rr(
      df_list = df_list,
      pred_list = pred_list,
      attr_thr_high = 0,
      attr_thr_low = 0,
      minpercgeog_ = minpercgeog_,
      save_fig = TRUE,
      output_folder_path = output_folder,
      country = "TestCountry"
    )
  )

  # Check PDF file exists and is non-empty
  output_path <- file.path(output_folder, "temp_mortality_rr_plot.pdf")
  expect_true(file.exists(output_path))
  expect_gt(file.info(output_path)$size, 0)
})


test_that("hc_attr produces expected output structure and values", {
  # Create synthetic df_list with sufficient rows for lag calculations
  set.seed(123)
  n_rows <- 60
  df_list <- list(
    Geog1 = data.frame(
      date = seq.Date(as.Date("2000-01-01"), by = "day", length.out = n_rows),
      region = "Geog 1",
      temp = rnorm(n_rows, 5, 2.5),
      dependent = rpois(n_rows, lambda = 2),
      population = rep(2600000, n_rows),
      year = rep(2000, n_rows),
      month = rep(1, n_rows)
    )
  )

  # Create cb_list mimicking crossbasis structure (60 x 5)
  cb <- matrix(rnorm(n_rows * 5), nrow = n_rows, ncol = 5)
  class(cb) <- c("crossbasis", "matrix")
  attr(cb, "df") <- c(5, 2)
  attr(cb, "range") <- c(-6, 24)
  attr(cb, "lag") <- c(0, 2)
  attr(cb, "argvar") <- list(fun = "bs", knots = c(0, 5, 10), degree = 2)
  attr(cb, "arglag") <- list(fun = "strata", breaks = 1)
  cb_list <- list(Geog1 = cb)

  # Create pred_list with coefficients and vcov (length 5)
  pred_list <- list(
    Geog1 = list(
      coefficients = c(-0.19, -0.30, -0.20, -0.14, -0.36),
      vcov = diag(c(0.18, 0.11, 0.15, 0.12, 0.16), 5, 5)
    )
  )

  # Percentile of minimum temperature
  minpercgeog_ <- c(Geog1 = 37)

  # Run hc_attr with balanced thresholds
  result <- hc_attr(
    df_list = df_list,
    cb_list = cb_list,
    pred_list = pred_list,
    minpercgeog_ = minpercgeog_,
    attr_thr_high = 90,
    attr_thr_low = 10
  )

  # Validate output structure
  expect_type(result, "list")
  expect_named(result, names(df_list))
  expect_true(all(sapply(result, is.data.frame)))

  # Expected columns
  expected_cols <- c(
    "region", "date", "temp", "year", "month", "dependent", "population",
    "threshold_temp_high",
    "af_heat", "af_heat_lower_ci", "af_heat_upper_ci",
    "an_heat", "an_heat_lower_ci", "an_heat_upper_ci",
    "ar_heat", "ar_heat_lower_ci", "ar_heat_upper_ci",
    "threshold_temp_low",
    "af_cold", "af_cold_lower_ci", "af_cold_upper_ci",
    "an_cold", "an_cold_lower_ci", "an_cold_upper_ci",
    "ar_cold", "ar_cold_lower_ci", "ar_cold_upper_ci"
  )

  for (geog in names(result)) {
    df <- result[[geog]]
    expect_true(all(expected_cols %in% names(df)))

    # Validate numeric columns contain at least some finite values
    num_cols <- setdiff(expected_cols, c("region", "date", "year", "month"))
    for (col in num_cols) {
      expect_true(is.numeric(df[[col]]))
      expect_true(any(is.finite(df[[col]])))
    }

    # Validate confidence interval ordering on finite rows
    ci_triads <- list(
      c("af_heat_lower_ci", "af_heat", "af_heat_upper_ci"),
      c("an_heat_lower_ci", "an_heat", "an_heat_upper_ci"),
      c("ar_heat_lower_ci", "ar_heat", "ar_heat_upper_ci"),
      c("af_cold_lower_ci", "af_cold", "af_cold_upper_ci"),
      c("an_cold_lower_ci", "an_cold", "an_cold_upper_ci"),
      c("ar_cold_lower_ci", "ar_cold", "ar_cold_upper_ci")
    )
    for (triad in ci_triads) {
      rows <- complete.cases(df[triad])
      if (any(rows)) {
        expect_true(all(df[rows, triad[1]] <= df[rows, triad[2]]))
        expect_true(all(df[rows, triad[2]] <= df[rows, triad[3]]))
      } else {
        succeed()
      }
    }

    # Validate threshold temperatures
    th_high <- unique(df$threshold_temp_high)
    th_low  <- unique(df$threshold_temp_low)
    expect_length(th_high, 1L)
    expect_length(th_low, 1L)
    expect_true(th_low  >= min(df$temp, na.rm = TRUE))
    expect_true(th_high <= max(df$temp, na.rm = TRUE))
  }
})


test_that("hc_attr adds region column when missing", {
  set.seed(42)
  n_rows <- 60
  df_list <- list(
    GeoNoRegion = data.frame(
      date = seq.Date(as.Date("2001-01-01"), by = "day", length.out = n_rows),
      temp = rnorm(n_rows, 7, 2),
      dependent = rpois(n_rows, lambda = 3),
      population = rep(500000, n_rows),
      year = rep(2001, n_rows),
      month = rep(2, n_rows)
    )
  )

  # Use the same cb dimensionality (60 x 5) and attributes
  cb <- matrix(rnorm(n_rows * 5), nrow = n_rows, ncol = 5)
  class(cb) <- c("crossbasis", "matrix")
  attr(cb, "df") <- c(5, 2)
  attr(cb, "range") <- c(-5, 22)
  attr(cb, "lag") <- c(0, 2)
  attr(cb, "argvar") <- list(fun = "bs", knots = c(2, 6, 10), degree = 2)
  attr(cb, "arglag") <- list(fun = "strata", breaks = 1)
  cb_list <- list(GeoNoRegion = cb)

  # Coefficients/vcov length 5
  pred_list <- list(
    GeoNoRegion = list(
      coefficients = c(0.05, -0.02, 0.03, 0.01, -0.04),
      vcov = diag(rep(0.02, 5))
    )
  )

  minpercgeog_ <- c(GeoNoRegion = 50)

  res <- hc_attr(
    df_list, cb_list, pred_list, minpercgeog_,
    attr_thr_high = 90, attr_thr_low = 10
  )

  expect_true("GeoNoRegion" %in% names(res))
  df <- res$GeoNoRegion
  expect_s3_class(df, "data.frame")
  expect_true("region" %in% names(df))
  expect_true(all(df$region == "GeoNoRegion"))
})


test_that("hc_attr_tables aggregates attributable estimates correctly", {
  # Set seed for reproducibility
  set.seed(123)

  # Create mock attr_list for two regions with heat and cold components
  # Ensure non-zero denominators to avoid division warnings
  make_region_df <- function(region_name, years, months, n_rows) {
    data.frame(
      year = rep(years, length.out = n_rows),
      month = rep(months, length.out = n_rows),
      region = region_name,
      population = rep(100000 + sample(0:20000, 1), n_rows),
      temp = rnorm(n_rows, mean = 15, sd = 3),
      threshold_temp_high = rep(27, n_rows),
      threshold_temp_low  = rep(3, n_rows),
      dependent = rep(10 + sample(0:5, 1), n_rows),

      # Heat component: totals = n_rows * 2, CI wider around it
      an_heat            = rep(2, n_rows),
      an_heat_lower_ci   = rep(1.5, n_rows),
      an_heat_upper_ci   = rep(2.5, n_rows),

      # Cold component: totals = n_rows * 1, CI wider around it
      an_cold            = rep(1, n_rows),
      an_cold_lower_ci   = rep(0.5, n_rows),
      an_cold_upper_ci   = rep(1.5, n_rows)
    )
  }

  # Region 1: 10 rows across 2000–2001 and months 1–5
  results_region1 <- make_region_df(
    region_name = "region1",
    years  = 2000:2001,
    months = 1:5,
    n_rows = 10
  )

  # Region 2: 8 rows across 2000–2001 and months 1–4
  results_region2 <- make_region_df(
    region_name = "region2",
    years  = 2000:2001,
    months = 1:4,
    n_rows = 8
  )

  attr_list <- list(
    region1 = results_region1,
    region2 = results_region2
  )

  # Run function
  result <- hc_attr_tables(attr_list, country = "National", meta_analysis = FALSE)

  # Structure tests
  expect_type(result, "list")
  expect_length(result, 3)

  res_attr_tot <- result[[1]]
  attr_yr_list <- result[[2]]
  attr_mth_list <- result[[3]]

  # Check overall totals dataframe
  expect_true(is.data.frame(res_attr_tot))
  required_cols_tot <- c(
    "region", "population", "temp",
    "threshold_temp_high", "threshold_temp_low",
    "dependent",
    "an_heat", "an_heat_lower_ci", "an_heat_upper_ci",
    "an_cold", "an_cold_lower_ci", "an_cold_upper_ci",
    "af_heat", "af_heat_lower_ci", "af_heat_upper_ci",
    "ar_heat", "ar_heat_lower_ci", "ar_heat_upper_ci",
    "af_cold", "af_cold_lower_ci", "af_cold_upper_ci",
    "ar_cold", "ar_cold_lower_ci", "ar_cold_upper_ci"
  )
  expect_true(all(required_cols_tot %in% names(res_attr_tot)))

  # Check yearly list structure
  expect_type(attr_yr_list, "list")
  expect_named(attr_yr_list, c("region1", "region2"))
  expect_true(all(sapply(attr_yr_list, is.data.frame)))

  # Check monthly list structure
  expect_type(attr_mth_list, "list")
  expect_named(attr_mth_list, c("region1", "region2"))
  expect_true(all(sapply(attr_mth_list, is.data.frame)))

  # Validate CI ordering: lower <= estimate <= upper, for heat and cold metrics
  check_ci_ordering <- function(df, lower, est, upper) {
    rows <- stats::complete.cases(df[, c(lower, est, upper)])
    if (any(rows)) {
      expect_true(all(df[rows, lower] <= df[rows, est]))
      expect_true(all(df[rows, est]  <= df[rows, upper]))
    } else {
      succeed()
    }
  }

  for (df in list(res_attr_tot,
                  attr_yr_list$region1, attr_yr_list$region2)) {
    check_ci_ordering(df, "an_heat_lower_ci", "an_heat", "an_heat_upper_ci")
    check_ci_ordering(df, "an_cold_lower_ci", "an_cold", "an_cold_upper_ci")
    check_ci_ordering(df, "af_heat_lower_ci", "af_heat", "af_heat_upper_ci")
    check_ci_ordering(df, "af_cold_lower_ci", "af_cold", "af_cold_upper_ci")
    check_ci_ordering(df, "ar_heat_lower_ci", "ar_heat", "ar_heat_upper_ci")
    check_ci_ordering(df, "ar_cold_lower_ci", "ar_cold", "ar_cold_upper_ci")
  }

  # Validate AF and AR are numeric; when denominators are positive, AF/AR should be finite
  expect_true(is.numeric(res_attr_tot$af_heat))
  expect_true(is.numeric(res_attr_tot$ar_heat))
  expect_true(is.numeric(res_attr_tot$af_cold))
  expect_true(is.numeric(res_attr_tot$ar_cold))

  # Positive denominator check (dependent > 0, population > 0) => finite AF/AR
  denom_rows <- with(res_attr_tot, is.finite(dependent) & dependent > 0 & is.finite(population) & population > 0)
  if (any(denom_rows)) {
    expect_true(all(is.finite(res_attr_tot$af_heat[denom_rows])))
    expect_true(all(is.finite(res_attr_tot$ar_heat[denom_rows])))
    expect_true(all(is.finite(res_attr_tot$af_cold[denom_rows])))
    expect_true(all(is.finite(res_attr_tot$ar_cold[denom_rows])))
  } else {
    succeed()
  }

  # Check that month names are converted correctly in monthly list
  expect_true(all(attr_mth_list$region1$month %in% month.name))
  expect_true(all(attr_mth_list$region2$month %in% month.name))
})


test_that("hc_plot_attr_heat_totals generates plots and validates dynamic elements", {
  # Set seed for reproducibility
  set.seed(123)

  # Mock df_list with date column for two regions
  n_days <- 10
  df_list <- list(
    region1 = data.frame(date = seq.Date(as.Date("2000-01-01"), by = "day", length.out = n_days)),
    region2 = data.frame(date = seq.Date(as.Date("2000-01-01"), by = "day", length.out = n_days))
  )

  # Mock res_attr_tot with required columns (heat only)
  res_attr_tot <- data.frame(
    region = c("region1", "region2"),
    af_heat = c(12.5, 8.3),
    af_heat_lower_ci = c(10.0, 6.0),
    af_heat_upper_ci = c(15.0, 10.0),
    ar_heat = c(5.2, 3.8),
    ar_heat_lower_ci = c(4.0, 3.0),
    ar_heat_upper_ci = c(6.5, 4.5)
  )

  # Plot to a null device for dynamic checks (no file saving)
  expect_no_error(suppress_plot({
    grDevices::pdf(NULL) # Direct to null device for testing
    hc_plot_attr_heat_totals(df_list, res_attr_tot, save_fig = FALSE, country = "region1")
    plot_calls <- recordPlot()
    grDevices::dev.off()
  }))

  # Validate sorting by AF and AR (descending)
  sorted_af <- order(res_attr_tot$af_heat, decreasing = TRUE)
  expect_equal(res_attr_tot$region[sorted_af][1], "region1") # region1 has highest AF

  sorted_ar <- order(res_attr_tot$ar_heat, decreasing = TRUE)
  expect_equal(res_attr_tot$region[sorted_ar][1], "region1") # region1 has highest AR

  # Validate highlight color logic for AF section
  # Colors used in function: bars = "#a04b58"; highlight (national) = "#7a855c"
  bar_col_af_heat <- rep("#a04b58", nrow(res_attr_tot))
  nat_ind_af_heat <- which(res_attr_tot$region[sorted_af] == "region1")
  if (length(nat_ind_af_heat) > 0) {
    bar_col_af_heat[nat_ind_af_heat] <- "#7a855c"
  }
  expect_true("#7a855c" %in% bar_col_af_heat)

  # Validate CI warning text formatting (heat)
  af_heat_ci_range <- c(min(res_attr_tot$af_heat_lower_ci), max(res_attr_tot$af_heat_upper_ci))
  af_warning <- sprintf("Warning: AF CI's range from %.2f%% to %.2f%%", af_heat_ci_range[1], af_heat_ci_range[2])
  expect_match(af_warning, "Warning: AF CI's range from")

  ar_heat_ci_range <- c(min(res_attr_tot$ar_heat_lower_ci), max(res_attr_tot$ar_heat_upper_ci))
  ar_warning <- sprintf("Warning: AR CI's range from %.2f to %.2f per 100,000", ar_heat_ci_range[1], ar_heat_ci_range[2])
  expect_match(ar_warning, "Warning: AR CI's range from")

  # Validate year range calculation for titles
  year_min <- min(sapply(df_list, function(x) min(lubridate::year(x$date), na.rm = TRUE)))
  year_max <- max(sapply(df_list, function(x) max(lubridate::year(x$date), na.rm = TRUE)))
  expect_equal(year_min, 2000)
  expect_equal(year_max, 2000)

  # Validate file saving when save_fig = TRUE
  output_path <- file.path(temp_dir, "mortality_total_heat_attr_plot.pdf")
  on.exit(unlink(output_path, force = TRUE), add = TRUE)

  expect_no_error(suppress_plot(
    hc_plot_attr_heat_totals(
      df_list = df_list,
      res_attr_tot = res_attr_tot,
      save_fig = TRUE,
      output_folder_path = temp_dir,
      country = "region1"
    )
  ))

  expect_true(file.exists(output_path))
  expect_gt(file.info(output_path)$size, 0)
})


test_that("hc_plot_attr_cold_totals generates plots and validates dynamic elements", {
  # Set seed for reproducibility
  set.seed(123)

  # Mock df_list with date column for two regions
  n_days <- 10
  df_list <- list(
    region1 = data.frame(date = seq.Date(as.Date("2000-01-01"), by = "day", length.out = n_days)),
    region2 = data.frame(date = seq.Date(as.Date("2000-01-01"), by = "day", length.out = n_days))
  )

  # Mock res_attr_tot with required columns (cold-specific)
  res_attr_tot <- data.frame(
    region = c("region1", "region2"),
    af_cold = c(9.4, 6.1),
    af_cold_lower_ci = c(7.0, 4.5),
    af_cold_upper_ci = c(11.5, 7.8),
    ar_cold = c(4.8, 2.9),
    ar_cold_lower_ci = c(3.9, 2.3),
    ar_cold_upper_ci = c(5.9, 3.6)
  )

  # Plot to a null device for dynamic checks (no file saving)
  expect_no_error(suppress_plot({
    grDevices::pdf(NULL) # Direct to null device for testing
    hc_plot_attr_cold_totals(df_list, res_attr_tot, save_fig = FALSE, country = "region1")
    plot_calls <- recordPlot()
    grDevices::dev.off()
  }))

  # Validate sorting by AF and AR (descending)
  sorted_af <- order(res_attr_tot$af_cold, decreasing = TRUE)
  expect_equal(res_attr_tot$region[sorted_af][1], "region1") # highest AF

  sorted_ar <- order(res_attr_tot$ar_cold, decreasing = TRUE)
  expect_equal(res_attr_tot$region[sorted_ar][1], "region1") # highest AR

  # Validate highlight color logic for AF section
  # Bars use "#0A2E4D" and highlight (national) "#7a855c"
  bar_col_af_cold <- rep("#0A2E4D", nrow(res_attr_tot))
  nat_ind_af_cold <- which(res_attr_tot$region[sorted_af] == "region1")
  if (length(nat_ind_af_cold) > 0) {
    bar_col_af_cold[nat_ind_af_cold] <- "#7a855c"
  }
  expect_true("#7a855c" %in% bar_col_af_cold)

  # Validate CI warning text formatting for AF and AR
  af_cold_ci_range <- c(min(res_attr_tot$af_cold_lower_ci), max(res_attr_tot$af_cold_upper_ci))
  af_warning <- sprintf("Warning: AF CI's range from %.2f%% to %.2f%%", af_cold_ci_range[1], af_cold_ci_range[2])
  expect_match(af_warning, "Warning: AF CI's range from")

  ar_cold_ci_range <- c(min(res_attr_tot$ar_cold_lower_ci), max(res_attr_tot$ar_cold_upper_ci))
  ar_warning <- sprintf("Warning: AR CI's range from %.2f to %.2f per 100,000", ar_cold_ci_range[1], ar_cold_ci_range[2])
  expect_match(ar_warning, "Warning: AR CI's range from")

  # Validate year range calculation for titles
  year_min <- min(sapply(df_list, function(x) min(lubridate::year(x$date), na.rm = TRUE)))
  year_max <- max(sapply(df_list, function(x) max(lubridate::year(x$date), na.rm = TRUE)))
  expect_equal(year_min, 2000)
  expect_equal(year_max, 2000)

  # Validate file saving when save_fig = TRUE
  output_path <- file.path(temp_dir, "mortality_total_cold_attr_plot.pdf")
  on.exit(unlink(output_path, force = TRUE), add = TRUE)

  expect_no_error(suppress_plot(
    hc_plot_attr_cold_totals(
      df_list = df_list,
      res_attr_tot = res_attr_tot,
      save_fig = TRUE,
      output_folder_path = temp_dir,
      country = "region1"
    )
  ))

  expect_true(file.exists(output_path))
  expect_gt(file.info(output_path)$size, 0)
})


test_that("hc_plot_af_heat_yearly produces yearly AF (heat) plots with CI shading and file output", {
  # Set seed for reproducibility
  set.seed(123)

  # Construct attr_yr_list for two geographies
  attr_yr_list <- list(
    GeogA = data.frame(
      year = 2018:2022,
      af_heat = c(4.5, 6.0, 7.2, 6.8, 8.1),
      af_heat_lower_ci = c(3.0, 4.2, 5.5, 5.0, 6.1),
      af_heat_upper_ci = c(6.0, 7.8, 8.9, 8.4, 10.0)
    ),
    GeogB = data.frame(
      year = 2019:2022,
      af_heat = c(3.8, 5.1, 4.7, 5.9),
      af_heat_lower_ci = c(2.4, 3.6, 3.1, 4.4),
      af_heat_upper_ci = c(5.2, 6.6, 6.3, 7.4)
    )
  )

  # Plot without saving (render to null device)
  expect_no_error(suppress_plot({
    grDevices::pdf(NULL)
    hc_plot_af_heat_yearly(attr_yr_list, save_fig = FALSE, country = "Testland")
    plot_calls <- recordPlot()
    grDevices::dev.off()
  }))

  # Validate year range derived from list
  year_min <- min(sapply(attr_yr_list, function(x) min(x$year, na.rm = TRUE)))
  year_max <- max(sapply(attr_yr_list, function(x) max(x$year, na.rm = TRUE)))
  expect_equal(year_min, 2018)
  expect_equal(year_max, 2022)

  # Validate CI warning message formatting logic (independent of device)
  # Use GeogA to compute expected format; function prints only if outside ylim.
  ga <- attr_yr_list$GeogA
  af_heat_ci_range <- c(min(ga$af_heat_lower_ci), max(ga$af_heat_upper_ci))
  ci_warning <- sprintf(
    "Warning: CI's are outside the bounds of this chart. CI's range from %.2f%% to %.2f%%",
    af_heat_ci_range[1], af_heat_ci_range[2]
  )
  expect_match(ci_warning, "Warning: CI's are outside the bounds of this chart")

  # Prepare output folder and ensure cleanup
  output_folder <- file.path(temp_dir, "hc_plot_af_heat_yearly_out")
  if (dir.exists(output_folder)) unlink(output_folder, recursive = TRUE)
  dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(output_folder, recursive = TRUE, force = TRUE), add = TRUE)

  # Plot with saving enabled
  expect_no_error(suppress_plot(
    hc_plot_af_heat_yearly(
      attr_yr_list = attr_yr_list,
      save_fig = TRUE,
      output_folder_path = output_folder,
      country = "Testland"
    )
  ))

  # Validate output PDF exists and is non-empty
  output_path <- file.path(output_folder, "mortality_af_heat_timeseries.pdf")
  expect_true(file.exists(output_path))
  expect_gt(file.info(output_path)$size, 0)
})


test_that("hc_plot_af_cold_yearly produces yearly AF (cold) plots with CI shading and file output", {
  # Set seed for reproducibility
  set.seed(123)

  # Construct attr_yr_list for two geographies
  attr_yr_list <- list(
    GeogA = data.frame(
      year = 2018:2022,
      af_cold = c(6.2, 5.9, 7.1, 6.5, 7.8),
      af_cold_lower_ci = c(4.7, 4.2, 5.4, 4.9, 6.1),
      af_cold_upper_ci = c(7.7, 7.6, 8.8, 8.1, 9.5)
    ),
    GeogB = data.frame(
      year = 2019:2022,
      af_cold = c(5.0, 5.6, 5.2, 6.0),
      af_cold_lower_ci = c(3.6, 4.1, 3.7, 4.6),
      af_cold_upper_ci = c(6.4, 7.1, 6.7, 7.5)
    )
  )

  # Plot without saving (render to null device)
  expect_no_error(suppress_plot({
    grDevices::pdf(NULL)
    hc_plot_af_cold_yearly(attr_yr_list, save_fig = FALSE, country = "Testland")
    plot_calls <- recordPlot()
    grDevices::dev.off()
  }))

  # Validate year range derived from list
  year_min <- min(sapply(attr_yr_list, function(x) min(x$year, na.rm = TRUE)))
  year_max <- max(sapply(attr_yr_list, function(x) max(x$year, na.rm = TRUE)))
  expect_equal(year_min, 2018)
  expect_equal(year_max, 2022)

  # Validate CI warning message formatting logic (computed independently)
  ga <- attr_yr_list$GeogA
  af_cold_ci_range <- c(min(ga$af_cold_lower_ci), max(ga$af_cold_upper_ci))
  ci_warning <- sprintf(
    "Warning: CI's are outside the bounds of this chart. CI's range from %.2f%% to %.2f%%",
    af_cold_ci_range[1], af_cold_ci_range[2]
  )
  expect_match(ci_warning, "Warning: CI's are outside the bounds of this chart")

  # Prepare output folder and ensure cleanup
  output_folder <- file.path(temp_dir, "hc_plot_af_cold_yearly_out")
  if (dir.exists(output_folder)) unlink(output_folder, recursive = TRUE)
  dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(output_folder, recursive = TRUE, force = TRUE), add = TRUE)

  # Plot with saving enabled
  expect_no_error(suppress_plot(
    hc_plot_af_cold_yearly(
      attr_yr_list = attr_yr_list,
      save_fig = TRUE,
      output_folder_path = output_folder,
      country = "Testland"
    )
  ))

  # Validate output PDF exists and is non-empty
  output_path <- file.path(output_folder, "mortality_af_cold_timeseries.pdf")
  expect_true(file.exists(output_path))
  expect_gt(file.info(output_path)$size, 0)
})


test_that("hc_plot_ar_heat_yearly produces yearly AR (heat) plots with CI shading and file output", {
  # Set seed for reproducibility
  set.seed(123)

  # Construct attr_yr_list for two geographies
  attr_yr_list <- list(
    GeogA = data.frame(
      year = 2018:2022,
      ar_heat = c(3.2, 3.8, 4.5, 4.1, 5.0),
      ar_heat_lower_ci = c(2.4, 2.9, 3.5, 3.2, 4.0),
      ar_heat_upper_ci = c(4.0, 4.7, 5.6, 5.0, 6.1)
    ),
    GeogB = data.frame(
      year = 2019:2022,
      ar_heat = c(2.9, 3.3, 3.0, 3.6),
      ar_heat_lower_ci = c(2.1, 2.5, 2.2, 2.8),
      ar_heat_upper_ci = c(3.7, 4.1, 3.8, 4.4)
    )
  )

  # Plot without saving (render to null device)
  expect_no_error(suppress_plot({
    grDevices::pdf(NULL)
    hc_plot_ar_heat_yearly(attr_yr_list, save_fig = FALSE, country = "Testland")
    plot_calls <- recordPlot()
    grDevices::dev.off()
  }))

  # Validate year range derived from list
  year_min <- min(sapply(attr_yr_list, function(x) min(x$year, na.rm = TRUE)))
  year_max <- max(sapply(attr_yr_list, function(x) max(x$year, na.rm = TRUE)))
  expect_equal(year_min, 2018)
  expect_equal(year_max, 2022)

  # Validate CI warning message formatting logic (computed independently)
  ga <- attr_yr_list$GeogA
  ar_heat_ci_range <- c(min(ga$ar_heat_lower_ci), max(ga$ar_heat_upper_ci))
  ci_warning <- sprintf(
    "Warning: CI's are outside the bounds of this chart. CI's range from %.2f to %.2f per 100,000",
    ar_heat_ci_range[1], ar_heat_ci_range[2]
  )
  expect_match(ci_warning, "Warning: CI's are outside the bounds of this chart")

  # Prepare output folder and ensure cleanup
  output_folder <- file.path(temp_dir, "hc_plot_ar_heat_yearly_out")
  if (dir.exists(output_folder)) unlink(output_folder, recursive = TRUE)
  dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(output_folder, recursive = TRUE, force = TRUE), add = TRUE)

  # Plot with saving enabled
  expect_no_error(suppress_plot(
    hc_plot_ar_heat_yearly(
      attr_yr_list = attr_yr_list,
      save_fig = TRUE,
      output_folder_path = output_folder,
      country = "Testland"
    )
  ))

  # Validate output PDF exists and is non-empty
  output_path <- file.path(output_folder, "mortality_ar_heat_timeseries.pdf")
  expect_true(file.exists(output_path))
  expect_gt(file.info(output_path)$size, 0)
})


#' Plot attributable rates by year - low temps
#'
#' @description Plot attributable rates by year and area with confidence intervals
#'
#' @param attr_yr_list A list of matrices containing yearly estimates of attributable
#' fractions, numbers and rates by area
#' @param save_fig Boolean. Whether to save the plot as an output. Defaults to
#' FALSE.
#' @param output_folder_path Path to folder where plots should be saved.
#' Defaults to NULL.
#' @param country Character. Name of country for national level estimates.
#'
#' @return Plots of yearly attributable rates per area
#'
#' @keywords internal
hc_plot_ar_cold_yearly <- function(attr_yr_list,
                                   save_fig = FALSE,
                                   output_folder_path = NULL,
                                   country = "National") {
  if (save_fig == TRUE) {
    grid <- c(min(length(attr_yr_list), 3), ceiling(length(attr_yr_list) / 3))
    output_path <- file.path(output_folder_path, "mortality_ar_cold_timeseries.pdf")
    pdf(output_path, width = max(10, grid[1] * 5.5), height = max(7, grid[2] * 4.5))

    par(mfrow = c(grid[2], grid[1]), oma = c(0, 0, 4, 0), mar = c(8, 4, 5, 4))
  }

  year_min <- min(sapply(attr_yr_list, function(x) min(x$year, na.rm = TRUE)))
  year_max <- max(sapply(attr_yr_list, function(x) max(x$year, na.rm = TRUE)))

  y_min <- min(sapply(attr_yr_list, function(x) min(x$ar_cold, na.rm = TRUE)))
  y_max <- max(sapply(attr_yr_list, function(x) max(x$ar_cold, na.rm = TRUE)))

  ylim <- c(min(c(0, y_min)), y_max) * 1.5

  for (geog in names(attr_yr_list)) {
    geog_ar <- as.data.frame(attr_yr_list[[geog]])

    plot(
      x = geog_ar$year,
      y = geog_ar$ar_cold,
      type = "l",
      xlim = c(year_min, year_max),
      ylim = ylim,
      xlab = "Year",
      ylab = "Low temperature AR (per 100,000 population)",
      main = geog,
      col = "#296991"
    )

    # Ensure data is sorted by Year
    geog_ar <- geog_ar[order(geog_ar$year), ]

    # Create x and y coordinates for the polygon
    x_poly <- c(geog_ar$year, rev(geog_ar$year))
    y_poly <- c(geog_ar$ar_cold_upper_ci, rev(geog_ar$ar_cold_lower_ci))

    # Draw shaded confidence interval
    polygon(
      x = x_poly,
      y = y_poly,
      col = adjustcolor("#296991", alpha.f = 0.2),
      border = NA
    )

    abline(
      h = 0,
      col = "black",
      lty = 2
    )

    legend("topright",
           inset = c(0, -0.1),
           legend = "95% CI",
           col = adjustcolor("#296991", alpha.f = 0.2),
           pch = 15,
           pt.cex = 2,
           bty = "n",
           xpd = TRUE,
           horiz = TRUE,
           cex = 0.9
    )

    if (save_fig == TRUE) {
      ar_cold_ci_range <- c(min(geog_ar$ar_cold_lower_ci), max(geog_ar$ar_cold_upper_ci))

      if (ar_cold_ci_range[1] < ylim[1] || ar_cold_ci_range[2] > ylim[2]) {
        ci_warning <- sprintf("Warning: CI's are outside the bounds of this chart. CI's range from %.2f to %.2f per 100,000", ar_cold_ci_range[1], ar_cold_ci_range[2])
        ovr_warning <- "(Please refer to the associated data table for more information on the uncertainty around each estimate)"

        mtext(ci_warning, side = 1, line = 5, cex = 0.6, col = "red", font = 3)
        mtext(ovr_warning, side = 1, line = 6, cex = 0.6, col = "red", font = 3)
      }
    }
  }

  if (save_fig == TRUE) {
    year_range <- paste0("(", year_min, " - ", year_max, ")")
    title <- paste0("Yearly attributable rate of low temperature mortality by geography, ", country, " ", year_range)

    mtext(title, outer = TRUE, cex = 1.5, line = 1, font = 2)

    dev.off()
  }
}


test_that("hc_plot_af_heat_monthly produces monthly AF (heat) plots with overlayed temperature and file output", {
  # Set seed for reproducibility
  set.seed(123)

  # Build attr_mth_list: monthly AF (heat) with CI and monthly mean temperature
  months <- month.name
  make_attr_mth_df <- function(base_af = 4, base_temp = 10) {
    data.frame(
      month = months,
      af_heat = round(runif(12, base_af - 1, base_af + 3), 1),
      af_heat_lower_ci = round(runif(12, base_af - 2, base_af), 1),
      af_heat_upper_ci = round(runif(12, base_af + 2, base_af + 4), 1),
      temp = round(runif(12, base_temp - 5, base_temp + 10), 1)
    )
  }

  attr_mth_list <- list(
    GeogA = make_attr_mth_df(base_af = 5, base_temp = 12),
    GeogB = make_attr_mth_df(base_af = 3, base_temp = 9)
  )

  # Build df_list: include date (for year range) and temp (for threshold computation)
  n_days <- 365
  start_date <- as.Date("2020-01-01")
  dates <- seq.Date(start_date, by = "day", length.out = n_days)

  df_list <- list(
    GeogA = data.frame(
      date = dates,
      temp = rnorm(n_days, mean = 12, sd = 5)
    ),
    GeogB = data.frame(
      date = dates,
      temp = rnorm(n_days, mean = 9, sd = 6)
    )
  )

  # Non-saving path: render to a null device to avoid side-effects
  expect_no_error(suppress_plot({
    grDevices::pdf(NULL)
    hc_plot_af_heat_monthly(
      attr_mth_list = attr_mth_list,
      df_list = df_list,
      country = "Testland",
      attr_thr_high = 97.5,
      save_fig = FALSE
    )
    plot_calls <- recordPlot()
    grDevices::dev.off()
  }))

  # Validate year range calculation from df_list
  year_min <- min(sapply(df_list, function(x) min(lubridate::year(x$date), na.rm = TRUE)))
  year_max <- max(sapply(df_list, function(x) max(lubridate::year(x$date), na.rm = TRUE)))
  expect_equal(year_min, 2020)
  expect_equal(year_max, 2020)

  # Validate CI warning text assembled from all regions’ monthly AF CI ranges
  af_heat_ci_min <- min(sapply(attr_mth_list, function(x) min(x$af_heat_lower_ci, na.rm = TRUE)))
  af_heat_ci_max <- max(sapply(attr_mth_list, function(x) max(x$af_heat_upper_ci, na.rm = TRUE)))
  ci_warning <- sprintf("Warning: CI's range from %.2f%% to %.2f%%", af_heat_ci_min, af_heat_ci_max)
  expect_match(ci_warning, "Warning: CI's range from")

  # Validate legend label formatting (uses percentile threshold from df_list temperatures)
  attr_thr_high <- 97.5
  # Example for GeogA
})


test_that("hc_plot_af_cold_monthly produces monthly AF (cold) plots with overlayed temperature and file output", {
  # Set seed for reproducibility
  set.seed(123)

  # Build attr_mth_list: monthly AF (cold) with CI and monthly mean temperature
  months <- month.name
  make_attr_mth_df <- function(base_af = 3.5, base_temp = 8) {
    data.frame(
      month = months,
      af_cold = round(runif(12, base_af - 1, base_af + 3), 1),
      af_cold_lower_ci = round(runif(12, base_af - 2, base_af), 1),
      af_cold_upper_ci = round(runif(12, base_af + 2, base_af + 4), 1),
      temp = round(runif(12, base_temp - 6, base_temp + 8), 1)
    )
  }

  attr_mth_list <- list(
    GeogA = make_attr_mth_df(base_af = 4.2, base_temp = 9),
    GeogB = make_attr_mth_df(base_af = 2.8, base_temp = 7)
  )

  # Build df_list: include date (for year range) and temp (for threshold computation)
  n_days <- 366
  start_date <- as.Date("2020-01-01")
  dates <- seq.Date(start_date, by = "day", length.out = n_days)

  df_list <- list(
    GeogA = data.frame(
      date = dates,
      temp = rnorm(n_days, mean = 9, sd = 5)
    ),
    GeogB = data.frame(
      date = dates,
      temp = rnorm(n_days, mean = 7, sd = 6)
    )
  )

  # Non-saving path: render to a null device to avoid side-effects
  expect_no_error(suppress_plot({
    grDevices::pdf(NULL)
    hc_plot_af_cold_monthly(
      attr_mth_list = attr_mth_list,
      df_list = df_list,
      country = "Testland",
      attr_thr_low = 2.5,
      save_fig = FALSE
    )
    plot_calls <- recordPlot()
    grDevices::dev.off()
  }))

  # Validate year range calculation from df_list
  year_min <- min(sapply(df_list, function(x) min(lubridate::year(x$date), na.rm = TRUE)))
  year_max <- max(sapply(df_list, function(x) max(lubridate::year(x$date), na.rm = TRUE)))
  expect_equal(year_min, 2020)
  expect_equal(year_max, 2020)

  # Validate CI warning text assembled from all regions’ monthly AF CI ranges
  af_cold_ci_min <- min(sapply(attr_mth_list, function(x) min(x$af_cold_lower_ci, na.rm = TRUE)))
  af_cold_ci_max <- max(sapply(attr_mth_list, function(x) max(x$af_cold_upper_ci, na.rm = TRUE)))
  ci_warning <- sprintf("Warning: CI's range from %.2f%% to %.2f%%", af_cold_ci_min, af_cold_ci_max)
  expect_match(ci_warning, "Warning: CI's range from")

  # Validate legend label formatting (uses percentile threshold from df_list temperatures)
  attr_thr_low <- 2.5
  thr_val <- round(stats::quantile(df_list$GeogA$temp, attr_thr_low / 100, na.rm = TRUE), 2)
  expected_leg <- paste0("Low temperature AF (%) - from treshold, ", thr_val, "\u00b0C (", attr_thr_low, "p)")
  expect_match(expected_leg, "Low temperature AF")

  # Saving path: create a temporary output folder and verify file
  output_folder <- file.path(temp_dir, "hc_plot_af_cold_monthly_out")
  if (dir.exists(output_folder)) unlink(output_folder, recursive = TRUE)
  dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(output_folder, recursive = TRUE, force = TRUE), add = TRUE)

  expect_no_error(suppress_plot(
    hc_plot_af_cold_monthly(
      attr_mth_list = attr_mth_list,
      df_list = df_list,
      country = "Testland",
      attr_thr_low = attr_thr_low,
      save_fig = TRUE,
      output_folder_path = output_folder
    )
  ))

  # Validate output PDF exists and is non-empty
  output_path <- file.path(output_folder, "mortality_af_cold_month_plot.pdf")
  expect_true(file.exists(output_path))
  expect_gt(file.info(output_path)$size, 0)
})


test_that("hc_plot_ar_heat_monthly produces monthly AR (heat) plots with overlayed temperature and file output", {
  # Set seed for reproducibility
  set.seed(123)

  # Build attr_mth_list: monthly AR (heat) with CI and monthly mean temperature
  months <- month.name
  make_attr_mth_df <- function(base_ar = 3.5, base_temp = 11) {
    data.frame(
      month = months,
      ar_heat = round(runif(12, base_ar - 1, base_ar + 3), 2),
      ar_heat_lower_ci = round(runif(12, base_ar - 2, base_ar), 2),
      ar_heat_upper_ci = round(runif(12, base_ar + 2, base_ar + 4), 2),
      temp = round(runif(12, base_temp - 6, base_temp + 8), 1)
    )
  }

  attr_mth_list <- list(
    GeogA = make_attr_mth_df(base_ar = 4.0, base_temp = 12),
    GeogB = make_attr_mth_df(base_ar = 2.8, base_temp = 9)
  )

  # Build df_list: include date (for year range) and temp (for threshold computation)
  n_days <- 365
  start_date <- as.Date("2020-01-01")
  dates <- seq.Date(start_date, by = "day", length.out = n_days)

  df_list <- list(
    GeogA = data.frame(
      date = dates,
      temp = rnorm(n_days, mean = 12, sd = 5)
    ),
    GeogB = data.frame(
      date = dates,
      temp = rnorm(n_days, mean = 9, sd = 6)
    )
  )

  # Non-saving path: render to a null device to avoid side-effects
  expect_no_error(suppress_plot({
    grDevices::pdf(NULL)
    hc_plot_ar_heat_monthly(
      attr_mth_list = attr_mth_list,
      df_list = df_list,
      country = "Testland",
      attr_thr_high = 97.5,
      save_fig = FALSE
    )
    plot_calls <- recordPlot()
    grDevices::dev.off()
  }))

  # Validate year range calculation from df_list
  year_min <- min(sapply(df_list, function(x) min(lubridate::year(x$date), na.rm = TRUE)))
  year_max <- max(sapply(df_list, function(x) max(lubridate::year(x$date), na.rm = TRUE)))
  expect_equal(year_min, 2020)
  expect_equal(year_max, 2020)

  # Validate CI warning text assembled from all regions’ monthly AR CI ranges
  ar_heat_ci_min <- min(sapply(attr_mth_list, function(x) min(x$ar_heat_lower_ci, na.rm = TRUE)))
  ar_heat_ci_max <- max(sapply(attr_mth_list, function(x) max(x$ar_heat_upper_ci, na.rm = TRUE)))
  ci_warning <- sprintf("Warning: CI's range from %.2f to %.2f per 100,000 population", ar_heat_ci_min, ar_heat_ci_max)
  expect_match(ci_warning, "Warning: CI's range from")

  # Validate legend label formatting (uses percentile threshold from df_list temperatures)
  attr_thr_high <- 97.5
  thr_val <- round(stats::quantile(df_list$GeogA$temp, attr_thr_high / 100, na.rm = TRUE), 2)
  expected_leg <- paste0("High temperature AR - from threshold, ", thr_val, "\u00b0C (", attr_thr_high, "p)")
  expect_match(expected_leg, "High temperature AR - from threshold")

  # Saving path: create a temporary output folder and verify file
  output_folder <- file.path(temp_dir, "hc_plot_ar_heat_monthly_out")
  if (dir.exists(output_folder)) unlink(output_folder, recursive = TRUE)
  dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(output_folder, recursive = TRUE, force = TRUE), add = TRUE)

  expect_no_error(suppress_plot(
    hc_plot_ar_heat_monthly(
      attr_mth_list = attr_mth_list,
      df_list = df_list,
      country = "Testland",
      attr_thr_high = attr_thr_high,
      save_fig = TRUE,
      output_folder_path = output_folder
    )
  ))

  # Validate output PDF exists and is non-empty
  output_path <- file.path(output_folder, "mortality_ar_heat_month_plot.pdf")
  expect_true(file.exists(output_path))
  expect_gt(file.info(output_path)$size, 0)
})


test_that("hc_plot_ar_cold_monthly produces monthly AR (cold) plots with overlayed temperature and file output", {
  # Set seed for reproducibility
  set.seed(123)

  # Build attr_mth_list: monthly AR (cold) with CI and monthly mean temperature
  months <- month.name
  make_attr_mth_df <- function(base_ar = 2.9, base_temp = 8) {
    data.frame(
      month = months,
      ar_cold = round(runif(12, base_ar - 1, base_ar + 3), 2),
      ar_cold_lower_ci = round(runif(12, base_ar - 2, base_ar), 2),
      ar_cold_upper_ci = round(runif(12, base_ar + 2, base_ar + 4), 2),
      temp = round(runif(12, base_temp - 6, base_temp + 8), 1)
    )
  }

  attr_mth_list <- list(
    GeogA = make_attr_mth_df(base_ar = 3.4, base_temp = 9),
    GeogB = make_attr_mth_df(base_ar = 2.5, base_temp = 7)
  )

  # Build df_list: include date (for year range) and temp (for threshold computation)
  n_days <- 365
  start_date <- as.Date("2020-01-01")
  dates <- seq.Date(start_date, by = "day", length.out = n_days)

  df_list <- list(
    GeogA = data.frame(
      date = dates,
      temp = rnorm(n_days, mean = 9, sd = 5)
    ),
    GeogB = data.frame(
      date = dates,
      temp = rnorm(n_days, mean = 7, sd = 6)
    )
  )

  # Non-saving path: render to a null device to avoid side-effects
  expect_no_error(suppress_plot({
    grDevices::pdf(NULL)
    hc_plot_ar_cold_monthly(
      attr_mth_list = attr_mth_list,
      df_list = df_list,
      country = "Testland",
      attr_thr_low = 2.5,
      save_fig = FALSE
    )
    plot_calls <- recordPlot()
    grDevices::dev.off()
  }))

  # Validate year range calculation from df_list
  year_min <- min(sapply(df_list, function(x) min(lubridate::year(x$date), na.rm = TRUE)))
  year_max <- max(sapply(df_list, function(x) max(lubridate::year(x$date), na.rm = TRUE)))
  expect_equal(year_min, 2020)
  expect_equal(year_max, 2020)

  # Validate CI warning text assembled from all regions’ monthly AR CI ranges
  ar_cold_ci_min <- min(sapply(attr_mth_list, function(x) min(x$ar_cold_lower_ci, na.rm = TRUE)))
  ar_cold_ci_max <- max(sapply(attr_mth_list, function(x) max(x$ar_cold_upper_ci, na.rm = TRUE)))
  ci_warning <- sprintf("Warning: CI's range from %.2f to %.2f per 100,000 population", ar_cold_ci_min, ar_cold_ci_max)
  expect_match(ci_warning, "Warning: CI's range from")

  # Validate legend label formatting (uses percentile threshold from df_list temperatures)
  attr_thr_low <- 2.5
  thr_val <- round(stats::quantile(df_list$GeogA$temp, attr_thr_low / 100, na.rm = TRUE), 2)
  expected_leg <- paste0("Low temperature AR - from threshold, ", thr_val, "\u00b0C (", attr_thr_low, "p)")
  expect_match(expected_leg, "Low temperature AR - from threshold")

  # Saving path: create a temporary output folder and verify file
  output_folder <- file.path(temp_dir, "hc_plot_ar_cold_monthly_out")
  if (dir.exists(output_folder)) unlink(output_folder, recursive = TRUE)
  dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(output_folder, recursive = TRUE, force = TRUE), add = TRUE)

  expect_no_error(suppress_plot(
    hc_plot_ar_cold_monthly(
      attr_mth_list = attr_mth_list,
      df_list = df_list,
      country = "Testland",
      attr_thr_low = attr_thr_low,
      save_fig = TRUE,
      output_folder_path = output_folder
    )
  ))

  # Validate output PDF exists and is non-empty
  output_path <- file.path(output_folder, "mortality_ar_cold_month_plot.pdf")
  expect_true(file.exists(output_path))
  expect_gt(file.info(output_path)$size, 0)
})

test_that("hc_save_results errors when output_folder_path is NULL", {
  # Minimal inputs; function should error before using them
  rr_results <- data.frame(region = character(), rr = numeric(), rr_lower_ci = numeric(), rr_upper_ci = numeric())
  res_attr_tot <- matrix(nrow = 0, ncol = 0)
  attr_yr_list <- list()
  attr_mth_list <- list()
  power_list_high <- list()
  power_list_low  <- list()

  expect_error(
    hc_save_results(
      rr_results = rr_results,
      res_attr_tot = res_attr_tot,
      attr_yr_list = attr_yr_list,
      attr_mth_list = attr_mth_list,
      power_list_high = power_list_high,
      power_list_low  = power_list_low,
      output_folder_path = NULL
    ),
    regexp = "Output path not specified"
  )
})


test_that("hc_save_results writes all expected CSV files and validates content", {
  # Create a temporary output folder and ensure the model_validation subfolder exists
  out_dir <- tempfile(pattern = "hc_save_results_out_", tmpdir = temp_dir)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  model_val_dir <- file.path(out_dir, "model_validation")
  if (!dir.exists(model_val_dir)) {
    dir.create(model_val_dir, recursive = TRUE)
  }

  # Cleanup after test
  on.exit(unlink(out_dir, recursive = TRUE, force = TRUE), add = TRUE)

  # Mock rr_results (data.frame)
  rr_results <- data.frame(
    region = c("region1", "region2"),
    rr = c(1.15, 1.08),
    rr_lower_ci = c(1.05, 1.02),
    rr_upper_ci = c(1.25, 1.15)
  )

  # Mock res_attr_tot (matrix) with named columns
  res_attr_tot <- matrix(
    c("region1", 12.4, 52, 5.6,  9.1, 45, 4.8,  # region, af_heat, an_heat, ar_heat, af_cold, an_cold, ar_cold
      "region2",  8.8, 38, 3.9,  7.3, 32, 3.4),
    nrow = 2, byrow = TRUE
  )
  colnames(res_attr_tot) <- c(
    "region",
    "af_heat", "an_heat", "ar_heat",
    "af_cold", "an_cold", "ar_cold"
  )

  # Mock attr_yr_list (list of data.frames); region first column is required
  attr_yr_list <- list(
    region1 = data.frame(
      region = "region1",
      year = c(2000, 2001),
      af_heat = c(10.5, 12.0),
      an_heat = c(25, 30),
      ar_heat = c(5.2, 6.0),
      af_cold = c(7.1, 8.0),
      an_cold = c(14, 16),
      ar_cold = c(3.1, 3.4)
    ),
    region2 = data.frame(
      region = "region2",
      year = c(2000, 2001),
      af_heat = c(8.2, 9.5),
      an_heat = c(20, 22),
      ar_heat = c(3.8, 4.5),
      af_cold = c(6.5, 7.2),
      an_cold = c(12, 13),
      ar_cold = c(2.9, 3.2)
    )
  )

  # Mock attr_mth_list (list of data.frames); region first column is required
  attr_mth_list <- list(
    region1 = data.frame(
      region = "region1",
      month = month.name[1:3],
      af_heat = c(10.5, 12.0, 11.3),
      an_heat = c(5, 6, 7),
      ar_heat = c(4.5, 5.0, 5.2),
      af_cold = c(7.2, 6.9, 7.5),
      an_cold = c(3, 4, 3),
      ar_cold = c(2.5, 2.8, 2.6)
    ),
    region2 = data.frame(
      region = "region2",
      month = month.name[1:3],
      af_heat = c(8.2, 9.5, 10.1),
      an_heat = c(4, 5, 6),
      ar_heat = c(3.2, 3.8, 4.1),
      af_cold = c(6.4, 6.8, 7.0),
      an_cold = c(2, 2, 3),
      ar_cold = c(2.0, 2.1, 2.3)
    )
  )

  # Mock power lists (rbind-able)
  power_list_high <- list(
    data.frame(region = "region1", temperature = 27.5, power = 0.85, threshold = 97.5),
    data.frame(region = "region2", temperature = 28.0, power = 0.88, threshold = 97.5)
  )
  power_list_low <- list(
    data.frame(region = "region1", temperature = 2.5, power = 0.81, threshold = 2.5),
    data.frame(region = "region2", temperature = 3.0, power = 0.83, threshold = 2.5)
  )

  # Execute and validate file creation
  expect_no_error(
    hc_save_results(
      rr_results = rr_results,
      res_attr_tot = res_attr_tot,
      attr_yr_list = attr_yr_list,
      attr_mth_list = attr_mth_list,
      power_list_high = power_list_high,
      power_list_low  = power_list_low,
      output_folder_path = out_dir
    )
  )

  # Expected file paths
  rr_path        <- file.path(out_dir, "mortality_rr_results.csv")
  attr_tot_path  <- file.path(out_dir, "mortality_attr_tot_results.csv")
  attr_yr_path   <- file.path(out_dir, "mortality_attr_yr_results.csv")
  attr_mth_path  <- file.path(out_dir, "mortality_attr_mth_results.csv")
  power_high_path <- file.path(out_dir, "model_validation", "mortality_high_temp_power_results.csv")
  power_low_path  <- file.path(out_dir, "model_validation", "mortality_low_temp_power_results.csv")

  # Files exist and are non-empty
  expect_true(file.exists(rr_path))
  expect_true(file.exists(attr_tot_path))
  expect_true(file.exists(attr_yr_path))
  expect_true(file.exists(attr_mth_path))
  expect_true(file.exists(power_high_path))
  expect_true(file.exists(power_low_path))

  expect_gt(file.info(rr_path)$size, 0)
  expect_gt(file.info(attr_tot_path)$size, 0)
  expect_gt(file.info(attr_yr_path)$size, 0)
  expect_gt(file.info(attr_mth_path)$size, 0)
  expect_gt(file.info(power_high_path)$size, 0)
  expect_gt(file.info(power_low_path)$size, 0)

  # Validate RR results content
  rr_out <- read.csv(rr_path, stringsAsFactors = FALSE)
  expect_true(all(c("region", "rr", "rr_lower_ci", "rr_upper_ci") %in% names(rr_out)))
  expect_equal(nrow(rr_out), nrow(rr_results))

  # Validate total attributable results: region column present; row count matches
  attr_tot_out <- read.csv(attr_tot_path, stringsAsFactors = FALSE)
  expect_true("region" %in% names(attr_tot_out))
  expect_equal(nrow(attr_tot_out), nrow(res_attr_tot))

  # Validate yearly results: region is the first column; row count equals sum over list
  attr_yr_out <- read.csv(attr_yr_path, stringsAsFactors = FALSE)
  expect_equal(names(attr_yr_out)[1], "region")
  expected_yr_rows <- sum(vapply(attr_yr_list, nrow, integer(1)))
  expect_equal(nrow(attr_yr_out), expected_yr_rows)

  # Validate monthly results: region is the first column; row count equals sum over list
  attr_mth_out <- read.csv(attr_mth_path, stringsAsFactors = FALSE)
  expect_equal(names(attr_mth_out)[1], "region")
  expected_mth_rows <- sum(vapply(attr_mth_list, nrow, integer(1)))
  expect_equal(nrow(attr_mth_out), expected_mth_rows)

  # Validate power results content
  power_high_out <- read.csv(power_high_path, stringsAsFactors = FALSE)
  expect_true(all(c("region", "power", "threshold") %in% names(power_high_out)))
  expect_equal(nrow(power_high_out), length(power_list_high))

  power_low_out <- read.csv(power_low_path, stringsAsFactors = FALSE)
  expect_true(all(c("region", "power", "threshold") %in% names(power_low_out)))
  expect_equal(nrow(power_low_out), length(power_list_low))
})
