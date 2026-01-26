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
