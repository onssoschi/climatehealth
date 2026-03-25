# Tests for descriptive_stats.R

if (!exists("with_parameters_test_that")) {
  source("tests/testthat/helper-libraries.R", local = FALSE)
}

if (!"package:climatehealth" %in% search()) {
  pkgload::load_all(".", export_all = TRUE, helpers = FALSE, quiet = TRUE)
}

# Tests for check_empty_dataframe
test_that("Empty dataframe throws an error", {
  expect_error(
    check_empty_dataframe(data.frame()),
    "Please provide a populated dataframe."
  )
})

# Tests for create_correlation_matrix

# Sample data
CORR_MATRIX_DATA <- data.frame(
  A = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  B = c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20),
  C = c(2, 5, 4, 7, 6, 9, 8, 11, 10, 13)
)

# Valid Responses
test_that("Test default behaviour.", {
  result <- create_correlation_matrix(CORR_MATRIX_DATA)
  expect_true(is.matrix(result))
  expect_equal(dim(result), c(3, 3)) # Return size
  expect_equal(as.vector(result["A", ]), c(1, 1, 0.9515227), 4.0E-3) # Test corr values
  expect_equal(rownames(result), c("A", "B", "C"))
})

test_that("Specifying columns returns correct correlation matrix", {
  result <- create_correlation_matrix(CORR_MATRIX_DATA, independent_cols = c("A", "B"))
  expect_equal(dim(result), c(2, 2))
  expect_equal(rownames(result), c("A", "B"))
})

patrick::with_parameters_test_that(
  "Test that all correlation methods work as intended.",
  {
    result <- create_correlation_matrix(
      CORR_MATRIX_DATA,
      correlation_method = corr_method
    )
    expect_equal(as.vector(result["A", ]), exp_output, 1.0e-07)
  },
  corr_method = c("pearson", "kendall", "spearman"),
  exp_output = list(
    c(1, 1, 0.9515227),
    c(1, 1, 0.8222222),
    c(1, 1, 0.9515152)
  ),
  .test_name = c("Pearson", "Kendall", "Spearman")
)

# Error Raises
test_that("Invalid correlation method throws error", {
  expect_error(
    create_correlation_matrix(CORR_MATRIX_DATA, correlation_method = "invalid"),
    "Chosen correlation method is invalid"
  )
})

test_that("Non-vector columns argument throws error", {
  expect_error(
    create_correlation_matrix(CORR_MATRIX_DATA, independent_cols = data.frame(x = 1)),
    "'independent_cols' expected a vector of column names"
  )
})

test_that("Non-existent column throws error", {
  expect_error(
    create_correlation_matrix(CORR_MATRIX_DATA, independent_cols = c("A", "Z")),
    "Column\\(s\\) not in dataset: Z"
  )
})

test_that("Non-numeric columns are filtered before correlation", {
  mixed_df <- data.frame(
    A = c(1, 2, 3, 4),
    B = c(2, 3, 4, 5),
    C = c("x", "y", "z", "w")
  )
  result <- create_correlation_matrix(mixed_df, independent_cols = c("A", "B", "C"))
  expect_true(is.matrix(result))
  expect_equal(rownames(result), c("A", "B"))
})

test_that("Correlation errors clearly when fewer than 2 numeric columns remain", {
  mixed_df <- data.frame(
    A = c(1, 2, 3, 4),
    C = c("x", "y", "z", "w")
  )
  expect_error(
    create_correlation_matrix(mixed_df, independent_cols = c("A", "C")),
    "requires at least 2 numeric columns"
  )
})

# Tests for create_column_summaries

# Test Data
COL_SUM_TEST_DATA <- data.frame(
  num1 = c(1, 2, 3, 4, 5),
  num2 = c(10, 20, 30, 40, 50),
  char1 = c("a", "b", "c", "d", "e"),
  factor1 = factor(c("low", "medium", "high", "medium", "low"))
)

# Valid Responses
test_that("Default behavior summarises all columns", {
  result <- create_column_summaries(COL_SUM_TEST_DATA)
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), ncol(COL_SUM_TEST_DATA))
  expect_true(is.na(result["char1", ]$Mean))
  expect_true(all(c("Variance", "SD", "Min.", "1st Qu.") %in% names(result)))
})

test_that("Summarising specific numeric columns works", {
  result <- create_column_summaries(COL_SUM_TEST_DATA, independent_cols = c("num1", "num2"))
  expect_equal(rownames(result), c("num1", "num2"))
  expect_equal(result["num1", ]$SD, 1.581139, tolerance=1.0e-06)
  expect_true(all(!is.na(result$Mean)))
})

test_that("Non-numeric columns return NA for numeric stats", {
  result <- create_column_summaries(COL_SUM_TEST_DATA, independent_cols = c("char1", "factor1"))
  expect_true(all(is.na(result$IQR)))
  expect_true(all(is.na(result$Variance)))
  expect_true(all(is.na(result$SD)))
})

test_that("Mixed column types are handled correctly", {
  result <- create_column_summaries(COL_SUM_TEST_DATA, independent_cols = c("num1", "char1"))
  expect_false(is.na(result["num1", "Mean"]))
  expect_true(is.na(result["char1", "Mean"]))
})

test_that("Invalid column name throws error", {
  expect_error(
    create_column_summaries(COL_SUM_TEST_DATA, independent_cols = c("num1", "missing_col")),
    "Column\\(s\\) not in dataset: missing_col"
  )
})

test_that("Non-vector columns argument throws error", {
  expect_error(
    create_column_summaries(COL_SUM_TEST_DATA, independent_cols = data.frame(x = 1)),
    "'independent_cols' expected a vector of column names"
  )
})

test_that("Empty independent_cols returns explicit empty summary output", {
  result <- create_column_summaries(COL_SUM_TEST_DATA, independent_cols = character(0))
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 0)
  expect_true(all(c("Data_Type", "Top", "Top_Freq") %in% names(result)))
})

test_that("Non-numeric summaries include mode-style fields", {
  result <- create_column_summaries(COL_SUM_TEST_DATA, independent_cols = c("char1", "factor1"))
  expect_true(all(!is.na(result$Top)))
  expect_true(all(!is.na(result$Top_Freq)))
  expect_true(all(is.na(result$Mean)))
})

# Tests for create_na_summary

# Test data
NA_SUM_TEST_DATA <- data.frame(
  a = c(1, NA, 3, NA, 5),
  b = c(NA, NA, NA, NA, NA),
  c = c("x", "y", NA, "z", "w"),
  d = factor(c("low", NA, "high", "medium", NA)),
  stringsAsFactors = FALSE
)

# Valid Responses
test_that("Default behavior summarises all columns", {
  result <- create_na_summary(NA_SUM_TEST_DATA)
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), ncol(NA_SUM_TEST_DATA))
  expect_equal(sort(result$column), sort(colnames(NA_SUM_TEST_DATA)))
  expect_equal(
    as.vector(result$na_percent),
    c(40, 100, 20, 40)
  )
})

test_that("Summarizing specific columns works", {
  result <- create_na_summary(NA_SUM_TEST_DATA, independent_cols = c("a", "b"))
  expect_equal(result$column, c("a", "b"))
  expect_equal(result$na_count[1], 2)
  expect_equal(result$na_count[2], 5)
})

# Error Raises
test_that("Non-vector columns argument throws error", {
  expect_error(
    create_na_summary(NA_SUM_TEST_DATA, independent_cols = data.frame(x = 1)),
    "'independent_cols' expected a vector of column names."
  )
})

test_that("Invalid column name throws error", {
  expect_error(
    create_na_summary(NA_SUM_TEST_DATA, independent_cols = c("a", "not_a_column")),
    "Column not_a_column not in dataset."
  )
})

# Tests for detect_outliers

test_that("detect_outliers works with default values", {
  df <- data.frame(
    a = c(1, 2, 3, 100),
    b = c(10, 20, 30, 40)
  )
  result <- detect_outliers(df)

  expect_s3_class(result, "data.frame")
  expect_named(result, c("row", "a", "b"))
  expect_true(result$a[4])
  expect_false(any(result$b))  # no outliers in b
})

test_that("detect_outliers selects columns correctly", {
  df <- data.frame(
    x = c(1, 2, 3, 100),
    y = c(10, 20, 30, 40)
  )
  result <- detect_outliers(df, independent_cols = "x")

  expect_named(result, c("row", "x"))
  expect_true(result$x[4])
})

test_that("detect_outliers skips non-numeric columns", {
  df <- data.frame(
    num = c(1, 2, 3, 100),
    chr = c("a", "b", "c", "d")
  )
  result <- detect_outliers(df)

  expect_named(result, c("row", "num"))
  expect_false("chr" %in% names(result))
})

test_that("detect_outliers handles NA values", {
  df <- data.frame(
    val = c(1, 2, NA, 100, 3, NA)
  )
  result <- detect_outliers(df)

  expect_named(result, c("row", "val"))
  expect_true(result$val[4])
  expect_false(result$val[1])
})

test_that("detect_outliers does not raise error on valid input", {
  df <- data.frame(a = c(1, 2, 3, 4))
  expect_error(detect_outliers(df), regexp = NA)
})


# Tests for label_with_unit

test_that("Passing a column with available units works.",
          {
            test_water <- label_with_unit(
              "Water Consumption",
              list("Water Consumption" = "L", "Frequency" = "Hz")
            )
            test_freq <- label_with_unit(
              "Frequency",
              list("Water Consumption" = "L", "Frequency" = "Hz")
            )
            expect_equal(test_water, "Water Consumption (L)")
            expect_equal(test_freq, "Frequency (Hz)")
          }
)

test_that("Passing a column without available units returns the column name",
          {
            test_none <- label_with_unit(
              "Frequency",
              list("Water Consumption" = "L")
            )
            expect_equal(test_none, "Frequency")
          }
)

# Tests for raise_if_null

test_that(
  "An error is raised if the passed value is NULL.",
  {
    expect_error(
      raise_if_null("test", NULL),
      "Unexpected NULL in test"
    )
  }
)

test_that(
  "No error is raised when a non NULL is passed.",
  {
    expect_error(
      raise_if_null("test", "not_null"),
      regexp = NA
    )
  }
)

# Tests for common_descriptive_stats_core

test_that("Fails when no column selection method is provided", {
  temp_dir <- withr::local_tempdir()
  df <- data.frame(x = 1:5)

  expect_error(
    suppressWarnings(common_descriptive_stats_core(
      df = df,
      output_path = temp_dir,
      title = "Subset",
      independent_cols = NULL,
      dependent_col = "x"
    )),
    "`independent_cols` must be a non-empty character vector."
  )
})

test_that("Creates summary file with columns specified", {
  temp_dir <- withr::local_tempdir()
  df <- data.frame(
    date = as.Date("2020-01-01") + 0:4,
    region = rep("A", 5),
    temp = c(10, 15, 20, NA, 25),
    dependent = c(100, 200, 150, 175, 300),
    population = c(1000, 1000, 1000, 1000, 1000)
  )
  units <- c(temp = "degrees", dependent = "cases")

  suppressWarnings(common_descriptive_stats_core(
    df = df,
    output_path = temp_dir,
    title = "Subset",
    units = units,
    dependent_col = "dependent",
    independent_cols = c("temp")
  ))

  expect_true(file.exists(file.path(temp_dir, "dataset_summary.csv")))
})

test_that("Generates all enabled plots and outputs", {
  temp_dir <- withr::local_tempdir()
  df <- data.frame(
    date = as.Date("2020-01-01") + 0:4,
    region = rep(c("A", "B"), length.out = 5),
    temp = c(10, 15, 20, 25, 30),
    dependent = c(100, 200, 150, 175, 300),
    population = c(1000, 1000, 1000, 1000, 1000)
  )
  units <- c(temp = "°C", dependent = "cases")

  suppressWarnings(common_descriptive_stats_core(
    df = df,
    output_path = temp_dir,
    title = "Subset",
    units = units,
    dependent_col = "dependent",
    independent_cols = c("temp", "population"),
    plot_box = TRUE,
    plot_corr_matrix = TRUE,
    plot_dist = TRUE,
    plot_na_counts = TRUE,
    plot_scatter = TRUE,
    plot_seasonal = TRUE,
    plot_regional = TRUE,
    detect_outliers = TRUE,
    calculate_rate = TRUE,
    plot_total = TRUE,
    population_col = "population",
    aggregation_column = "region"
  ))

  expected_files <- c(
    "dataset_summary.csv",
    "boxplots.pdf",
    "correlation_matrix.png",
    "histograms.pdf",
    "na_counts.pdf",
    "dependent_vs_independents.pdf",
    "monthly_averages.pdf",
    "outlier_table.csv",
    "annual_rate_health_outcome_per_100k.pdf",
    "annual_total_counts.pdf"
  )

  for (f in expected_files) {
    expect_true(file.exists(file.path(temp_dir, f)), info = paste("Missing:", f))
  }
})


test_that("common_descriptive_stats_core handles empty dataframes", {
  temp_dir <- withr::local_tempdir()
  df <- data.frame()

  expect_error(
    common_descriptive_stats_core(
      df = df,
      output_path = temp_dir,
      title = "Empty",
      dependent_col = "x",
      independent_cols = c("y")
    ),
    "Please provide a populated dataframe."
  )
})

test_that("Triggers warning and creates empty outlier table when no valid outlier flags", {
  temp_dir <- withr::local_tempdir()
  df <- data.frame(
    date = as.Date("2020-01-01") + 0:4,
    region = c("A", "B", "A", "B", "A"),
    category = c("X", "Y", "X", "Y", "X"),  # non-numeric
    dependent = c(100, 200, 150, 175, 300)
  )

  expect_warning({
    common_descriptive_stats_core(
      df = df,
      output_path = temp_dir,
      title = "No numeric columns",
      dependent_col = "dependent",
      independent_cols = c("category"),
      detect_outliers = TRUE
    )
    outlier_table <- read.csv(file.path(temp_dir, "outlier_table.csv"))
    expect_equal(nrow(outlier_table), 0)
  }, "No valid outlier flag columns found in the dataframe.")
})


# Tests for common_descriptive_stats

test_that("common_descriptive_stats creates the expected files", {
  # Create tmpdir
  tmp_root <- local_tempdir()
  # Prepare minimal data: two regions and their data frames
  df_region_a <- data.frame(
    date = seq.Date(as.Date("2020-01-01"), by = "day", length.out = 10),
    value = rnorm(10)
  )
  df_region_b <- data.frame(
    date = seq.Date(as.Date("2020-01-01"), by = "day", length.out = 10),
    value = rnorm(10)
  )
  df_list <- list(
    RegionA=df_region_a,
    RegionB=df_region_b
  )
  # Create descriptive stats
  out <- suppressWarnings(common_descriptive_stats(
    df_list = df_list,
    output_path = tmp_root,
    timeseries_col = "date",
    dependent_col = "value",
    independent_cols = c("value"),
    aggregation_column = "region",
    plot_ma = TRUE,
    ma_days = 3,
    ma_sides = 1,
    units = list(value = "units"),
    plot_corr_matrix = FALSE,
    plot_dist = FALSE,
    plot_box = FALSE,
    plot_na_counts = FALSE,
    plot_scatter = FALSE,
    plot_seasonal = FALSE,
    plot_regional = FALSE,
    plot_total = FALSE,
    detect_outliers = FALSE,
    calculate_rate = FALSE
  ))
  # Validate outputs have been created
  expected_parent <- file.path(tmp_root, "descriptive_stats")
  expect_true(dir.exists(expected_parent))
  expect_true(dir.exists(out[1]))
  expect_true(startsWith(
    normalizePath(out[1], winslash = "/", mustWork = TRUE),
    normalizePath(expected_parent, winslash = "/", mustWork = TRUE)
  ))
  expect_equal(out[2], "descriptive_stats")

  # Validate correct output files are present
  all_folder <- file.path(out[1], "All")
  expect_true(dir.exists(all_folder))
  expect_true(file.exists(file.path(all_folder, "dataset_summary.csv")))

  for (region in c("RegionA", "RegionB")) {
    region_folder <- file.path(out[1], region)
    expect_true(dir.exists(region_folder), info = paste("Missing region folder:", region))
    pdf_path <- file.path(region_folder, "moving_average.pdf")
    expect_true(file.exists(pdf_path), info = paste("Missing moving average PDF for", region))
    expect_true(file.exists(file.path(region_folder, "dataset_summary.csv")))
    expect_gt(file.info(pdf_path)$size, 0)
  }
})

test_that(
  "common_descriptive_stats creates region output directories without plot_ma=T",
  {
    # Create tmpdir
    tmp_root <- local_tempdir()
    df <- data.frame(
      date = seq.Date(as.Date("2020-01-01"), by = "day", length.out = 10),
      value = rnorm(10)
    )
    out <- common_descriptive_stats(
      df_list = list(region1=df),
      output_path = tmp_root,
      timeseries_col = "date",
      dependent_col = "value",
      independent_cols = c("value"),
      aggregation_column = "region",
      plot_ma = FALSE,
      units = list(value = "units"),
      plot_corr_matrix = FALSE,
      plot_dist = FALSE,
      plot_box = FALSE,
      plot_na_counts = FALSE,
      plot_scatter = FALSE,
      plot_seasonal = FALSE,
      plot_regional = FALSE,
      plot_total = FALSE,
      detect_outliers = FALSE,
      calculate_rate = FALSE
    )
    expect_false(
      file.exists(file.path(out[1], "region1", "moving_average.pdf"))
    )
  }
)


# Tests for common_descriptive_stats_api

# Helper dataset
cds_api_df <- data.frame(
  date = c("2020-01-01", "2020-01-02", "2020-01-03", "2020-01-04"),
  value = c(10, 20, 30, 40),
  region = c("North", "South", "North", "South"),
  population = c(100, 200, 150, 250),
  humidity = c(60, 65, 63, 62)
)

test_that("API runs with all features enabled", {
  tmp <- local_tempdir()

  out <- suppressWarnings(common_descriptive_stats_api(
    data = cds_api_df,
    aggregation_column = "region",
    population_col = "population",
    dependent_col = "value",
    independent_cols = c("population", "humidity"),
    units = c(value = "units", population = "people", humidity = "percent"),
    plot_correlation = TRUE,
    plot_dist_hists = TRUE,
    plot_ma = TRUE,
    plot_na_counts = TRUE,
    plot_scatter = TRUE,
    plot_box = TRUE,
    plot_seasonal = TRUE,
    plot_regional = TRUE,
    plot_total = TRUE,
    correlation_method = "pearson",
    ma_days = 2,
    ma_sides = 1,
    timeseries_col = "date",
    detect_outliers = TRUE,
    calculate_rate = TRUE,
    output_path = tmp
  ))

  expect_true(dir.exists(out[1]))
  expect_equal(out[2], "descriptive_stats")
})

test_that("API runs with minimal required inputs and no aggregation", {
  tmp <- local_tempdir()

  out <- common_descriptive_stats_api(
    data = cds_api_df,
    dependent_col = "value",
    independent_cols = c("population"),
    output_path = tmp,
    plot_correlation = FALSE,
    plot_dist_hists = FALSE,
    plot_ma = FALSE,
    plot_na_counts = FALSE,
    plot_scatter = FALSE,
    plot_box = FALSE,
    plot_seasonal = FALSE,
    plot_regional = FALSE,
    plot_total = FALSE,
    detect_outliers = FALSE,
    calculate_rate = FALSE
  )

  expect_true(dir.exists(out[1]))
})

test_that("API uses aligned moving-average defaults when plot_ma = TRUE", {
  tmp <- local_tempdir()

  out <- common_descriptive_stats_api(
    data = cds_api_df,
    dependent_col = "value",
    independent_cols = c("population"),
    output_path = tmp,
    plot_ma = TRUE,
    plot_correlation = FALSE,
    plot_dist_hists = FALSE,
    timeseries_col = "date",
    detect_outliers = FALSE,
    calculate_rate = FALSE
  )

  expect_true(dir.exists(out[1]))
})

test_that("API uses aligned correlation defaults when plot_correlation = TRUE", {
  tmp <- local_tempdir()

  out <- common_descriptive_stats_api(
    data = cds_api_df,
    dependent_col = "value",
    independent_cols = c("population"),
    output_path = tmp,
    plot_correlation = TRUE,
    plot_dist_hists = FALSE,
    plot_ma = FALSE,
    detect_outliers = FALSE,
    calculate_rate = FALSE
  )

  expect_true(dir.exists(out[1]))
})

test_that("API errors if dependent_col is not in dataset", {
  tmp <- local_tempdir()

  bad_df <- cds_api_df[, c("date", "region", "population")]

  expect_error(
    common_descriptive_stats_api(
      data = bad_df,
      dependent_col = "value",
      independent_cols = c("population"),
      output_path = tmp,
      plot_correlation = FALSE,
      plot_dist_hists = FALSE,
      plot_ma = FALSE,
      detect_outliers = FALSE,
      calculate_rate = FALSE
    ),
    "Column 'value' not in passed dataset"
  )
})

test_that("API converts date column correctly", {
  tmp <- local_tempdir()

  df <- data.frame(
    date = c("01/01/2020", "02/01/2020"),
    value = c(1, 2),
    region = c("A", "B"),
    population = c(100, 200)
  )

  out <- common_descriptive_stats_api(
    data = df,
    aggregation_column = "region",
    dependent_col = "value",
    independent_cols = c("population"),
    output_path = tmp,
    plot_correlation = FALSE,
    plot_dist_hists = FALSE,
    plot_ma=FALSE,
    timeseries_col = "date",
    detect_outliers = FALSE,
    calculate_rate = FALSE
  )

  expect_true(dir.exists(out[1]))
})

# Tests for run_descriptive_stats / run_descriptive_stats_api

test_that("run_descriptive_stats returns structured output with run folder", {
  tmp <- local_tempdir()
  df <- data.frame(
    date = as.Date("2024-01-01") + 0:5,
    region = rep(c("North", "South"), each = 3),
    value = c(10, 12, 11, 8, 9, 10),
    population = c(100, 100, 100, 120, 120, 120)
  )

  out <- run_descriptive_stats(
    data = df,
    output_path = tmp,
    aggregation_column = "region",
    population_col = "population",
    dependent_col = "value",
    independent_cols = c("population"),
    timeseries_col = "date",
    plot_corr_matrix = FALSE,
    plot_dist = FALSE,
    plot_ma = FALSE,
    plot_na_counts = FALSE,
    plot_scatter = FALSE,
    plot_box = FALSE,
    plot_seasonal = FALSE,
    plot_regional = FALSE,
    plot_total = FALSE,
    detect_outliers = FALSE,
    calculate_rate = FALSE
  )

  expect_s3_class(out, "descriptive_stats_run")
  expect_true(all(c(
    "base_output_path",
    "run_id",
    "run_output_path",
    "region_output_paths"
  ) %in% names(out)))
  expect_true(dir.exists(out$run_output_path))
  expect_true(startsWith(
    normalizePath(out$run_output_path, winslash = "/", mustWork = TRUE),
    normalizePath(file.path(tmp, "descriptive_stats"), winslash = "/", mustWork = TRUE)
  ))
  expect_true(dir.exists(out$region_output_paths$All))
  expect_true(dir.exists(out$region_output_paths$North))
  expect_true(dir.exists(out$region_output_paths$South))
})

test_that("run_descriptive_stats respects provided run_id", {
  tmp <- local_tempdir()
  df <- data.frame(
    date = as.Date("2024-01-01") + 0:2,
    value = c(1, 2, 3)
  )
  out <- run_descriptive_stats(
    data = df,
    output_path = tmp,
    dependent_col = "value",
    independent_cols = c("value"),
    timeseries_col = "date",
    run_id = "req_12345",
    plot_corr_matrix = FALSE,
    plot_dist = FALSE,
    plot_ma = FALSE,
    plot_na_counts = FALSE,
    plot_scatter = FALSE,
    plot_box = FALSE,
    plot_seasonal = FALSE,
    plot_regional = FALSE,
    plot_total = FALSE,
    detect_outliers = FALSE,
    calculate_rate = FALSE
  )
  expect_equal(out$run_id, "req_12345")
  expect_true(grepl("req_12345$", out$run_output_path))
})

test_that("run_descriptive_stats create_base_dir handling is enforced", {
  missing_dir <- tempfile("missing_ds_")
  df <- data.frame(
    date = as.Date("2024-01-01") + 0:2,
    value = c(1, 2, 3)
  )

  expect_error(
    run_descriptive_stats(
      data = df,
      output_path = missing_dir,
      dependent_col = "value",
      independent_cols = c("value"),
      timeseries_col = "date",
      create_base_dir = FALSE,
      plot_corr_matrix = FALSE,
      plot_dist = FALSE,
      plot_ma = FALSE,
      plot_na_counts = FALSE,
      plot_scatter = FALSE,
      plot_box = FALSE,
      plot_seasonal = FALSE,
      plot_regional = FALSE,
      plot_total = FALSE,
      detect_outliers = FALSE,
      calculate_rate = FALSE
    ),
    "Set `create_base_dir = TRUE`"
  )

  expect_error(
    run_descriptive_stats(
      data = df,
      output_path = missing_dir,
      dependent_col = "value",
      independent_cols = c("value"),
      timeseries_col = "date",
      create_base_dir = TRUE,
      plot_corr_matrix = FALSE,
      plot_dist = FALSE,
      plot_ma = FALSE,
      plot_na_counts = FALSE,
      plot_scatter = FALSE,
      plot_box = FALSE,
      plot_seasonal = FALSE,
      plot_regional = FALSE,
      plot_total = FALSE,
      detect_outliers = FALSE,
      calculate_rate = FALSE
    ),
    regexp = NA
  )
  expect_true(dir.exists(missing_dir))
})

test_that("run_descriptive_stats preflight validation errors on missing required fields", {
  tmp <- local_tempdir()
  df <- data.frame(
    date = as.Date("2024-01-01") + 0:2,
    value = c(1, 2, 3)
  )

  expect_error(
    run_descriptive_stats(
      data = df,
      output_path = tmp,
      dependent_col = "value",
      independent_cols = c("value"),
      plot_ma = TRUE,
      ma_days = 2,
      ma_sides = 1
    ),
    "timeseries_col"
  )

  expect_error(
    run_descriptive_stats(
      data = df,
      output_path = tmp,
      dependent_col = "value",
      independent_cols = c("value"),
      timeseries_col = "date",
      calculate_rate = TRUE
    ),
    "population_col"
  )
})

test_that("run_descriptive_stats_api contract works with list payload input", {
  tmp <- local_tempdir()
  payload_df <- data.frame(
    date = c("2024-01-01", "2024-01-02", "2024-01-03"),
    region = c("N", "S", "N"),
    value = c(1, 2, 3),
    population = c(10, 20, 10)
  )

  out <- run_descriptive_stats_api(
    data = as.list(payload_df),
    output_path = tmp,
    aggregation_column = "region",
    population_col = "population",
    dependent_col = "value",
    independent_cols = c("population"),
    plot_corr_matrix = FALSE,
    plot_dist = FALSE,
    plot_ma = FALSE,
    plot_na_counts = FALSE,
    plot_scatter = FALSE,
    plot_box = FALSE,
    plot_seasonal = FALSE,
    plot_regional = FALSE,
    plot_total = FALSE,
    detect_outliers = FALSE,
    calculate_rate = FALSE,
    create_base_dir = TRUE
  )

  expect_s3_class(out, "descriptive_stats_run")
  expect_true(dir.exists(out$run_output_path))
  expect_true(all(c("All", "N", "S") %in% names(out$region_output_paths)))
})

test_that("run_descriptive_stats_api example path works with default moving-average settings", {
  tmp <- local_tempdir()

  out <- run_descriptive_stats_api(
    data = list(
      date = as.character(as.Date("2024-01-01") + 0:29),
      region = rep(c("A", "B"), each = 15),
      outcome = seq_len(30),
      temp = seq(20, 34.5, by = 0.5)
    ),
    output_path = tmp,
    aggregation_column = "region",
    dependent_col = "outcome",
    independent_cols = c("temp"),
    timeseries_col = "date",
    plot_corr_matrix = TRUE,
    create_base_dir = TRUE
  )

  expect_s3_class(out, "descriptive_stats_run")
  expect_true(file.exists(file.path(out$region_output_paths$All, "moving_average.pdf")))
})

test_that("run_descriptive_stats_api contract returns clear errors for bad payloads", {
  tmp <- local_tempdir()

  missing_col_payload <- list(
    date = c("2024-01-01", "2024-01-02"),
    region = c("N", "S"),
    value = c(1, 2)
  )

  expect_error(
    run_descriptive_stats_api(
      data = missing_col_payload,
      output_path = tmp,
      aggregation_column = "region",
      dependent_col = "value",
      independent_cols = c("population"),
      plot_corr_matrix = FALSE,
      plot_dist = FALSE,
      plot_ma = FALSE,
      plot_na_counts = FALSE,
      plot_scatter = FALSE,
      plot_box = FALSE,
      plot_seasonal = FALSE,
      plot_regional = FALSE,
      plot_total = FALSE,
      detect_outliers = FALSE,
      calculate_rate = FALSE,
      create_base_dir = TRUE
    ),
    "Column 'population' not in passed dataset"
  )

  expect_error(
    run_descriptive_stats_api(
      data = list(
        date = c("2024-01-01", "2024-01-02"),
        value = c(1, 2),
        population = c(10, 20)
      ),
      output_path = tmp,
      dependent_col = "value",
      independent_cols = c("population"),
      plot_ma = TRUE,
      ma_days = 2,
      ma_sides = 1,
      plot_corr_matrix = FALSE,
      plot_dist = FALSE,
      plot_na_counts = FALSE,
      plot_scatter = FALSE,
      plot_box = FALSE,
      plot_seasonal = FALSE,
      plot_regional = FALSE,
      plot_total = FALSE,
      detect_outliers = FALSE,
      calculate_rate = FALSE,
      create_base_dir = TRUE
    ),
    "timeseries_col"
  )
})

invoke_descriptive_stats_endpoint <- function(payload) {
  tryCatch(
    {
      result <- do.call(run_descriptive_stats_api, payload)
      list(
        status = 200L,
        body = list(
          success = TRUE,
          result = result
        )
      )
    },
    error = function(e) {
      list(
        status = 400L,
        body = list(
          success = FALSE,
          error = conditionMessage(e)
        )
      )
    }
  )
}

test_that("endpoint-style payload mapping returns expected response shape", {
  tmp <- local_tempdir()
  payload <- list(
    data = list(
      date = c("2024-01-01", "2024-01-02", "2024-01-03"),
      region = c("N", "S", "N"),
      value = c(2, 3, 4),
      population = c(100, 200, 100)
    ),
    output_path = tmp,
    aggregation_column = "region",
    population_col = "population",
    dependent_col = "value",
    independent_cols = c("population"),
    timeseries_col = "date",
    plot_corr_matrix = FALSE,
    plot_dist = FALSE,
    plot_ma = FALSE,
    plot_na_counts = FALSE,
    plot_scatter = FALSE,
    plot_box = FALSE,
    plot_seasonal = FALSE,
    plot_regional = FALSE,
    plot_total = FALSE,
    detect_outliers = FALSE,
    calculate_rate = FALSE,
    create_base_dir = TRUE
  )

  res <- invoke_descriptive_stats_endpoint(payload)
  expect_equal(res$status, 200L)
  expect_true(isTRUE(res$body$success))
  expect_s3_class(res$body$result, "descriptive_stats_run")
  expect_true(all(c(
    "base_output_path",
    "run_id",
    "run_output_path",
    "region_output_paths"
  ) %in% names(res$body$result)))
})

test_that("endpoint-style payload mapping returns clear contract errors", {
  tmp <- local_tempdir()

  missing_required <- list(
    data = list(
      date = c("2024-01-01", "2024-01-02"),
      value = c(1, 2)
    ),
    output_path = tmp,
    independent_cols = c("value"),
    plot_corr_matrix = FALSE,
    plot_dist = FALSE,
    plot_ma = FALSE,
    plot_na_counts = FALSE,
    plot_scatter = FALSE,
    plot_box = FALSE,
    plot_seasonal = FALSE,
    plot_regional = FALSE,
    plot_total = FALSE,
    detect_outliers = FALSE,
    calculate_rate = FALSE,
    create_base_dir = TRUE
  )
  missing_required_res <- invoke_descriptive_stats_endpoint(missing_required)
  expect_equal(missing_required_res$status, 400L)
  expect_false(missing_required_res$body$success)
  expect_match(missing_required_res$body$error, "dependent_col")

  invalid_column <- list(
    data = list(
      date = c("2024-01-01", "2024-01-02"),
      value = c(1, 2)
    ),
    output_path = tmp,
    dependent_col = "value",
    independent_cols = c("population"),
    plot_corr_matrix = FALSE,
    plot_dist = FALSE,
    plot_ma = FALSE,
    plot_na_counts = FALSE,
    plot_scatter = FALSE,
    plot_box = FALSE,
    plot_seasonal = FALSE,
    plot_regional = FALSE,
    plot_total = FALSE,
    detect_outliers = FALSE,
    calculate_rate = FALSE,
    create_base_dir = TRUE
  )
  invalid_column_res <- invoke_descriptive_stats_endpoint(invalid_column)
  expect_equal(invalid_column_res$status, 400L)
  expect_false(invalid_column_res$body$success)
  expect_match(invalid_column_res$body$error, "Column 'population' not in passed dataset")
})

test_that("deprecated wrapper aliases remain functional", {
  tmp <- local_tempdir()
  df <- data.frame(
    date = as.Date("2024-01-01") + 0:2,
    value = c(1, 2, 3)
  )

  old_out <- suppressWarnings(
    common_descriptive_stats(
      df_list = list(region1 = df),
      output_path = tmp,
      dependent_col = "value",
      independent_cols = c("value"),
      timeseries_col = "date",
      plot_corr_matrix = FALSE,
      plot_dist = FALSE,
      plot_ma = FALSE,
      plot_na_counts = FALSE,
      plot_scatter = FALSE,
      plot_box = FALSE,
      plot_seasonal = FALSE,
      plot_regional = FALSE,
      plot_total = FALSE,
      detect_outliers = FALSE,
      calculate_rate = FALSE
    )
  )
  expect_type(old_out, "character")
  expect_true(dir.exists(old_out[1]))

  old_api_out <- suppressWarnings(
    common_descriptive_stats_api(
      data = data.frame(
        date = c("2024-01-01", "2024-01-02"),
        value = c(1, 2),
        population = c(10, 20)
      ),
      output_path = tmp,
      dependent_col = "value",
      independent_cols = c("population"),
      plot_correlation = FALSE,
      plot_dist_hists = FALSE,
      plot_ma = FALSE,
      plot_na_counts = FALSE,
      plot_scatter = FALSE,
      plot_box = FALSE,
      plot_seasonal = FALSE,
      plot_regional = FALSE,
      plot_total = FALSE,
      detect_outliers = FALSE,
      calculate_rate = FALSE
    )
  )
  expect_type(old_api_out, "character")
  expect_true(dir.exists(old_api_out[1]))
})
