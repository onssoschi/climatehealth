# Tests for descriptive_stats.R

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
  result <- create_correlation_matrix(CORR_MATRIX_DATA, columns = c("A", "B"))
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
    create_correlation_matrix(CORR_MATRIX_DATA, columns = data.frame(x = 1)),
    "'columns' expected a vector of column names"
  )
})

test_that("Non-existent column throws error", {
  expect_error(
    create_correlation_matrix(CORR_MATRIX_DATA, columns = c("A", "Z")),
    "Column Z not in dataset"
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
  result <- create_column_summaries(COL_SUM_TEST_DATA, columns = c("num1", "num2"))
  expect_equal(rownames(result), c("num1", "num2"))
  expect_equal(result["num1", ]$SD, 1.581139, tolerance=1.0e-06)
  expect_true(all(!is.na(result$Mean)))
})

test_that("Non-numeric columns return NA for numeric stats", {
  result <- create_column_summaries(COL_SUM_TEST_DATA, columns = c("char1", "factor1"))
  expect_true(all(is.na(result$IQR)))
  expect_true(all(is.na(result$Variance)))
  expect_true(all(is.na(result$SD)))
})

test_that("Mixed column types are handled correctly", {
  result <- create_column_summaries(COL_SUM_TEST_DATA, columns = c("num1", "char1"))
  expect_false(is.na(result["num1", "Mean"]))
  expect_true(is.na(result["char1", "Mean"]))
})

test_that("Invalid column name throws error", {
  expect_error(
    create_column_summaries(COL_SUM_TEST_DATA, columns = c("num1", "missing_col")),
    "Column missing_col not in dataset"
  )
})

test_that("Non-vector columns argument throws error", {
  expect_error(
    create_column_summaries(COL_SUM_TEST_DATA, columns = data.frame(x = 1)),
    "'columns' expected a vector of column names"
  )
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
    as.vector(result$na_proportion),
    c(0.4, 1.0, 0.2, 0.4)
  )
})

test_that("Summarizing specific columns works", {
  result <- create_na_summary(NA_SUM_TEST_DATA, columns = c("a", "b"))
  expect_equal(result$column, c("a", "b"))
  expect_equal(result$na_count[1], 2)
  expect_equal(result$na_count[2], 5)
})

# Error Raises
test_that("Non-vector columns argument throws error", {
  expect_error(
    create_na_summary(NA_SUM_TEST_DATA, columns = data.frame(x = 1)),
    "'columns' expected a vector of column names."
  )
})

test_that("Invalid column name throws error", {
  expect_error(
    create_na_summary(NA_SUM_TEST_DATA, columns = c("a", "not_a_column")),
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
  result <- detect_outliers(df, columns = "x")

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
