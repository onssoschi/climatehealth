# Tests for descriptive_stats.R

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

# Error Raisees
test_that("Empty dataframe throws an error", {
  expect_error(
    create_column_summaries(data.frame()),
    "Please provide a populated dataframe."
  )
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
