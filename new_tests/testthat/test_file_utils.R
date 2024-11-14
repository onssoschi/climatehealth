# Tests for check_file_exists().

# Passing test
test_that(
  "Passing test for check_file_exists().",
  {
    test_data_path <- file.path("testdata", "temperature_test_data.csv")
    exists <- expect_no_error(
      check_file_exists(test_data_path)
    )
    expect_true(
      exists,
      info = "check_file_exists() was expected to return TRUE. Got FALSE"
    )
  }
)

# Test raises when file does not exist
test_that(
  "Test for raises (stops) in check_file_exists().",
  {
    expected_msg <- "No file was found at path:.*"
    expect_error(check_file_exists("not/a/path.no"), regexp = expected_msg)
  }
)

# Test that FALSE is returned when raise=F

test_that(
  "Test that FALSE is returned when file does not exist.",
  {
    additional_info <- "check_file_exists() was expected to return FALSE."
    expect_false(check_file_exists("no/no", F), info = additional_info)
  }
)


