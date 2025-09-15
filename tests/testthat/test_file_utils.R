# Tests for check_file_exists().

# Passing test
test_that(
  "Passing tests for check_file_exists().",
  {
    test_data_path <- file.path(
      system.file(
        "testdata", package="climatehealth"
      ),
      "temperature_test_data.csv"
    )
    exists <- expect_no_error(
      check_file_exists(test_data_path)
    )
    expect_true(
      exists,
      info = "check_file_exists() was expected to return TRUE. Got FALSE."
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

# Tests for check_file_extension().

# Passing test
with_parameters_test_that(
  "Passing tests for check_file_extension().",
  {
    valid <- expect_no_error(check_file_extension(fpath, exp_ext))
    expect_true(valid, info = "check_file_extension() was expected to return TRUE. Got FALSE.")
  },
  fpath = c("test/test.csv",  "tester.csv"),
  exp_ext = c(".csv", "csv"),
  .test_name = c("extension with '.'", "extension without '.")
)

# Test raises when extension is incorrect (parameterized for dynamic error msg)
with_parameters_test_that(
  "Test for raises (stop) when an passed extension is incorrect.",
  {
    exp_msg = paste0("Parameter.*", param, ".* expected filetype.*")
    expect_error(check_file_extension(fpath, ext, param, raise = T), regexp = exp_msg)
  },
  fpath = c("no.no", "no/no.no"),
  ext = c(".yaml", ".shp"),
  param = c("test_param", "param_test"),
  .test_name = c("T1", "T2")
)

# Test that F is returned when raise=F
test_that(
  "Test that FALSE is returned when extension is incorrect and raise=F.",
  {
    additional_info <- "check_file_extension was expected to return FALSE. Got TRUE."
    expect_false(check_file_extension("path.no", "txt", raise = F), info = additional_info)
  }
)



