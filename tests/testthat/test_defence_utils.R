# Tests for defence_utils.R

if (!exists("with_parameters_test_that")) {
  source("tests/testthat/helper-libraries.R", local = FALSE)
}

if (!"package:climatehealth" %in% search()) {
  pkgload::load_all(".", export_all = TRUE, helpers = FALSE, quiet = TRUE)
}

# Test is_list_of_dfs().

# Passing test
test_that(
  "Test that is_list_of_dfs() passes with correct data.",
  {
    test_df1 <- data.frame(row1 = c(1, 2, 3), row2 = c("a", "b", "c"))
    test_df2 <- data.frame(row1 = c(4, 5, 6), row2 = c("d", "e", "f"))
    test_list <- list(test_df1, test_df2)
    valid <- expect_no_error(is_list_of_dfs(test_list))
    expect_true(valid, info = "is_list_of_dfs was expected to return T. Got F.")
  }
)

# Tests for raises in is_list_of_dfs().
patrick::with_parameters_test_that(
  "Test that the raises (stop) in is_list_of_dfs work correctly.",
  {
    expect_error(is_list_of_dfs(input_data, param_nm = param), regexp = error_msg)
  },
  # define test params
  input_data = list(
    double(4L),
    list(T)
  ),
  param = c("first_test", "second_test"),
  error_msg = c(
    "first_test expected a list.*Got",
    "second_test expected a list of dataframes.*"
  ),
  .test_name = c("Not a list", "Not a list of dataframes")
)

# Test that is_list_of_dfs() returns FALSE.

patrick::with_parameters_test_that(
  "Test that the raises (stop) in is_list_of_dfs work correctly.",
  {
    expect_false(
      is_list_of_dfs(input_data, raise = F),
      info = "is_list_of_dfs was expected to return F. Got T"
    )
  },
  # define test params
  input_data = list(
    double(4L),
    list(T)
  ),
  .test_name = c("Not a list", "Not a list of dataframes")
)
