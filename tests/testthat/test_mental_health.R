# Tests for diseases_shared.R
# source("../../R/mental_health.R")

# Test mh_read_and_format_data
test_that("Test mh_read_and_format_data", {

  mock_check_file_extension <- function(data_path, extension) {
    identical(tools::file_ext("test.txt"), extension)
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
    expect_output(
      mh_read_and_format_data("mock_file.csv",
                              "date_column",
                              "region",
                              "temp",
                              "health_outcomes",
                              "pop"),
      data.frame(
        date_column = c("2023-01-01", "02-01-2023", "2023-01-03", "04-01-2023", "2023-01-05"),
        region = c("North", "South", "East", "West", "Central"),
        temp = c(23.5, 25.1, 22.8, 24.3, 21.9),
        health_outcomes = c(10.2, 12.5, 9.8, 11.1, 8.7),
        pop = c(1000L, 1200L, 950L, 1100L, 1050L)
        )
    )
  }

})

# Test mh_pop_totals

