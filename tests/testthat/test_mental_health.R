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
      pop = c(1050L, 950L, 1000L, 1200L, 1100L),
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

