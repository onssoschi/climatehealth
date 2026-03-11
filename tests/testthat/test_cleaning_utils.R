# Tests for cleaning_utils.R

if (!"package:climatehealth" %in% search()) {
  pkgload::load_all(".", export_all = TRUE, helpers = FALSE, quiet = TRUE)
}

# Tests for reformat_data

date_test_df <- data.frame(row=c(1, 2, 3), date=c("21/01/2004", "31/12/2004", "01/03/2004"))

# Reformatting date
test_that(
  "Test that reformat_data converts a column to datetype.",
  {
    test_df <- reformat_data(date_test_df, T, c(), F)
    valid <- inherits(test_df$date[[1]], "Date")
    expect_true(valid, info = "reformat_data was expected to turn date column to 'Date' type.")
  }
)

# Fill NAs
test_that(
  "Test that reformat_data fill's NULL values with 0 in specified columns.",
  {
    test_df <- data.frame(
      row1 = c(1, 2, NA, 3, NA),
      row2 = c("a", "b", NA, "c", "d"),
      row3 = c(F, F, F, T, F)
    )
    expect_equal(sum(is.na(test_df)), 3)
    # Fill NA's
    test_df <- reformat_data(
      test_df, reformat_date = FALSE, fill_na = c("row1", "row2"), year_from_date = FALSE
    )
    expect_equal(sum(is.na(test_df)), 0)
  }
)

# Year from Date
test_that(
  "Test that reformat_date can derive the year from a date column.",
  {
    test_df <- reformat_data(date_test_df, T, c(), T)
    expect_equal(
      as.vector(test_df$year),
      c(2004, 2004, 2004)
    )
  }
)

# Tests for aggregate_by_column

# Passing test
test_that(
  "Test that aggregate_by_column splits a dataframe into a list of dataframes.",
  {
    # create df and disagg by column
    test_df <- data.frame(
      region = c("r1", "r2", "r3", "r1", "r1", "r2"),
      val = c(1, 2, 3, 4, 5, 6)
    )
    df_list <- aggregate_by_column(test_df, "region")
    # assert length
    expect_equal(length(df_list), 3)
    # assert values
    r1_df <- data.frame(region="r1", val=c(1, 4, 5))
    r2_df <- data.frame(region="r2", val=c(2, 6))
    r3_df <- data.frame(region="r3", val=c(3))
    test_df_list <- list(r1_df, r2_df, r3_df)
    names(test_df_list) <- c("r1", "r2", "r3")
    expect_equal(df_list, test_df_list)
  }
)
