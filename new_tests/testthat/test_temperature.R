# Tests for temperature.R

# Tests for filter_on_rr_distribution

rr_dist_test_df <- data.frame(
  ind = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  year = c(2004, 2005, 2006, 2006, 2007, 2004, 2008, 2009, 2010, 2008),
  rand_val = c(T, T, F, T, F, F, F, F, T, T)
)

test_that(
  "Test for the case when output_year=0.",
  {
    output <- filter_on_rr_distribution(rr_dist_test_df,
                                        5,
                                        5,
                                        15,
                                        0)
    # expect func to create a dist of 5 years from output year (2010-max)
    # case - 2006, 2007, 2008, 2009, 2010 (5 years)
    expected_output <- dplyr::filter(.data=rr_dist_test_df, year > 2005)
    expect_equal(output, expected_output)
  }
)

test_that(
  "Test for the case when RR_distribution_length=0.",
  {
    output <- filter_on_rr_distribution(rr_dist_test_df,
                                        0,
                                        5,
                                        15,
                                        2010)
    # expect rr_dist of 6 (=exclude 2004)
    expected_output <- dplyr::filter(.data=rr_dist_test_df, year > 2004)
    expect_equal(output, expected_output)
  }
)

test_that(
  "Test for the case where the lower range has been exceeded.",
  {
    expect_error(filter_on_rr_distribution(
      rr_dist_test_df,
      5,
      7,
      15,
      0), regexp = "Timeseries to calculate the RR is less than 7 years.*")
  }
)

test_that(
  "Test for the case where the upper range has been exceeded.",
  {
    expect_error(filter_on_rr_distribution(
      rr_dist_test_df,
      14,
      7,
      13,
      0), regexp = "Timeseries to calculate the RR is more than 13 years.*")
  }
)
# TODO: review whether we also need a case for both arguments being 0.

# Tests for load_temperature_data

TEST_DATA_PATH <- "testdata/temperature_sample_data.csv"

test_that(
  "Test that the dataframe is returned as expected (using testdata).",
  {
    resultant <- load_temperature_data(TEST_DATA_PATH,
                                       "death",
                                       "date",
                                       "regnames",
                                       "tmean",
                                       "pop",
                                       0,
                                       0)[[1]]
    # check list length is correct
    expect(length(resultant[[1]]), 2)
    # check shape of first df
    rows <- dim(resultant[1][[1]])[1]
    cols <- dim(resultant[1][[1]])[2]
    expect_equal(rows, 2374, info  = "load_temperature_data expected to return y rows")
    expect_equal(cols, 18, info  = "load_temperature_data expected to return x column")
    # check colnames are correct - checks renaming functionality
    expected_columns<-c("X.1","X","date","year","month","day","time",
                    "yday","dow","region","regnames","temp","tmin","tmax",
                    "dewp","rh","dependent","pop_col")
    expect_equal(colnames(resultant[1][[1]]), expected_columns)
  }
)

test_that(
  "Test that columns are filled automatically (population_col, region_col).",
  {
    resultant <- load_temperature_data(TEST_DATA_PATH,
                                       "death",
                                       "date",
                                       NULL,
                                       "tmean",
                                       NULL,
                                       0,
                                       0)[[1]]
    df <- resultant[1][[1]]
    expect_equal(unique(df$pop_col), c("NONE"))
    expect_equal(unique(df$regnames), c("aggregated"))
  }
)

