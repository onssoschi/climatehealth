library(testthat)
library(indicatorfunctions)
library(zeallot)

context("Test errors for incorrect inputs")
test_that('Test min_mortality() produces appropriate errors', {

  # dlist not a list
  expect_error(calculate_min_mortality_temp(
    df_list = data.frame(x = c(1, 2, 3), y = c(1,2,3)),
    regions_df = data.frame(x = 1, y = 1),
    blup = list(1, 2),
    "Argument 'dlist' must be a list of data frames", fixed = TRUE))

  # cities not a data frame
  expect_error(calculate_min_mortality_temp(
    df_list = list(a <- data.frame(x = c(1, 2, 3), y = c(1, 2, 3)),
                 b <- data.frame(x = c(4, 5, 6), y = c(4, 5, 6))),
    regions_df = c(1:10),
    blup = list(1, 2),
    "Argument 'cities' must be a data frame", fixed = TRUE))

  # blup not a list
  expect_error(calculate_min_mortality_temp(
    df_list = list(a <- data.frame(x = c(1, 2, 3), y = c(1, 2, 3)),
                 b <- data.frame(x = c(4, 5, 6), y = c(4, 5, 6))),
    regions_df = data.frame(x = 1, y = 1),
    blup = c(1, 2),
    "Argument 'blup' must be a list", fixed = TRUE))

})

context("Test output data types")
test_that('Test min_mortality() returns correct data types', {


  c(df_list_unordered_, regions_) %<-%
    load_data(
      input_path =  'testdata/regEngWales.csv'
    )

  c(regions_df_, df_list_) %<-%
    get_region_metadata(
      regions = regions_,
      df_list_unordered = df_list_unordered_,
      region_names = c("North East","North West",
                       "Yorkshire & Humber","East Midlands",
                       "West Midlands","East","London",
                       "South East","South West", "Wales")
    )

  c(coef_, vcov_) %<-%
    run_model(df_list = df_list_,
              regions_df = regions_df_)

  c(mv_, blup_) %<-%
    run_meta_model(
      df_list = df_list_,
      regions_df = regions_df_,
      coef = coef_,
      vcov = vcov_
    )

  c(mintempregions_) %<-%
    calculate_min_mortality_temp(
      df_list = df_list_,
      regions_df = regions_df_,
      blup = blup_
    )

  # mintempcity
  expect_equal(typeof(mintempregions_), "double")
  expect_equal(is.vector(mintempregions_), TRUE)
  expect_equal(is.numeric(mintempregions_), TRUE)

})

