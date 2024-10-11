library(testthat)
library(climatehealth)
library(config)
library(zeallot)

test_that('Test expected outputs have been created and contain data', {

  config <- config::get()

  (c(output_df, temp_df, anregions_publication, antot_bind, arregions_publication, artot_bind) %<-%
    climatehealth::heat_and_cold_analysis(input_csv_path_ = config$input_csv_path,
                               output_folder_path_ = config$output_folder_path,
                               save_fig_ = config$save_fig,
                               save_csv_ = config$save_csv,
                               meta_analysis_ = config$meta_analysis,
                               by_region_ = config$by_region,
                               RR_distribution_length_ = config$RR_distribution_length,
                               output_year_ = config$output_year,
                               dependent_col_ = config$dependent_col,
                               independent_cols_ = config$independent_cols,
                               time_col_ = config$time_col,
                               region_col_ = config$region_col,
                               temp_col_ = config$temp_col,
                               population_col_ = config$population_col,
                               varfun_ = config$varfun,
                               vardegree_ = config$vardegree,
                               lag_ = config$lag,
                               lagnk_ = config$lagnk,
                               dfseas_ = config$dfseas
    )
  )

  # expected_output <- read.csv('testdata/output_one_region_data_original.csv')
  # actual_output <- read.csv('testdata/output_one_region_data_new.csv')
  # expect_equal(actual_output, expected_output)

  output_a <- 'testdata/output_all_regions_data.csv'
  output_b <- 'testdata/attributable_deaths_regions.csv'
  output_c <- 'testdata/attributable_deaths_total.csv'
  output_d <- 'testdata/attributable_rates_regions.csv'
  output_e <- 'testdata/attributable_rates_total.csv'
  output_f <- 'testdata/wald_test_results.csv'

  output_a_df <- read.csv('testdata/output_all_regions_data.csv')
  output_b_df <- read.csv('testdata/attributable_deaths_regions.csv')
  output_c_df <- read.csv('testdata/attributable_deaths_total.csv')
  output_d_df <- read.csv('testdata/attributable_rates_regions.csv')
  output_e_df <- read.csv('testdata/attributable_rates_total.csv')
  output_f_df <- read.csv('testdata/wald_test_results.csv')

  expect_true(file.exists(output_a))
  expect_true(file.exists(output_b))
  expect_true(file.exists(output_c))
  expect_true(file.exists(output_d))
  expect_true(file.exists(output_e))
  expect_true(file.exists(output_f))

  expect_true(nrow(output_a_df) > 0)
  expect_true(nrow(output_b_df) > 0)
  expect_true(nrow(output_c_df) > 0)
  expect_true(nrow(output_d_df) > 0)
  expect_true(nrow(output_e_df) > 0)
  expect_true(nrow(output_f_df) > 0)

})
