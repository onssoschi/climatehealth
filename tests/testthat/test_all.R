library(testthat)
library(climatehealth)
library(config)

test_that('Test new output matches original output', {

  config <- config::get()

  c(output_df,
    tmean_df,
    anregions_bind,
    attrdl_yr_all,
    attr_fractions_yr) %<-%
    climatehealth::do_analysis(input_csv_path_ = config$input_csv_path,
                               output_folder_path_ = config$output_folder_path,
                               save_fig_ = config$save_fig,
                               save_csv_ = config$save_csv,
                               meta_analysis = config$meta_analysis,
                               by_region = config$by_region,
                               time_range_start_ = config$time_range_start,
                               time_range_end_ = config$time_range_end,
                               dependent_col_ = config$dependent_col,
                               independent_col1_ = config$independent_col1,
                               independent_col2_ = config$independent_col2,
                               independent_col3_ = config$independent_col3,
                               time_col_ = config$time_col,
                               region_col_ = config$region_col,
                               temp_col_ = config$temp_col,
                               varfun_ = config$varfun,
                               vardegree_ = config$vardegree,
                               lag_ = config$lag,
                               lagnk_ = config$lagnk,
                               dfseas_ = config$dfseas
    )

  expected_output <- read.csv('testdata/output_one_region_data_original.csv')

  actual_output <- read.csv('testdata/output_one_region_data_new.csv')

  expect_equal(actual_output, expected_output)

})
