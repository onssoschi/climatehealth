library(testthat)
library(indicatorfunctions)

test_that('Test new output matches original output', {

  do_analysis('testdata/regEngWales.csv', 'testdata/output_one_region_data_new.csv')

  expected_output <- read.csv('testdata/output_one_region_data_original.csv')

  actual_output <- read.csv('testdata/output_one_region_data_new.csv')

  expect_equal(actual_output, expected_output)

})
