library(testthat)
library(indicatorfunctions)

test_that('Test data loads correctly', {

  c(df_list_unordered_, regions_) %<-%
    load_data(
      input_path =  'testdata/regEngWales.csv'
    )

  expect_that(regions_, is_a("character"))
  expect_equal(length(regions_), length(df_list_unordered_))

})
