library(testthat)
library(indicatorfunctions)

test_that('Test data loads correctly', {

  c(dlist, regions) %<-% load_data('testdata/regEngWales.csv')

  expect_that(regions, is_a("character"))
  expect_equal(length(regions), length(dlist))

})
