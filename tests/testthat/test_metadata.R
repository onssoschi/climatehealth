library(testthat)
library(indicatorfunctions)

test_that('Test metadata sorted correctly', {

  regions_test <- c('z', 'a')

  dlist_test <- list()
  dlist_test[[1]] <- data.frame(regnames = 'z')
  dlist_test[[2]] <- data.frame(regnames = 'a')

  c(cities, dlist) %<-% get_region_metadata(regions_test, dlist_test)

  expect_equal(c(cities$city), c('a', 'z'))

})
