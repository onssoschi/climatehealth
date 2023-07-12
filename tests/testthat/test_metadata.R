library(testthat)
library(indicatorfunctions)

test_that('Test metadata sorted correctly', {

  regions_test <- c('z', 'a')

  df_list_test <- list()
  df_list_test[[1]] <- data.frame(regnames = 'z')
  df_list_test[[2]] <- data.frame(regnames = 'a')

  c(regions_df_, df_list_) %<-%
    get_region_metadata(
      regions = regions_test,
      df_list_unordered = df_list_test)

  expect_equal(c(regions_df_$regions), c('a', 'z'))

})
