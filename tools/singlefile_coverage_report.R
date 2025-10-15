library(patrick)
library(testthat)

fname <- "diseases_shared.R"
covr::report(
  covr::file_coverage(
    source_files = file.path("R", fname),
    test_files = paste0("tests/testthat/test_", fname)
  )
)