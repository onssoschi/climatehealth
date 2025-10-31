library(patrick)
library(testthat)
library(mockery)

# NOTE: by - Charlie Browning; on - 20/10/2025
# This will not show coverage for functions called with :: or :::
fname <- "install_utils.R"
covr::report(
  covr::file_coverage(
    source_files = file.path("R", fname),
    test_files = paste0("tests/testthat/test_", fname)
  )
)