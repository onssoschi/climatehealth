<<<<<<< HEAD
fname <- "mental_health.R"
=======
library(patrick)
library(testthat)
library(mockery)

# NOTE: by - Charlie Browning; on - 20/10/2025
# This will not show coverage for functions called with :: or :::
<<<<<<< HEAD
fname <- "install_utils.R"
>>>>>>> dev
=======
fname <- "diseases_shared.R"
>>>>>>> 96261105ab876141845704b6dbda824ab47a73c4
covr::report(
  covr::file_coverage(
    source_files = file.path("R", fname),
    test_files = paste0("tests/testthat/test_", fname)
  )
<<<<<<< HEAD
)
=======
)
>>>>>>> dev
