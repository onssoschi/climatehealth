# Utils to help users install required packages for climatehealth

#' Check for Rtools Installation on Windows
#'
#' Verifies whether Rtools is installed and properly configured on a Windows system.
#'
#' @details
#' The function uses \code{pkgbuild::check_build_tools(debug = TRUE)} to test for the presence
#' of Rtools and its integration with R. If Rtools is missing or misconfigured, the function
#' throws an error with installation instructions.
#'
#' @return Returns \code{TRUE} invisibly if Rtools is detected and functional. Otherwise, throws an error.
#'
#' @examples
#' \dontrun{
#' check_has_rtools()
#' }
#'
#' @seealso \code{\link[pkgbuild]{check_build_tools}}, \url{https://cran.r-project.org/bin/windows/Rtools/}
#'
#' @keywords internal
check_has_rtools <- function() {
    # Create temp dir
    temp_dir <- tempdir()
    temp_file <- file.path(temp_dir, "test.c")
    temp_file <- normalizePath(temp_file, winslash = "\\", mustWork = FALSE)

    # Try to run an rtools dependent command
    result <- pkgbuild::check_build_tools(debug=TRUE)
    # catch errors and warn users
    if (!any(grepl("TRUE", result, ignore.case = TRUE))) {
        stop(paste0(
            "\n",
            "Rtools is required to install INLA from source on Windows.\n\n",
            "Please download and install the correct version for your R setup from:\n",
            "https://cran.r-project.org/bin/windows/Rtools/\n\n",
            "After installing Rtools, restart R and try again."
        ))
    }
    return(T)
}

#' Install the INLA Package from Its Official Repository
#'
#' This function installs the \code{INLA} package from its official repository
#' at \url{https://inla.r-inla-download.org/R/stable}. On Windows, it checks
#' whether Rtools is available and forces a source install to avoid broken binaries.
#'
#' @details
#' On Windows systems, the function verifies that Rtools is installed using
#' \code{pkgbuild::has_build_tools()}. If Rtools is missing, it displays a warning
#' and aborts the installation. The function then forces installation from source
#' to bypass broken binary packages that may fail due to Unix-style commands.
#'
#' On non-Windows systems, the package is installed normally from the repository.
#'
#' @return Invisibly returns \code{NULL}. The function is called for its side effect.
#'
#' @examples
#' \dontrun{
#' install_INLA()
#' }
#'
#' @export
install_INLA <- function(os = .Platform$OS.type) {
  # Install fmesher
  message("Installing fmesher as INLA is dependent on it...\n")
  install.packages("fmesher")
  # Install INLA
  message("Installing INLA from its official repository...\n")
  # Detect OS and install INLA accordingly
  if (os == "windows") {
    # Ensure rtools is installed
    check_has_rtools()

    # Install from source tarball
    tarball <- "https://inla.r-inla-download.org/R/stable/bin/windows/contrib/4.4/INLA_24.12.11.zip"
    install.packages(
        tarball, repos = NULL, type = "source"
    )
  } else {
    install.packages(
      "INLA",
      repos = "https://inla.r-inla-download.org/R/stable"
    )
  }
  if (requireNamespace("INLA", quietly=T)) message("INLA succesfully installed.")
}

#' Install the terra Package from the CRAN Archive
#'
#' This function installs the \code{terra} package at version 1.8-60 from the 
#' CRAN archive.
#'
#' @details
#' On Windows systems, the function verifies that Rtools is installed using
#' \code{pkgbuild::has_build_tools()}. If Rtools is missing, it displays a warning
#' and aborts the installation. The function then forces installation from source.
#'
#' @return Invisibly returns \code{NULL}. The function is called for its side effect.
#'
#' @examples
#' \dontrun{
#' install_terra()
#' }
#'
#' @export
install_terra <- function(os = .Platform$OS.type) {
  message("Installing terra==1.8-60 from the CRAN archive...\n")

  # Install from source tarball
  tarball <- "https://cran.r-project.org/src/contrib/Archive/terra/terra_1.8-60.tar.gz"
  # Check for RTools on Windows
  if (os == "windows") {
    # Ensure rtools is installed
    check_has_rtools()
  }
  install.packages(
    tarball, repos = NULL, type = "source"
  )
  if (requireNamespace("terra", quietly=T)) message("terra==1.8-60 succesfully installed.")
}
