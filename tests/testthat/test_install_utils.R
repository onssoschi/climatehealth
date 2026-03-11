# Unit Tests for install_utils.R

if (!"package:climatehealth" %in% search()) {
  pkgload::load_all(".", export_all = TRUE, helpers = FALSE, quiet = TRUE)
}

# Tests for check_has_rtools
test_that(
    "check_has_rtools raises an error if RTools (or build tools) aren't installed.",
    with_mocked_bindings(
        check_build_tools = function(debug) c(FALSE),
        .package = "pkgbuild",
        code = {
            error_msg <- "Rtools is required to install INLA from source"
            expect_error(
                check_has_rtools(),
                error_msg
            )
        }
    )

)

test_that(
    "check_has_rtools returns TRUE if rtools are installed.",
    with_mocked_bindings(
        check_build_tools = function(debug) c(TRUE),
        .package = "pkgbuild",
        code = {
            result <- check_has_rtools()
            expect_equal(result, TRUE)
        }
    )
)

# Tests for install_INLA
install_pkg_mock <- function(...) {
    # define args
    args <- list(...)
    name <- args[[1]][1]
    repos <- args$repos
    type <- args$type
    # return values based on arg values
    INLA_tarball <- paste0(
        "https://inla.r-inla-download.org/R/stable/bin/windows/contrib/4.4/",
        "INLA_24.12.11.zip"
    )
    if (name==INLA_tarball && is.null(repos) && type=="source") {
        message("windows install")
    }
    INLA_repo_source = "https://inla.r-inla-download.org/R/stable"
    if (name=="INLA" && repos==INLA_repo_source) {
        message("other install")
    }
}


test_that(
  "install_INLA runs and prints messages correctly on Windows",
  {
    stub(install_INLA, "install.packages", install_pkg_mock)
    stub(install_INLA, "check_has_rtools", function(...) TRUE)
    stub(install_INLA, "requireNamespace", function(pkg, quietly = TRUE) {
      if (identical(pkg, "INLA")) TRUE else base::requireNamespace(pkg, quietly)
    })


    # Capture all messages from a single invocation
    msgs <- testthat::capture_messages( install_INLA(os = "windows") )
    all_msgs <- paste(msgs, collapse = "\n")

    expect_true(
      grepl("Installing fmesher", all_msgs, fixed = TRUE),
      info = paste0("Expected to find 'Installing fmesher' in messages.\nActual messages:\n", all_msgs)
    )

    expect_true(
      grepl("Installing INLA", all_msgs, fixed = TRUE),
      info = paste0("Expected to find 'Installing INLA' in messages.\nActual messages:\n", all_msgs)
    )

    expect_true(
      grepl("INLA succesfully installed", all_msgs, fixed = TRUE),
      info = paste0("Expected to find 'INLA succesfully installed' in messages.\nActual messages:\n", all_msgs)
    )

    expect_true(
      grepl("windows install", all_msgs, fixed = TRUE),
      info = paste0("Expected to find 'windows install' in messages.\nActual messages:\n", all_msgs)
    )
  }
)


test_that(
  "install_INLA runs and prints messages correctly on non-windows OS",
  {
    stub(install_INLA, "install.packages", install_pkg_mock)
    stub(install_INLA, "check_has_rtools", function(...) TRUE)
    stub(install_INLA, "requireNamespace", function(pkg, quietly = TRUE) {
      if (identical(pkg, "INLA")) TRUE else base::requireNamespace(pkg, quietly)
    })


    # Capture all messages from a single invocation
    msgs <- testthat::capture_messages( install_INLA(os = "unix") )
    all_msgs <- paste(msgs, collapse = "\n")

    expect_true(
      grepl("Installing fmesher", all_msgs, fixed = TRUE),
      info = paste0("Expected to find 'Installing fmesher' in messages.\nActual messages:\n", all_msgs)
    )

    expect_true(
      grepl("Installing INLA", all_msgs, fixed = TRUE),
      info = paste0("Expected to find 'Installing INLA' in messages.\nActual messages:\n", all_msgs)
    )

    expect_true(
      grepl("INLA succesfully installed", all_msgs, fixed = TRUE),
      info = paste0("Expected to find 'INLA succesfully installed' in messages.\nActual messages:\n", all_msgs)
    )

    expect_true(
      grepl("other install", all_msgs, fixed = TRUE),
      info = paste0("Expected to find 'other install' in messages.\nActual messages:\n", all_msgs)
    )
  }
)


# Tests for install_terra
check_rtools_mock <- function() message("rtools check")

test_that(
    "install_terra runs and prints messages correctly on Windows",
    {
        stub(install_terra, "install.packages", function(...) TRUE)
        stub(install_terra, "check_has_rtools", function() message("rtools check"))
        stub(install_terra, "requireNamespace", function(...) TRUE)
        expect_message(
            install_terra(os="windows"),
            "Installing terra==1.8-60"
        )
        expect_message(
            install_terra(os="windows"),
            "rtools check"
        )
        expect_message(
            install_terra(os="windows"),
            "terra==1.8-60 succesfully installed"
        )
    }
)

test_that(
    "install_terra runs and prints messages correctly on non-windows OS",
    {
        stub(install_terra, "install.packages", function(...) TRUE)
        stub(install_terra, "requireNamespace", function(...) TRUE)
        expect_message(
            install_terra(os="unix"),
            "Installing terra==1.8-60"
        )
        expect_message(
            install_terra(os="unix"),
            "terra==1.8-60 succesfully installed"
        )
    }
)
