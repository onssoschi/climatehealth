# Unit Tests for install_utils.R

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
        expect_message(
            install_INLA(os="windows"),
            "Installing fmesher"
        )
        expect_message(
            install_INLA(os="windows"),
            "Installing INLA"
        )
        expect_message(
            install_INLA(os="windows"),
            "INLA succesfully installed"
        )
        expect_message(
            install_INLA(os="windows"),
            "windows install"
        )
    }
)

test_that(
    "install_INLA runs and prints messages correctly on non-windows OS",
    {
        stub(install_INLA, "install.packages", install_pkg_mock)
        stub(install_INLA, "check_has_rtools", function(...) TRUE)
        expect_message(
            install_INLA(os="unix"),
            "Installing fmesher"
        )
        expect_message(
            install_INLA(os="unix"),
            "Installing INLA"
        )
        expect_message(
            install_INLA(os="unix"),
            "INLA succesfully installed"
        )
        expect_message(
            install_INLA(os="unix"),
            "other install"
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
            "Installing terra<=1.8-60"
        )
        expect_message(
            install_terra(os="windows"),
            "rtools check"
        )
        expect_message(
            install_terra(os="windows"),
            "terra<=1.8-60 succesfully installed"
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
            "Installing terra<=1.8-60"
        )
        expect_message(
            install_terra(os="unix"),
            "terra<=1.8-60 succesfully installed"
        )
    }
)