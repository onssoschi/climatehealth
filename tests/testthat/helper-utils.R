suppress_plot <- function(expr) {
  tmp <- tempfile(fileext = ".pdf")
  grDevices::pdf(tmp, width = 16, height = 12)
  plot_dev <- grDevices::dev.cur()
  on.exit({
    open_devices <- grDevices::dev.list()
    if (!is.null(open_devices) && plot_dev %in% open_devices) {
      grDevices::dev.off(which = plot_dev)
    }
  }, add = TRUE)
  force(expr)
}

skip_if_integration_disabled <- function() {
  skip_on_cran()

  run_integration <- tolower(Sys.getenv("RUN_INTEGRATION", "false")) %in% c("true", "t", "1")
  skip_if_not(
    run_integration,
    "Integration tests are disabled by default. Set RUN_INTEGRATION=true to enable them."
  )
}

# Assert that an expression leaves the set of open graphics devices unchanged.
#
# This is a CLEANUP check, not a proof that nothing was drawn: it still passes
# if code draws on a device that was already open, or opens a device, draws,
# and closes it again. Use expect_no_plotting() when the requirement is "no
# drawing happened at all".
expect_no_device_left_open <- function(expr) {
  before <- grDevices::dev.list()
  value <- force(expr)
  after <- grDevices::dev.list()

  testthat::expect_identical(
    after,
    before,
    info = paste(
      "The expression changed the open graphics devices.",
      "In API mode nothing may be drawn:",
      "check that the entrypoint routes through apply_api_mode()",
      "and that every plot helper is behind a save_fig guard."
    )
  )

  invisible(value)
}


# Assert that an expression performs no drawing.
#
# Tripwires the shared drawing layer that every figure in this package passes
# through, so a stray plot fails the test by name regardless of which helper
# introduced it, and combines that with the device-list check above.
#
# Known gap: base graphics called directly (graphics::plot() and friends)
# bypass these helpers. Those are caught by the device check only when no
# device is already open, so module tests should also mock their own
# plot helpers to fail -- see the API-mode tests in each test_<module>.R.
expect_no_plotting <- function(expr, .local_envir = parent.frame()) {
  tripwire <- function(name) {
    force(name)
    function(...) {
      testthat::fail(
        paste0(
          name, "() was called. Nothing may be drawn or written here: ",
          "the entrypoint should have skipped this plotting path."
        )
      )
    }
  }

  testthat::local_mocked_bindings(
    open_accessible_pdf = tripwire("open_accessible_pdf"),
    open_diag_pdf = tripwire("open_diag_pdf"),
    close_diag_pdf = tripwire("close_diag_pdf"),
    save_accessible_ggplot = tripwire("save_accessible_ggplot"),
    run_accessible_pdf_plot = tripwire("run_accessible_pdf_plot"),
    add_plot_logo = tripwire("add_plot_logo"),
    .local_envir = .local_envir
  )

  expect_no_device_left_open(expr)
}
