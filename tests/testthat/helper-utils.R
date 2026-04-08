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
