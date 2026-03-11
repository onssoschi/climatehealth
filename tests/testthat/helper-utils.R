suppress_plot <- function(expr) {
  tmp <- tempfile(fileext = ".pdf")
  grDevices::pdf(tmp, width = 12, height = 9)
  plot_dev <- grDevices::dev.cur()
  on.exit({
    open_devices <- grDevices::dev.list()
    if (!is.null(open_devices) && plot_dev %in% open_devices) {
      grDevices::dev.off(which = plot_dev)
    }
  }, add = TRUE)
  force(expr)
}
