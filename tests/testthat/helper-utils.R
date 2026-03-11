suppress_plot <- function(expr) {
  tmp <- tempfile(fileext = ".pdf")
  pdf(tmp, width = 12, height = 9)
  on.exit(dev.off(), add = TRUE)
  force(expr)
}
