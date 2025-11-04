suppress_plot <- function(expr) {
  tmp <- tempfile(fileext = ".pdf")
  pdf(tmp)
  on.exit(dev.off(), add = TRUE)
  force(expr)
}