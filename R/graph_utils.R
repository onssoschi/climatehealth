# Utilities associated with creating graphs for indicator outputs

#' Generate a grid size for a certain number of plots.
#'
#' This function calculates the minimum grid size required to plot X amount of
#' plots on a a figure. For example, 6 plots would require a 3x2, where as 7
#' would require a 3x3, and so on.
#'
#' @param plot_count The number of plots required for the grid.
#'
#' @return A vector containing [1] x value and [2] y value for the grid.
#' @export
#'
create_grid <- function(plot_count) {
  est <- sqrt(plot_count)
  if (est==floor(est)){
    x <- y <- est
  } else {
    base <- est - floor(est)
    if (base < 0.5){
      y <- floor(est)
    }
    else {
      y <- floor(est) + 1
    }
    x <- floor(est) + 1
  }
  return(c(x, y))
}
