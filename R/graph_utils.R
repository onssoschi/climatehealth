library(gplots)

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


plot_correlation_matrix <- function(matrix_, title, output_path) {
  # validate output path
  if (!endsWith(output_path, ".png")) {
    output_path <- strsplit(output_path, "\\.")[1]
    output_path <- paste0(output_path, ".png")
  }
  # round correlation metrics
  matrix_ <- round(matrix_, 3)
  # draw and save correlation matrix
  png(output_path, width = 1000)
  gplots::heatmap.2(x=as.matrix(matrix_), Rowv = FALSE, Colv = FALSE, dendrogram = "none",
                    cellnote = matrix_, notecol = "black", notecex = 0.7, cexRow = 0.7, cexCol = 0.7,
                    trace = "none", key = FALSE, margins = c(11, 11), main = title)
  dev.off()
}


plot_distributions <- function(df, columns, title, save_hists = FALSE, output_path = NULL) {
  # create a pdf if a save is selected
  if (save_hists == T) {
    # normalise output path
    if (!endsWith(output_path, ".pdf")) {
      output_path <- strsplit(output_path, "\\.")[[1]]
      output_path <- paste0(output_path, ".pdf")
    }
    pdf(output_path)
  }
  # normalise columns
   columns <- c(columns)
  df %>% select(all_of(columns))
  # get grid size for plotting
  grid_size <- create_grid(length(columns))
  # format page and plot
  par(
    mfrow = grid_size,
    col = "white",
    oma = c(0, 0, 4, 0)
  )
  for (col_name in columns) {
    hist(
      df[[col_name]],
      col = "green",
      xlab = col_name,
      main = paste0("Distribution of '", col_name, "'")
    )
  }
  mtext(title, outer = TRUE, cex = 1.6, line = 1, font = 2, col = "black")
  # save pdf if requested
  if (save_hists == T) {
    dev.off()
  }
}

# alter the alpha value for a hex colour (returns rgb)
get_alpha_colour <- function(hex, alpha) {
  rgb_vals <- col2rgb(hex)
  alpha_colour <- rgb(rgb_vals[1, ] / 255, rgb_vals[2, ] / 255, rgb_vals[3, ] /
                        255, alpha = alpha)
  return(alpha_colour)
}


plot_moving_average <- function(df,
                                time_col,
                                value_col,
                                ma_days,
                                ma_sides,
                                title,
                                save_plot = FALSE,
                                output_path = "") {
  # create a pdf if a save is selected
  if (save_plot == T) {
    # normalise output path
    if (!endsWith(output_path, ".pdf")) {
      output_path <- strsplit(output_path, "\\.")[[1]]
      output_path <- paste0(output_path, ".pdf")
    }
    pdf(output_path)
  }

  # select data
  df %>%
    select(all_of(c(time_col, value_col)))

  # format plot
  par(bg = "white",
      mfrow = c(1, 1),
      oma = c(0, 0, 4, 0))
  plot_title <- paste0(
    "Moving Average (n=",
    as.character(ma_days),
    ", sides=",
    as.character(ma_sides),
    ")"
  )
  line_colour <- get_alpha_colour("#c907ba", 0.25)

  # plot raw data
  plot(
    main = plot_title,
    x = df[[time_col]],
    y = df[[value_col]],
    type = "l",
    xlab = time_col,
    ylab = value_col,
    col = line_colour,
    ylim = c(min(df[[value_col]]), max(df[[value_col]]))
  )

  # create moving average column
  ma <- function(x, n, sides = 1) {
    stats::filter(x, rep(1 / n, n), sides = sides)
  }

  col_name <- paste0("MA_",
                     as.character(ma_days),
                     "days_",
                     as.character(ma_sides),
                     "sides")
  df[[col_name]] <- ma(df[[value_col]], ma_days, ma_sides)

  # plot moving average
  lines(
    main = plot_title,
    x = df[[time_col]],
    y = df[[col_name]],
    type = "l",
    xlab = time_col,
    ylab = "Moving Average",
    col = "#c907ba",
    ylim = c(min(df[[value_col]]), max(df[[value_col]]))
  )
  mtext(
    title,
    outer = TRUE,
    cex = 1.6,
    line = 1,
    font = 2,
    col = "black"
  )

  if (save_plot == TRUE) {
    dev.off()
  }
}


plot_scatter_grid <- function(
    df,
    main_col,
    comparison_cols,
    title,
    save_scatters = F,
    output_path = ""
) {
  # filter the dataframe to required columns
  all_columns <- c(main_col, comparison_cols)
  df <- df %>%
    select(all_of(all_columns))
  # obtain the needed grid size for plotting
  grid_size <- create_grid(length(all_columns))
  # set up output layout
  if (save_scatters == T) {
    if (!endsWith(output_path, ".pdf")) {
      output_path <- strsplit(output_path, "\\.")[[1]]
      output_path <- paste0(output_path, ".pdf")
    }
    pdf(output_path, width=8, height=8)
  }
  par(
    mfrow = grid_size,
    col = "white",
    oma = c(0, 0, 4, 0)
  )
  point_colour <- get_alpha_colour("#c907ba", 0.25)
  for (i in 1:length(comparison_cols)) {
    plot(
      y = df[[main_col]],
      x = df[[comparison_cols[i]]],
      col = point_colour,
      ylab = main_col,
      xlab = comparison_cols[i],
      main = paste0(main_col, " vs ", comparison_cols[i])
    )
  }
  mtext(
    title,
    outer = TRUE,
    cex = 1.6,
    line = 1,
    font = 2,
    col = "black"
  )
  if (save_scatters == T) {
    dev.off()
  }
}

