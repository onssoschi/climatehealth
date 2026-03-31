# Plot accessibility helpers for base R PDF outputs

#' Get project plot palette
#'
#' Returns the standard project palette for accessible plots.
#'
#' @return A named list of hex colour values.
#' @keywords internal
get_accessible_palette <- function() {
  list(
    prussian_blue = "#1B365D",
    deep_water = "#2E5E7E",
    dusky_rose = "#B07A8F",
    olive_green = "#6E7F46",
    smoke_grey = "#D9DEE5",
    text = "#1F1F1F",
    axis = "#4A4A4A"
  )
}

#' Get plot grid dimensions
#'
#' Calculates the number of columns and rows for a multi-panel plot layout.
#' Defaults to a maximum of 2 columns to avoid squished panels.
#'
#' @param n_plots Integer. Number of plots.
#' @param max_cols Integer. Maximum number of columns.
#'
#' @return A list with n_col and n_row.
#' @keywords internal
get_plot_grid <- function(n_plots, max_cols = 2) {
  n_col <- min(max_cols, n_plots)
  n_row <- ceiling(n_plots / n_col)

  list(
    n_col = n_col,
    n_row = n_row
  )
}

#' Get PDF size for multi-panel plot
#'
#' Calculates PDF width and height based on the number of rows and columns.
#'
#' @param n_col Integer. Number of columns.
#' @param n_row Integer. Number of rows.
#' @param panel_width Numeric. Width per panel in inches.
#' @param panel_height Numeric. Height per panel in inches.
#' @param min_width Numeric. Minimum PDF width in inches.
#' @param min_height Numeric. Minimum PDF height in inches.
#'
#' @return A list with width and height.
#' @keywords internal
get_pdf_size <- function(
    n_col,
    n_row,
    panel_width = 7,
    panel_height = 5.2,
    min_width = 9,
    min_height = 6
) {
  list(
    width = max(min_width, n_col * panel_width),
    height = max(min_height, n_row * panel_height)
  )
}

#' Get layout matrix for multi-panel plot
#'
#' Creates a layout matrix for use with layout().
#'
#' @param n_plots Integer. Number of plots.
#' @param n_col Integer. Number of columns.
#' @param n_row Integer. Number of rows.
#'
#' @return A matrix for layout().
#' @keywords internal
get_layout_matrix <- function(n_plots, n_col, n_row) {
  mat <- matrix(0, nrow = n_row, ncol = n_col)
  mat[seq_len(n_plots)] <- seq_len(n_plots)
  mat
}


#' Set base plotting parameters for accessible plots
#'
#' Applies consistent plotting parameters for accessible base R plots.
#'
#' @param mar Numeric vector of length 4. Inner margins.
#' @param oma Numeric vector of length 4. Outer margins.
#' @param cex_axis Numeric. Axis text size.
#' @param cex_lab Numeric. Axis label size.
#' @param cex_main Numeric. Plot title size.
#' @param mgp Numeric vector of length 3. Axis title, label, and line placement.
#'
#' @return No return value. Called for side effects.
#' @keywords internal
setup_accessible_par <- function(
    mar = c(5.2, 5.2, 3.6, 4.2),
    oma = c(0, 0.5, 4.5, 0.5),
    cex_axis = 1.0,
    cex_lab = 1.1,
    cex_main = 1.15,
    mgp = c(2.7, 0.8, 0)
) {
  graphics::par(
    mar = mar,
    oma = oma,
    cex.axis = cex_axis,
    cex.lab = cex_lab,
    cex.main = cex_main,
    mgp = mgp,
    family = "sans"
  )
}

#' Open accessible PDF device
#'
#' Opens a PDF graphics device with a multi-panel layout optimised for
#' accessibility and readability.
#'
#' @param file Character. Output PDF file path.
#' @param n_plots Integer. Number of plots.
#' @param max_cols Integer. Maximum number of columns.
#' @param panel_width Numeric. Width per panel in inches.
#' @param panel_height Numeric. Height per panel in inches.
#'
#' @return Invisibly returns the grid specification.
#' @keywords internal
open_accessible_pdf <- function(
    file,
    n_plots,
    max_cols = 2,
    panel_width = 7,
    panel_height = 5.2,
    mar = c(5.2, 5.2, 3.6, 4.2),
    oma = c(0, 0.5, 4.5, 0.5)
) {
  grid <- get_plot_grid(n_plots, max_cols = max_cols)

  size <- get_pdf_size(
    n_col = grid$n_col,
    n_row = grid$n_row,
    panel_width = panel_width,
    panel_height = panel_height
  )

  grDevices::pdf(
    file = file,
    width = size$width,
    height = size$height,
    paper = "special",
    family = "sans"
  )

  dev_id <- grDevices::dev.cur()

  on.exit(substitute(if (grDevices::dev.cur() == ID) grDevices::dev.off(),
                     list(ID = dev_id)), add = TRUE)

  graphics::layout(
    get_layout_matrix(n_plots, grid$n_col, grid$n_row),
    widths = rep(1, grid$n_col),
    heights = rep(1, grid$n_row)
  )

  setup_accessible_par(
    mar = mar,
    oma = oma
  )

  invisible(grid)
}


#' Add outer title and subtitle to a multi-panel plot
#'
#' Adds a figure-level title and optional subtitle using outer margins.
#'
#' @param title Character. Main title.
#' @param subtitle Character or NULL. Subtitle.
#' @param title_cex Numeric. Main title size.
#' @param subtitle_cex Numeric. Subtitle size.
#' @param line_title Numeric. Line position for main title.
#' @param line_subtitle Numeric. Line position for subtitle.
#' @param col Character. Text colour.
#'
#' @return No return value. Called for side effects.
#' @keywords internal
add_accessible_outer_title <- function(
    title,
    subtitle = NULL,
    title_cex = 1.4,
    subtitle_cex = 0.94,
    line_title = 1.8,
    line_subtitle = 0.0,
    col = "#1F1F1F"
) {
  graphics::mtext(
    title,
    side = 3,
    outer = TRUE,
    line = line_title,
    cex = title_cex,
    font = 2,
    col = col
  )

  if (!is.null(subtitle)) {
    graphics::mtext(
      subtitle,
      side = 3,
      outer = TRUE,
      line = line_subtitle,
      cex = subtitle_cex,
      col = col
    )
  }
}

#' Add right-side axis and optional label
#'
#' Adds a right-hand axis with optional axis title. Useful for dual-axis plots.
#'
#' @param at Numeric vector. Axis tick positions.
#' @param labels Vector. Axis tick labels.
#' @param ylim Numeric vector of length 2. Plot y limits used to position label.
#' @param side_label Character or NULL. Optional axis title.
#' @param axis_col Character. Axis text colour.
#' @param cex_axis Numeric. Axis tick label size.
#' @param cex_label Numeric. Axis title size.
#' @param line Numeric. Margin line for axis title.
#'
#' @return No return value. Called for side effects.
#' @keywords internal
add_right_axis_label <- function(
    at,
    labels,
    ylim,
    side_label = NULL,
    axis_col = "#1F1F1F",
    cex_axis = 0.9,
    cex_label = 0.8,
    line = 2.8
) {
  graphics::axis(
    side = 4,
    at = at,
    labels = labels,
    las = 1,
    col.axis = axis_col,
    cex.axis = cex_axis
  )

  if (!is.null(side_label)) {
    midpoint <- mean(range(at, na.rm = TRUE))
    adj_val <- (midpoint - ylim[1]) / (ylim[2] - ylim[1])

    graphics::mtext(
      side_label,
      side = 4,
      line = line,
      adj = adj_val,
      cex = cex_label,
      col = axis_col
    )
  }
}

#' Standard month labels
#'
#' Returns abbreviated month labels for use on x-axes.
#'
#' @return Character vector of month abbreviations.
#' @keywords internal
get_month_labels <- function() {
  c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")
}


#' Add SOSCHI logo to current plot
#'
#' Draws a logo image on the current graphics device using grid.
#'
#' @param logo_path Character. Path to logo file.
#' @param x Numeric. Horizontal position from 0 to 1.
#' @param y Numeric. Vertical position from 0 to 1.
#' @param width Numeric. Width relative to device.
#' @param just Character vector. Justification.
#'
#' @return No return value. Called for side effects.
#' @keywords internal
add_plot_logo <- function(
    logo_path,
    x = 0.001,
    y = 0.995,
    width = 0.12,
    just = c("left", "top")
) {
  if (!file.exists(logo_path)) {
    return(invisible(NULL))
  }

  img <- try(png::readPNG(logo_path), silent = TRUE)

  if (inherits(img, "try-error")) {
    return(invisible(NULL))
  }

  grid::grid.raster(
    img,
    x = x,
    y = y,
    width = width,
    just = just,
    interpolate = TRUE
  )

  invisible(NULL)
}

add_accessible_alt_text <- function(
    alt_text,
    width = 245,
    line_start = 0.8,
    line_spacing = 1.07,
    cex = 0.8,
    col = "#1F1F1F"
    ) {
  wrapped_text <- base::strwrap(alt_text, width = width)

  for (i in seq_along(wrapped_text)) {
    graphics::mtext(
      text = wrapped_text[[i]],
      side = 1,
      outer = TRUE,
      line = line_start + (i -1) * line_spacing,
      adj = 0.03,
      cex = cex,
      col = col,
      font = 3
    )
  }

  invisible(NULL)
}

#' Add a figure-level legend at the top-right of the page
#'
#' Draws a single legend anchored to the device's top-right corner,
#' independent of panel layout. Uses a 0..1 overlay coordinate system.
#'
#' @param legend Character vector of legend labels.
#' @param col    Vector of colors for legend lines.
#' @param lty    Vector of line types.
#' @param lwd    Vector of line widths.
#' @param cex    Numeric. Text size.
#' @param seg.len Numeric. Length of legend line segments.
#' @param ncol   Integer. Number of columns in the legend.
#' @param inset  Numeric in 0..1 device coords. Margin from top-right.
#' @param text.col Color for legend text.
#' @param bty    Box type, default "n" for none.
#'
#' @return Invisibly returns NULL.
#' @keywords internal
add_figure_legend <- function(
    legend,
    col,
    lty,
    lwd,
    pch = NA,
    pt.cex = 2.1,
    cex = 1.0,
    seg.len = 2.2,
    ncol = length(legend),
    inset = 0.018,
    text.col = "black",
    bty = "o",
    bg = "white",
    vpad = 0.075,
    x= 0.5
) {
  op <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(op), add = TRUE)

  # Open an overlay that spans the whole device in 0..1 coords
  graphics::par(new = TRUE, xpd = NA, mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0),
                fig = c(0,1,0,1))
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1), xaxs = "i", yaxs = "i")

  graphics::legend(
    x = x, y = 1-vpad,
    legend = legend,
    col = col,
    lty = lty,
    lwd = lwd,
    cex = cex,
    pch = pch,
    pt.cex = pt.cex,
    seg.len = seg.len,
    horiz = TRUE,
    ncol = ncol,
    xjust = 0.5, yjust = 1,
    bty = bty,
    text.col = text.col,
    bg = bg,
    box.col = text.col,
    merge = TRUE
  )

  invisible(NULL)
}

#' Run an accessible multi-panel PDF plot
#'
#' Opens a PDF device, applies standard plotting parameters, runs a user-supplied
#' plotting function, adds an optional figure title and subtitle, and adds the
#' SOSCHI logo if available.
#'
#' @param file Character. Output PDF path.
#' @param n_plots Integer. Number of panels.
#' @param plot_fun Function. A function taking one argument, cols, containing the palette.
#' @param title Character or NULL. Figure title.
#' @param subtitle Character or NULL. Figure subtitle.
#' @param max_cols Integer. Maximum number of columns.
#' @param panel_width Numeric. Width per panel in inches.
#' @param panel_height Numeric. Height per panel in inches.
#' @param mar Numeric vector of length 4. Inner margins.
#' @param oma Numeric vector of length 4. Outer margins.
#'
#' @return Invisibly returns NULL.
#' @keywords internal
run_accessible_pdf_plot <- function(
    title = NULL,
    subtitle = NULL,
    line_title = 1.8,
    line_subtitle = 0
) {
  cols <- get_accessible_palette()

  if (!is.null(title)) {
    add_accessible_outer_title(
      title = title,
      subtitle = subtitle,
      col = cols$text,
      line_title = line_title,
      line_subtitle = line_subtitle
    )
  }

  logo_path <- system.file(
    "extdata",
    "soschi_logo.png",
    package = utils::packageName()
  )

  if (nzchar(logo_path) && file.exists(logo_path)) {
    add_plot_logo(logo_path)
  }


  invisible(NULL)
}



