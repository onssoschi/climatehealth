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
#' @return A list with `n_col` and `n_row`.
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
#' @return A list with `width` and `height`.
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
#' Creates a layout matrix for use with `layout()`.
#'
#' @param n_plots Integer. Number of plots.
#' @param n_col Integer. Number of columns.
#' @param n_row Integer. Number of rows.
#'
#' @return A matrix for `layout()`.
#' @keywords internal
get_layout_matrix <- function(n_plots, n_col, n_row) {
  mat <- matrix(seq_len(n_row * n_col), nrow = n_row, ncol = n_col, byrow = TRUE)
  mat[mat > n_plots] <- 0
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
#' accessibility and readability. Also applies standard plotting parameters.
#'
#' @param file Character. Output PDF file path.
#' @param n_plots Integer. Number of plots.
#' @param max_cols Integer. Maximum number of columns.
#' @param panel_width Numeric. Width per panel in inches.
#' @param panel_height Numeric. Height per panel in inches.
#' @param mar Numeric vector of length 4. Inner margins.
#' @param oma Numeric vector of length 4. Outer margins.
#'
#' @return Invisibly returns the grid specification with `n_col` and `n_row`.
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

  # Fixed size for single-plot pages
  if (n_plots == 1) {
    size <- get_pdf_size(
      n_col = 1,
      n_row = 1,
      panel_width = panel_width * 1.5,
      panel_height = panel_height * 1.5
    )
    pdf_width <- size$width
    pdf_height <- size$height
  } else {
    size <- get_pdf_size(
      n_col = grid$n_col,
      n_row = grid$n_row,
      panel_width = panel_width,
      panel_height = panel_height
    )
    pdf_width <- size$width
    pdf_height <- size$height
  }

  grDevices::pdf(
    file = file,
    width = pdf_width,
    height = pdf_height,
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
      col = col,
      font = 1
    )
  }
}

#' Add right-side axis and optional label
#'
#' Adds a right-hand axis with an optional axis title. Useful for dual-axis plots.
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

#' Add accessible alt text to the current figure
#'
#' Writes wrapped alt text into the outer bottom margin using `mtext()`. Intended
#' for saved PDFs produced with `open_accessible_pdf()` where adequate outer
#' margin space is reserved.
#'
#' @param alt_text Character. Alt text describing the figure.
#' @param width Integer. Wrap width for `strwrap()`.
#' @param line_start Numeric. Starting line in the outer bottom margin.
#' @param line_spacing Numeric. Spacing between wrapped lines.
#' @param cex Numeric. Text size.
#' @param col Character. Text colour.
#'
#' @return Invisibly returns NULL.
#' @keywords internal
add_accessible_alt_text <- function(
    alt_text,
    width = 170,
    line_start = 0.8,
    line_spacing = 1.4,
    cex = 1,
    col = "#1F1F1F"
) {
  wrapped_text <- base::strwrap(alt_text, width = width)

  for (i in seq_along(wrapped_text)) {
    graphics::mtext(
      text = wrapped_text[[i]],
      side = 1,
      outer = TRUE,
      line = line_start + (i - 1) * line_spacing,
      adj = 0.03,
      cex = cex,
      col = col,
      font = 3
    )
  }

  invisible(NULL)
}

#' Add a figure-level legend using a device overlay
#'
#' Draws a single legend in an overlay that spans the full device (0..1
#' coordinates), independent of the panel layout created by `layout()`. The
#' legend position is controlled by `x` (horizontal) and `vpad` (vertical).
#'
#' @param legend Character vector of legend labels.
#' @param col Vector. Colours for legend items.
#' @param lty Vector. Line types for legend items.
#' @param lwd Vector. Line widths for legend items.
#' @param pch Vector. Point characters for legend items.
#' @param pt.cex Numeric. Point size for legend items.
#' @param cex Numeric. Text size.
#' @param seg.len Numeric. Length of legend line segments.
#' @param inset Numeric. Reserved for future use. Currently not used by the implementation.
#' @param text.col Character. Colour for legend text and border.
#' @param bty Character. Box type passed to `legend()`.
#' @param bg Character. Legend background colour.
#' @param vpad Numeric. Vertical padding from the top of the device. Smaller values move the legend up.
#' @param x Numeric. Horizontal position in 0..1 device coordinates.
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
    inset = 0.018,
    text.col = "black",
    bty = "o",
    bg = "white",
    vpad = 0.075,
    x = 0.5
) {
  op <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(op), add = TRUE)

  # Open an overlay that spans the whole device in 0..1 coords
  graphics::par(new = TRUE, xpd = NA, mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0),
                fig = c(0, 1, 0, 1))
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1), xaxs = "i", yaxs = "i")

  graphics::legend(
    x = x, y = 1 - vpad,
    legend = legend,
    col = col,
    lty = lty,
    lwd = lwd,
    cex = cex,
    pch = pch,
    pt.cex = pt.cex,
    seg.len = seg.len,
    horiz = TRUE,
    xjust = 0.5, yjust = 1,
    bty = bty,
    text.col = text.col,
    bg = bg,
    box.col = text.col,
    merge = TRUE
  )

  invisible(NULL)
}

#' Add figure-level title, subtitle, and optional logo
#'
#' Adds a figure-level title and subtitle using outer margins and attempts to
#' add the SOSCHI logo if it is available in the package `extdata` directory.
#' This function does not open or close graphics devices.
#'
#' @param title Character or NULL. Figure title. If NULL, no title is added.
#' @param subtitle Character or NULL. Figure subtitle. If NULL, no subtitle is added.
#' @param line_title Numeric. Outer margin line position for the title.
#' @param line_subtitle Numeric. Outer margin line position for the subtitle.
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

#' Open a diagnostic PDF device
#'
#' Convenience wrapper around `open_accessible_pdf()` with parameters tuned for
#' diagnostic plots (single column, readable margins).
#'
#' @param file Character. Output PDF file path.
#' @param n_panels Integer. Number of panels to plot.
#'
#' @return Invisibly returns the grid specification with `n_col` and `n_row`.
#' @keywords internal
open_diag_pdf <- function(file, n_panels) {
  open_accessible_pdf(
    file = file,
    n_plots = n_panels,
    max_cols = 1,
    panel_width = 6.2,
    panel_height = 5.0,
    mar = c(5.2, 4.8, 3.2, 3.8),
    oma = c(6.2, 0.6, 7.2, 0.6)
  )
}

#' Close a diagnostic PDF device with title and alt text
#'
#' Adds a figure-level title and subtitle, writes alt text into the outer bottom
#' margin, and closes the current device.
#'
#' @param title Character. Figure-level title.
#' @param subtitle Character. Figure-level subtitle.
#' @param alt_text Character. Alt text describing the diagnostic figure.
#'
#' @return No return value. Called for side effects.
#' @keywords internal
close_diag_pdf <- function(title, subtitle, alt_text) {
  run_accessible_pdf_plot(
    title = title,
    subtitle = subtitle,
    line_title = 4.7,
    line_subtitle = 3.0
  )
  add_accessible_alt_text(
    alt_text = alt_text, width = 120
  )
  dev.off()
}
