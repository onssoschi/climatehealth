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
  grDevices::dev.off()
}

#' Get generic accessible plot colours
#'
#' Returns a reusable accessible colour set for ggplot outputs. This builds on
#' the package-level accessible palette used for base R plots.
#'
#' @return A named list of hex colour values.
#' @keywords internal
get_accessible_plot_colours <- function() {
  cols <- get_accessible_palette()

  list(
    primary = cols$deep_water,
    primary_dark = cols$prussian_blue,
    secondary = cols$dusky_rose,
    secondary_dark = "#A04B58",
    highlight = cols$olive_green,
    uncertainty = cols$smoke_grey,
    reference = cols$text,
    axis = cols$axis,
    text = cols$text,
    background = "white",
    panel = "white",
    grid = "#E8EBEF",
    warning = "#B00020",
    national = cols$olive_green,
    regional = cols$deep_water
  )
}



' Get accessible ggplot grid dimensions
#'
#' Calculates grid dimensions for ggplot facets or patchwork plots. Defaults to
#' a maximum of two columns to avoid squished panels.
#'
#' @param n_plots Integer. Number of panels or plots.
#'
#' @return A list with `n_col` and `n_row`.
#' @keywords internal
get_accessible_ggplot_grid <- function(n_plots) {
  grid <- get_plot_grid(n_plots, max_cols = 2)

  list(
    n_col = grid$n_col,
    n_row = grid$n_row
  )
}


#' Get accessible ggplot output size
#'
#' Calculates output dimensions based on the number of panels.
#'
#' @param n_plots Integer. Number of panels or plots.
#'
#' @return A list with `n_col`, `n_row`, `width`, and `height`.
#' @keywords internal
get_accessible_ggplot_size <- function(n_plots) {
  grid <- get_accessible_ggplot_grid(n_plots)

  list(
    n_col = grid$n_col,
    n_row = grid$n_row,
    width = max(9, grid$n_col * 6.8),
    height = max(6, grid$n_row * 5.2)
  )
}


#' Create a safe plot filename stem
#'
#' @param x Character. Filename stem, with or without extension.
#'
#' @return Character safe for filenames.
#' @keywords internal
make_safe_plot_filename <- function(x) {
  x <- gsub("\\.[A-Za-z0-9]+$", "", x)
  x <- gsub("[^A-Za-z0-9_\\-]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}


#' Accessible ggplot theme
#'
#' Applies a consistent, readable theme for ggplot outputs across indicators.
#'
#' @return A ggplot2 theme.
#' @keywords internal
theme_accessible_ggplot <- function() {
  cols <- get_accessible_plot_colours()

  ggplot2::theme_minimal(base_size = 13, base_family = "sans") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        hjust = 0.5,
        face = "bold",
        size = 17,
        colour = cols$text,
        lineheight = 1.08
      ),
      plot.subtitle = ggplot2::element_text(
        hjust = 0.5,
        size = 13,
        colour = cols$axis,
        lineheight = 1.15,
        margin = ggplot2::margin(t = 0, r = 0, b = 25, l = 0)
      ),
      plot.caption = ggplot2::element_text(
        hjust = 0,
        size = 11,
        colour = cols$text,
        face = "italic",
        lineheight = 1.12,
        margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)
      ),
      axis.title = ggplot2::element_text(
        face = "bold",
        colour = cols$text,
        size = 11
      ),
      axis.text = ggplot2::element_text(
        colour = cols$axis,
        size = 10
      ),
      axis.text.x = ggplot2::element_text(
        angle = 45,
        hjust = 1,
        vjust = 1
      ),
      axis.line = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(
        face = "bold",
        size = 13,
        colour = cols$text,
        margin = ggplot2::margin(t = 10, r = 0, b = 10, l = 0)
      ),
      strip.background = ggplot2::element_rect(
        fill = cols$uncertainty,
        colour = NA
      ),
      strip.clip = "off",
      panel.spacing.y = grid::unit(1.6, "lines"),
      panel.spacing.x = grid::unit(0.8, "lines"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(
        colour = cols$grid,
        linewidth = 0.25
      ),
      plot.background = ggplot2::element_rect(
        fill = cols$background,
        colour = NA
      ),
      panel.background = ggplot2::element_rect(
        fill = cols$panel,
        colour = NA
      ),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(face = "bold"),
      plot.margin = ggplot2::margin(36, 16, 16, 10)
    )
}


#' Accessible ggplot panel theme
#'
#' Applies consistent styling for individual ggplot panels used in patchwork
#' layouts. Intended for plots where region names are shown as panel titles
#' rather than facet strips.
#'
#' @return A ggplot2 theme.
#' @keywords internal
theme_accessible_ggplot_panel <- function() {
  cols <- get_accessible_plot_colours()

  ggplot2::theme(
    plot.title = ggplot2::element_text(
      hjust = 0.5,
      face = "bold",
      size = 12,
      colour = cols$text
    ),
    axis.title = ggplot2::element_text(
      face = "plain",
      colour = cols$text,
      size = 11
    ),
    axis.text = ggplot2::element_text(
      colour = cols$axis,
      size = 10
    ),
    panel.border = ggplot2::element_rect(
      colour = cols$axis,
      fill = NA,
      linewidth = 0.45
    ),
    plot.subtitle = ggplot2::element_blank(),
    plot.caption = ggplot2::element_blank(),
    plot.margin = ggplot2::margin(10, 15, 22, 10)
  )
}


#' Accessible patchwork plot annotation
#'
#' Adds a consistent accessible title, subtitle, and wrapped alt text caption
#' to patchwork plot outputs.
#'
#' @param title Character. Figure title.
#' @param subtitle Character. Figure subtitle.
#' @param alt_text Character. Alt text to include in the caption.
#'
#' @return A patchwork plot annotation.
#' @keywords internal
accessible_plot_annotation <- function(title, subtitle, alt_text) {
  cols <- get_accessible_plot_colours()

  patchwork::plot_annotation(
    title = title,
    subtitle = subtitle,
    caption = paste(
      strwrap(paste0("Alt text: ", alt_text), width = 180),
      collapse = "\n"
    ),
    theme = ggplot2::theme(
      plot.title = ggplot2::element_text(
        hjust = 0.5,
        face = "bold",
        size = 17,
        colour = cols$text,
        lineheight = 1.08,
        margin = ggplot2::margin(t = 0, r = 0, b = 4, l = 0)
      ),
      plot.subtitle = ggplot2::element_text(
        hjust = 0.5,
        size = 13,
        colour = cols$axis,
        lineheight = 1.15,
        margin = ggplot2::margin(t = 0, r = 0, b = 10, l = 0)
      ),
      plot.caption = ggplot2::element_text(
        hjust = 0,
        size = 11,
        colour = cols$text,
        face = "italic",
        lineheight = 1.12,
        margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)
      )
    )
  )
}


#' Add alt text as ggplot caption
#'
#' @param plot_object ggplot object.
#' @param alt_text Character. Alt text.
#'
#' @return ggplot object.
#' @keywords internal
add_ggplot_alt_caption <- function(plot_object, alt_text) {
  plot_object +
    ggplot2::labs(
      caption = paste(
        strwrap(paste0("Alt text: ", alt_text), width = 180),
        collapse = "\n"
      )
    )
}


#' Add logo to a ggplot or patchwork object
#'
#' Adds the package logo to saved ggplot outputs. For patchwork objects, the logo
#' is added in a compact header row to avoid panel overlap. For single ggplot
#' objects, the logo is added as an inset so the plot title is not pushed down.
#'
#' @param plot_object ggplot or patchwork object.
#'
#' @return ggplot or patchwork object.
#' @keywords internal
add_ggplot_logo <- function(plot_object) {
  pkg <- utils::packageName(environment(add_ggplot_logo))

  if (is.null(pkg) || !nzchar(pkg)) {
    pkg <- "climatehealth"
  }

  logo_path <- system.file(
    "extdata",
    "soschi_logo.png",
    package = pkg
  )

  # Fallback for devtools::load_all() / local development
  if (!nzchar(logo_path) || !file.exists(logo_path)) {
    local_logo_path <- file.path("inst", "extdata", "soschi_logo.png")

    if (file.exists(local_logo_path)) {
      logo_path <- local_logo_path
    }
  }

  if (!nzchar(logo_path) || !file.exists(logo_path)) {
    return(plot_object)
  }

  if (!requireNamespace("png", quietly = TRUE)) {
    warning("Package 'png' is required to add logo to ggplot outputs.")
    return(plot_object)
  }

  if (!requireNamespace("patchwork", quietly = TRUE)) {
    warning("Package 'patchwork' is required to add logo to ggplot outputs.")
    return(plot_object)
  }

  img <- try(png::readPNG(logo_path), silent = TRUE)

  if (inherits(img, "try-error")) {
    warning("Could not read logo file: ", logo_path)
    return(plot_object)
  }

  is_patchwork_plot <- inherits(plot_object, "patchwork")

  # Patchwork plots need a header row. Inset logos can align to an internal
  # panel and appear in the middle of the figure.
  if (is_patchwork_plot) {
    logo_grob <- grid::rasterGrob(
      img,
      x = grid::unit(0, "npc"),
      y = grid::unit(1, "npc"),
      just = c("left", "top"),
      width = grid::unit(0.16, "npc"),
      interpolate = TRUE
    )

    logo_plot <- ggplot2::ggplot() +
      ggplot2::annotation_custom(
        grob = logo_grob,
        xmin = -Inf,
        xmax = Inf,
        ymin = -Inf,
        ymax = Inf
      ) +
      ggplot2::theme_void() +
      ggplot2::theme(
        plot.margin = ggplot2::margin(0, 0, 0, 0)
      )

    full_plot <- patchwork::wrap_elements(full = plot_object)

    return(
      logo_plot / full_plot +
        patchwork::plot_layout(heights = c(0.025, 1))
    )
  }

  # Single ggplot outputs use an inset logo. This avoids adding a header row,
  # which otherwise creates too much space between the logo and plot title.
  logo_grob <- grid::rasterGrob(
    img,
    interpolate = TRUE
  )

  plot_object +
    patchwork::inset_element(
      logo_grob,
      left = 0.012,
      bottom = 0.94,
      right = 0.13,
      top = 0.995,
      align_to = "full",
      clip = TRUE
    )
}


#' Save accessible ggplot output
#'
#' Saves a ggplot or patchwork object as a PDF and adds the package logo when
#' available. Alt text should be included in the plot caption before saving.
#'
#' @param plot_object ggplot or patchwork object.
#' @param output_dir Character. Output directory.
#' @param filename Character. File stem without extension.
#' @param width Numeric. Width in inches.
#' @param height Numeric. Height in inches.
#' @param alt_text Character or NULL. Retained for compatibility. Not written to a separate file.
#'
#' @return Invisibly returns the saved plot path.
#' @keywords internal
save_accessible_ggplot <- function(
    plot_object,
    output_dir,
    filename,
    width,
    height,
    alt_text = NULL
) {
  if (is.null(output_dir)) {
    stop("output_dir must be specified.")
  }

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  filename <- make_safe_plot_filename(filename)
  output_path <- file.path(output_dir, paste0(filename, ".pdf"))

  plot_object <- add_ggplot_logo(plot_object)

  ggplot2::ggsave(
    filename = output_path,
    plot = plot_object,
    width = width,
    height = height,
    device = "pdf",
    bg = "white",
    limitsize = FALSE
  )

  invisible(output_path)
}
