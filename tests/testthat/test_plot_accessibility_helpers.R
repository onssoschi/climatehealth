# Test for plot_accessibility_helpers.R

# Tests for get_accessible_palette
test_that(
  "get_accessible_palette returns expected named hex colours",
  {
    cols <- get_accessible_palette()

    expect_type(cols, "list")
    expect_named(
      cols,
      c(
        "prussian_blue",
        "deep_water",
        "dusky_rose",
        "olive_green",
        "smoke_grey",
        "text",
        "axis"
      )
    )

    expect_true(all(vapply(cols, is.character, logical(1))))
    expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", unlist(cols))))
  }
)


# Tests for get_plot_grid
patrick::with_parameters_test_that(
  "get_plot_grid calculates rows and columns correctly",
  {
    grid <- get_plot_grid(n_plots = n_plots, max_cols = max_cols)

    expect_equal(grid, expected_grid)
  },
  n_plots = c(1, 2, 3, 5, 4),
  max_cols = c(2, 2, 2, 2, 1),
  expected_grid = list(
    list(n_col = 1, n_row = 1),
    list(n_col = 2, n_row = 1),
    list(n_col = 2, n_row = 2),
    list(n_col = 2, n_row = 3),
    list(n_col = 1, n_row = 4)
  ),
  .test_name = c(
    "single plot",
    "two plots",
    "three plots",
    "five plots",
    "single column layout"
  )
)


# Tests for get_pdf_size
patrick::with_parameters_test_that(
  "get_pdf_size respects panel dimensions and minimum size",
  {
    size <- get_pdf_size(
      n_col = n_col,
      n_row = n_row,
      panel_width = panel_width,
      panel_height = panel_height,
      min_width = min_width,
      min_height = min_height
    )

    expect_equal(size, expected_size)
  },
  n_col = c(1, 2, 2, 1),
  n_row = c(1, 1, 3, 1),
  panel_width = c(7, 7, 7, 12),
  panel_height = c(5.2, 5.2, 5.2, 8),
  min_width = c(9, 9, 9, 9),
  min_height = c(6, 6, 6, 6),
  expected_size = list(
    list(width = 9, height = 6),
    list(width = 14, height = 6),
    list(width = 14, height = 15.6),
    list(width = 12, height = 8)
  ),
  .test_name = c(
    "minimum size",
    "two columns",
    "multiple rows",
    "custom panel size"
  )
)


# Tests for get_layout_matrix
patrick::with_parameters_test_that(
  "get_layout_matrix creates expected layout matrices",
  {
    mat <- get_layout_matrix(
      n_plots = n_plots,
      n_col = n_col,
      n_row = n_row
    )

    expect_equal(mat, expected_matrix)
  },
  n_plots = c(1, 3, 5),
  n_col = c(1, 2, 2),
  n_row = c(1, 2, 3),
  expected_matrix = list(
    matrix(1, nrow = 1, ncol = 1),
    matrix(c(1, 2, 3, 0), nrow = 2, ncol = 2, byrow = TRUE),
    matrix(c(1, 2, 3, 4, 5, 0), nrow = 3, ncol = 2, byrow = TRUE)
  ),
  .test_name = c(
    "single panel",
    "one empty panel",
    "six cell layout with one empty panel"
  )
)


# Tests for setup_accessible_par
test_that(
  "setup_accessible_par applies expected plotting parameters",
  {
    file <- tempfile(fileext = ".pdf")
    grDevices::pdf(file)
    on.exit(grDevices::dev.off(), add = TRUE)

    setup_accessible_par(
      mar = c(1, 2, 3, 4),
      oma = c(5, 6, 7, 8),
      cex_axis = 0.8,
      cex_lab = 0.9,
      cex_main = 1.1,
      mgp = c(2, 0.5, 0)
    )

    expect_equal(graphics::par("mar"), c(1, 2, 3, 4))
    expect_equal(graphics::par("oma"), c(5, 6, 7, 8))
    expect_equal(graphics::par("cex.axis"), 0.8)
    expect_equal(graphics::par("cex.lab"), 0.9)
    expect_equal(graphics::par("cex.main"), 1.1)
    expect_equal(graphics::par("mgp"), c(2, 0.5, 0))
    expect_equal(graphics::par("family"), "sans")
  }
)


# Tests for open_accessible_pdf
patrick::with_parameters_test_that(
  "open_accessible_pdf opens a PDF device and returns grid specification",
  {
    file <- tempfile(fileext = ".pdf")
    dev_before <- grDevices::dev.cur()

    grid <- open_accessible_pdf(
      file = file,
      n_plots = n_plots,
      max_cols = max_cols
    )

    expect_equal(grid, expected_grid)
    expect_false(identical(grDevices::dev.cur(), dev_before))
    expect_gt(grDevices::dev.cur(), 1)

    graphics::plot.new()
    grDevices::dev.off()
  },
  n_plots = c(1, 3),
  max_cols = c(2, 2),
  expected_grid = list(
    list(n_col = 1, n_row = 1),
    list(n_col = 2, n_row = 2)
  ),
  .test_name = c(
    "single plot layout",
    "multi panel layout"
  )
)


# Tests for add_accessible_outer_title
test_that(
  "add_accessible_outer_title runs without error",
  {
    file <- tempfile(fileext = ".pdf")
    grDevices::pdf(file)
    on.exit(grDevices::dev.off(), add = TRUE)

    graphics::plot.new()

    expect_no_error(
      add_accessible_outer_title(
        title = "Main title",
        subtitle = "Subtitle"
      )
    )

    expect_no_error(
      add_accessible_outer_title(
        title = "Main title only",
        subtitle = NULL
      )
    )
  }
)


# Tests for add_right_axis_label
test_that(
  "add_right_axis_label runs without error with and without label",
  {
    file <- tempfile(fileext = ".pdf")
    grDevices::pdf(file)
    on.exit(grDevices::dev.off(), add = TRUE)

    graphics::plot(
      x = 1:10,
      y = 1:10,
      type = "n",
      ylim = c(0, 10),
      axes = FALSE,
      xlab = "",
      ylab = ""
    )

    expect_no_error(
      add_right_axis_label(
        at = c(0, 5, 10),
        labels = c("Low", "Mid", "High"),
        ylim = c(0, 10),
        side_label = "Right axis"
      )
    )

    expect_no_error(
      add_right_axis_label(
        at = c(0, 5, 10),
        labels = c("0", "5", "10"),
        ylim = c(0, 10),
        side_label = NULL
      )
    )
  }
)


# Tests for get_month_labels
test_that(
  "get_month_labels returns one-letter month labels",
  {
    expect_equal(
      get_month_labels(),
      c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")
    )

    expect_length(get_month_labels(), 12)
  }
)


# Tests for add_plot_logo
test_that(
  "add_plot_logo returns invisibly when file is missing",
  {
    file <- tempfile(fileext = ".pdf")
    grDevices::pdf(file)
    on.exit(grDevices::dev.off(), add = TRUE)

    graphics::plot.new()

    expect_null(
      add_plot_logo("missing-logo-file.png")
    )
  }
)

test_that(
  "add_plot_logo handles invalid image files gracefully",
  {
    file <- tempfile(fileext = ".pdf")
    grDevices::pdf(file)
    on.exit(grDevices::dev.off(), add = TRUE)

    graphics::plot.new()

    invalid_logo <- tempfile(fileext = ".png")
    writeLines("not a real png", invalid_logo)

    expect_null(
      add_plot_logo(invalid_logo)
    )
  }
)


# Tests for add_accessible_alt_text
test_that(
  "add_accessible_alt_text writes wrapped alt text without error",
  {
    file <- tempfile(fileext = ".pdf")
    grDevices::pdf(file)
    on.exit(grDevices::dev.off(), add = TRUE)

    graphics::plot.new()

    expect_no_error(
      add_accessible_alt_text(
        alt_text = paste(
          "This figure shows a diagnostic plot with accessible alt text.",
          "The text is wrapped into the outer bottom margin."
        ),
        width = 40
      )
    )
  }
)


# Tests for add_figure_legend
test_that(
  "add_figure_legend runs without error and restores par",
  {
    file <- tempfile(fileext = ".pdf")
    grDevices::pdf(file)
    on.exit(grDevices::dev.off(), add = TRUE)

    graphics::plot.new()
    old_mar <- graphics::par("mar")
    old_oma <- graphics::par("oma")

    expect_no_error(
      add_figure_legend(
        legend = c("Observed", "Expected"),
        col = c("#1B365D", "#B07A8F"),
        lty = c(1, 2),
        lwd = c(2, 2),
        pch = c(16, NA)
      )
    )

    expect_equal(graphics::par("mar"), old_mar)
    expect_equal(graphics::par("oma"), old_oma)
  }
)


# Tests for run_accessible_pdf_plot
test_that(
  "run_accessible_pdf_plot adds title and optional logo without error",
  {
    file <- tempfile(fileext = ".pdf")
    grDevices::pdf(file)
    on.exit(grDevices::dev.off(), add = TRUE)

    graphics::plot.new()

    expect_no_error(
      run_accessible_pdf_plot(
        title = "Figure title",
        subtitle = "Figure subtitle"
      )
    )

    expect_no_error(
      run_accessible_pdf_plot(
        title = NULL,
        subtitle = NULL
      )
    )
  }
)


# Tests for open_diag_pdf
test_that(
  "open_diag_pdf delegates to open_accessible_pdf with diagnostic layout",
  {
    file <- tempfile(fileext = ".pdf")

    grid <- open_diag_pdf(
      file = file,
      n_panels = 3
    )

    expect_equal(
      grid,
      list(n_col = 1, n_row = 3)
    )

    expect_gt(grDevices::dev.cur(), 1)

    graphics::plot.new()
    grDevices::dev.off()
  }
)


# Tests for close_diag_pdf
test_that(
  "close_diag_pdf adds annotations and closes the current PDF device",
  {
    file <- tempfile(fileext = ".pdf")

    dev_before <- grDevices::dev.cur()

    grDevices::pdf(file)
    pdf_dev <- grDevices::dev.cur()

    graphics::plot.new()

    expect_no_error(
      close_diag_pdf(
        title = "Diagnostic figure",
        subtitle = "Model diagnostics",
        alt_text = "Diagnostic plots showing model behaviour."
      )
    )

    open_devices <- grDevices::dev.list()

    expect_false(pdf_dev %in% open_devices)
    expect_equal(grDevices::dev.cur(), dev_before)
    expect_true(file.exists(file))
  }
)

# Tests for get_accessible_plot_colours
test_that(
  "get_accessible_plot_colours maps values from accessible palette",
  {
    palette <- get_accessible_palette()
    cols <- get_accessible_plot_colours()

    expect_equal(cols$primary, palette$deep_water)
    expect_equal(cols$primary_dark, palette$prussian_blue)
    expect_equal(cols$secondary, palette$dusky_rose)
    expect_equal(cols$highlight, palette$olive_green)
    expect_equal(cols$uncertainty, palette$smoke_grey)
    expect_equal(cols$reference, palette$text)
    expect_equal(cols$axis, palette$axis)
    expect_equal(cols$text, palette$text)
    expect_equal(cols$national, palette$olive_green)
    expect_equal(cols$regional, palette$deep_water)

    expect_equal(cols$background, "white")
    expect_equal(cols$panel, "white")
    expect_equal(cols$grid, "#E8EBEF")
expect_equal(cols$warning, "#B00020")
expect_equal(cols$secondary_dark, "#A04B58")
}
)


test_that(
  "get_accessible_plot_colours returns valid hex or named colours",
  {
    cols <- get_accessible_plot_colours()

    is_valid_colour <- function(x) {
      is_hex <- grepl("^#[0-9A-Fa-f]{6}$", x)
      is_named <- x %in% grDevices::colors()
      is_hex || is_named
    }

    expect_true(all(vapply(cols, is_valid_colour, logical(1))))
  }
)


# Tests for get_accessible_ggplot_grid
patrick::with_parameters_test_that(
  "get_accessible_ggplot_grid calculates rows and columns with max two columns",
  {
    grid <- get_accessible_ggplot_grid(n_plots)

    expect_equal(grid, expected_grid)
  },
  n_plots = c(1, 2, 3, 4, 5, 6),
  expected_grid = list(
    list(n_col = 1, n_row = 1),
    list(n_col = 2, n_row = 1),
    list(n_col = 2, n_row = 2),
    list(n_col = 2, n_row = 2),
    list(n_col = 2, n_row = 3),
    list(n_col = 2, n_row = 3)
  ),
  .test_name = c(
    "one plot",
    "two plots",
    "three plots",
    "four plots",
    "five plots",
    "six plots"
  )
)


# Tests for get_accessible_ggplot_size
patrick::with_parameters_test_that(
  "get_accessible_ggplot_size returns expected size from accessible grid",
  {
    size <- get_accessible_ggplot_size(n_plots)

    expect_equal(size, expected_size)
  },
  n_plots = c(1, 2, 3, 6),
  expected_size = list(
    list(n_col = 1, n_row = 1, width = 9, height = 6),
    list(n_col = 2, n_row = 1, width = 13.6, height = 6),
    list(n_col = 2, n_row = 2, width = 13.6, height = 10.4),
    list(n_col = 2, n_row = 3, width = 13.6, height = 15.6)
  ),
  .test_name = c(
    "single plot minimum size",
    "two plot width",
    "three plot two-row size",
    "six plot three-row size"
  )
)


# Tests for make_safe_plot_filename
patrick::with_parameters_test_that(
  "make_safe_plot_filename removes extensions and unsafe characters",
  {
    expect_equal(
      make_safe_plot_filename(input),
      expected
    )
  },
  input = c(
    "plot name.pdf",
    "air pollution: WHO ref 15.png",
    "__messy___name!!!.pdf",
    "already-safe_name-1",
    "name.with.many.parts.jpeg"
  ),
  expected = c(
    "plot_name",
    "air_pollution_WHO_ref_15",
    "messy_name",
    "already-safe_name-1",
    "name_with_many_parts"
  ),
  .test_name = c(
    "space and pdf extension",
    "punctuation and png extension",
    "messy underscores",
    "already safe",
    "multiple dots"
  )
)


# Tests for theme_accessible_ggplot
test_that(
  "theme_accessible_ggplot returns expected ggplot theme settings",
  {
    th <- theme_accessible_ggplot()

    expect_s3_class(th, "theme")

    expect_equal(th$plot.title$hjust, 0.5)
    expect_equal(th$plot.title$face, "bold")
    expect_equal(th$plot.title$size, 17)

    expect_equal(th$plot.subtitle$hjust, 0.5)
    expect_equal(th$plot.subtitle$size, 13)

    expect_equal(th$plot.caption$hjust, 0)
    expect_equal(th$plot.caption$face, "italic")

    expect_equal(th$axis.text.x$angle, 45)
    expect_equal(th$axis.text.x$hjust, 1)
    expect_equal(th$legend.position, "bottom")

    expect_s3_class(th$panel.grid.minor, "element_blank")
    expect_s3_class(th$plot.background, "element_rect")
    expect_s3_class(th$panel.background, "element_rect")
  }
)


# Tests for theme_accessible_ggplot_panel
test_that(
  "theme_accessible_ggplot_panel returns expected panel theme settings",
  {
    th <- theme_accessible_ggplot_panel()

    expect_s3_class(th, "theme")

    expect_equal(th$plot.title$hjust, 0.5)
    expect_equal(th$plot.title$face, "bold")
    expect_equal(th$plot.title$size, 12)

    expect_equal(th$axis.title$face, "plain")
    expect_equal(th$axis.title$size, 11)
    expect_equal(th$axis.text$size, 10)

    expect_s3_class(th$panel.border, "element_rect")
    expect_equal(th$panel.border$linewidth, 0.45)

    expect_s3_class(th$plot.subtitle, "element_blank")
    expect_s3_class(th$plot.caption, "element_blank")
  }
)


# Tests for accessible_plot_annotation
test_that(
  "accessible_plot_annotation creates patchwork annotation with title subtitle and alt text",
  {
    ann <- accessible_plot_annotation(
      title = "Main title",
      subtitle = "Useful subtitle",
      alt_text = "This figure describes a test plot."
    )

    expect_s3_class(ann, "plot_annotation")

    expect_equal(ann$title, "Main title")
    expect_equal(ann$subtitle, "Useful subtitle")
    expect_true(grepl("Alt text:", ann$caption))
    expect_true(grepl("test plot", ann$caption))
  }
)


# Tests for add_ggplot_alt_caption
test_that(
  "add_ggplot_alt_caption adds alt text to ggplot caption",
  {
    p <- ggplot2::ggplot(
      data.frame(x = 1:3, y = 1:3),
      ggplot2::aes(x = x, y = y)
    ) +
      ggplot2::geom_point()

    p2 <- add_ggplot_alt_caption(
      plot_object = p,
      alt_text = "Scatter plot showing y increasing as x increases."
    )

    expect_s3_class(p2, "ggplot")
    expect_true(grepl("Alt text:", p2$labels$caption))
    expect_true(grepl("Scatter plot", p2$labels$caption))
  }
)


# Tests for add_ggplot_logo
test_that(
  "add_ggplot_logo returns a ggplot or patchwork object without error",
  {
    p <- ggplot2::ggplot(
      data.frame(x = 1:3, y = 1:3),
      ggplot2::aes(x = x, y = y)
    ) +
      ggplot2::geom_point()

    result <- add_ggplot_logo(p)

    # If logo is unavailable, the original ggplot is returned.
    # If logo is available, single ggplot may become a patchwork object due to inset.
    expect_true(
      inherits(result, "ggplot") || inherits(result, "patchwork")
    )
  }
)


test_that(
  "add_ggplot_logo handles patchwork objects without error",
  {
    p1 <- ggplot2::ggplot(
      data.frame(x = 1:3, y = 1:3),
      ggplot2::aes(x = x, y = y)
    ) +
      ggplot2::geom_point()

    p2 <- ggplot2::ggplot(
      data.frame(x = 1:3, y = 3:1),
      ggplot2::aes(x = x, y = y)
    ) +
      ggplot2::geom_line()

    pw <- p1 + p2

    result <- add_ggplot_logo(pw)

    expect_true(
      inherits(result, "patchwork") || inherits(result, "ggplot")
    )
  }
)


# Tests for save_accessible_ggplot
test_that(
  "save_accessible_ggplot creates output directory and calls ggsave with pdf path",
  {
    p <- ggplot2::ggplot(
      data.frame(x = 1:3, y = 1:3),
      ggplot2::aes(x = x, y = y)
    ) +
      ggplot2::geom_point()

    tmpdir <- file.path(withr::local_tempdir(), "nested", "plots")

    captured <- new.env(parent = emptyenv())
    captured$filename <- NULL
    captured$plot <- NULL
    captured$width <- NULL
    captured$height <- NULL
    captured$device <- NULL
    captured$bg <- NULL
    captured$limitsize <- NULL

    mock_ggsave <- function(filename,
                            plot,
                            width,
                            height,
                            device,
                            bg,
                            limitsize,
                            ...) {
      captured$filename <- filename
      captured$plot <- plot
      captured$width <- width
      captured$height <- height
      captured$device <- device
      captured$bg <- bg
      captured$limitsize <- limitsize

      invisible(filename)
    }

    pkg_ns <- getNamespaceName(environment(save_accessible_ggplot))

    output_path <- testthat::with_mocked_bindings(
      {
        testthat::with_mocked_bindings(
          {
            save_accessible_ggplot(
              plot_object = p,
              output_dir = tmpdir,
              filename = "unsafe plot name.png",
              width = 10,
              height = 8,
              alt_text = "Alt text should not create a separate file."
            )
          },
          ggsave = mock_ggsave,
          .package = "ggplot2"
        )
      },
      add_ggplot_logo = function(plot_object) plot_object,
      .package = pkg_ns
    )

    expected_path <- file.path(tmpdir, "unsafe_plot_name.pdf")

    expect_true(dir.exists(tmpdir))
    expect_equal(output_path, expected_path)

    expect_equal(captured$filename, expected_path)
    expect_s3_class(captured$plot, "ggplot")
    expect_equal(captured$width, 10)
    expect_equal(captured$height, 8)
    expect_equal(captured$device, "pdf")
    expect_equal(captured$bg, "white")
    expect_false(captured$limitsize)

    expect_false(
      file.exists(file.path(tmpdir, "unsafe_plot_name_alt_text.txt"))
    )
  }
)


test_that(
  "save_accessible_ggplot sanitizes filename and always saves as pdf",
  {
    p <- ggplot2::ggplot(
      data.frame(x = 1:3, y = 1:3),
      ggplot2::aes(x = x, y = y)
    ) +
      ggplot2::geom_point()

    tmpdir <- withr::local_tempdir()

    captured <- new.env(parent = emptyenv())
    captured$filename <- NULL

    mock_ggsave <- function(filename, plot, ...) {
      captured$filename <- filename
      invisible(filename)
    }

    pkg_ns <- getNamespaceName(environment(save_accessible_ggplot))

    output_path <- testthat::with_mocked_bindings(
      {
        testthat::with_mocked_bindings(
          {
            save_accessible_ggplot(
              plot_object = p,
              output_dir = tmpdir,
              filename = "A messy/file:name?.png",
              width = 7,
              height = 5
            )
          },
          ggsave = mock_ggsave,
          .package = "ggplot2"
        )
      },
      add_ggplot_logo = function(plot_object) plot_object,
      .package = pkg_ns
    )

    expected_path <- file.path(tmpdir, "A_messy_file_name.pdf")

    expect_equal(output_path, expected_path)
    expect_equal(captured$filename, expected_path)
    expect_true(grepl("\\.pdf$", captured$filename))
    expect_false(grepl("\\.png$", captured$filename))
  }
)


test_that(
  "save_accessible_ggplot errors when output_dir is NULL",
  {
    p <- ggplot2::ggplot(
      data.frame(x = 1:3, y = 1:3),
      ggplot2::aes(x = x, y = y)
    ) +
      ggplot2::geom_point()

    expect_error(
      save_accessible_ggplot(
        plot_object = p,
        output_dir = NULL,
        filename = "plot",
        width = 10,
        height = 8
      ),
      "output_dir must be specified"
    )
  }
)
