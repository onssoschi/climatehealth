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
