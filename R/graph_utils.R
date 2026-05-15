# Utilities associated with creating graphs for indicator outputs

#' Generate a grid size for a certain number of plots.
#'
#' This function calculates the minimum grid size required to plot X amount of
#' plots on a a figure. For example, 6 plots would require a 3x2, where as 7
#' would require a 3x3, and so on.
#'
#' @param plot_count The number of plots required for the grid.
#'
#' @return A numeric vector: c(x, y), where x and y define the grid dimensions.
#'
#' @keywords internal
create_grid <- function(plot_count) {
  est <- sqrt(plot_count)
  if (est == floor(est)) {
    x <- y <- est
  } else {
    base <- est - floor(est)
    if (base < 0.5) {
      y <- floor(est)
    } else {
      y <- floor(est) + 1
    }
    x <- floor(est) + 1
  }
  return(c(x, y))
}


#' Plot a correlation matrix as an accessible heatmap.
#'
#' @param matrix_ The matrix to plot.
#' @param title The title for the correlation matrix.
#' @param output_path The path to output the plot to.
#'
#' @return Invisibly returns the ggplot object.
#'
#' @keywords internal
plot_correlation_matrix <- function(matrix_, title, output_path) {
  if (is.null(output_path) || !nzchar(output_path)) {
    stop("`output_path` must be provided.")
  }

  cols <- get_accessible_plot_colours()

  # Round correlation metrics for display
  matrix_ <- round(matrix_, 3)

  # Convert matrix to long format for ggplot
  corr_df <- as.data.frame(as.table(matrix_), stringsAsFactors = FALSE)
  names(corr_df) <- c("row_var", "col_var", "correlation")

  # Preserve matrix ordering
  corr_df$row_var <- factor(corr_df$row_var, levels = rev(rownames(matrix_)))
  corr_df$col_var <- factor(corr_df$col_var, levels = colnames(matrix_))

  # Format labels: show 1 and -1 instead of 1.00 and -1.00
  corr_df$label <- ifelse(
    is.na(corr_df$correlation),
    "",
    ifelse(
      abs(corr_df$correlation - 1) < 1e-10,
      "1",
      ifelse(
        abs(corr_df$correlation + 1) < 1e-10,
        "-1",
        sprintf("%.2f", corr_df$correlation)
      )
    )
  )

  n_vars <- ncol(matrix_)

  alt_text <- paste(
    "Correlation matrix heatmap showing pairwise correlations between selected numeric variables.",
    "Cell colour represents correlation strength and direction, with blue indicating negative correlations, grey indicating values near zero, and rose indicating positive correlations.",
    "Each cell contains the rounded correlation coefficient."
  )

  corr_plot <- ggplot2::ggplot(
    corr_df,
    ggplot2::aes(
      x = .data$col_var,
      y = .data$row_var,
      fill = .data$correlation
    )
  ) +
    ggplot2::geom_tile(
      colour = "white",
      linewidth = 0.5
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = .data$label),
      colour = cols$text,
      size = 4
    ) +
    ggplot2::scale_fill_gradient2(
      low = cols$primary_dark,
      mid = "white",
      high = cols$secondary,
      midpoint = 0,
      limits = c(-1, 1),
      na.value = cols$uncertainty,
      name = "Correlation"
    ) +
    ggplot2::coord_fixed() +
    ggplot2::labs(
      title = title,
      x = NULL,
      y = NULL,
      caption = paste(
        strwrap(paste0("Alt text: ", alt_text), width = 120),
        collapse = "\n"
      )
    ) +
    theme_accessible_ggplot() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = 45,
        hjust = 1,
        vjust = 1
      ),
      axis.text.y = ggplot2::element_text(
        angle = 0
      ),
      panel.grid = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5),
      legend.position = "right",
      plot.margin = ggplot2::margin(16, 16, 16, 16)
    )

  plot_width <- max(14, 0.95 * n_vars + 6)
  plot_height <- max(8, 0.95 * n_vars + 4)

  save_accessible_ggplot(
    plot_object = corr_plot,
    output_dir = dirname(output_path),
    filename = tools::file_path_sans_ext(basename(output_path)),
    width = plot_width,
    height = plot_height,
    alt_text = alt_text
  )

  invisible(corr_plot)
}


#' Plot histograms of column distributions.
#'
#' @param df The dataframe containing the data.
#' @param columns The columns to plot distributions for.
#' @param title The title of your plot.
#' @param xlabs A character vector of x-axis labels (e.g., with units)
#' corresponding to the columns.
#' @param save_hists Whether to save the histograms to file.
#' @param output_path The path to save your distributions to.
#'
#' @keywords internal
plot_distributions <- function(
    df,
    columns,
    title,
    xlabs = NULL,
    save_hists = FALSE,
    output_path = NULL) {

  cols <- get_accessible_palette()

  # create a pdf if a save is selected
  if (save_hists == TRUE) {
    # normalise output path
    output_path <- enforce_file_extension(output_path, ".pdf")
    open_accessible_pdf(
      file = output_path,
      n_plots = length(columns),
      max_cols = 2,
      panel_width = 7,
      panel_height = 5.2,
      mar = c(5.2, 5.2, 3.6, 2.2),
      oma = c(6.5, 0.5, 5.2, 0.5)
    )
    on.exit(grDevices::dev.off(), add = TRUE)
  } else {
    old_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old_par), add = TRUE)

    grid_size <- get_plot_grid(length(columns), max_cols = 2)

    graphics::par(
      mfrow = c(grid_size$n_row, grid_size$n_col),
      col = "white",
      oma = c(0, 1, 6, 0),
      mar = c(5.2, 5.2, 3.6, 2.2),
      cex.axis = 1.0,
      cex.lab = 1.1,
      cex.main = 1.15,
      family = "sans"
    )
  }

  # normalise columns
  columns <- c(columns)
  df <- df %>% dplyr::select(dplyr::all_of(columns))

  alt_text <- paste(
    "Alt text: Multi-panel histogram figure showing distributions for selected variables.",
    "Each panel shows the frequency distribution for one variable.",
    "Bars show counts within value ranges for the selected variable."
  )

  for (i in seq_along(columns)) {
    col_name <- columns[i]
    xlab <- if (!is.null(xlabs) && length(xlabs) >= i) xlabs[i] else col_name

    x <- df[[col_name]]
    rng <- range(x, na.rm = TRUE)
    span <- rng[2] - rng[1]

    # fallback for degenerate ranges (all values equal or NA)
    if (!is.finite(span) || span <= 0) {
      eps <- 1e-6
      br <- seq(rng[1] - eps, rng[1] + eps, length.out = 15)
    } else {
      br <- seq(rng[1], rng[2], length.out = 15)
    }

    graphics::hist(
      x,
      col = cols$deep_water,
      border = cols$prussian_blue,
      xlab = xlab,
      ylab = "Frequency",
      main = paste0("Distribution of '", col_name, "'"),
      breaks = br,
      col.axis = cols$axis,
      col.lab = cols$text,
      col.main = cols$text
    )
  }

  if (save_hists == TRUE) {
    run_accessible_pdf_plot(
      title = title,
      subtitle = "Histograms show the distribution of selected variables.",
      line_title = 3.1,
      line_subtitle = 1.7
    )

    add_accessible_alt_text(alt_text = alt_text, width = 170, line_start = 1.0)
  } else {
    graphics::mtext(title, outer = TRUE, cex = 1.6, line = 1, font = 2, col = cols$text)
  }
  invisible(NULL)
}

#' Plot the moving average of a column.
#'
#' @param df The dataframe containing the raw data.
#' @param time_col The column name of the column containing the timeseries.
#' @param value_col The column name of the column containing the value.
#' @param ma_days The number of days to use for MA calculations.
#' @param ma_sides The number of sides to use for MA calculations (1 or 2).
#' @param title The title for your plot.
#' @param save_plot Whether or not to save the plot.
#' @param output_path The path to output the plot to.
#' @param units A named character vector of units for each variable.
#'
#' @keywords internal
#' Plot the moving average of a column.
#'
#' @param df The dataframe containing the raw data.
#' @param time_col The column name of the column containing the timeseries.
#' @param value_col The column name of the column containing the value.
#' @param ma_days The number of days to use for MA calculations.
#' @param ma_sides The number of sides to use for MA calculations (1 or 2).
#' @param title The title for your plot.
#' @param save_plot Whether or not to save the plot.
#' @param output_path The path to output the plot to.
#' @param units A named character vector of units for each variable.
#'
#' @keywords internal
plot_moving_average <- function(
    df,
    time_col,
    value_col,
    ma_days,
    ma_sides,
    title,
    save_plot = FALSE,
    output_path = "",
    units = NULL) {

  cols <- get_accessible_palette()

  # Create a PDF if saving is requested
  if (save_plot == TRUE) {
    output_path <- enforce_file_extension(output_path, ".pdf")
    dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

    open_accessible_pdf(
      file = output_path,
      n_plots = 1,
      max_cols = 1,
      panel_width = 7,
      panel_height = 5.2,
      mar = c(5.2, 5.2, 3.6, 2.2),
      oma = c(6.5, 0.5, 5.2, 0.5)
    )

    on.exit(grDevices::dev.off(), add = TRUE)
  }

  # Select relevant columns
  df <- df %>%
    dplyr::select(dplyr::all_of(c(time_col, value_col)))

  # Check for valid values
  valid_values <- df[[value_col]][!is.na(df[[value_col]]) & is.finite(df[[value_col]])]
  if (length(valid_values) == 0) {
    warning(paste("No valid data to plot for", value_col))
    return(invisible(NULL))
  }

  # Compute y-limits that include 0, with headroom
  vr <- range(valid_values, na.rm = TRUE)
  y_lo0 <- min(0, vr[1])
  y_hi0 <- max(0, vr[2])
  span <- y_hi0 - y_lo0
  if (!is.finite(span) || span == 0) {
    span <- max(1, abs(y_hi0) + abs(y_lo0))
  }
  y_lim <- c(y_lo0 - 0.05 * span, y_hi0 + 0.10 * span)

  line_colour <- grDevices::adjustcolor(cols$deep_water, alpha.f = 0.5)

  # Plot raw data
  graphics::plot(
    x = df[[time_col]],
    y = df[[value_col]],
    type = "l",
    xlab = time_col,
    ylab = label_with_unit(value_col, units),
    ylim = y_lim,
    col = line_colour,
    lwd = 1.2,
    col.axis = cols$axis,
    col.lab = cols$text,
    col.main = cols$text
  )

  graphics::title(
    main = paste0("Moving average for `", value_col, "`"),
    cex.main = 1.2,
    font.main = 2,
    col.main = cols$text
  )

  # Create moving average column
  ma <- function(x, n, sides = 1) {
    stats::filter(x, rep(1 / n, n), sides = sides)
  }

  n_obs <- sum(!is.na(df[[value_col]]) & is.finite(df[[value_col]]))
  if (ma_days >= n_obs) {
    warning(paste0(
      "Skipping moving average overlay for '", value_col, "': ma_days (", ma_days,
      ") must be less than the number of valid observations (", n_obs, ")."
    ))

    if (save_plot == TRUE) {
      run_accessible_pdf_plot(
        title = title,
        subtitle = paste0("Moving average unavailable for `", value_col, "` because the window is too large."),
        line_title = 3.1,
        line_subtitle = 1.7
      )

      add_accessible_alt_text(
        alt_text = paste(
          "Alt text: Line plot showing raw values for", value_col,
          "over time. The moving average overlay was not shown because the moving average window",
          "was greater than or equal to the number of valid observations."
        ),
        width = 160,
        line_start = 1.0
      )
    }

    return(invisible(NULL))
  }

  col_name <- paste0(
    "MA_",
    as.character(ma_days),
    "days_",
    as.character(ma_sides),
    "sides"
  )

  df[[col_name]] <- ma(df[[value_col]], ma_days, ma_sides)

  # Plot moving average
  graphics::lines(
    x = df[[time_col]],
    y = df[[col_name]],
    type = "l",
    col = cols$prussian_blue,
    lwd = 1.8
  )

  # Legend
  graphics::legend(
    "topleft",
    legend = c("Actual values", paste0(ma_days, "-day moving average")),
    col = c(cols$deep_water, cols$prussian_blue),
    lwd = c(1.3, 1.8),
    lty = c(1, 1),
    bg = "white",
    box.lwd = 0.8,
    text.col = cols$text,
    cex = 0.9
  )

  if (save_plot == TRUE) {
    run_accessible_pdf_plot(
      title = title,
      subtitle = paste0("Moving average for `", value_col, "`"),
      line_title = 3.1,
      line_subtitle = 1.7
    )

    add_accessible_alt_text(
      alt_text = paste(
        "Alt text: Line plot showing actual values and a moving average for",
        value_col,
        "over time. The pale blue line shows actual values and the dark blue line shows the",
        paste0(ma_days, "-day moving average.")
      ),
      width = 120,
      line_start = 1.0
    )
  }

  invisible(NULL)
}


#' Plot a grid of scatter graphs comparing one column to various others.
#'
#' @param df The dataframe containing the raw data.
#' @param main_col The main column to compare with all other columns.
#' @param comparison_cols The columns to compare with.
#' @param title The title of your plot.
#' @param save_scatters Whether or not to save the plot.
#' @param output_path The path to output the plot to.
#' @param units A named character vector of units for each variable.
#'
#' @keywords internal
plot_scatter_grid <- function(
    df,
    main_col,
    comparison_cols,
    title,
    save_scatters = FALSE,
    output_path = "",
    units = NULL) {

  cols <- get_accessible_palette()

  all_columns <- c(main_col, comparison_cols)
  if (length(all_columns) < 2) {
    return()
  }

  df <- df %>% dplyr::select(all_of(all_columns))
  grid_size <- get_plot_grid(length(comparison_cols), max_cols = 2)

  if (save_scatters) {
    output_path <- enforce_file_extension(output_path, ".pdf")
    dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
    plot_height <- max(10, grid_size$n_row * 5.2)
    grDevices::pdf(output_path,
      width = 14, height = plot_height, paper = "special", family = "sans")
    on.exit(grDevices::dev.off(), add = TRUE)
  } else {
    old_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old_par), add = TRUE)
  }
  par(mfrow = c(grid_size$n_row, grid_size$n_col), col = "white",
      oma = c(6.5, 0.6, 7.2, 0.6), mar = c(5.2, 5.2, 3.6, 2.2),
      xpd = NA, cex.axis = 1.0, cex.lab = 1.1, cex.main = 1.15, family = "sans")

  point_colour <- grDevices::adjustcolor(cols$deep_water, alpha.f = 0.5)
  line_colour <- cols$dusky_rose

  # sample 20% to reduce file size; keep all rows if tiny dataset
  n <- nrow(df)
  idx_all <- if (n >= 10000) sample.int(n, max(2L, floor(n * 0.20))) else seq_len(n)

  for (i in seq_along(comparison_cols)) {
    x_col <- comparison_cols[i]
    xlab  <- label_with_unit(x_col, units)
    ylab  <- label_with_unit(main_col, units)

    # sample, drop NA/Inf
    x <- df[[x_col]][idx_all]
    y <- df[[main_col]][idx_all]
    ok <- is.finite(x) & is.finite(y)
    x <- x[ok]; y <- y[ok]

    # scatter
    plot(
      x = x, y = y,
      col = point_colour,
      xlab = xlab, ylab = ylab,
      main = paste0(main_col, " vs ", x_col),
      col.axis = cols$axis, col.lab = cols$text, col.main = cols$text
    )

    # LOESS line of best fit (span tuned for stability)
    if (length(x) >= 10) {
      df_fit <- data.frame(x = x, y = y)
      fit <- try(stats::loess(y ~ x, data = df_fit, span = 0.7), silent = TRUE)
      if (!inherits(fit, "try-error")) {
        xs <- seq(min(x), max(x), length.out = 200)
        lines(xs, predict(fit, newdata = data.frame(x = xs)), col = line_colour, lwd = 2)
      }
    }
  }

  is_full   <- grepl("\\bfull dataset\\b", tolower(title))
  sample_note <- if (is_full) "* 20% random sample shown to reduce file size" else " "
  subtitle <- paste(
    "Points show observed data. Rose line shows LOESS smooth.",
    if (!is.null(sample_note)) sample_note else ""
  )

  if (save_scatters) {
    run_accessible_pdf_plot(title = title, subtitle = subtitle,
      line_title = 4.7, line_subtitle = 3.0)

    add_accessible_alt_text(
      alt_text = paste(
        "Alt text: Multi-panel scatter plot comparing",
        main_col,
        "against selected independent variables.",
        "Each panel shows observed data points for one comparison variable.",
        "A rose LOESS smooth line is shown where sufficient data are available.",
        if (!is.null(sample_note)) sample_note else ""
      ),
      width = 165,
      line_start = 1.0
    )
  } else {
    graphics::mtext(title, outer = TRUE, side = 3, line = 4.7, cex = 1.4,
      font = 2, col = cols$text)

    graphics::mtext(subtitle, outer = TRUE, side = 3, line = 3.0,
      cex = 0.94, col = cols$text)
  }
  invisible(NULL)
}


#' Plot a grid of box plots for multiple numeric variables
#'
#' @param df The dataframe containing the data
#' @param columns A character vector of numeric column names to plot
#' @param select_numeric If TRUE, all numeric columns in `df` will be selected for plotting.
#' @param title The overall title for the plot
#' @param ylabs A character vector of y-axis labels (e.g., with units) corresponding to the columns.
#' @param save_plot Whether to save the plot as a PDF
#' @param output_path The file path to save the PDF (if save_plot is TRUE)
#'
#' @keywords internal
plot_boxplots <- function(
    df,
    columns = NULL,
    select_numeric = FALSE,
    title = "Boxplots",
    ylabs = NULL,
    save_plot = FALSE,
    output_path = NULL) {

  cols <- get_accessible_palette()

  # Select columns based on user input
  if (!is.null(columns)) {
    selected_cols <- columns
  } else if (select_numeric) {
    selected_cols <- names(df)[sapply(df, is.numeric)]
  } else {
    stop("Please specify columns or 'select_numeric' to TRUE")
  }

  df <- df %>% dplyr::select(all_of(selected_cols))

  # Save to PDF if requested
  if (save_plot) {
    output_path <- enforce_file_extension(output_path, ".pdf")
    open_accessible_pdf(
      file = output_path,
      n_plots = length(selected_cols),
      max_cols = 2,
      panel_width = 7,
      panel_height = 5.2,
      mar = c(5.2, 5.2, 3.6, 2.2),
      oma = c(6.5, 0.6, 5.2, 0.6)
    )
    on.exit(grDevices::dev.off(), add = TRUE)
  } else {
    old_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old_par), add = TRUE)
    grid_size <- get_plot_grid(length(selected_cols), max_cols = 2)
    graphics::par(
      mfrow = c(grid_size$n_row, grid_size$n_col),
      oma = c(0, 1, 4, 0),
      mar = c(5.2, 5.2, 3.6, 2.2),
      cex.axis = 1.0,
      cex.lab = 1.1,
      cex.main = 1.15,
      family = "sans"
    )
  }

  alt_text <- paste(
    "Alt text: Multi-panel boxplot figure showing distributions for selected variables.",
    "Each panel shows one variable.",
    "Each box shows the interquartile range, the horizontal line shows the median, whiskers show the spread of non-outlier values, and points show outliers where present."
  )

  # Plot each variable
  for (i in seq_along(selected_cols)) {
    col_name <- selected_cols[i]
    ylab <- if (!is.null(ylabs) && length(ylabs) >= i) ylabs[i] else col_name

    boxplot(df[[col_name]],
            main = col_name,
            col = cols$deep_water,
            border = cols$prussian_blue,
            outline = TRUE,
            horizontal = FALSE,
            ylab = ylab,
            col.axis = cols$axis,
            col.lab = cols$text,
            col.main = cols$text
    )
  }

  if (save_plot) {
    run_accessible_pdf_plot(
      title = title,
      subtitle = "Boxplots show distribution, median, interquartile range, whiskers, and outliers.",
      line_title = 3.1,
      line_subtitle = 1.7
    )
    add_accessible_alt_text(alt_text = alt_text, width = 165, line_start = 1.0)
  } else {
    graphics::mtext(title, outer = TRUE, cex = 1.6, line = 1, font = 2, col = cols$text)
  }

  invisible(NULL)
}


#' Plot seasonal trends of a health outcome and climate by month.
#'
#' @param df The dataframe containing the raw data.
#' @param date_col The name of the column containing date values.
#' @param outcome_cols Character Vector. The names of the outcome columns to analyse.
#' @param title The title of your plot.
#' @param ylabs A character vector of y-axis labels (e.g., with units) corresponding to the columns.
#' @param save_plot Whether or not to save the plot.
#' @param output_path The path to output the plot to.
#'
#' @keywords internal
plot_seasonal_trends <- function(
    df,
    date_col,
    outcome_cols,
    title = "Seasonal Averages",
    ylabs = NULL,
    save_plot = FALSE,
    output_path = "") {

  cols <- get_accessible_palette()

  # Ensure date column is Date type
  df[[date_col]] <- as.Date(df[[date_col]])

  # Extract month
  df$month <- lubridate::month(df[[date_col]], label = TRUE)

  # Set up PDF output if needed
  if (save_plot) {
    output_path <- enforce_file_extension(output_path, ".pdf")
    open_accessible_pdf(
      file = output_path,
      n_plots = length(outcome_cols),
      max_cols = 2,
      panel_width = 7,
      panel_height = 5.2,
      mar = c(5.2, 5.2, 3.6, 2.2),
      oma = c(6.5, 0.6, 5.2, 0.6)
    )
    on.exit(grDevices::dev.off(), add = TRUE)
  } else {
    old_par <- graphics::par(no.readonly = TRUE)
    grid_size <- get_plot_grid(length(outcome_cols), max_cols = 2)
    graphics::par(
      mfrow = c(grid_size$n_row, grid_size$n_col),
      oma = c(0, 1, 5, 0),
      mar = c(5.2, 5.2, 3.6, 2.2),
      cex.axis = 1.1,
      cex.lab = 1.2,
      cex.main = 1.3,
      family = "sans"
    )
  }

  alt_text <- paste(
    "Alt text: Multi-panel bar chart showing monthly average values for selected variables.",
    "Each panel represents one variable.",
    "Bars show the mean value for each calendar month.",
    "A note on the figure states that mean values are used for calculations."
  )

  # Loop through each outcome column
  for (i in seq_along(outcome_cols)) {
    col <- outcome_cols[i]
    ylab <- if (!is.null(ylabs) && length(ylabs) >= i) ylabs[i] else paste("Average", col)

    monthly_avg <- aggregate(df[[col]], by = list(Month = df$month), FUN = mean, na.rm = TRUE)

    barplot(
      height = monthly_avg$x,
      names.arg = monthly_avg$Month,
      col = cols$deep_water,
      border = cols$prussian_blue,
      main = col,
      ylab = ylab,
      las = 2,
      yaxs = "i",
      col.axis = cols$axis,
      col.lab = cols$text,
      col.main = cols$text
    )
  }

  if (save_plot) {
    run_accessible_pdf_plot(
      title = title,
      subtitle = "Monthly averages are calculated using mean values.",
      line_title = 3.1,
      line_subtitle = 1.7
    )

    add_accessible_alt_text(alt_text = alt_text, width = 165, line_start = 1.0)
  } else {
    graphics::mtext(title, outer = TRUE, side = 3, line = 3, cex = 1.6, font = 2, col = cols$text)
    graphics::mtext(
      "(Note: mean used for calculations)",
      outer = TRUE,
      side = 3,
      line = 1.4,
      adj = 0.5,
      col = cols$text,
      cex = 0.90
    )
  }
  invisible(NULL)
}


#' Plot regional trends of a climate and healthoutcome.
#'
#' @param df The dataframe containing the raw data.
#' @param region_col The name of the column containing regions.
#' @param outcome_cols Character Vector. The names of the outcome columns to analyse.
#' @param title The title of your plot.
#' @param ylabs A character vector of y-axis labels (e.g., with units) corresponding to the columns.
#' @param save_plot Whether or not to save the plot.
#' @param output_path The path to output the plot to.
#'
#' @keywords internal
plot_regional_trends <- function(
    df,
    region_col,
    outcome_cols,
    title = "Regional Averages",
    ylabs = NULL,
    save_plot = FALSE,
    output_path = "") {

  cols <- get_accessible_palette()

  # Helper to truncate labels with ellipsis
  truncate_labels <- function(x, n = 12) {
    vapply(x, function(s) {
      if (is.na(s)) return(NA_character_)
      s <- as.character(s)
      if (nchar(s) <= n) s else paste0(substr(s, 1, max(1, n - 1)), "...")
    }, character(1))
  }

  # Set up PDF output if needed
  if (save_plot) {
    output_path <- enforce_file_extension(output_path, ".pdf")
    open_accessible_pdf(
      file = output_path,
      n_plots = length(outcome_cols),
      max_cols = 2,
      panel_width = 7,
      panel_height = 5.2,
      mar = c(9.5, 5.2, 3.6, 2.2),
      oma = c(6.5, 0.6, 5.2, 0.6)
    )
    on.exit(grDevices::dev.off(), add = TRUE)
  } else {
    old_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old_par), add = TRUE)
    grid_size <- get_plot_grid(length(outcome_cols), max_cols = 2)
    graphics::par(
      mfrow = c(grid_size$n_row, grid_size$n_col),
      oma = c(0, 1, 5, 0),
      mar = c(9.5, 5.2, 3.6, 2.2),
      cex.axis = 1.1,
      cex.lab = 1.2,
      cex.main = 1.6,
      family = "sans"
    )
  }

  alt_text <- paste(
    "Alt text: Multi-panel bar chart showing regional average values for selected variables.",
    "Each panel represents one variable.",
    "Bars show mean values by region.",
    "Region labels may be shortened with ellipses where names are long."
  )

  # Loop through each outcome column
  for (i in seq_along(outcome_cols)) {
    col <- outcome_cols[i]
    ylab <- if (!is.null(ylabs) && length(ylabs) >= i) ylabs[i] else col

    valid_rows <- !is.na(df[[col]]) & !is.na(df[[region_col]])
    if (sum(valid_rows) == 0) {
      warning(paste("No valid data to plot for", col))
      next
    }

    regional_avg <- aggregate(
      df[[col]][valid_rows],
      by = list(Region = df[[region_col]][valid_rows]),
      FUN = function(x) mean(x, na.rm = TRUE)
    )

    # Truncate long labels
    lab_trunc <- truncate_labels(regional_avg$Region, n = 15)
    # Headroom for readability
    y_max <- max(regional_avg$x, na.rm = TRUE)
    y_top <- if (is.finite(y_max) && y_max > 0) y_max * 1.10 else 1

    barplot(
      height = regional_avg$x,
      names.arg = lab_trunc,
      col = cols$deep_water,
      border = cols$prussian_blue,
      main = col,
      ylab = ylab,
      las = 2,
      yaxs = "i",
      col.axis = cols$axis,
      col.lab = cols$text,
      col.main = cols$text
    )
  }

  if (save_plot) {
    run_accessible_pdf_plot(
      title = title,
      subtitle = "Bars show mean values per region.",
      line_title = 3.1,
      line_subtitle = 1.7
    )
    add_accessible_alt_text(alt_text = alt_text, width = 165, line_start = 1.0)
  } else {
    graphics::mtext(title, outer = TRUE, side = 3, line = 3, cex = 1.6, font = 2, col = cols$text)
    graphics::mtext(
      "(Note: mean used for calculations)",
      outer = TRUE,
      side = 3,
      line = 1.4,
      adj = 0.5,
      col = cols$text,
      cex = 0.90
    )
  }
  invisible(NULL)
}

#' Plot the rate of a dependent variable per 100,000 population per year.
#'
#' @param df The dataframe containing the data.
#' @param dependent_col The name of the column representing the dependent variable.
#' @param population_col The name of the column representing the population.
#' @param date_col The name of the column containing date values.
#' @param title Character. The specific title for the subset of data being used.
#' @param save_rate Whether to save the plot as a PDF.
#' @param output_path The file path to save the plot if save_rate is TRUE.
#'
#' @keywords internal
plot_rate_overall <- function(
    df,
    dependent_col,
    population_col,
    date_col,
    title,
    save_rate = FALSE,
    output_path = NULL) {

  cols <- get_accessible_plot_colours()

  # Clean numeric columns
  df[[population_col]] <- as.numeric(gsub(",", "", df[[population_col]]))
  df[[dependent_col]] <- as.numeric(df[[dependent_col]])

  # Extract year
  df$Year <- lubridate::year(as.Date(df[[date_col]]))

  # Aggregate
  yearly_data <- df %>%
    dplyr::group_by(.data$Year) %>%
    dplyr::summarise(
      Total_Dependent = sum(.data[[dependent_col]], na.rm = TRUE),
      Total_Population = mean(.data[[population_col]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      Rate_per_100k = round((.data$Total_Dependent / .data$Total_Population) * 100000, 3)
    )
  # Dynamic y limit with headroom
  y_max <- max(yearly_data$Rate_per_100k, na.rm = TRUE)
  y_top <- if (is.finite(y_max)) y_max * 1.10 else NA


  alt_text <- paste(
    "Line chart showing annual rate per 100,000 population for",
    dependent_col,
    "in",
    title,
    ". Points show yearly rates and the line connects annual rates over time."
  )

  # Create plot
  plot_overall_rate <- ggplot2::ggplot(yearly_data, ggplot2::aes(x = .data$Year, y = .data$Rate_per_100k)) +
    ggplot2::geom_line(group = 1, color = cols$primary, linewidth = 1.2) +
    ggplot2::geom_point(color = cols$primary_dark, size = 2) +
    ggplot2::labs(
      title = paste0("Annual " , dependent_col, " rate per 100,000 population - ", title),
      y = "Rate per 100,000 population",
      x = "Year",
      caption = paste(
        strwrap(paste0("Alt text: ", alt_text), width = 165),
        collapse = "\n"
      )
    ) +
    ggplot2::scale_y_continuous(limits = c(0, y_top),
                                expand = ggplot2::expansion(mult = c(0, 0.02))) +
    theme_accessible_ggplot() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5),
      plot.title = ggplot2::element_text(hjust = 0.5)
    )

  # Save or return
  if (save_rate && !is.null(output_path)) {
    output_path <- enforce_file_extension(output_path, ".pdf")
    save_accessible_ggplot(
      plot_object = plot_overall_rate,
      output_dir = dirname(output_path),
      filename = tools::file_path_sans_ext(basename(output_path)),
      width = 14,
      height = 8,
      alt_text = alt_text
    )
  } else {
    return(plot_overall_rate)
  }
  invisible(plot_overall_rate)
}


#' Plot the total of selected variables per year.
#'
#' @param df A dataframe containing the data.
#' @param date_col The name of the column containing date values.
#' @param variables Column names to be summed and plotted.
#' @param title Character. The specific title for the subset of data being used.
#' @param save_total if TRUE, saves each plot as a PDF.
#' @param output_path The file path for saving plots.
#'
#' @return Plots are printed to the console or saved as PDF files.
#'
#' @keywords internal
plot_total_variables_by_year <- function(
    df,
    date_col,
    variables,
    title,
    save_total = FALSE,
    output_path = "") {

  cols <- get_accessible_plot_colours()

  variables <- unique(as.character(variables))

  if (length(variables) == 0) {
    stop("`variables` must contain at least one column.")
  }

  required_cols <- unique(c(date_col, variables))
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(
      paste0(
        "Column(s) not in dataset: ",
        paste(missing_cols, collapse = ", ")
      )
    )
  }

  # Convert date column to Date and extract year
  df[[date_col]] <- as.Date(df[[date_col]])
  df$Year <- lubridate::year(df[[date_col]])

  # Aggregate totals by year
  yearly_totals <- stats::aggregate(
    df[variables],
    by = list(Year = df$Year),
    FUN = sum,
    na.rm = TRUE
  )

  plots <- lapply(variables, function(var) {
    y_max <- max(yearly_totals[[var]], na.rm = TRUE)
    y_top <- if (is.finite(y_max) && y_max > 0) {
      y_max * 1.10
    } else {
      1
    }

    alt_text <- paste(
      "Line chart showing annual total counts for",
      var,
      "in",
      title,
      ". Points show yearly totals and the line connects annual values over time."
    )

    p <- ggplot2::ggplot(
      yearly_totals,
      ggplot2::aes(x = .data$Year, y = .data[[var]])
    ) +
      ggplot2::geom_line(
        color = cols$primary,
        linewidth = 1.2
      ) +
      ggplot2::geom_point(
        color = cols$primary_dark,
        size = 2
      ) +
      ggplot2::labs(
        title = paste("Annual", var, "counts -", title),
        x = "Year",
        y = paste("Total", var)
      ) +
      ggplot2::scale_y_continuous(
        labels = scales::comma,
        limits = c(0, y_top),
        expand = ggplot2::expansion(mult = c(0, 0.02))
      ) +
      theme_accessible_ggplot() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5),
        plot.title = ggplot2::element_text(hjust = 0.5)
      )

    add_ggplot_alt_caption(
      plot_object = p,
      alt_text = alt_text
    )
  })

  if (length(plots) == 1) {
    final_plot <- plots[[1]]

    final_alt_text <- paste(
      "Line chart showing annual total counts for",
      variables[[1]],
      "in",
      title,
      ". Points show yearly totals and the line connects annual values over time."
    )

    plot_width <- 14
    plot_height <- 8
  } else {
    grid_dims <- get_accessible_ggplot_grid(length(plots))

    final_alt_text <- paste(
      "Multi-panel line chart showing annual total counts by year.",
      "Each panel represents one variable.",
      "Points show yearly totals and lines connect annual values over time."
    )

    final_plot <- patchwork::wrap_plots(
      plots,
      ncol = grid_dims$n_col
    ) +
      accessible_plot_annotation(
        title = paste("Annual total counts -", title),
        subtitle = "Points show yearly totals. Lines connect annual values over time.",
        alt_text = final_alt_text
      )

    plot_width <- 14
    plot_height <- max(10, 5.2 * grid_dims$n_row)
  }

  if (save_total) {
    if (is.null(output_path) || !nzchar(output_path)) {
      stop("`output_path` must be provided when `save_total = TRUE`.")
    }

    output_path <- enforce_file_extension(output_path, ".pdf")

    save_accessible_ggplot(
      plot_object = final_plot,
      output_dir = dirname(output_path),
      filename = tools::file_path_sans_ext(basename(output_path)),
      width = plot_width,
      height = plot_height,
      alt_text = final_alt_text
    )
  } else {
    return(final_plot)
  }

  invisible(final_plot)
}

