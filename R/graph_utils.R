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


#' Plot a correlation matrix include a heatmap.
#'
#' @param matrix_ The matrix to plot.
#' @param title The title for the correlation matrix.
#' @param output_path The path to output the plot to.
#'
#' @keywords internal
plot_correlation_matrix <- function(matrix_, title, output_path) {
  # validate output path
  output_path <- enforce_file_extension(output_path, ".png")
  # round correlation metrics
  matrix_ <- round(matrix_, 3)

  # design palette (Prussian Blue/Deep water -> Smoke grey -> Dusky Rose)
  n <- 256; half <- n / 2
  cols_neg <- grDevices::colorRampPalette(c("#0A2E4D", "#296991", "#F2F2F2"))(half)  # strong blue at -1 to white at 0
  cols_pos <- grDevices::colorRampPalette(c("#F2F2F2", "#C75E70"))(half)             # white at 0 to strong red at +1
  col_scheme <- c(cols_neg, cols_pos)

  # symmetric breaks centered on 0 so white is exactly neutral
  breaks <- c(
    seq(-1, 0, length.out = half + 1),
    seq(0, 1, length.out = half + 1)[-1]
  )

  # draw and save correlation matrix
  png(filename = output_path, units = "in", width = 11.69, height = 8.27, res = 300)

  gplots::heatmap.2(
    x = as.matrix(matrix_),
    Rowv = FALSE, Colv = FALSE, dendrogram = "none",
    cellnote = matrix_,
    notecol = "#000000", notecex = 2,
    cexRow = 1.6, cexCol = 1.6,
    trace = "none", key = FALSE,
    margins = c(11, 11),
    main = title,
    col = col_scheme, breaks = breaks
  )
  grDevices::dev.off()
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
  # create a pdf if a save is selected
  if (save_hists == T) {
    # normalise output path
    output_path <- enforce_file_extension(output_path, ".pdf")
    plot_height <- max(10, length(columns)*2)
    pdf(output_path, width = 14, height = plot_height)
  }
  # normalise columns
  columns <- c(columns)
  df <- df %>% select(all_of(columns))
  # get grid size for plotting
  grid_size <- create_grid(length(columns))
  # format page and plot
  par(
    mfrow = as.numeric(grid_size),
    col = "white",
    oma = c(0, 1, 6, 0),
    cex.axis = 1.35,
    cex.lab  = 1.35,
    cex.main = 1.50
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

    hist(
      df[[col_name]],
      col = "#296991",
      xlab = xlab,
      main = paste0("Distribution of '", col_name, "'"),
      breaks = br
    )
  }
  mtext(title, outer = TRUE, cex = 1.6, line = 1, font = 2, col = "black")
  # save pdf if requested
  if (save_hists == T) {
    dev.off()
  }
}


#' Generate and RGB colour value with alpha from a hex value.
#'
#' @param hex The hex code of the colour to convert.
#' @param alpha The alpha of the converted colour (ranging from 0-1).
#'
#' @return The converted RGB colour.
#' @keywords internal
get_alpha_colour <- function(hex, alpha) {
  rgb_vals <- col2rgb(hex)
  alpha_colour <- rgb(rgb_vals[1, ] / 255, rgb_vals[2, ] / 255, rgb_vals[3, ] /
                        255, alpha = alpha)
  return(alpha_colour)
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
  # Create a PDF if saving is requested
  if (save_plot == TRUE) {
    output_path <- enforce_file_extension(output_path, ".pdf")
    dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
    pdf(output_path, width = 14, height = 8)
    par(oma = c(0, 0, 4, 0), mar = c(5, 4, 3.5, 2) + 0.1)
  }

  # Select relevant columns
  df <- df %>% dplyr::select(all_of(c(time_col, value_col)))

  # Check for valid values
  valid_values <- df[[value_col]][!is.na(df[[value_col]]) & is.finite(df[[value_col]])]
  if (length(valid_values) == 0) {
    warning(paste("No valid data to plot for", value_col))
    if (save_plot) dev.off()
    return(NULL)
  }

  # Compute y-limits that always include 0, with headroom
  vr    <- range(valid_values, na.rm = TRUE)
  y_lo0 <- min(0, vr[1])
  y_hi0 <- max(0, vr[2])
  span  <- y_hi0 - y_lo0
  if (!is.finite(span) || span == 0) span <- max(1, abs(y_hi0) + abs(y_lo0))
  y_lim <- c(y_lo0 - 0.05 * span, y_hi0 + 0.10 * span)

  line_colour <- get_alpha_colour("#296991", 0.25)

  # Plot raw data
  plot(
    x = df[[time_col]],
    y = df[[value_col]],
    type = "l",
    xlab = time_col,
    ylab = label_with_unit(value_col, units),
    ylim = range(valid_values, na.rm = TRUE),
    col = line_colour
  )

  graphics::title(
    main = paste0("Moving average for `", value_col, "`"),
    cex.main = 1.2,
    font.main = 2,
    col.main  = "black"
  )

  # create moving average column
  ma <- function(x, n, sides = 1) {
    stats::filter(x, rep(1 / n, n), sides = sides)
  }

  col_name <- paste0(
    "MA_",
    as.character(ma_days),
    "days_",
    as.character(ma_sides),
    "sides"
  )
  df[[col_name]] <- ma(df[[value_col]], ma_days, ma_sides)

  # plot moving average
  lines(
    x = df[[time_col]],
    y = df[[col_name]],
    type = "l",
    col = "#0A2E4D",
    lwd = 1.6
  )

  # Legend
  legend(
    "topleft",
    legend = c("Actual values", paste0(ma_days, "-day moving average")),
    col    = c("#296991", "#0A2E4D"),
    lwd    = c(1.3, 1.6),
    lty    = c(1, 1),
    bg     = "white",
    box.lwd = 0.8,
    cex    = 0.9
  )

  # close pdf if saving
  if (save_plot == TRUE) {
    dev.off()
  }
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
  all_columns <- c(main_col, comparison_cols)
  if (length(all_columns) < 2) {
    return()
  }

  df <- df %>% dplyr::select(all_of(all_columns))
  grid_size <- create_grid(length(comparison_cols))

  if (save_scatters) {
    output_path <- enforce_file_extension(output_path, ".pdf")
    plot_height <- max(10, length(comparison_cols)*2)
    pdf(output_path, width = 14, height = plot_height)
  }
  par(mfrow = grid_size, col = "white", oma = c(0, 1, 7, 0), xpd = NA,
      cex.axis = 1.35, cex.lab = 1.35, cex.main = 1.50)

  point_colour <- get_alpha_colour("#296991", 0.5)
  line_colour  <- "#C75E70"                          # Dusky Rose

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
      xlab = xlab,
      ylab = ylab,
      main = paste0(main_col, " vs ", x_col)
    )

    # LOESS line of best fit (span tuned for stability)
    if (length(x) >= 10) {
      df_fit <- data.frame(x = x, y = y)
      fit <- try(loess(y ~ x, data = df_fit, span = 0.7), silent = TRUE)
      if (!inherits(fit, "try-error")) {
        xs <- seq(min(x), max(x), length.out = 200)
        lines(xs, predict(fit, newdata = data.frame(x = xs)), col = line_colour, lwd = 2)
      }
    }
  }

  ## Title (outer margin, top)
  mtext(title, outer = TRUE, side = 3, line = 3, cex = 1.6, font = 2, col = "black")

  is_full   <- grepl("\\bfull dataset\\b", tolower(title))
  note_text <- if (is_full) "* 20% random sample shown to reduce file size" else " "

  # points: blue symbol, black text
  mtext("\u2022", outer = TRUE, side = 3, line = 0.9,
        adj = 0.295, col = point_colour, cex = 1.7)
  mtext("Observed data points",outer = TRUE, side = 3, line = 1.2,
        adj = 0.34, col = "black", cex = 0.90)

  # LOESS: red short line, black text
  mtext("\u2014", outer = TRUE, side = 3, line = 1.2, adj = 0.53,
        col = line_colour, cex = 1.5)
  mtext("LOESS smooth", outer = TRUE, side = 3, line = 1.2, adj = 0.59,
        col = "black", cex = 0.90)

  # Right item: note text (black), right aligned
  mtext(note_text, outer = TRUE, side = 3, line = 1.2, adj = 0.9, col = "black", cex = 0.90)

  if (save_scatters) {
    dev.off()
  }
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
    plot_height <- max(10, length(selected_cols)*2)
    pdf(output_path, width = 14, height = plot_height)
  }

  grid_size <- create_grid(length(selected_cols))
  par(mfrow = grid_size, oma = c(0, 1, 4, 0),
      cex.axis = 1.35,
      cex.lab  = 1.35,
      cex.main = 1.50)

  # Plot each variable
  for (i in seq_along(selected_cols)) {
    col_name <- selected_cols[i]
    ylab <- if (!is.null(ylabs) && length(ylabs) >= i) ylabs[i] else col_name

    boxplot(df[[col_name]],
            main = paste0(col_name),
            col = "#296991",
            border = "#003c57",
            outline = TRUE,
            horizontal = FALSE,
            ylab = ylab
    )
  }
  # Add overall title
  mtext(title, outer = TRUE, cex = 1.6, line = 1, font = 2, col = "black")

  if (save_plot) {
    dev.off()
  }
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
  # Ensure date column is Date type
  df[[date_col]] <- as.Date(df[[date_col]])

  # Extract month
  df$month <- lubridate::month(df[[date_col]], label = TRUE)

  # Layout
  grid_size <- create_grid(length(outcome_cols))

  # Set up PDF output if needed
  if (save_plot) {
    output_path <- enforce_file_extension(output_path, ".pdf")
    plot_height <- max(10, length(outcome_cols)*4)
    pdf(output_path, width = 14, height = plot_height)
  }

  # Layout: readable margins + outer title room
  par(mfrow = grid_size, oma = c(0, 1, 5, 0),
      cex.axis = 1.35,
      cex.lab  = 1.35,
      cex.main = 1.50)

  # Loop through each outcome column
  for (i in seq_along(outcome_cols)) {
    col <- outcome_cols[i]
    ylab <- if (!is.null(ylabs) && length(ylabs) >= i) ylabs[i] else paste("Average", col)

    monthly_avg <- aggregate(df[[col]], by = list(Month = df$month), FUN = mean, na.rm = TRUE)

    barplot(
      height = monthly_avg$x,
      names.arg = monthly_avg$Month,
      col = "#296991",
      main = paste(col),
      ylab = ylab
    )
  }

  # Overall title
  mtext(title, outer = TRUE, side = 3, line = 3, cex = 1.6, font = 2, col = "black")
  mtext("(Note: mean used for calculations)",outer=TRUE, side=3, line=1.4, adj=0.5, col="black", cex=0.90)

  if (save_plot) {
    dev.off()
  }
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

  # Helper to truncate labels with ellipsis
  truncate_labels <- function(x, n = 12) {
    vapply(x, function(s) {
      if (is.na(s)) return(NA_character_)
      s <- as.character(s)
      if (nchar(s) <= n) s else paste0(substr(s, 1, max(1, n - 1)), "...")
    }, character(1))
  }

  # Grid and dynamic device sizing
  grid_size <- create_grid(length(outcome_cols))

  # Set up PDF output if needed
  if (save_plot) {
    output_path <- enforce_file_extension(output_path, ".pdf")
    plot_height <- max(12, length(outcome_cols)*4)
    pdf(output_path, width = 14, height = plot_height)
  }

  # Layout: readable margins + outer title room
  par(mfrow = grid_size, oma = c(0, 1, 5, 0), mar = c(10, 4.5, 6, 1),
      cex.axis = 1.35,
      cex.lab  = 1.35,
      cex.main = 1.50)

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
      col = "#296991",
      main = paste(col),
      ylab = ylab,
      las = 2,
      ylim = c(0, y_top),
      yaxs = "i"
    )
  }
  # Overall title
  mtext(title, outer = TRUE, cex = 1.5, line = 2.2, font = 2, col = "black")
  # Note about the statistic used
  mtext("Note: bars show mean values per region",
        outer = TRUE, cex = 0.55, line = 0.95, col = "black")

  if (save_plot) {
    dev.off()
  }
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
      Total_Population = sum(.data[[population_col]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      Rate_per_100k = round((.data$Total_Dependent / .data$Total_Population) * 100000, 3)
    )
  # Dynamic y limit with headroom
  y_max <- max(yearly_data$Rate_per_100k, na.rm = TRUE)
  y_top <- if (is.finite(y_max)) y_max * 1.10 else NA

  # Create plot
  plot_overall_rate <- ggplot2::ggplot(yearly_data, ggplot2::aes(x = .data$Year, y = .data$Rate_per_100k)) +
    ggplot2::geom_line(group = 1, color = "#296991", linewidth = 1.2) +
    ggplot2::geom_point(color = "#296991", size = 2) +
    ggplot2::labs(
      title = paste0("Annual " , dependent_col, " rate per 100,000 population - ", title),
      y = "Rate per 100,000 population",
      x = "Year"
    ) +
    ggplot2::scale_y_continuous(limits = c(0, y_top),
                                expand = ggplot2::expansion(mult = c(0, 0.02))) +
    ggplot2::theme_minimal()

  # Save or return
  if (save_rate && !is.null(output_path)) {
    output_path <- enforce_file_extension(output_path, ".pdf")
    grDevices::pdf(output_path, height = 8, width = 14)
    print(plot_overall_rate)
    grDevices::dev.off()
  } else {
    return(plot_overall_rate)
  }
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
  # Convert date column to Date and extract year
  df[[date_col]] <- as.Date(df[[date_col]])
  df$Year <- lubridate::year(df[[date_col]])

  # Aggregate totals by year
  yearly_totals <- aggregate(df[variables], by = list(Year = df$Year), FUN = sum, na.rm = TRUE)

  # Set up PDF output if needed
  if (save_total) {
    output_path <- enforce_file_extension(output_path, ".pdf")
    pdf(output_path, height = 8, width = 14)
  }

  # Plot each variable
  for (var in variables) {
    y_max <- max(yearly_totals[[var]], na.rm = TRUE)
    y_top <- if (is.finite(y_max) && y_max > 0) y_max * 1.1 else 1

    p <- ggplot2::ggplot(yearly_totals, ggplot2::aes(x = .data$Year, y = .data[[var]])) +
      ggplot2::geom_line(color = "#296991", linewidth = 1.2) +
      ggplot2::geom_point(color = "#296991", size = 2) +
      ggplot2::labs(
        title = paste("Annual", var, "counts -", title),
        x = "Year",
        y = paste("Total ", var)
      ) +
      ggplot2::scale_y_continuous(limits = c(0, y_top),
                                  expand = ggplot2::expansion(mult = c(0, 0.02))) +
      ggplot2::theme_minimal()
    print(p)
  }

  if (save_total) {
    dev.off()
  }
}
