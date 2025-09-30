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
#' @export
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


#' Plot a correlation matrix include a heatmap.
#'
#' @param matrix_ The matrix to plot.
#' @param title The title for the correlation matrix.
#' @param output_path The path to output the plot to.
#'
#' @export
plot_correlation_matrix <- function(matrix_, title, output_path) {
  # validate output path
  output_path <- enforce_file_extension(output_path, ".png")
  # round correlation metrics
  matrix_ <- round(matrix_, 3)
  # draw and save correlation matrix
  png(output_path, width = 1000)
  gplots::heatmap.2(x=as.matrix(matrix_), Rowv = FALSE, Colv = FALSE, dendrogram = "none",
                    cellnote = matrix_, notecol = "black", notecex = 2, cexRow = 2, cexCol = 2,
                    trace = "none", key = FALSE, margins = c(11, 11), main = title)
  dev.off()
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
#' @export
plot_distributions <- function(
    df,
    columns,
    title,
    xlabs = NULL,
    save_hists = FALSE,
    output_path = NULL
    ) {
  # create a pdf if a save is selected
  if (save_hists == T) {
    # normalise output path
    output_path <- enforce_file_extension(output_path, ".pdf")
    pdf(output_path)
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
    oma = c(0, 0, 6, 0)
  )

  for (i in seq_along(columns)) {
    col_name <- columns[i]
    xlab <- if (!is.null(xlabs) && length(xlabs) >= i) xlabs[i] else col_name

    hist(
      df[[col_name]],
      col = "#a8bd3a",
      xlab = xlab,
      main = paste0("Distribution of '", col_name, "'"),
      breaks = 14
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
#' @export
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
#' @export
plot_moving_average <- function(
    df,
    time_col,
    value_col,
    ma_days,
    ma_sides,
    title,
    save_plot = FALSE,
    output_path = "",
    units = NULL
) {
  # Create a PDF if saving is requested
  if (save_plot == TRUE) {
    output_path <- enforce_file_extension(output_path, ".pdf")
    dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
    pdf(output_path)
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

  # Set up plot layout
  par(bg = "white", mfrow = c(1, 1), oma = c(0, 0, 4, 0))
  plot_title <- paste0("Moving Average (n=", ma_days, ", sides=", ma_sides, ")")
  line_colour <- get_alpha_colour("#00a3a6", 0.25)

  # Plot raw data
  plot(
    main = plot_title,
    x = df[[time_col]],
    y = df[[value_col]],
    type = "l",
    xlab = time_col,
    ylab = label_with_unit(value_col, units),
    ylim = range(valid_values, na.rm = TRUE),
    col = line_colour
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
    x = df[[time_col]],
    y = df[[col_name]],
    type = "l",
    col = "#00a3a6"
  )

  # add outer title
  mtext(
    title,
    outer = TRUE,
    cex = 1.6,
    line = 1,
    font = 2,
    col = "black"
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
#' @export
plot_scatter_grid <- function(
    df,
    main_col,
    comparison_cols,
    title,
    save_scatters = FALSE,
    output_path = "",
    units = NULL
) {
  all_columns <- c(main_col, comparison_cols)
  if (length(all_columns) < 2) return()

  df <- df %>% dplyr::select(all_of(all_columns))

  grid_size <- create_grid(length(comparison_cols))

  if (save_scatters) {
    output_path <- enforce_file_extension(output_path, ".pdf")
    pdf(output_path, width = 8, height = 8)
  }

  par(mfrow = grid_size, col = "white", oma = c(0, 0, 6, 0))
  point_colour <- get_alpha_colour("#27a0cc", 0.25)

  for (i in seq_along(comparison_cols)) {
    x_col <- comparison_cols[i]
    xlab <- label_with_unit(x_col, units)
    ylab <- label_with_unit(main_col, units)

    plot(
      x = df[[x_col]],
      y = df[[main_col]],
      col = point_colour,
      xlab = xlab,
      ylab = ylab,
      main = paste0(main_col, " vs ", x_col)
    )
  }

  mtext(title, outer = TRUE, cex = 1.6, line = 1, font = 2, col = "black")

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
#' @export
plot_boxplots <- function(
    df,
    columns = NULL,
    select_numeric = FALSE,
    title = "Boxplots",
    ylabs = NULL,
    save_plot = FALSE,
    output_path = NULL
) {
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
    pdf(output_path)
  }

  grid_size <- create_grid(length(selected_cols))
  par(mfrow = grid_size, oma = c(0, 0, 4, 0))

  # Plot each variable

  for (i in seq_along(selected_cols)) {
    col_name <- selected_cols[i]
    ylab <- if (!is.null(ylabs) && length(ylabs) >= i) ylabs[i] else col_name

    boxplot(df[[col_name]],
            main = paste0("Boxplot of '", col_name, "'"),
            col = "#27a0cc",
            border = "#003c57",
            outline = TRUE,
            horizontal = FALSE,
            ylab = ylab)
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
#' @export
plot_seasonal_trends <- function(
    df,
    date_col,
    outcome_cols,
    title = "Seasonal Trends",
    ylabs = NULL,
    save_plot = FALSE,
    output_path = ""
) {
  # Ensure date column is Date type
  df[[date_col]] <- as.Date(df[[date_col]])

  # Extract month
  df$month <- lubridate::month(df[[date_col]], label = TRUE)

  # Set up PDF output if needed
  if (save_plot) {
    output_path <- enforce_file_extension(output_path, ".pdf")
    pdf(output_path, width = 10, height = 6)
  }

  # Layout
  grid_size <- create_grid(length(outcome_cols))
  par(mfrow = grid_size, oma = c(0, 0, 4, 0))

  # Loop through each outcome column
  for (i in seq_along(outcome_cols)) {
    col <- outcome_cols[i]
    ylab <- if (!is.null(ylabs) && length(ylabs) >= i) ylabs[i] else paste("Average", col)

    monthly_avg <- aggregate(df[[col]], by = list(Month = df$month), FUN = mean, na.rm = TRUE)

    barplot(
      height = monthly_avg$x,
      names.arg = monthly_avg$Month,
      col = "#00a3a6",
      main = paste("Average by Month:", col),
      ylab = ylab
    )
  }

  # Overall title
  mtext(title, outer = TRUE, cex = 1.6, line = 1, font = 2, col = "black")

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
#' @export
plot_regional_trends <- function(
    df,
    region_col,
    outcome_cols,
    title = "Regional Trends",
    ylabs = NULL,
    save_plot = FALSE,
    output_path = ""
) {
  # Set up PDF output if needed
  if (save_plot) {
    output_path <- enforce_file_extension(output_path, ".pdf")
    pdf(output_path, width = 10, height = 6)
  }

  grid_size <- create_grid(length(outcome_cols))
  par(mfrow = grid_size, oma = c(0, 0, 4, 0))

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
      FUN = mean
    )

    barplot(
      height = regional_avg$x,
      names.arg = regional_avg$Region,
      col = "#206095",
      main = paste("Average by Region:", col),
      ylab = ylab,
      las = 2
    )
  }


  # Overall title
  mtext(title, outer = TRUE, cex = 1.5, line = 1, font = 2, col = "black")

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
#' @param save_rate Whether to save the plot as a PDF.
#' @param output_path The file path to save the plot if save_rate is TRUE.
#'
#' @export
plot_rate_overall <- function(
    df,
    dependent_col,
    population_col,
    date_col,
    save_rate = FALSE,
    output_path = NULL
) {
  # Clean numeric columns
  df[[population_col]] <- as.numeric(gsub(",", "", df[[population_col]]))
  df[[dependent_col]] <- as.numeric(df[[dependent_col]])

  # Extract year
  df$Year <- lubridate::year(as.Date(df[[date_col]]))

  # Aggregate
  yearly_data <- df  %>%
    dplyr::group_by(Year)  %>%
    dplyr::summarise(
      Total_Dependent = sum(.data[[dependent_col]], na.rm = TRUE),
      Total_Population = sum(.data[[population_col]], na.rm = TRUE)
    )  %>%
    dplyr::mutate(Rate_per_100k = round((Total_Dependent / Total_Population) * 100000, 3))

  # Create plot
  plot_overall_rate <- ggplot2::ggplot(yearly_data, ggplot2::aes(x = Year, y = Rate_per_100k)) +
    ggplot2::geom_line(group = 1, color = "#003c57", linewidth = 1.2) +
    ggplot2::geom_point(color = "#003c57", size = 2) +
    ggplot2::labs(
      title = paste(dependent_col, "Rate per 100,000 Population"),
      y = "Rate per 100,000", x = "Year"
    ) +
    ggplot2::expand_limits(y = 0) +

    ggplot2::theme_minimal()

  # Save or return
  if (save_rate && !is.null(output_path)) {
    output_path <- enforce_file_extension(output_path, ".pdf")
    pdf(output_path, width = 10, height = 6)
    print(plot_overall_rate)
    dev.off()
  } else {
    return(plot_overall_rate)
  }
}


#' Plot the total of selected variables per year.
#'
#' @param df A dataframe containing the data.
#' @param date_col The name of the column containing date values.
#' @param variables Column names to be summed and plotted.
#' @param save_total if TRUE, saves each plot as a PDF.
#' @param output_path The file path for saving plots.
#'
#' @return Plots are printed to the console or saved as PDF files.
#'
#' @export
plot_total_variables_by_year <- function(
    df,
    date_col,
    variables,
    save_total = FALSE,
    output_path = ""
) {
  # Convert date column to Date and extract year
  df[[date_col]] <- as.Date(df[[date_col]])
  df$Year <- lubridate::year(df[[date_col]])

  # Aggregate totals by year
  yearly_totals <- aggregate(df[variables], by = list(Year = df$Year), FUN = sum, na.rm = TRUE)

  # Set up PDF output if needed
  if (save_total) {
    output_path <- enforce_file_extension(output_path, ".pdf")
    pdf(output_path, width = 10, height = 6)
  }

  # Plot each variable
  for (var in variables) {
    p <-  ggplot2::ggplot(yearly_totals, ggplot2::aes(x = Year, y = .data[[var]])) +
      ggplot2::geom_line(color = "#27a0cc", linewidth = 1.2) +
      ggplot2::geom_point(color = "#27a0cc", size = 2) +
      ggplot2::labs(
        title = paste("Total", var, "per Year"),
        x = "Year",
        y = paste("Total", var)
      ) +
      ggplot2::expand_limits(y = 0) +
      ggplot2::theme_minimal()
    print(p)
  }

  if (save_total) {
    dev.off()
  }
}
