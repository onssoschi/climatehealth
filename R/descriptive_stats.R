# Utilities to allow for statistics to be derived from datasets

#' Check if a dataframe is empty.
#' @description 
#' Checks if a dataframe is empty, and raises an error if it is.
#' 
#' @param df Dataframe. The dataframe to check.
#' 
#' @return NULL. No return if the dataframe is not empty.
#' 
#' @export
check_empty_dataframe <- function(df) {
  if (!is.data.frame(df) || nrow(df) == 0 || ncol(df) == 0) {
    stop("Please provide a populated dataframe.")
  }
}

#' Create a correlation matrix for columns in a dataframe.
#'
#' @param df Dataframe. The dataframe to use to create a correlation matrix.
#' @param columns vector. The columns to calculate correlation between.
#' @param correlation_method string. The method to use for correlation calculations.
#'
#' @return Matrix. Correlation matrix for selected columns in the input dataset.
#'
#' @export
create_correlation_matrix <- function(
    df,
    columns = NULL,
    correlation_method = "pearson"
) {
  # check if the dataframe is populated
  check_empty_dataframe(df)
  # use all columns if columns=NULL
  if (is.null(columns)) {
    columns <- colnames(df)
  }
  # assert columns is a vector
  if (!is.vector(columns)) {
    stop("'columns' expected a vector of column names.")
  }
  # assert columns exist in the dataset
  for (col in columns) {
    if (!(col %in% colnames(df))) {
      stop(paste0("Column ", col, " not in dataset."))
    }
  }
  # assert that the chosen correlation method is valid
  correlation_method <- tolower(correlation_method)
  VALID_METHODS <- c("pearson", "kendall", "spearman")
  if (!(correlation_method %in%  VALID_METHODS)) {
    stop("Chosen correlation method is invalid. Must be one of; pearson, kendall, spearman")
  }
  # calculate correlation
  corr_df <- df %>%
    select(all_of(columns)) %>%
    cor(method = correlation_method)
  return (corr_df)
}


#' Create statistical summaries of columns in a dataframe.
#'
#' @param df Datarame. Input data.
#' @param columns vector. The columns to create summaries for.
#'
#' @return Dataframe. Column summaries
#'
#' @export
create_column_summaries <- function(df, columns = NULL) {
  # check dataframe is populated
  check_empty_dataframe(df)
  # use all columns if columns=NULL
  if (is.null(columns)) {
    columns <- colnames(df)
  }
  # assert columns is a vector
  if (!is.vector(columns)) {
    stop("'columns' expected a vector of column names.")
  }
  # assert columns exist in the dataset
  for (col in columns) {
    if (!(col %in% colnames(df))) {
      stop(paste0("Column ", col, " not in dataset."))
    }
  }
  # get summaries
  summary_list <- list()
  for (col in columns) {
    col_data <- df[[col]]
    stats <- summary(col_data)

    # Ensure all expected fields are present
    expected_fields <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")
    stats <- stats[expected_fields]
    # Calculate IQR, Variance, and SD if numeric
    if (is.numeric(col_data)) {
      iqr <- stats["3rd Qu."] - stats["1st Qu."]
      var_val <- var(col_data, na.rm = TRUE)
      sd_val <- sd(col_data, na.rm = TRUE)
    } else {
      iqr <- NA
      var_val <- NA
      sd_val <- NA
    }

    summary_df <- as.data.frame(t(c(stats,
      IQR = iqr,
      Variance = var_val,
      SD = sd_val
    )))
    final_fields <- c(
      "Min.", "1st Qu.", "Median", "Mean", "3rd Qu.",
      "Max.", "IQR", "Variance", "SD"
    )
    colnames(summary_df) <- final_fields
    summary_list[[col]] <- summary_df
  }
  # Combine all summaries into one data frame
  result <- do.call(rbind, summary_list)
  rownames(result) <- columns
  return(result)
}


#' Create a summary of all NA values in a dataset.
#'
#' @param df Dataframe. The input dataset.
#' @param columns vector. The columns to summaries NA counts for.
#'
#' @return Dataframe. A summary of NA values in the dataset.
#' @export
create_na_summary <- function(
    df,
    columns = NULL
) {
  # check if the dataframe is populated
  check_empty_dataframe(df)
  # use all columns if columns=NULL
  if (is.null(columns)) {
    columns <- colnames(df)
  }
  # assert columns is a vector
  if (!is.vector(columns)) {
    stop("'columns' expected a vector of column names.")
  }
  # assert columns exist in the dataset
  for (col in columns) {
    if (!(col %in% colnames(df))) {
      stop(paste0("Column ", col, " not in dataset."))
    }
  }
  total_count <- nrow(df)
  na_count <- sapply(columns, function(col) sum(is.na(df[[col]])))
  na_percent <- (na_count / total_count) * 100
  na_summary <- data.frame(
    column = columns,
    na_count = na_count,
    na_proportion = (round(na_percent, 2)/100)
  )
  rownames(na_summary) <- NULL
  return(na_summary)
}


#' Detect Outliers Using the IQR Method
#'
#' @param df A data frame containing the data to check for outliers.
#' @param columns A character vector of column names to check for outliers.
#'
#' @return Dataframe. Column summaries
#'
#' @export
detect_outliers <- function(df, columns = NULL) {
  if (is.null(columns)) {
    columns <- colnames(df)
  }

  outlier_flags <- data.frame(row = 1:nrow(df))

  for (col in columns) {
    if (!is.numeric(df[[col]])) next

    Q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
    Q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
    IQR_val <- Q3 - Q1
    lower_bound <- Q1 - (1.5 * IQR_val)
    upper_bound <- Q3 + (1.5 * IQR_val)
    outlier_flags[[col]] <- df[[col]] < lower_bound | df[[col]] > upper_bound
  }

  return(outlier_flags)
}


#' Append the units to a column label.
#'
#' @param col Character. The column name.
#' @param units Named Character vector. A vector of units (str) that map to
#' columns.
#'
#' @return The new column label containing units (if col in units).
#'
#' @export
label_with_unit <- function(col, units) {
  unit <- units[[col]]
  lbl_with_unit <- (
    if (!is.null(unit) && nzchar(unit)) paste0(col, " (", unit, ")") else col
  )
  return(lbl_with_unit)
}


#' Core Functionality for Producing Descriptive Statistics
#'
#' @param dataset_title Character. The title for the dataset being used.
#' @param df Dataframe. The input DataFrame.
#' @param output_path Character. The path to write outputs to.
#' @param title Character. The specific title for the subset of data being used.
#' @param aggregation_column Character. Column to aggregate data by.
#' @param population_col Character. Column containing population data.
#' @param plot_corr_matrix Logical. Whether or not to plot correlation matrix.
#' @param correlation_method Character. The correlation method. One of 'pearson', 'spearman', 'kendall'.
#' @param plot_dist Logical. Whether or not to plot distribution histograms.
#' @param dependent_col Character. The dependent column.
#' @param independent_cols Character vector. The independent columns.
#' @param columns Character vector. Specific columns to use for analysis.
#' @param units Named character vector. Units to use for plots (maps to columns parameter).
#' @param select_all_numeric Logical. Whether to select all numerical columns for plotting (columns parameter overwrites this).
#' @param plot_na_counts Logical. Whether to plot NA counts.
#' @param plot_scatter Logical. Whether to plot scatter plots.
#' @param plot_box Logical. Whether to plot box plots.
#' @param plot_seasonal Logical. Whether to plot seasonal plots.
#' @param plot_regional Logical. Whether to plot regional plots.
#' @param plot_total Logical. Whether to plot total health outcomes per year.
#' @param timeseries_col Character. Column containing timeseries data (e.g., date).
#' @param detect_outliers Logical. Whether to output a table containing outlier information.
#' @param calculate_rate Logical. Whether to calculate the rate of health outcomes per 100k people.
#'
#' @return None. Outputs are written to files.
#'
#' @export
common_descriptive_stats_core <- function(
  dataset_title,
  df,
  output_path,
  title,
  aggregation_column = NULL,
  population_col = NULL,
  plot_corr_matrix = F,
  correlation_method = "pearson",
  plot_dist = F,
  dependent_col,
  independent_cols = c(),
  columns = NULL,
  units = NULL,
  select_all_numeric = F,
  plot_na_counts = F,
  plot_scatter = F,
  plot_box = F,
  plot_seasonal = F,
  plot_regional = F,
  plot_total = F,
  timeseries_col = "date",
  detect_outliers = F,
  calculate_rate = F
) {
  raise_if_null("dependent_col", dependent_col)

  # get dataframe summary
  full_summary <- create_column_summaries(df, columns)

  # Determine selected columns
  if (!is.null(columns)) {
    selected_cols <- columns
  } else if (select_all_numeric) {
    selected_cols <- names(df)[sapply(df, is.numeric)]
  } else {
    stop("Please specify `columns` or set `select_all_numeric = TRUE`.")
  }
  selected_cols <- unique(c(selected_cols, independent_cols, dependent_col))

  # plot box plots
  if (plot_box==T) {
    boxplot_title <- paste0("Boxplots for the ", stringr::str_to_title(dataset_title), " Dataset \n(", title, ")")
    boxplot_path <- file.path(output_path, "boxplots.pdf")

    ylabs <- sapply(columns, function(col) {
      unit <- units[[col]]
      unit <- if (!is.null(unit) && nzchar(unit)) paste0(col, " (", unit, ")") else col
      return(unit)
    })

    plot_boxplots(
      df,
      selected_cols,
      ylabs = ylabs,
      boxplot_title,
      save_plot = TRUE,
      output_path = boxplot_path
      )
  }

  # plot correlation matrix
  if (plot_corr_matrix) {
    full_corr <- create_correlation_matrix(df, columns, correlation_method)
    corr_path <- file.path(output_path, "correlation_matrix.png")
    plot_correlation_matrix(
      full_corr,
      paste0(
      "Correlation Matrix for the ",
      stringr::str_to_title(dataset_title),
      " Dataset \n(",
      title,
      ", Method: ",
      stringr::str_to_title(correlation_method),
      ")"),
      corr_path
    )
  }

  # Column distributions
  if (plot_dist==T) {
    dist_path <- file.path(output_path, "column_distributions.pdf")

    xlabs <- sapply(columns, function(col) {
      unit <- units[[col]]
      unit <- if (!is.null(unit) && nzchar(unit)) paste0(col, " (", unit, ")") else col
      return(unit)
    })

    plot_distributions(
      df,
      columns,
      xlabs = xlabs,
      paste0("Column Distributions for the ", stringr::str_to_title(dataset_title), " dataset \n(", title, ")"),
      T,
      dist_path
    )
  }
  # Count NAs and visualise
  na_counts_path <- file.path(output_path, "na_counts.pdf")
  if (plot_na_counts==T) {
    na_summary <- create_na_summary(df)
    pdf(na_counts_path)
    par(mar = c(8, 4, 4, 4) + 0.1)

    bar_midpoints <- barplot(
      height = na_summary$na_count,
      names.arg = na_summary$column,
      las = 2,
      col = "#003c57",
      ylab = "NA Count",
      main = paste0("NA Counts for Columns in the ", stringr::str_to_title(dataset_title), " Dataset \n(", title, ")")
    )

    par(new = TRUE)
    plot(
      x = bar_midpoints,
      y = na_summary$na_percent,
      type = "b",
      axes = FALSE,
      xlab = "",
      ylab = "",
      col = "#a8bd3a",
      pch = 16
    )
    axis(side = 4, col = "#a8bd3a", col.axis = "#a8bd3a")
    mtext("NA Percentage", side = 4, line = 3, col = "#a8bd3a")

    dev.off()
  }


  # Dependent vs independent variables
  scatter_path <- file.path(output_path, "dependent_vs_independents.pdf")
  if (plot_scatter==T) {
    plot_scatter_grid(
      df,
      dependent_col,
      independent_cols,
      units = units,
      paste0("Dependent vs Independent Column(s) \n(", title, ")"),
      T,
      scatter_path
    )
  }
  # Seasonal tends
  if (plot_seasonal && !is.null(timeseries_col)) {
    seasonal_path <- file.path(output_path, "seasonal_trends.pdf")

    # Create y-axis labels with units
    ylabs <- sapply(columns, function(col) {
      unit <- units[[col]]
      unit <- if (!is.null(unit) && nzchar(unit)) paste0(col, " (", unit, ")") else col
      return(unit)
    })

    plot_seasonal_trends(
      df = df,
      date_col = timeseries_col,
      outcome_col = columns,
      ylabs = ylabs,
      title = paste("Seasonal Trends for", dataset_title),
      save_plot = TRUE,
      output_path = seasonal_path
    )
  }
  # Plot regional trends
  if (plot_regional && !is.null(timeseries_col)) {
    for (col in columns) {
      unit <- units[[col]]

      regional_path <- file.path(output_path, paste0("regional_trends.pdf"))

      # Create y-axis labels with units
      ylabs <- sapply(columns, function(col) {
        unit <- units[[col]]
        unit <- if (!is.null(unit) && nzchar(unit)) paste0(col, " (", unit, ")") else col
        return(unit)
      })

      plot_regional_trends(
        df = df,
        region_col = aggregation_column,
        outcome_col = columns,
        title = paste("Regional Trends for", dataset_title),
        ylab = ylabs,
        save_plot = TRUE,
        output_path = regional_path
      )
    }
  }
  # Outlier table
  if (detect_outliers == TRUE) {
    outlier_columns <- setdiff(
      selected_cols[sapply(df[selected_cols], is.numeric)], dependent_col
    )
    outlier_flags <- detect_outliers(df, outlier_columns)

    # Add outlier flags to df
    for (col in outlier_columns) {
      df[[paste0("is_outlier_", col)]] <- outlier_flags[[col]]
    }

    # Identify rows with any outlier
    outlier_flag_cols <- paste0("is_outlier_", outlier_columns)
    outlier_flag_cols <- outlier_flag_cols[outlier_flag_cols %in% names(df)]

    if (length(outlier_flag_cols) > 0) {
      outlier_rows <- df[apply(df[, outlier_flag_cols, drop = FALSE], 1, any), ]
    } else {
      warning("No valid outlier flag columns found in the dataframe.")
      outlier_rows <- df[FALSE, ]  # empty dataframe with same structure
    }

    # Always include timeseries_col and region
    base_cols <- c()
    if (!is.null(timeseries_col) && timeseries_col %in% names(df)) {
      base_cols <- c(base_cols, timeseries_col)
    }
    if (!is.null(aggregation_column) && aggregation_column %in% names(df)) {
      base_cols <- c(base_cols, aggregation_column)
    }
    selected_cols <- unique(c(base_cols, outlier_columns, outlier_flag_cols))
    selected_cols <- intersect(selected_cols, names(outlier_rows))  # Ensure they exist

    # Create and save the outlier table
    outlier_table <- outlier_rows[, selected_cols, drop = FALSE]
    write.csv(outlier_table, file.path(output_path, "outlier_table.csv"), row.names = FALSE)
  }

  # Rate based metrics
  if (calculate_rate == TRUE) {
    rate_path <- file.path(output_path, "rate_health_outcome.pdf")
    plot_rate_overall(
      df = df,
      dependent_col = dependent_col,
      population_col = population_col,
      date_col = timeseries_col,
      save_rate = TRUE,
      output_path = rate_path
    )
  }
  # Plot total by year
  if (plot_total == TRUE) {
    total_path <- file.path(output_path, "plot_total_by_year.pdf")
    plot_total_variables_by_year(
      df = df,
      date_col = timeseries_col,
      variables = dependent_col,
      save_total = TRUE,
      output_path = total_path
    )
  }

  # Save analysis
  summary_path <- file.path(output_path, "dataset_summary.csv")
  write.csv(full_summary, summary_path)
}

#' Wrapper Function to Compute Descriptive Statistics for Heat and Cold Indicators
#'
#' @param dataset_title Character. The title for the dataset being used.
#' @param df_list List of dataframes. A list of input dataframes.
#' @param output_path Character. The path to write outputs to.
#' @param aggregation_column Character. The column to use for aggregating the dataset into smaller subsets of regions.
#' @param population_col Character. The column containing the population.
#' @param plot_corr_matrix Logical. Whether or not to plot correlation matrix.
#' @param correlation_method Character. The correlation method. One of 'pearson', 'spearman', 'kendall'.
#' @param plot_dist Logical. Whether or not to plot distribution histograms.
#' @param plot_ma Logical. Whether to plot moving averages over a timeseries.
#' @param ma_days Integer. The number of days to use for a moving average.
#' @param ma_sides Integer. The number of sides to use for a moving average (1 or 2).
#' @param ma_columns Character vector. Additional columns to plot moving average for. Dependent done by default.
#' @param timeseries_col Character. The column used as the timeseries for moving averages.
#' @param dependent_col Character. The column in the data containing the dependent variable.
#' @param independent_cols Character vector. The columns in the data containing the independent variables.
#' @param columns Character vector. Specific columns to use for analysis.
#' @param units Named character vector. A named character vector of units for each variable.
#' @param select_all_numeric Logical. Whether to select all numerical columns for plotting.
#' @param plot_na_counts Logical. Whether to plot NA counts.
#' @param plot_scatter Logical. Whether to plot scatter plots.
#' @param plot_box Logical. Whether to plot box plots.
#' @param plot_seasonal Logical. Whether to plot seasonal plots.
#' @param plot_regional Logical. Whether to plot regional plots.
#' @param plot_total Logical. Whether to plot total health outcomes per year.
#' @param detect_outliers Logical. Whether to output a table containing outlier information.
#' @param calculate_rate Logical. Whether to calculate the rate of health outcomes per 100k people.
#'
#' @return Character vector. The output directory path and summary directory name.
#'
#' @export
common_descriptive_stats <- function(
  dataset_title,
  df_list,
  output_path,
  aggregation_column = NULL,
  population_col = NULL,
  plot_corr_matrix = F,
  correlation_method = "pearson",
  plot_dist = F,
  plot_ma = F,
  ma_days = 100,
  ma_sides = 1,
  ma_columns = c(),
  timeseries_col = NULL,
  dependent_col,
  independent_cols,
  columns = NULL,
  units = NULL,
  select_all_numeric = F,
  plot_na_counts = F,
  plot_scatter = F,
  plot_box = F,
  plot_seasonal = F,
  plot_regional = F,
  plot_total = F,
  detect_outliers = F,
  calculate_rate = F
) {
  # validate output path
  check_file_exists(output_path)
  normalised_title = tolower(gsub(" ", "_", dataset_title))
  output_path <- file.path(output_path, paste0(normalised_title, "_descriptive_stats"))
  if (!check_file_exists(output_path, raise=F)) {
    dir.create(output_path)
  }
  # combine all smaller df's into one
  combined_df <- do.call(rbind, df_list)
  # Create the folder if it doesn't exist
  all_folder <- file.path(output_path, "All")
  if (!dir.exists(all_folder)) {
    dir.create(all_folder, recursive = TRUE)
  }
  # obtain desc. stats
  common_descriptive_stats_core(
    dataset_title = dataset_title,
    df = combined_df,
    output_path = all_folder,
    title = "Full Dataset",
    aggregation_column = aggregation_column,
    population_col = population_col,
    plot_corr_matrix = plot_corr_matrix,
    correlation_method = correlation_method,
    plot_dist = plot_dist,
    dependent_col = dependent_col,
    independent_cols = independent_cols,
    columns = columns,
    units = units,
    select_all_numeric = select_all_numeric,
    plot_na_counts = plot_na_counts,
    plot_scatter = plot_scatter,
    plot_box = plot_box,
    plot_seasonal = plot_seasonal,
    plot_regional = plot_regional,
    plot_total = plot_total,
    detect_outliers = detect_outliers,
    calculate_rate = calculate_rate
  )
  # Moving Average
  if (plot_ma) {
    ma_vars <- c(ma_columns, dependent_col)
    for (col_i in seq_along(ma_vars)) {
      for (i in seq_along(df_list)) {
      region_name <- names(df_list)[i]
      region_folder <- file.path(output_path, region_name)

      # Create the folder if it doesn't exist
      if (!dir.exists(region_folder)) {
        dir.create(region_folder, recursive = TRUE)
      }

      fname <- paste0(ma_vars[[col_i]], "_moving_average.pdf")
      file_path <- file.path(region_folder, fname)

      pdf(file_path)
      plot_moving_average(
        df_list[[i]],
        timeseries_col,
        ma_vars[[col_i]],
        ma_days,
        ma_sides,
        units = units,
        paste0("Moving average for the ", stringr::str_to_title(dataset_title), " Dataset \n(", region_name, ")")
      )
      dev.off()
      }
    }
  }


  # create summary, corr. and dist. for each df
  for (region_name in setdiff(names(df_list), "All")) {
    df <- df_list[[region_name]]

    # Use group_name in titles or filenames
    region_title <- paste0(dataset_title, " - ", region_name)
    region_output_path <- file.path(output_path, region_name)
    if (!dir.exists(region_output_path)) {
      dir.create(region_output_path, recursive = TRUE)
    }

    # save out statistics
    common_descriptive_stats_core(
      dataset_title = region_title,
      df = df,
      output_path = region_output_path,
      title = region_title,
      aggregation_column = aggregation_column,
      population_col = population_col,
      plot_corr_matrix = plot_corr_matrix,
      correlation_method = correlation_method,
      plot_dist = plot_dist,
      dependent_col = dependent_col,
      independent_cols = independent_cols,
      columns = columns,
      select_all_numeric = select_all_numeric,
      plot_na_counts = plot_na_counts,
      plot_scatter = plot_scatter,
      plot_box = plot_box,
      plot_seasonal = plot_seasonal,
      plot_regional = plot_regional,
      plot_total = plot_total,
      detect_outliers = detect_outliers,
      calculate_rate = calculate_rate
    )
  }
  return (c(output_path, paste0(normalised_title, "_descriptive_stats")))
}

#' Raise an Error if a Parameter's Value is NULL
#'
#' @param param_nm Character. The parameter name.
#' @param value Any. The value of the parameter.
#'
#' @return None. Stops execution if value is NULL.
#'
#' @export
raise_if_null <- function(param_nm, value) {
  if (is.null(value)) {
    stop(paste0("Unexpected NULL in ", param_nm))
  }
}

#' Create Descriptive Statistics via an API Endpoint
#'
#' @param data The dataset used for descriptive stats (as a vector).
#' @param aggregation_column Character. The column to use for aggregating the dataset into smaller subsets.
#' @param dataset_title Character. The dataset's title used for outputs.
#' @param population_col Character. The column containing the population.
#' @param dependent_col Character. The dependent column.
#' @param independent_cols Character vector. The independent columns.
#' @param columns Character vector. Specific columns to use for analysis.
#' @param units Named character vector. A named character vector of units for each variable.
#' @param select_all_numeric Logical. Whether to select all numeric columns.
#' @param plot_correlation Logical. Whether to plot a correlation matrix.
#' @param plot_dist_hists Logical. Whether to plot histograms showing column distributions.
#' @param plot_ma Logical. Whether to plot moving averages over a timeseries.
#' @param plot_na_counts Logical. Whether to plot counts of NAs in each column.
#' @param plot_scatter Logical. Whether to plot the dependent column against the independent columns.
#' @param plot_box Logical. Whether to generate box plots for selected columns.
#' @param plot_seasonal Logical. Whether to plot seasonal trends of the variables in columns.
#' @param plot_regional Logical. Whether to plot regional trends of the variables in columns.
#' @param plot_total Logical. Whether to plot the total of the dependent column per year.
#' @param correlation_method Character. The correlation method. One of 'pearson', 'spearman', 'kendall'.
#' @param ma_days Integer. The number of days to use in moving average calculations.
#' @param ma_sides Integer. The number of sides to use in moving average calculations (1 or 2).
#' @param ma_columns Character vector. Columns to plot moving average for.
#' @param timeseries_col Character. The column used as the timeseries for moving averages.
#' @param detect_outliers Logical. Whether to have a table of outliers.
#' @param calculate_rate Logical. Whether to plot a rate based metric of the dependent column per year.
#' @param output_path Character. The path to save outputs to.
#'
#' @return Character vector. The full directory path that the descriptive stats are saved to.
#'
#' @export
common_descriptive_stats_api <- function(
  data,
  aggregation_column = NULL,
  population_col = NULL,
  dataset_title,
  dependent_col,
  independent_cols,
  columns = NULL,
  units = NULL,
  select_all_numeric = T,
  plot_correlation = T,
  plot_dist_hists = T,
  plot_ma = T,
  plot_na_counts = T,
  plot_scatter = T,
  plot_box = T,
  plot_seasonal = T,
  plot_regional = T,
  plot_total = T,
  correlation_method = NULL,
  ma_days = NULL,
  ma_sides = 1,
  ma_columns = NULL,
  timeseries_col = NULL,
  detect_outliers =T,
  calculate_rate = T,
  output_path
) {
  # Parameter Checks
  if(plot_ma) {
  raise_if_null("ma_days", ma_days)
  raise_if_null("ma_sides", ma_sides)
  raise_if_null("ma_columns", ma_columns)
  raise_if_null("timeseries_col", timeseries_col)
  ma_days <- as.numeric(ma_days)
  ma_sides <- as.numeric(ma_sides)
  }
  if(plot_correlation) {
    raise_if_null("correlation_method", correlation_method)
  }

  # Convert data to the correct format
  df <- read_input_data(data)
  # Check columns
  exp_columns = c(
    dependent_col,
    independent_cols
  )
  for (col in 1:length(exp_columns)) {
  if (!(exp_columns[col] %in% colnames(df))) {
    stop(paste0("Column '", exp_columns[col], "' not in passed dataset."))
  }
  }
  # Reformat Date
  if (!is.null(timeseries_col)) {
  df <- df %>%
    dplyr::mutate(
    !!rlang::sym(timeseries_col) :=
      as.Date(!!rlang::sym(timeseries_col), tryFormats = c("%d/%m/%Y", "%Y-%m-%d"))
    )
  }
  # Full dataset under All and per region
  df_list <- list(All = df)
  # Split per region
  if (!is.null(aggregation_column)) {
  region_list <- aggregate_by_column(df, aggregation_column)
    df_list <- c(df_list, region_list)
  }

  # Create descriptive stats
  final_paths <- common_descriptive_stats(
    dataset_title = dataset_title,
    df_list = df_list,
    output_path = output_path,
    aggregation_column = aggregation_column,
    population_col = population_col,
    plot_corr_matrix = plot_correlation,
    correlation_method = correlation_method,
    plot_dist = plot_dist_hists,
    plot_ma = plot_ma,
    ma_days = ma_days,
    ma_sides = ma_sides,
    ma_columns = ma_columns,
    timeseries_col = timeseries_col,
    dependent_col = dependent_col,
    independent_cols = independent_cols,
    columns = columns,
    units = units,
    select_all_numeric = select_all_numeric,
    plot_na_counts = plot_na_counts,
    plot_scatter = plot_scatter,
    plot_box = plot_box,
    plot_seasonal = plot_seasonal,
    plot_regional = plot_regional,
    plot_total = plot_total,
    detect_outliers = detect_outliers,
    calculate_rate = calculate_rate
  )
  return (final_paths)
}
