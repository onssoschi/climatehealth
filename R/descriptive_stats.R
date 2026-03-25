# Utilities to allow for statistics to be derived from datasets

#' Check if a dataframe is empty.
#' @description
#' Checks if a dataframe is empty, and raises an error if it is.
#'
#' @param df Dataframe. The dataframe to check.
#'
#' @return NULL. No return if the dataframe is not empty.
#'
#' @keywords internal
check_empty_dataframe <- function(df) {
  if (!is.data.frame(df) || nrow(df) == 0 || ncol(df) == 0) {
    stop("Please provide a populated dataframe.")
  }
}


#' Create a correlation matrix for columns in a dataframe.
#'
#' @param df Dataframe. The dataframe to use to create a correlation matrix.
#' @param independent_cols Character vector. The columns in the data containing the independent variables.
#' @param correlation_method string. The method to use for correlation calculations.
#'
#' @return Matrix. Correlation matrix for selected columns in the input dataset.
#'
#' @keywords internal
create_correlation_matrix <- function(
    df,
    independent_cols = NULL,
    correlation_method = "pearson") {
  # check if the dataframe is populated
  check_empty_dataframe(df)

  # use all columns if independent_cols=NULL
  if (is.null(independent_cols)) {
    independent_cols <- colnames(df)
  }
  # assert columns is a vector
  if (!is.vector(independent_cols)) {
    stop("'independent_cols' expected a vector of column names.")
  }
  independent_cols <- unique(as.character(independent_cols))

  # assert columns exist in the dataset
  missing_cols <- setdiff(independent_cols, colnames(df))
  if (length(missing_cols) > 0) {
    stop(
      paste0(
        "Column(s) not in dataset: ",
        paste(missing_cols, collapse = ", ")
      )
    )
  }

  # keep only numeric columns for correlation
  numeric_cols <- independent_cols[vapply(
    df[independent_cols],
    is.numeric,
    logical(1)
  )]
  if (length(numeric_cols) < 2) {
    stop(
      paste0(
        "Correlation matrix requires at least 2 numeric columns after filtering. Found ",
        length(numeric_cols),
        "."
      )
    )
  }

  # assert that the chosen correlation method is valid
  correlation_method <- tolower(correlation_method)
  VALID_METHODS <- c("pearson", "kendall", "spearman")
  if (!(correlation_method %in% VALID_METHODS)) {
    stop("Chosen correlation method is invalid. Must be one of; pearson, kendall, spearman")
  }
  # calculate correlation
  corr_df <- df %>%
    select(all_of(numeric_cols)) %>%
    cor(method = correlation_method,
        use = "pairwise.complete.obs")
  return(corr_df)
}


#' Create statistical summaries of columns in a dataframe.
#'
#' @param df Datarame. Input data.
#' @param independent_cols Character vector. The columns in the data containing the independent variables.
#'
#' @return Dataframe. Column summaries
#'
#' @keywords internal
create_column_summaries <- function(df, independent_cols = NULL) {
  # check dataframe is populated
  check_empty_dataframe(df)
  # use all columns if columns=NULL
  if (is.null(independent_cols)) {
    independent_cols <- colnames(df)
  }
  # assert independent_cols is a vector
  if (!is.vector(independent_cols)) {
    stop("'independent_cols' expected a vector of column names.")
  }
  independent_cols <- unique(as.character(independent_cols))

  final_fields <- c(
    "Data_Type", "Non_Missing", "Missing", "Unique",
    "Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.",
    "IQR", "Variance", "SD", "Top", "Top_Freq"
  )

  # explicit output for empty column selections
  if (length(independent_cols) == 0) {
    empty_result <- data.frame(matrix(nrow = 0, ncol = length(final_fields)))
    colnames(empty_result) <- final_fields
    return(empty_result)
  }

  # assert columns exist in the dataset
  missing_cols <- setdiff(independent_cols, colnames(df))
  if (length(missing_cols) > 0) {
    stop(
      paste0(
        "Column(s) not in dataset: ",
        paste(missing_cols, collapse = ", ")
      )
    )
  }

  # get type-aware summaries
  summary_list <- list()
  for (col in independent_cols) {
    col_data <- df[[col]]
    non_missing <- sum(!is.na(col_data))
    missing <- sum(is.na(col_data))
    unique_vals <- length(unique(col_data[!is.na(col_data)]))

    min_val <- NA_real_
    q1_val <- NA_real_
    median_val <- NA_real_
    mean_val <- NA_real_
    q3_val <- NA_real_
    max_val <- NA_real_
    iqr_val <- NA_real_
    var_val <- NA_real_
    sd_val <- NA_real_
    top_val <- NA_character_
    top_freq <- NA_integer_

    if (is.numeric(col_data)) {
      if (non_missing > 0) {
        q_vals <- as.numeric(stats::quantile(
          col_data,
          probs = c(0, 0.25, 0.5, 0.75, 1),
          na.rm = TRUE,
          names = FALSE
        ))
        min_val <- q_vals[1]
        q1_val <- q_vals[2]
        median_val <- q_vals[3]
        mean_val <- mean(col_data, na.rm = TRUE)
        q3_val <- q_vals[4]
        max_val <- q_vals[5]
        iqr_val <- stats::IQR(col_data, na.rm = TRUE)
        var_val <- stats::var(col_data, na.rm = TRUE)
        sd_val <- stats::sd(col_data, na.rm = TRUE)
      }
    } else {
      if (non_missing > 0) {
        mode_counts <- sort(table(as.character(col_data[!is.na(col_data)])), decreasing = TRUE)
        top_val <- names(mode_counts)[1]
        top_freq <- as.integer(mode_counts[1])
      }
    }

    summary_list[[col]] <- data.frame(
      Data_Type = class(col_data)[1],
      Non_Missing = non_missing,
      Missing = missing,
      Unique = unique_vals,
      `Min.` = min_val,
      `1st Qu.` = q1_val,
      Median = median_val,
      Mean = mean_val,
      `3rd Qu.` = q3_val,
      `Max.` = max_val,
      IQR = iqr_val,
      Variance = var_val,
      SD = sd_val,
      Top = top_val,
      Top_Freq = top_freq,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }

  # Combine all summaries into one data frame
  result <- do.call(rbind, summary_list)
  rownames(result) <- independent_cols
  return(result)
}


#' Create a summary of all NA values in a dataset.
#'
#' @param df Dataframe. The input dataset.
#' @param independent_cols Character vector. The columns in the data containing the independent variables.
#'
#' @return Dataframe. A summary of NA values in the dataset.
#' @keywords internal
create_na_summary <- function(
    df,
    independent_cols = NULL) {
  # check if the dataframe is populated
  check_empty_dataframe(df)
  # use all columns if columns=NULL
  if (is.null(independent_cols)) {
    independent_cols <- colnames(df)
  }
  # assert columns is a vector
  if (!is.vector(independent_cols)) {
    stop("'independent_cols' expected a vector of column names.")
  }
  # assert columns exist in the dataset
  for (col in independent_cols) {
    if (!(col %in% colnames(df))) {
      stop(paste0("Column ", col, " not in dataset."))
    }
  }
  total_count <- nrow(df)
  na_count <- sapply(independent_cols, function(col) sum(is.na(df[[col]])))
  na_percent <- (na_count / total_count) * 100
  na_summary <- data.frame(
    column = independent_cols,
    na_count = na_count,
    na_percent = round(na_percent, 2)
  )
  rownames(na_summary) <- NULL
  return(na_summary)
}


#' Detect Outliers Using the IQR Method
#'
#' @param df A data frame containing the data to check for outliers.
#' @param independent_cols Character vector. The columns in the data containing the independent variables.
#'
#' @return Dataframe. Column summaries
#'
#' @keywords internal
detect_outliers <- function(df, independent_cols = NULL) {
  if (is.null(independent_cols)) {
    independent_cols <- colnames(df)
  }

  outlier_flags <- data.frame(row = 1:nrow(df))
  for (col in independent_cols) {
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
#' @keywords internal
label_with_unit <- function(col, units) {
  unit <- NULL
  if (!is.null(units) && !is.null(names(units)) && col %in% names(units)) {
    unit <- units[[col]]
  }
  lbl_with_unit <- if (!is.null(unit) && nzchar(unit)) {
    paste0(col, " (", unit, ")")
  } else {
    col
  }
  return(lbl_with_unit)
}


#' Core Functionality for Producing Descriptive Statistics
#'
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
#' @param units Named character vector. Units to use for plots (maps to columns parameter).
#' @param plot_na_counts Logical. Whether to plot NA counts.
#' @param plot_scatter Logical. Whether to plot scatter plots.
#' @param plot_box Logical. Whether to plot box plots.
#' @param plot_seasonal Logical. Whether to plot seasonal plots.
#' @param plot_regional Logical. Whether to plot regional plots.
#' @param plot_total Logical. Whether to plot total health outcomes per year.
#' @param timeseries_col Character. Column containing timeseries data (e.g., date).
#' @param write_outlier_table Logical. Whether to output a table containing outlier information.
#' @param calculate_rate Logical. Whether to calculate the rate of health outcomes per 100k people.
#'
#' @return None. Outputs are written to files.
#'
#' @keywords internal
descriptive_stats_core <- function(
    df,
    output_path,
    title,
    aggregation_column = NULL,
    population_col = NULL,
    plot_corr_matrix = FALSE,
    correlation_method = "pearson",
    plot_dist = FALSE,
    dependent_col,
    independent_cols = c(),
    units = NULL,
    plot_na_counts = FALSE,
    plot_scatter = FALSE,
    plot_box = FALSE,
    plot_seasonal = FALSE,
    plot_regional = FALSE,
    plot_total = FALSE,
    timeseries_col = "date",
    write_outlier_table = FALSE,
    calculate_rate = FALSE) {
  check_empty_dataframe(df)
  raise_if_null("dependent_col", dependent_col)
  independent_cols <- unique(as.character(independent_cols))
  validate_descriptive_columns(
    df = df,
    context = paste0("'", title, "'"),
    dependent_col = dependent_col,
    independent_cols = independent_cols,
    aggregation_column = aggregation_column,
    population_col = population_col,
    timeseries_col = timeseries_col,
    plot_corr_matrix = plot_corr_matrix,
    plot_dist = plot_dist,
    plot_ma = FALSE,
    plot_scatter = plot_scatter,
    plot_box = plot_box,
    plot_seasonal = plot_seasonal,
    plot_regional = plot_regional,
    plot_total = plot_total,
    write_outlier_table = write_outlier_table,
    calculate_rate = calculate_rate,
    is_full_dataset = grepl("\\bfull dataset\\b", tolower(title))
  )
  dir.create(output_path, recursive = TRUE, showWarnings = FALSE)

  # get dataframe summary
  full_summary <- create_column_summaries(df, independent_cols)

  # Determine selected columns
  if (!is.null(independent_cols)) {
    selected_cols <- independent_cols
  } else {
    stop("Please specify `independent_cols`")
  }
  selected_cols <- unique(c(independent_cols, dependent_col))

  # plot box plots
  if (plot_box == TRUE) {
    boxplot_title <- paste0("Boxplots - ", title)
    boxplot_path <- file.path(output_path, "boxplots.pdf")

    ylabs <- sapply(independent_cols, function(col) {
      label_with_unit(col, units)
    })

    plot_boxplots(
      df = df,
      columns = selected_cols,
      select_numeric = FALSE,
      ylabs = ylabs,
      title = boxplot_title,
      save_plot = TRUE,
      output_path = boxplot_path
    )
  }

  # plot correlation matrix
  if (plot_corr_matrix == TRUE) {
    full_corr <- create_correlation_matrix(df, independent_cols, correlation_method)
    corr_path <- file.path(output_path, "correlation_matrix.png")
    plot_correlation_matrix(
      matrix_ = full_corr,
      title = paste0(
        "Correlation Matrix, Method: ",
        stringr::str_to_title(correlation_method),
        "\n",
        title
      ),
      output_path = corr_path
    )
  }

  # Column distributions
  if (plot_dist == TRUE) {
    dist_path <- file.path(output_path, "histograms.pdf")

    xlabs <- sapply(independent_cols, function(col) {
      label_with_unit(col, units)
    })

    plot_distributions(
      df = df,
      columns = independent_cols,
      title = paste0("Histograms - ", title),
      xlabs = xlabs,
      save_hists = TRUE,
      output_path = dist_path
    )
  }
  # Count NAs and visualise
  na_counts_path <- file.path(output_path, "na_counts.pdf")
  if (plot_na_counts == TRUE) {
    na_summary <- create_na_summary(df)
    with_pdf_device(
      output_path = na_counts_path,
      width = 14,
      height = 8,
      context = paste0("NA counts plot for ", title),
      plot_fn = function() {
        par(mar = c(8, 4, 4, 4) + 0.1)

        # left axis: counts - force a sensible upper bound even if all zeros
        y_max <- max(na_summary$na_count, na.rm = TRUE)

        ylim <- c(0, max(10, y_max))
        y_ticks <- pretty(ylim, n = 11)
        ylim <- c(0, max(y_ticks))

        bar_midpoints <- barplot(
          height = na_summary$na_count,
          names.arg = na_summary$column,
          las = 2,
          col = "#296991",
          ylab = "NA Count",
          main = paste0("NA counts - ", title),
          ylim = ylim,
          yaxs = "i",
          yaxt = "n"
        )
        axis(side = 2,
             at = y_ticks)

        pmax <- max(na_summary$na_percent, na.rm = TRUE)
        lim_hi <- if (is.finite(pmax) && pmax <= 2) 2 else 100
        epsilon <- lim_hi * 0.02
        ticks <- if (lim_hi == 100) seq(0, 100, by = 10) else seq(0, lim_hi, length.out = 11)
        par(new = TRUE)
        plot(
          x = bar_midpoints,
          y = na_summary$na_percent + epsilon,
          type = "b",
          axes = FALSE,
          xlab = "",
          ylab = "",
          col = "#C75E70",
          pch = 16,
          ylim = c(0, lim_hi),
          yaxs = "i"
        )
        axis(side = 4,
             at = ticks,
             labels = ticks)
        mtext("NA Percent", side = 4, line = 3, col = "black")

        legend("topright",
               inset = 0.02,
               legend = c("NA count", "NA percent"),
               col = c("#296991", "#C75E70"),
               pch = c(15, 16),
               lty = c(NA, 1),
               pt.cex = 1)
      }
    )
  }

  # Dependent vs independent variables
  scatter_path <- file.path(output_path, "dependent_vs_independents.pdf")
  if (plot_scatter == TRUE) {
    plot_scatter_grid(
      df = df,
      main_col = dependent_col,
      comparison_cols = independent_cols,
      title = paste0("Dependent vs Independent Column(s) - ", title),
      save_scatters = TRUE,
      output_path = scatter_path,
      units = units
    )
  }
  # Seasonal trends
  if (plot_seasonal == TRUE && !is.null(timeseries_col)) {
    seasonal_path <- file.path(output_path, "monthly_averages.pdf")

    # Create y-axis labels with units
    ylabs <- sapply(independent_cols, function(col) {
      label_with_unit(col, units)
    })

    plot_seasonal_trends(
      df = df,
      date_col = timeseries_col,
      outcome_cols = independent_cols,
      ylabs = ylabs,
      title = paste("Monthly Average for", title),
      save_plot = TRUE,
      output_path = seasonal_path
    )
  }
  # Plot regional trends
  if (plot_regional == TRUE && !is.null(timeseries_col)) {
    # only produce on the full dataset and when regional disaggregation exists
    has_regions <- aggregation_column %in% names(df) && length(unique(stats::na.omit(df[[aggregation_column]]))) > 1
    is_full <- grepl("\\bfull dataset\\b", tolower(title))
    if (has_regions && is_full) {
      # y-axis labels with units
      ylabs <- vapply(independent_cols, function(col) {
        label_with_unit(col, units)
      }, character(1))

      regional_path <- file.path(output_path, "regional_averages.pdf")

      plot_regional_trends(
        df = df,
        region_col = aggregation_column,
        outcome_cols = independent_cols,
        title = paste("Regional Averages - ", title),
        ylabs = ylabs,
        save_plot = TRUE,
        output_path = regional_path
      )
    }
  }

  # Outlier table
  if (write_outlier_table == TRUE) {
    outlier_columns <- setdiff(
      selected_cols[sapply(df[selected_cols], is.numeric)],
      dependent_col
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
      outlier_rows <- df[apply(df[, outlier_flag_cols, drop = FALSE], 1, any, na.rm = TRUE), ]
    } else {
      warning("No valid outlier flag columns found in the dataframe.")
      outlier_rows <- df[FALSE, ] # empty dataframe with same structure
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
    selected_cols <- intersect(selected_cols, names(outlier_rows))

    # Create and save the outlier table
    outlier_table <- outlier_rows[, selected_cols, drop = FALSE]
    write.csv(outlier_table, file.path(output_path, "outlier_table.csv"), row.names = FALSE)
  }

  # Rate based metrics
  if (calculate_rate == TRUE) {
    rate_path <- file.path(output_path, "annual_rate_health_outcome_per_100k.pdf")
    plot_rate_overall(
      df = df,
      dependent_col = dependent_col,
      population_col = population_col,
      date_col = timeseries_col,
      title = title,
      save_rate = TRUE,
      output_path = rate_path
    )
  }
  # Plot total by year
  if (plot_total == TRUE) {
    total_path <- file.path(output_path, "annual_total_counts.pdf")
    plot_total_variables_by_year(
      df = df,
      date_col = timeseries_col,
      variables = dependent_col,
      title = title,
      save_total = TRUE,
      output_path = total_path
    )
  }

  # Save analysis
  summary_path <- file.path(output_path, "dataset_summary.csv")
  write.csv(full_summary, summary_path)
}

#' Raise an Error if a Parameter's Value is NULL
#'
#' @param param_nm Character. The parameter name.
#' @param value Any. The value of the parameter.
#'
#' @return None. Stops execution if value is NULL.
#'
#' @keywords internal
raise_if_null <- function(param_nm, value) {
  if (is.null(value)) {
    stop(paste0("Unexpected NULL in ", param_nm))
  }
}

#' Run plotting code inside a safely managed PDF device.
#'
#' @param output_path Character. Output path for the PDF file.
#' @param width Numeric. PDF width in inches.
#' @param height Numeric. PDF height in inches.
#' @param context Character. Context label used in error messages.
#' @param plot_fn Function. Plotting function to execute.
#'
#' @return None. Writes a PDF and closes device safely.
#'
#' @keywords internal
with_pdf_device <- function(
    output_path,
    width = 14,
    height = 8,
    context = "plot",
    plot_fn) {
  if (!is.function(plot_fn)) {
    stop("`plot_fn` must be a function.")
  }

  output_path <- enforce_file_extension(output_path, ".pdf")
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  grDevices::pdf(output_path, width = width, height = height)
  on.exit(grDevices::dev.off(), add = TRUE)

  tryCatch(
    {
      plot_fn()
    },
    error = function(e) {
      stop(
        paste0("Failed while creating ", context, ": ", e$message),
        call. = FALSE
      )
    }
  )
}

#' Emit a consistent deprecation warning for descriptive stats wrappers.
#'
#' @param old_fn Character. Deprecated function name.
#' @param new_fn Character. Replacement function name.
#'
#' @return None. Emits a warning.
#'
#' @keywords internal
deprecate_descriptive_stats <- function(old_fn, new_fn) {
  if (requireNamespace("lifecycle", quietly = TRUE)) {
    lifecycle::deprecate_warn(
      when = "next-release",
      what = paste0("climatehealth::", old_fn, "()"),
      with = paste0("climatehealth::", new_fn, "()")
    )
  } else {
    warning(
      paste0("`", old_fn, "()` is deprecated; use `", new_fn, "()` instead."),
      call. = FALSE
    )
  }
}

#' Generate a run id for descriptive statistics output folders.
#'
#' @return Character. Run id in the format YYYYmmdd_HHMMSS_NNNN.
#'
#' @keywords internal
generate_descriptive_stats_run_id <- function() {
  paste0(
    format(Sys.time(), "%Y%m%d_%H%M%S"),
    "_",
    sprintf("%04d", sample.int(9999, 1))
  )
}

#' Validate and prepare base output directory for descriptive stats.
#'
#' @param output_path Character. Base output path.
#' @param create_base_dir Logical. Whether to create a missing base directory.
#'
#' @return Character. Validated output path.
#'
#' @keywords internal
prepare_descriptive_output_dir <- function(output_path, create_base_dir = FALSE) {
  if (!is.character(output_path) || length(output_path) != 1 || !nzchar(output_path)) {
    stop("`output_path` must be a single non-empty character string.")
  }

  exists <- check_file_exists(output_path, raise = FALSE)
  if (!exists && create_base_dir == TRUE) {
    dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
    exists <- check_file_exists(output_path, raise = FALSE)
  }

  if (!exists) {
    stop(
      paste0(
        "No file or directory was found at `output_path`: ", output_path,
        ". Set `create_base_dir = TRUE` to create it."
      )
    )
  }
  if (!dir.exists(output_path)) {
    stop("`output_path` exists but is not a directory.")
  }
  return(output_path)
}

#' Normalise descriptive stats data input to combined and regional dataframes.
#'
#' @param data Dataframe or list of dataframes.
#' @param aggregation_column Character. Region column for splitting dataframes.
#' @param timeseries_col Character. Date column for timeseries analysis.
#'
#' @return A list with `combined_df` and `region_df_list`.
#'
#' @keywords internal
prepare_descriptive_input <- function(data, aggregation_column = NULL, timeseries_col = NULL) {
  if (is.data.frame(data)) {
    combined_df <- data
    region_df_list <- list()

    if (!is.null(aggregation_column)) {
      if (!(aggregation_column %in% names(data))) {
        stop(paste0("Column '", aggregation_column, "' not in passed dataset."))
      }
      region_df_list <- aggregate_by_column(data, aggregation_column)
    }
  } else if (is.list(data)) {
    if (length(data) == 0) {
      stop("`data` list cannot be empty.")
    }
    if (!all(vapply(data, is.data.frame, logical(1)))) {
      stop("`data` list must contain only dataframes.")
    }

    if (is.null(names(data))) {
      names(data) <- rep("", length(data))
    }
    empty_names <- names(data) == ""
    names(data)[empty_names] <- paste0("Region_", which(empty_names))

    if ("All" %in% names(data)) {
      combined_df <- data[["All"]]
      region_df_list <- data[setdiff(names(data), "All")]
    } else {
      combined_df <- do.call(rbind, data)
      region_df_list <- data
    }
  } else {
    stop("`data` must be either a dataframe or a named list of dataframes.")
  }

  if (!is.null(timeseries_col)) {
    if (timeseries_col %in% names(combined_df)) {
      combined_df <- combined_df %>%
        dplyr::mutate(
          !!rlang::sym(timeseries_col) :=
            as.Date(
              !!rlang::sym(timeseries_col),
              tryFormats = c("%d/%m/%Y", "%Y-%m-%d")
            )
        )
    }
    if (length(region_df_list)) {
      for (nm in names(region_df_list)) {
        df_i <- region_df_list[[nm]]
        if (timeseries_col %in% names(df_i)) {
          region_df_list[[nm]] <- df_i %>%
            dplyr::mutate(
              !!rlang::sym(timeseries_col) :=
                as.Date(
                  !!rlang::sym(timeseries_col),
                  tryFormats = c("%d/%m/%Y", "%Y-%m-%d")
                )
            )
        }
      }
    }
  }

  return(list(combined_df = combined_df, region_df_list = region_df_list))
}

#' Preflight validation for descriptive statistics columns based on enabled features.
#'
#' @param df Dataframe. Dataset to validate.
#' @param context Character. Context label for error messages.
#' @param dependent_col Character. Dependent column.
#' @param independent_cols Character vector. Independent columns.
#' @param aggregation_column Character. Region aggregation column.
#' @param population_col Character. Population column.
#' @param timeseries_col Character. Timeseries column.
#' @param plot_corr_matrix Logical. Correlation matrix toggle.
#' @param plot_dist Logical. Distribution plot toggle.
#' @param plot_ma Logical. Moving average toggle.
#' @param plot_scatter Logical. Scatter plot toggle.
#' @param plot_box Logical. Boxplot toggle.
#' @param plot_seasonal Logical. Seasonal plot toggle.
#' @param plot_regional Logical. Regional plot toggle.
#' @param plot_total Logical. Total-by-year plot toggle.
#' @param write_outlier_table Logical. Outlier table toggle.
#' @param calculate_rate Logical. Rate plot toggle.
#' @param is_full_dataset Logical. Whether this dataset is the full combined dataset.
#'
#' @return None. Stops execution if required columns/params are missing.
#'
#' @keywords internal
validate_descriptive_columns <- function(
    df,
    context = "dataset",
    dependent_col,
    independent_cols,
    aggregation_column = NULL,
    population_col = NULL,
    timeseries_col = NULL,
    plot_corr_matrix = FALSE,
    plot_dist = FALSE,
    plot_ma = FALSE,
    plot_scatter = FALSE,
    plot_box = FALSE,
    plot_seasonal = FALSE,
    plot_regional = FALSE,
    plot_total = FALSE,
    write_outlier_table = FALSE,
    calculate_rate = FALSE,
    is_full_dataset = FALSE) {
  check_empty_dataframe(df)

  if (!is.character(dependent_col) || length(dependent_col) != 1 || !nzchar(dependent_col)) {
    stop("`dependent_col` must be a single non-empty character string.")
  }

  if (!is.vector(independent_cols) || length(independent_cols) == 0) {
    stop("`independent_cols` must be a non-empty character vector.")
  }
  independent_cols <- unique(as.character(independent_cols))
  if (any(!nzchar(independent_cols))) {
    stop("`independent_cols` contains empty column names.")
  }

  feature_flags <- c(
    plot_corr_matrix,
    plot_dist,
    plot_ma,
    plot_scatter,
    plot_box,
    plot_seasonal,
    plot_regional,
    plot_total,
    write_outlier_table,
    calculate_rate
  )
  if (any(!is.logical(feature_flags) | is.na(feature_flags))) {
    stop("Feature toggles must be non-NA logical values.")
  }

  missing_params <- c()
  needs_time_col <- (plot_ma || plot_seasonal || plot_total || calculate_rate)
  if (needs_time_col && (is.null(timeseries_col) || !is.character(timeseries_col) ||
      length(timeseries_col) != 1 || !nzchar(timeseries_col))) {
    missing_params <- c(missing_params, "`timeseries_col` is required for enabled time-based features.")
  }
  if (calculate_rate && (is.null(population_col) || !is.character(population_col) ||
      length(population_col) != 1 || !nzchar(population_col))) {
    missing_params <- c(missing_params, "`population_col` is required when `calculate_rate=TRUE`.")
  }
  if (plot_regional && is_full_dataset && (is.null(aggregation_column) ||
      !is.character(aggregation_column) || length(aggregation_column) != 1 ||
      !nzchar(aggregation_column))) {
    missing_params <- c(missing_params, "`aggregation_column` is required when `plot_regional=TRUE` on the full dataset.")
  }
  if (length(missing_params) > 0) {
    stop(
      paste0(
        "Preflight validation failed for ",
        context,
        ": ",
        paste(missing_params, collapse = " ")
      )
    )
  }

  required_cols <- unique(c(dependent_col, independent_cols))
  if (needs_time_col) {
    required_cols <- c(required_cols, timeseries_col)
  }
  if (calculate_rate) {
    required_cols <- c(required_cols, population_col)
  }
  if (plot_regional && is_full_dataset && !is.null(aggregation_column)) {
    required_cols <- c(required_cols, aggregation_column)
  }
  required_cols <- unique(required_cols)

  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(
      paste0(
        "Preflight validation failed for ",
        context,
        ". Missing required column(s): ",
        paste(missing_cols, collapse = ", "),
        "."
      )
    )
  }

}

#' Run generic descriptive statistics and EDA outputs for indicator datasets.
#'
#' @param data Dataframe or named list of dataframes. If a dataframe is provided and `aggregation_column`
#'   is passed, data are split by that column.
#' @param output_path Character. Base output directory.
#' @param aggregation_column Character. Column used to aggregate/split data by region.
#' @param population_col Character. The column containing population data.
#' @param plot_corr_matrix Logical. Whether to plot correlation matrix.
#' @param correlation_method Character. Correlation method. One of 'pearson', 'spearman', 'kendall'.
#' @param plot_dist Logical. Whether to plot distribution histograms.
#' @param plot_ma Logical. Whether to plot moving averages over a timeseries.
#' @param ma_days Integer. Number of days to use for moving average.
#' @param ma_sides Integer. Sides to use for moving average (1 or 2).
#' @param timeseries_col Character. Timeseries column used for moving averages and time-based plots.
#' @param dependent_col Character. Dependent variable column.
#' @param independent_cols Character vector. Independent variable columns.
#' @param units Named character vector. Units for variables.
#' @param plot_na_counts Logical. Whether to plot NA counts.
#' @param plot_scatter Logical. Whether to plot scatter plots.
#' @param plot_box Logical. Whether to plot box plots.
#' @param plot_seasonal Logical. Whether to plot seasonal trends.
#' @param plot_regional Logical. Whether to plot regional trends.
#' @param plot_total Logical. Whether to plot total health outcomes by year.
#' @param detect_outliers Logical. Whether to output an outlier table.
#' @param calculate_rate Logical. Whether to plot annual rates per 100k.
#' @param run_id Character. Optional run id. If `NULL`, a timestamped id is generated.
#' @param create_base_dir Logical. Whether to create `output_path` if missing.
#'
#' @return A list with `base_output_path`, `run_id`, `run_output_path`, and `region_output_paths`.
#'
#' @examples
#' \donttest{
#' df <- data.frame(
#'   date = as.Date("2024-01-01") + 0:29,
#'   region = rep(c("A", "B"), each = 15),
#'   outcome = sample(1:20, 30, replace = TRUE),
#'   temp = rnorm(30, 25, 3)
#' )
#'
#' run_descriptive_stats(
#'   data = df,
#'   output_path = tempdir(),
#'   aggregation_column = "region",
#'   dependent_col = "outcome",
#'   independent_cols = c("temp"),
#'   timeseries_col = "date",
#'   run_id = NULL
#' )
#' }
#'
#' @export
run_descriptive_stats <- function(
    data,
    output_path,
    aggregation_column = NULL,
    population_col = NULL,
    plot_corr_matrix = FALSE,
    correlation_method = "pearson",
    plot_dist = FALSE,
    plot_ma = FALSE,
    ma_days = 100,
    ma_sides = 1,
    timeseries_col = NULL,
    dependent_col,
    independent_cols,
    units = NULL,
    plot_na_counts = FALSE,
    plot_scatter = FALSE,
    plot_box = FALSE,
    plot_seasonal = FALSE,
    plot_regional = FALSE,
    plot_total = FALSE,
    detect_outliers = FALSE,
    calculate_rate = FALSE,
    run_id = NULL,
    create_base_dir = FALSE) {

  if ("data.table" %in% class(data)) {
    data <- as.data.frame(data)
  }

  raise_if_null("dependent_col", dependent_col)
  raise_if_null("independent_cols", independent_cols)
  independent_cols <- unique(as.character(independent_cols))

  # Validate output path and create run folder
  output_path <- prepare_descriptive_output_dir(
    output_path = output_path,
    create_base_dir = create_base_dir
  )
  if (is.null(run_id)) {
    run_id <- generate_descriptive_stats_run_id()
  }
  run_output_path <- file.path(output_path, "descriptive_stats", run_id)
  dir.create(run_output_path, recursive = TRUE, showWarnings = FALSE)

  # Normalise input into full and regional datasets
  input_data <- prepare_descriptive_input(
    data = data,
    aggregation_column = aggregation_column,
    timeseries_col = timeseries_col
  )
  combined_df <- input_data$combined_df
  region_df_list <- input_data$region_df_list

  # Preflight validation across all datasets before writing outputs
  preflight_df_list <- c(list(All = combined_df), region_df_list)
  for (nm in names(preflight_df_list)) {
    validate_descriptive_columns(
      df = preflight_df_list[[nm]],
      context = paste0("dataset '", nm, "'"),
      dependent_col = dependent_col,
      independent_cols = independent_cols,
      aggregation_column = aggregation_column,
      population_col = population_col,
      timeseries_col = timeseries_col,
      plot_corr_matrix = plot_corr_matrix,
      plot_dist = plot_dist,
      plot_ma = plot_ma,
      plot_scatter = plot_scatter,
      plot_box = plot_box,
      plot_seasonal = plot_seasonal,
      plot_regional = plot_regional,
      plot_total = plot_total,
      write_outlier_table = detect_outliers,
      calculate_rate = calculate_rate,
      is_full_dataset = identical(nm, "All")
    )
  }

  # Always produce full dataset outputs under All
  all_output_path <- file.path(run_output_path, "All")
  dir.create(all_output_path, recursive = TRUE, showWarnings = FALSE)
  descriptive_stats_core(
    df = combined_df,
    output_path = all_output_path,
    title = "Full Dataset",
    aggregation_column = aggregation_column,
    population_col = population_col,
    plot_corr_matrix = plot_corr_matrix,
    correlation_method = correlation_method,
    plot_dist = plot_dist,
    dependent_col = dependent_col,
    independent_cols = independent_cols,
    units = units,
    plot_na_counts = plot_na_counts,
    plot_scatter = plot_scatter,
    plot_box = plot_box,
    plot_seasonal = plot_seasonal,
    plot_regional = plot_regional,
    plot_total = plot_total,
    timeseries_col = timeseries_col,
    write_outlier_table = detect_outliers,
    calculate_rate = calculate_rate
  )

  region_output_paths <- list(All = all_output_path)

  # Create summary and plots for each regional subset
  for (region_name in names(region_df_list)) {
    df <- region_df_list[[region_name]]
    region_output_path <- file.path(run_output_path, region_name)
    dir.create(region_output_path, recursive = TRUE, showWarnings = FALSE)

    descriptive_stats_core(
      df = df,
      output_path = region_output_path,
      title = region_name,
      aggregation_column = aggregation_column,
      population_col = population_col,
      plot_corr_matrix = plot_corr_matrix,
      correlation_method = correlation_method,
      plot_dist = plot_dist,
      dependent_col = dependent_col,
      independent_cols = independent_cols,
      units = units,
      plot_na_counts = plot_na_counts,
      plot_scatter = plot_scatter,
      plot_box = plot_box,
      plot_seasonal = plot_seasonal,
      plot_regional = plot_regional,
      plot_total = plot_total,
      timeseries_col = timeseries_col,
      write_outlier_table = detect_outliers,
      calculate_rate = calculate_rate
    )

    region_output_paths[[region_name]] <- region_output_path
  }

  # Moving Average (wrapper concern)
  if (plot_ma == TRUE) {
    raise_if_null("ma_days", ma_days)
    raise_if_null("ma_sides", ma_sides)
    raise_if_null("timeseries_col", timeseries_col)
    ma_days <- as.numeric(ma_days)
    ma_sides <- as.numeric(ma_sides)

    ma_vars <- c(independent_cols, dependent_col)
    ma_df_list <- c(list(All = combined_df), region_df_list)

    for (region_name in names(ma_df_list)) {
      region_folder <- region_output_paths[[region_name]]
      file_path <- file.path(region_folder, "moving_average.pdf")
      with_pdf_device(
        output_path = file_path,
        width = 14,
        height = 8,
        context = paste0("moving average plot for region '", region_name, "'"),
        plot_fn = function() {
          par(oma = c(0, 0, 4, 0), mar = c(5, 4, 3.5, 2) + 0.1)

          for (col_i in seq_along(ma_vars)) {
            plot_moving_average(
              df = ma_df_list[[region_name]],
              time_col = timeseries_col,
              value_col = ma_vars[[col_i]],
              ma_days = ma_days,
              ma_sides = ma_sides,
              title = paste0("Moving average - ", region_name),
              save_plot = FALSE,
              output_path = "",
              units = units
            )
            if (col_i == 1) {
              mtext(
                paste0("Moving average - ", region_name),
                outer = TRUE,
                cex = 1.5,
                line = 1.2,
                font = 2,
                col = "black"
              )
              mtext(
                paste0("(n=", ma_days, ", sides=", ma_sides, ")"),
                outer = TRUE,
                cex = 1.1,
                line = 0.1,
                col = "black"
              )
            }
          }
        }
      )
    }
  }

  result <- list(
    base_output_path = output_path,
    run_id = run_id,
    run_output_path = run_output_path,
    region_output_paths = region_output_paths
  )
  class(result) <- c("descriptive_stats_run", "list")
  return(result)
}

#' Create descriptive statistics via API-friendly inputs.
#'
#' @param data The dataset for descriptive stats (list-like object or CSV path).
#' @param output_path Character. Base output directory.
#' @param aggregation_column Character. Column used to aggregate/split data by region.
#' @param population_col Character. The column containing the population.
#' @param dependent_col Character. The dependent column.
#' @param independent_cols Character vector. The independent columns.
#' @param units Named character vector. Units for each variable.
#' @param plot_corr_matrix Logical. Whether to plot a correlation matrix.
#' @param plot_dist Logical. Whether to plot histograms.
#' @param plot_ma Logical. Whether to plot moving averages over a timeseries.
#' @param plot_na_counts Logical. Whether to plot counts of NAs in each column.
#' @param plot_scatter Logical. Whether to plot dependent vs independent columns.
#' @param plot_box Logical. Whether to generate box plots for selected columns.
#' @param plot_seasonal Logical. Whether to plot seasonal trends.
#' @param plot_regional Logical. Whether to plot regional trends.
#' @param plot_total Logical. Whether to plot total dependent values per year.
#' @param correlation_method Character. Correlation method. One of 'pearson', 'spearman', 'kendall'.
#' @param ma_days Integer. Number of days used in moving average calculations.
#' @param ma_sides Integer. Number of sides used in moving average calculations (1 or 2).
#' @param timeseries_col Character. Timeseries column.
#' @param detect_outliers Logical. Whether to output an outlier table.
#' @param calculate_rate Logical. Whether to plot annual rates per 100k.
#' @param run_id Character. Optional run id.
#' @param create_base_dir Logical. Whether to create `output_path` if missing. Defaults to `TRUE`.
#'
#' @return A list with `base_output_path`, `run_id`, `run_output_path`, and `region_output_paths`.
#'
#' @examples
#' \donttest{
#' run_descriptive_stats_api(
#'   data = list(
#'     date = as.character(as.Date("2024-01-01") + 0:29),
#'     region = rep(c("A", "B"), each = 15),
#'     outcome = sample(1:20, 30, replace = TRUE),
#'     temp = rnorm(30, 25, 3)
#'   ),
#'   output_path = tempdir(),
#'   aggregation_column = "region",
#'   dependent_col = "outcome",
#'   independent_cols = c("temp"),
#'   timeseries_col = "date",
#'   plot_corr_matrix = TRUE
#' )
#' }
#'
#' @export
run_descriptive_stats_api <- function(
    data,
    output_path,
    aggregation_column = NULL,
    population_col = NULL,
    dependent_col,
    independent_cols,
    units = NULL,
    plot_corr_matrix = TRUE,
    plot_dist = TRUE,
    plot_ma = TRUE,
    plot_na_counts = TRUE,
    plot_scatter = TRUE,
    plot_box = TRUE,
    plot_seasonal = TRUE,
    plot_regional = TRUE,
    plot_total = TRUE,
    correlation_method = "pearson",
    ma_days = 100,
    ma_sides = 1,
    timeseries_col = NULL,
    detect_outliers = TRUE,
    calculate_rate = TRUE,
    run_id = NULL,
    create_base_dir = TRUE) {
  # Parameter checks
  if (plot_ma == TRUE) {
    raise_if_null("ma_days", ma_days)
    raise_if_null("ma_sides", ma_sides)
    raise_if_null("independent_cols", independent_cols)
    raise_if_null("timeseries_col", timeseries_col)
    ma_days <- as.numeric(ma_days)
    ma_sides <- as.numeric(ma_sides)
  }
  if (plot_corr_matrix == TRUE) {
    raise_if_null("correlation_method", correlation_method)
  }

  # Convert data to the correct format
  df <- read_input_data(data)

  # Check columns
  exp_columns <- c(dependent_col, independent_cols)
  for (col in exp_columns) {
    if (!(col %in% colnames(df))) {
      stop(paste0("Column '", col, "' not in passed dataset."))
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

  run_descriptive_stats(
    data = df,
    output_path = output_path,
    aggregation_column = aggregation_column,
    population_col = population_col,
    plot_corr_matrix = plot_corr_matrix,
    correlation_method = correlation_method,
    plot_dist = plot_dist,
    plot_ma = plot_ma,
    ma_days = ma_days,
    ma_sides = ma_sides,
    timeseries_col = timeseries_col,
    dependent_col = dependent_col,
    independent_cols = independent_cols,
    units = units,
    plot_na_counts = plot_na_counts,
    plot_scatter = plot_scatter,
    plot_box = plot_box,
    plot_seasonal = plot_seasonal,
    plot_regional = plot_regional,
    plot_total = plot_total,
    detect_outliers = detect_outliers,
    calculate_rate = calculate_rate,
    run_id = run_id,
    create_base_dir = create_base_dir
  )
}

#' Deprecated alias for `descriptive_stats_core()`.
#'
#' Deprecated. Use `descriptive_stats_core()` instead.
#'
#' @keywords internal
common_descriptive_stats_core <- function(
    df,
    output_path,
    title,
    aggregation_column = NULL,
    population_col = NULL,
    plot_corr_matrix = FALSE,
    correlation_method = "pearson",
    plot_dist = FALSE,
    dependent_col,
    independent_cols = c(),
    units = NULL,
    plot_na_counts = FALSE,
    plot_scatter = FALSE,
    plot_box = FALSE,
    plot_seasonal = FALSE,
    plot_regional = FALSE,
    plot_total = FALSE,
    timeseries_col = "date",
    detect_outliers = FALSE,
    calculate_rate = FALSE) {
  deprecate_descriptive_stats("common_descriptive_stats_core", "descriptive_stats_core")
  descriptive_stats_core(
    df = df,
    output_path = output_path,
    title = title,
    aggregation_column = aggregation_column,
    population_col = population_col,
    plot_corr_matrix = plot_corr_matrix,
    correlation_method = correlation_method,
    plot_dist = plot_dist,
    dependent_col = dependent_col,
    independent_cols = independent_cols,
    units = units,
    plot_na_counts = plot_na_counts,
    plot_scatter = plot_scatter,
    plot_box = plot_box,
    plot_seasonal = plot_seasonal,
    plot_regional = plot_regional,
    plot_total = plot_total,
    timeseries_col = timeseries_col,
    write_outlier_table = detect_outliers,
    calculate_rate = calculate_rate
  )
}

#' Deprecated alias for `run_descriptive_stats()`.
#'
#' Generic wrapper function to compute descriptive statistics and EDA outputs.
#'
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
#' @param timeseries_col Character. The column used as the timeseries for moving averages.
#' @param dependent_col Character. The column in the data containing the dependent variable.
#' @param independent_cols Character vector. The columns in the data containing the independent variables.
#' @param units Named character vector. A named character vector of units for each variable.
#' @param plot_na_counts Logical. Whether to plot NA counts.
#' @param plot_scatter Logical. Whether to plot scatter plots.
#' @param plot_box Logical. Whether to plot box plots.
#' @param plot_seasonal Logical. Whether to plot seasonal plots.
#' @param plot_regional Logical. Whether to plot regional plots.
#' @param plot_total Logical. Whether to plot total health outcomes per year.
#' @param detect_outliers Logical. Whether to output a table containing outlier information.
#' @param calculate_rate Logical. Whether to calculate the rate of health outcomes per 100k people.
#'
#' @return Character vector. Backward-compatible output path format.
#'
#' Deprecated. Use `run_descriptive_stats()` instead.
#'
#' @keywords internal
common_descriptive_stats <- function(
    df_list,
    output_path,
    aggregation_column = NULL,
    population_col = NULL,
    plot_corr_matrix = FALSE,
    correlation_method = "pearson",
    plot_dist = FALSE,
    plot_ma = FALSE,
    ma_days = 100,
    ma_sides = 1,
    timeseries_col = NULL,
    dependent_col,
    independent_cols,
    units = NULL,
    plot_na_counts = FALSE,
    plot_scatter = FALSE,
    plot_box = FALSE,
    plot_seasonal = FALSE,
    plot_regional = FALSE,
    plot_total = FALSE,
    detect_outliers = FALSE,
    calculate_rate = FALSE) {
  deprecate_descriptive_stats("common_descriptive_stats", "run_descriptive_stats")

  final_paths <- run_descriptive_stats(
    data = df_list,
    output_path = output_path,
    aggregation_column = aggregation_column,
    population_col = population_col,
    plot_corr_matrix = plot_corr_matrix,
    correlation_method = correlation_method,
    plot_dist = plot_dist,
    plot_ma = plot_ma,
    ma_days = ma_days,
    ma_sides = ma_sides,
    timeseries_col = timeseries_col,
    dependent_col = dependent_col,
    independent_cols = independent_cols,
    units = units,
    plot_na_counts = plot_na_counts,
    plot_scatter = plot_scatter,
    plot_box = plot_box,
    plot_seasonal = plot_seasonal,
    plot_regional = plot_regional,
    plot_total = plot_total,
    detect_outliers = detect_outliers,
    calculate_rate = calculate_rate,
    create_base_dir = FALSE
  )
  return(c(final_paths$run_output_path, "descriptive_stats"))
}

#' Deprecated alias for `run_descriptive_stats_api()`.
#'
#' @param data The dataset used for descriptive stats (as a vector).
#' @param aggregation_column Character. The column to use for aggregating the dataset into smaller subsets.
#' @param population_col Character. The column containing the population.
#' @param dependent_col Character. The dependent column.
#' @param independent_cols Character vector. The independent columns.
#' @param units Named character vector. A named character vector of units for each variable.
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
#' @param timeseries_col Character. The column used as the timeseries for moving averages.
#' @param detect_outliers Logical. Whether to have a table of outliers.
#' @param calculate_rate Logical. Whether to plot a rate based metric of the dependent column per year.
#' @param output_path Character. The path to save outputs to.
#'
#' @return Character vector. Backward-compatible output path format.
#'
#' Deprecated. Use `run_descriptive_stats_api()` instead.
#'
#' @keywords internal
common_descriptive_stats_api <- function(
    data,
    aggregation_column = NULL,
    population_col = NULL,
    dependent_col,
    independent_cols,
    units = NULL,
    plot_correlation = TRUE,
    plot_dist_hists = TRUE,
    plot_ma = TRUE,
    plot_na_counts = TRUE,
    plot_scatter = TRUE,
    plot_box = TRUE,
    plot_seasonal = TRUE,
    plot_regional = TRUE,
    plot_total = TRUE,
    correlation_method = "pearson",
    ma_days = 100,
    ma_sides = 1,
    timeseries_col = NULL,
    detect_outliers = TRUE,
    calculate_rate = TRUE,
    output_path) {
  deprecate_descriptive_stats("common_descriptive_stats_api", "run_descriptive_stats_api")

  final_paths <- run_descriptive_stats_api(
    data = data,
    output_path = output_path,
    aggregation_column = aggregation_column,
    population_col = population_col,
    dependent_col = dependent_col,
    independent_cols = independent_cols,
    units = units,
    plot_corr_matrix = plot_correlation,
    plot_dist = plot_dist_hists,
    plot_ma = plot_ma,
    plot_na_counts = plot_na_counts,
    plot_scatter = plot_scatter,
    plot_box = plot_box,
    plot_seasonal = plot_seasonal,
    plot_regional = plot_regional,
    plot_total = plot_total,
    correlation_method = correlation_method,
    ma_days = ma_days,
    ma_sides = ma_sides,
    timeseries_col = timeseries_col,
    detect_outliers = detect_outliers,
    calculate_rate = calculate_rate,
    create_base_dir = TRUE
  )
  return(c(final_paths$run_output_path, "descriptive_stats"))
}
