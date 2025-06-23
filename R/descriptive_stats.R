# Utilities to allow for statistics to be derived from datasets

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
create_column_summaries <- function(
    df,
    columns = NULL
) {
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
  sums <- data.frame()
  for (i in 1:length(columns)) {
    if (i==1){
      sums <- summary(df[[columns[i]]])
    } else {sums <- rbind(sums, summary(df[[columns[i]]]))}
  }
  rownames(sums) <- columns
  sums <- as.data.frame(sums)
  # calculate IQR
  sums[, "IQR"] <- (sums[, '3rd Qu.'] - sums[, '1st Qu.'])
  return(sums)
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
  na_counts <- sapply(df, function(y) sum(length(which(is.na(y)))))
  na_counts <- data.frame(na_counts)
  colnames(na_counts) <- c("na_count")
  return(na_counts)
}

# Desc stats main functions to use across multiple indicators

#' The core functionality for producing descriptive stats
#'
#' @param dataset_title String. The title for the dataset being used.
#' @param df DataFrame. Input data for descriptive stats/
#' @param output_path string. The path to write outputs to.
#' @param title string. title used for the plots in the ds.
#' @param correlation_method string. The correlation method used in ds.
#' @param dist_columns vector. The columns to plot distributions for.
#' @param dependent_col str. The column in the data containing the dependent var.
#' @param independent_cols str. The column in the data containing the independent var.
#'
#' @export
common_descriptive_stats_core <- function(
    dataset_title,
    df,
    output_path,
    title,
    plot_corr_matrix = F,
    correlation_method = "pearson",
    plot_dist = F,
    dist_columns = c("temp"),
    dependent_col = "dependent",
    independent_cols = c("temp"),
    plot_na_counts = F,
    plot_scatter = F
) {
  # get numeric columns
  numeric_cols <- colnames(select_if(df, is.numeric))
  # get dataframe summary
  full_summary <- create_column_summaries(df, numeric_cols)
  # plot correlation matrix
  if (plot_corr_matrix) {
    full_corr <- create_correlation_matrix(df, numeric_cols, correlation_method)
    corr_path <- file.path(output_path, "correlation_matrix.png")
    plot_correlation_matrix(
      full_corr,
      paste0("Correlation Matrix for the ", stringr::str_to_title(dataset_title), " Dataset \n(", title, ")"),
      corr_path
    )
  }
  # Column distributions
  if (plot_dist==T) {
    dist_path <- file.path(output_path, "column_distributions.pdf")
    plot_distributions(
      df,
      dist_columns,
      paste0("Column Distributions for the ", stringr::str_to_title(dataset_title), " dataset \n(", title, ")"),
      T,
      dist_path
    )
  }
  # Count NAs and visualise
  na_counts_path <- file.path(output_path, "na_counts.pdf")
  if (plot_na_counts==T) {
    na_counts <- create_na_summary(df)
    pdf(na_counts_path)
    barplot(
      height=na_counts$na_count,
      names.arg=rownames(na_counts),
      xlab="NA Count",
      ylab="Column Name",
      main=paste0("NA Counts for Columns in the ", stringr::str_to_title(dataset_title), " Dataset \n(", title, ")")
    )
    dev.off()
  }
  # Dependent vs independent variables
  scatter_path <- file.path(output_path, "dependent_vs_independents.pdf")
  if (plot_scatter==T) {
    plot_scatter_grid(
      df,
      dependent_col,
      independent_cols,
      paste0("Dependent vs Independent Column(s) \n(", title, ")"),
      T,
      scatter_path
    )
  }
  # Save analysis
  summary_path <- file.path(output_path, "dataset_summary.csv")
  write.csv(full_summary, summary_path)
}

#' The wrapper function to compute descriptive stats for the heat and cold indicator.
#'
#' @param dataset_title String. The title for the dataset being used.
#' @param df_list list(dataframe) A list of input dataframes.
#' @param output_path string. The path to write outputs to.
#' @param use_individual_dfs bool. Whether or not to use all dfs or concat them into one.
#' @param title string. title used for the plots in the ds.
#' @param correlation_method string. The correlation method used in ds.
#' @param dist_columns vector. The columns to plot distributions for.
#' @param ma_days int. The number of days to use for a moving average.
#' @param ma_sides int. The number of sides to use for a moving average (1 or 2).
#' @param ma_columns vector. Additional columns to plot moving average for. Dependent done by default.
#' @param dependent_col str. The column in the data containing the dependent var.
#' @param independent_cols str. The column in the data containing the independent var.
#'
#' @export
common_descriptive_stats <- function(
    dataset_title,
    df_list,
    output_path,
    plot_corr_matrix = F,
    correlation_method = "pearson",
    plot_dist = F,
    dist_columns = c(),
    plot_ma = F,
    ma_days = 100,
    ma_sides = 2,
    ma_columns = c(),
    timeseries_col = NULL,
    dependent_col,
    independent_cols,
    plot_na_counts = F,
    plot_scatter = F
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
  # obtain desc. stats
  common_descriptive_stats_core(
    dataset_title = dataset_title,
    df = combined_df,
    output_path = output_path,
    title = "Full Dataset",
    plot_corr_matrix = plot_corr_matrix,
    correlation_method = correlation_method,
    plot_dist = plot_dist,
    dist_columns = dist_columns,
    dependent_col = dependent_col,
    independent_cols = independent_cols,
    plot_na_counts = plot_na_counts,
    plot_scatter = plot_scatter
  )
  # Moving Average
  if (plot_ma) {
    ma_vars = c(ma_columns, dependent_col)
    for (col_i in 1:length(ma_vars)) {
      fname <- paste0(ma_vars[[col_i]], "_moving_average.pdf")
      pdf(file.path(output_path, fname))
      for (i in 1:length(df_list)) {
        plot_moving_average(
          df_list[[i]],
          timeseries_col,
          ma_vars[[col_i]],
          ma_days,
          ma_sides,
          paste0("Moving average for the ", stringr::str_to_title(dataset_title), " Dataset \n(", names(df_list)[i], ")")
        )
      }
      dev.off()
    }
  }
  # create summary, corr. and dist. for each df
  # DEV NOTE: creating a new for loop to avoid calling dev.off early.
  for (i in 1:length(df_list)) {
    # create a new subdir
    df_name <- names(df_list)[i]
    sub_df_path <- file.path(output_path, gsub(" ", "_", tolower(df_name)))
    if (!check_file_exists(sub_df_path, raise=FALSE)) {
      dir.create(sub_df_path)
    }
    # save out statistics
    common_descriptive_stats_core(
      dataset_title = dataset_title,
      df = df_list[[i]],
      output_path = sub_df_path,
      title = df_name,
      plot_corr_matrix = plot_corr_matrix,
      correlation_method = correlation_method,
      plot_dist = plot_dist,
      dist_columns = dist_columns,
      dependent_col = dependent_col,
      independent_cols = independent_cols,
      plot_na_counts = plot_na_counts,
      plot_scatter = plot_scatter
    )
  }
  return (c(output_path, paste0(normalised_title, "_descriptive_stats")))

}

raise_if_null <- function(param_nm, value) {
  if (is.null(value)) {
    stop(paste0("Unexpected NULL in ", param_nm))
  }
}

#' The function used to create desciprtive stats via an API endpoint
#'
#' @param data The dataset used for descriptive stats (as a vector)
#' @param aggregation_column The column to use for aggregating the dataset into smaller subsets
#' @param dataset_title The datasets title used for outputs
#' @param dependent_col The dependent column
#' @param independent_cols A vector of independent columns
#' @param plot_correlation Whether to plot a correlation matrix
#' @param plot_dist_hists Whether to plot histograms showing column distributions
#' @param plot_ma Whether to plot moving averages over a timeseries
#' @param plot_na_counts Whether to plot counts of NAs in each column
#' @param plot_scatter Whether to plot the dependent column against the independent columns
#' @param correlation_method The correlation method. One of 'pearson', 'spearman', 'kendall'
#' @param dist_columns The columns to plot distributions for
#' @param ma_days The number of days to use in moving average calculations
#' @param ma_sides The number of sides to use in moving average calculations (1 or2)
#' @param ma_columns A vector of columns to plot moving average for
#' @param timeseries_col The column used as the timerseries for moving averages
#' @param output_path The path to save outputs to
#'
#' @return The full directory path that the descriptive stats are saved to.
#' @export
#'
common_descriptive_stats_api <- function(
  data,
  aggregation_column = NULL,
  dataset_title,
  dependent_col,
  independent_cols,
  plot_correlation = T,
  plot_dist_hists = T,
  plot_ma = T,
  plot_na_counts = T,
  plot_scatter = T,
  correlation_method = NULL,
  dist_columns = NULL,
  ma_days = NULL,
  ma_sides = 2,
  ma_columns = NULL,
  timeseries_col = NULL,
  output_path
) {
  # Parameter Checks
  if(plot_ma) {
    raise_if_null("ma_days", ma_days)
    raise_if_null("ma_sides", ma_sides)
    raise_if_null("ma_columns", ma_columns)
    raise_if_null("timerseries_col", timeseries_col)
  }
  if(plot_correlation) {
    raise_if_null("correlation_method", correlation_method)
  }
  if(plot_dist_hists) {
    raise_if_null("plot_dist_hists", plot_dist_hists)
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
  # Dis aggregate if needed
  df_list <- df
  if (!is.null(aggregation_column)) {
    df_list <- aggregate_by_column(df_list, aggregation_column)
  }
  # Create descriptive stats
  final_paths <- common_descriptive_stats(
    dataset_title = dataset_title,
    df_list = df_list,
    output_path = output_path,
    plot_corr_matrix = plot_correlation,
    correlation_method = correlation_method,
    plot_dist = plot_dist_hists,
    dist_columns = dist_columns,
    plot_ma = plot_ma,
    ma_days = ma_days,
    ma_sides = ma_sides,
    ma_columns = ma_columns,
    timeseries_col = timeseries_col,
    dependent_col = dependent_col,
    independent_cols = independent_cols,
    plot_na_counts = plot_na_counts,
    plot_scatter = plot_scatter
  )
  return (final_paths)
}
