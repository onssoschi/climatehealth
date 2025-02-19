# Utilities to allow for statistics to be derived from datasets

#' Create a correlation matrix for columns in a dataframe.
#'
#' @param df Dataframe. The dataframe to use to create a correlation matrix.
#' @param columns vector. The columns to calculate correlation between.
#' @param correlation_method string. The method to use for correlation calculations.
#'
#' @return Matrix. Correlation matrix for selected columns in the input dataset.
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
  na_counts <- sapply(df_list[[1]], function(y) sum(length(which(is.na(y)))))
  na_counts <- data.frame(na_counts)
  colnames(na_counts) <- c("na_count")
  return(na_counts)
}


