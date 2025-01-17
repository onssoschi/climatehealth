# Utilities to allow for statistics to be derived from datasets

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

create_columns_summaries <- function(
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
