# Utilities to allow for statistics to be derived from datasets

create_correlation_matrix <- function(
    df,
    columns = NULL,
    correlation_method = "pearson"
  ) {
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
  corr_df <- data %>%
    select(columns) %>%
    cor(method = correlation_method)
  return (corr_df)
}
