# Functions to help clean and aggregate input data.
# AUTHOR: charlie.browning@ons.gov.uk
# DATE CREATED: 04/10/2024

library(dplyr) # used to load pipe operator

reformat_data <- function(
    df,
    reformat_date=FALSE,
    fill_na=c(),
    year_from_date=FALSE
) {
  # TODO: Add type checks to all arguments
  # Reformat the date column
  if (reformat_date) {
    df <- df %>%
      dplyr::mutate(date = as.Date(date,
                    try_formats = c("%Y-%m-%d", "%d/%m/%Y")))
  }
  # Fill Na's
  for (col in fill_na) {
    df <- df %>%
      dplyr::mutate(col=ifelse(is.na(col), 0, col))
  }
  # Derive the year from the date column
  if (year_from_date) {
    df <- df %>%
      dplyr::mutate(year = as.numeric(format(date, "%Y")))
  }

  return (df)
}

aggregate_by_column <- function(df, column_name) {

  unique_values = sort(as.character(unique(df[[column_name]])))
  aggregated_dfs = lapply(
    unique_values,
    function(x) df %>% dplyr::filter(!!sym(column_name)==x)
  )
  names(aggregated_dfs) <- unique_values

  return (aggregated_dfs)
}
