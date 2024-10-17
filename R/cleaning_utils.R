# Functions to help clean and aggregate input data.
# AUTHOR: charlie.browning@ons.gov.uk
# DATE CREATED: 04/10/2024

library(dplyr) # used to load pipe operator
# TODO: consider splitting this up into smaller functions


#' Reformat a dataframe using various different cleaning techniques.
#'
#' @description
#' Take a dataframe, and apply various different cleaning methods to it in order
#' to prepare the data for use with a climate indicator.
#'
#'
#' @param df The dataframe to apply cleaning/reformatting to.
#' @param reformat_date Whether or not to reformat the data to the Date datatype.
#' @param fill_na A vector of column names to fill NA values in (fills with 0).
#' @param year_from_date
#'
#' @return The cleaned/reformatted data frame.
#' @export
#'
#' @examples fill_na = c("col1", "col2")
reformat_data <- function(
    df,
    reformat_date = FALSE,
    fill_na = c(),
    year_from_date = FALSE
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
      dplyr::mutate(col = ifelse(is.na(col), 0, col))
  }
  # Derive the year from the date column
  if (year_from_date) {
    df <- df %>%
      dplyr::mutate(year = as.numeric(format(date, "%Y")))
  }
  return (df)
}


#' Aggregate a dataframe into multiple dataframes, based on a columns value.
#'
#' @param df The dataframe to aggregate.
#' @param column_name The column to aggregate the data by.
#'
#' @return A list of dataframes, split up based on the value of column_name.
#' @export
#'
aggregate_by_column <- function(df, column_name) {

  unique_values = sort(as.character(unique(df[[column_name]])))
  aggregated_dfs = lapply(
    unique_values,
    function(x) df %>% dplyr::filter(!!sym(column_name) == x)
  )
  names(aggregated_dfs) <- unique_values

  return (aggregated_dfs)
}
