# Functions to help clean and aggregate input data.
# Cleaning Utilities

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
#' @param year_from_date Derive a new column 'year' from the date column.
#'
#' @return The cleaned/reformatted data frame.
#'
#' @keywords internal
reformat_data <- function(
    df,
    reformat_date = TRUE,
    fill_na = c(),
    year_from_date = TRUE) {
  # TODO: Add type checks to all arguments
  # Reformat the date column
  if (reformat_date == TRUE) {
    df <- df %>%
      dplyr::mutate(
        date =
          as.Date(date, tryFormats = c("%d/%m/%Y", "%Y-%m-%d"))
      )
  }
  # Fill Na's
  for (col in fill_na) {
    df <- df %>%
      dplyr::mutate(!!col := ifelse(is.na(df[[col]]), 0, df[[col]]))
  }
  # Derive the year from the date column
  if (year_from_date) {
    df <- df %>%
      dplyr::mutate(year = as.numeric(format(date, "%Y")))
  }
  return(df)
}


#' Split dataframe into multiple dataframes, based on a columns value.
#'
#' @param df The dataframe to aggregate.
#' @param column_name The column to aggregate the data by.
#'
#' @return A list of dataframes, split up based on the value of column_name.
#'
#' @keywords internal
aggregate_by_column <- function(df, column_name) {
  unique_values <- sort(as.character(unique(df[[column_name]])))
  aggregated_dfs <- lapply(
    unique_values,
    function(x) df %>% dplyr::filter(!!rlang::sym(column_name) == x)
  )
  names(aggregated_dfs) <- unique_values

  return(aggregated_dfs)
}

#' English month names
#'
#' @description Provides consistent English month names regardless of system locale
#'
#' @param month_numbers Optional vector of month numbers (1-12) to return
#' @param short Logical. Return abbreviated names? Default FALSE.
#'
#' @return Character vector of month names
#'
#' @keywords internal
.english_month_names <- function(month_numbers = NULL, short = FALSE) {
  if (short) {
    months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  } else {
    months <- c("January", "February", "March", "April", "May", "June",
                "July", "August", "September", "October", "November", "December")
  }

  if (is.null(month_numbers)) {
    return(months)
  } else {
    return(months[month_numbers])
  }
}

#===============================================================================
#' English day of week names
#'
#' @description Provides consistent English day names regardless of system locale
#'
#' @param day_numbers Optional vector of day numbers (1-7, where 1=Sunday)
#' @param short Logical. Return abbreviated names? Default FALSE.
#'
#' @return Character vector of day names
#'
#' @keywords internal
.english_dow_names <- function(day_numbers = NULL, short = FALSE) {
  if (short) {
    days <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
  } else {
    days <- c("Sunday", "Monday", "Tuesday", "Wednesday",
              "Thursday", "Friday", "Saturday")
  }

  if (is.null(day_numbers)) {
    return(days)
  } else {
    return(days[day_numbers])
  }
}


#' Temporarily set English locale for date operations
#'
#' @description Temporarily sets the locale to English for date parsing and formatting
#'
#' @param expr Expression to evaluate with English locale
#'
#' @return Result of the expression
#'
#' @keywords internal
.with_english_locale <- function(expr) {
  # Store original locale
  original_locale <- Sys.getlocale("LC_TIME")

  # Try to set to English
  english_locales <- c("English", "en_US.UTF-8", "en_GB.UTF-8", "C")

  success <- FALSE
  for (loc in english_locales) {
    try_locale <- tryCatch({
      Sys.setlocale("LC_TIME", loc)
      TRUE
    }, error = function(e) FALSE, warning = function(w) FALSE)

    if (try_locale) {
      success <- TRUE
      break
    }
  }

  # Evaluate expression
  result <- tryCatch({
    force(expr)
  }, finally = {
    # Always restore original locale
    if (success) {
      Sys.setlocale("LC_TIME", original_locale)
    }
  })

  return(result)
}
