# Utility functions related to file actions

#' Check that a file exists at a passed path.
#'
#' @description Checks the files on disk to assert that the passed file is
#'  present.
#'
#' @param fpath The filepath to check exists.
#' @param raise Whether or not to raise an error if the file does not exist,
#'    Default: TRUE
#'
#' @return 'exists'. Whether or not the file exists on disk.
#' @export
check_file_exists <- function(fpath, raise = TRUE) {
  # assert if the file exists
  exists <- file.exists(fpath)
  # raise an error if raise=TRUE
  if (raise & !exists) {
    stop(paste("No file was found at path:", fpath))
  } else {
    return(exists)
  }
}

#' Check that a file extension on a given path matches the expected.
#'
#' @description This function takes an expected file extension, and validates it
#'  against a user-inputted file path.
#'
#' @param fpath The filepath.
#' @param expected_ext The expected file extension.
#' @param param_nm The parameter name that the filepath was passed to
#'  (for error raising), Default: 'fpath'
#' @param raise Whether or not to raise an error, Default: TRUE
#'
#' @return Whether or not the passed file has a valid file extension.
#' @export
check_file_extension <- function(
    fpath,
    expected_ext,
    param_nm = "fpath",
    raise = TRUE
  ) {
  # obtain and normalise file extensions
  expected_ext <- tolower(gsub("\\.", "", expected_ext))
  found_ext <- tools::file_ext(fpath)
  found_ext <- tolower(gsub("\\.", "", found_ext))
  # assert that the passed file ext is valid
  valid <- (identical(expected_ext, found_ext))
  if (!valid & raise) {
    stop(
      paste(
        "Parameter '",
        param_nm,
        "' expected filetype '",
        expected_ext,
        "'. Got ",
        found_ext,
        sep = ""
      )
    )
  } else {
    return(valid)
  }

}


#' Read a csv file into memory as a data frame.
#'
#' @param input_csv_path The path to the csv to read as a dataframe.
#'
#' @return A dataframe containing the data from the csv.
#' @export
#'
#' @examples input_csv_path = "directory/file_name.csv"
read_input_data <- function(input_csv_path) {
  if (is.list(input_csv_path)) {
    df <- data.frame(input_csv_path)


  } else if (is.character(input_csv_path)) {
    check_file_extension(input_csv_path, ".csv", "input_csv_path")
    check_file_exists(input_csv_path, raise = TRUE)
    df <- read.csv(input_csv_path)
  } else {
    # Raise an error when the input_csv argument isn't valid
    stop(paste(
      "'input_csv' expected a list or a string. Got",
      typeof(input_csv)
    ))
  }

  return(df)
}

