# Structured error handling helpers for climatehealth package
#
# These functions create typed conditions that can be caught and inspected
# by the API layer, enabling rich error messages to propagate through the stack.
#
# Usage:
#   Instead of: stop("Column 'tmean' not found")
#   Use: abort_column_not_found("tmean", colnames(data))
#
# The errors work as normal R errors if not caught specially, ensuring
# backwards compatibility.

#' Raise a typed error with structured metadata
#'
#' Creates a classed condition that can be caught and inspected by the API layer.
#' This is the base helper - prefer using specific helpers like
#' `abort_column_not_found()` or `abort_validation()` when applicable.
#'
#' @param message Human-readable error message
#' @param type Error type for classification. One of:
#'   - "validation_error": Data/parameter validation issues (HTTP 400)
#'   - "column_not_found": Missing column in dataset (HTTP 400)
#'   - "model_error": Statistical model failures (HTTP 422)
#'   - "generic_error": Unclassified errors (HTTP 500)
#' @param ... Additional metadata to include in the error (e.g., column = "tmean")
#' @param call The call to include in the error (defaults to caller's call)
#'
#' @return Never returns; always raises an error.
#'
#' @examples
#' \donttest{
#' # Basic usage
#' abort_climate("Something went wrong", "generic_error")
#'
#' # With metadata
#' abort_climate(
#'   "Invalid lag value",
#'   "validation_error",
#'   param = "nlag",
#'   value = -1,
#'   expected = "non-negative integer"
#' )
#' }
#'
#' @export
abort_climate <- function(message, type = "generic_error", ..., call = rlang::caller_env()) {
  # Build metadata list from additional arguments
  metadata <- list(...)

  # Add the type to metadata for API extraction
  metadata$type <- type

  # Class hierarchy: specific type -> category -> package -> base
  # Handlers can catch at any level of specificity
  classes <- c(type, "climate_error", "error", "condition")

  # Use rlang::abort for proper condition creation
  rlang::abort(
    message = message,
    class = classes,
    !!!metadata,
    call = call
  )
}


#' Raise a validation error (data/parameter issues)
#'
#' Use this for general validation failures where the user's input or data
#' doesn't meet requirements. For missing columns specifically, use
#' `abort_column_not_found()`.
#'
#' @param message Human-readable error message
#' @param ... Additional metadata (e.g., param = "nlag", value = -1)
#' @param call The call to include in the error
#'
#' @return Never returns; always raises an error.
#'
#' @examples
#' \donttest{
#' # Parameter validation
#' if (nlag < 0) {
#'   abort_validation(
#'     "nlag must be >= 0",
#'     param = "nlag",
#'     value = nlag,
#'     expected = "non-negative integer"
#'   )
#' }
#' }
#'
#' @export
abort_validation <- function(message, ..., call = rlang::caller_env()) {
  abort_climate(message, type = "validation_error", ..., call = call)
}


#' Raise a column-not-found error with available columns
#'
#' Use this when a required column is missing from a dataset. Includes
#' fuzzy matching to suggest the closest available column name.
#'
#' @param column The column name that was not found
#' @param available Character vector of available column names
#' @param dataset_name Optional name of the dataset for clearer messages
#' @param call The call to include in the error
#'
#' @return Never returns; always raises an error.
#'
#' @examples
#' \donttest{
#' if (!("tmean" %in% colnames(data))) {
#'   abort_column_not_found("tmean", colnames(data))
#' }
#' }
#'
#' @export
abort_column_not_found <- function(column, available, dataset_name = "dataset",
                                   call = rlang::caller_env()) {
  # Build descriptive message
  msg <- sprintf("Column '%s' not found in %s", column, dataset_name)

  # Try fuzzy match suggestion using Jaro-Winkler distance
  suggestion <- suggest_column_match(column, available)

  abort_climate(
    msg,
    type = "column_not_found",
    column = column,
    available = available,
    suggestion = suggestion,
    dataset_name = dataset_name,
    call = call
  )
}


#' Raise a model error (statistical/computational failures)
#'
#' Use this when statistical models fail to converge, produce singular matrices,
#' or encounter other computational issues that aren't due to obvious user error.
#'
#' @param message Human-readable error message
#' @param model_type Type of model that failed (e.g., "dlnm", "glm", "meta-analysis")
#' @param ... Additional diagnostic metadata
#' @param call The call to include in the error
#'
#' @return Never returns; always raises an error.
#'
#' @examples
#' \donttest{
#' tryCatch({
#'   fit_model(data)
#' }, error = function(e) {
#'   abort_model_error(
#'     "Model failed to converge",
#'     model_type = "dlnm",
#'     original_error = conditionMessage(e)
#'   )
#' })
#' }
#'
#' @export
abort_model_error <- function(message, model_type = "unknown", ...,
                              call = rlang::caller_env()) {
  abort_climate(
    message,
    type = "model_error",
    model_type = model_type,
    ...,
    call = call
  )
}


#' Suggest a column name based on fuzzy matching
#'
#' Uses Jaro-Winkler distance to find the closest match to a misspelled
#' or incorrect column name.
#'
#' @param input The column name that was not found
#' @param available Character vector of available column names
#' @param threshold Maximum distance threshold (0-1). Lower = stricter matching.
#'
#' @return The best matching column name, or NULL if no good match found.
#'
#' @keywords internal
suggest_column_match <- function(input, available, threshold = 0.3) {
  if (length(available) == 0 || is.null(input) || input == "") {
    return(NULL)
  }

  # Try stringdist if available (more accurate Jaro-Winkler)
  if (requireNamespace("stringdist", quietly = TRUE)) {
    distances <- stringdist::stringdist(
      tolower(input),
      tolower(available),
      method = "jw"
    )

    best_idx <- which.min(distances)
    if (length(best_idx) > 0 && distances[best_idx] < threshold) {
      return(available[best_idx])
    }
  } else {
    # Fallback: use base R's agrep for approximate matching
    matches <- agrep(input, available, value = TRUE, max.distance = threshold,
                     ignore.case = TRUE)
    if (length(matches) > 0) {
      return(matches[1])
    }
  }

  NULL
}


#' Check if an error is a climate_error
#'
#' Utility function to check if a caught condition is a typed climate error.
#'
#' @param error A condition object
#'
#' @return TRUE if the error inherits from "climate_error", FALSE otherwise.
#'
#' @examples
#' \donttest{
#' tryCatch({
#'   some_function()
#' }, error = function(e) {
#'   if (is_climate_error(e)) {
#'     # Handle structured error
#'   } else {
#'     # Handle untyped error
#'   }
#' })
#' }
#'
#' @export
is_climate_error <- function(error) {
  inherits(error, "climate_error")
}


#' Extract metadata from a climate_error
#'
#' Extracts the structured metadata from a typed climate error for use
#' in API responses or logging.
#'
#' @param error A climate_error condition object
#'
#' @return A list containing the error metadata (type, column, available, etc.)
#'
#' @keywords internal
extract_error_metadata <- function(error) {
  if (!is_climate_error(error)) {
    return(list(
      type = "generic_error",
      message = conditionMessage(error)
    ))
  }

  # rlang errors store metadata as named list elements, not attributes
  # Standard rlang fields to exclude from custom metadata
  standard_fields <- c("message", "call", "trace", "parent", "rlang")
  all_fields <- names(error)
  custom_fields <- setdiff(all_fields, standard_fields)

  metadata <- list(
    message = conditionMessage(error),
    type = error$type %||% class(error)[1]
  )

  # Add custom fields
  for (field_name in custom_fields) {
    if (field_name != "type") {  # Already added
      metadata[[field_name]] <- error[[field_name]]
    }
  }

  metadata
}


# Null coalescing operator (if not already available from rlang)
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
