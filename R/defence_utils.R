# Utility functions related validating user inputs

# TODO: explore how to make this function more dynamic (take a type as an arg)


#' Check that a passed argument is a list of dataframes.
#'
#' @param list_ A list of items.
#' @param raise Whether or not to raise an error.
#' @param param_nm The name of the parameter that the list was originally passed
#' to.
#'
#' @return Whether or not the item passed (list_) is a list of dfs.
#'
#' @export
is_list_of_dfs <- function(list_,
                           raise = TRUE,
                           param_nm = "df_list") {
  valid <- TRUE
  if (is.list(list_)) {
    for (item in list_) {
      if (!is.data.frame(item)) {
        valid <- FALSE
        if (raise) {
          stop(
            paste(
              param_nm,
              " expected a list of dataframes. List contains item of type ",
              toString(typeof(item)),
              sep = ""
            )
          )
        }
        break
      }
    }
  } else {
    valid <- FALSE
    if (raise) {
      stop(paste(param_nm, " expected a list. Got ", toString(typeof(list_)),
                 sep = ""))
    }
  }
  return (valid)
}
