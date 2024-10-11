# Utility functions related validating user inputs
# AUTHOR: charlie.browning@ons.gov.uk
# DATE CREATED: 08/10/2024

# TODO: explore how to make this function more dynamic (take a type as an arg)


is_list_of_dfs <- function(list_,
                           raise = TRUE,
                           param_nm = "df_list") {
  invalid <- FALSE
  if (is.list(list_)) {
    for (item in list_) {
      if (!is.data.frame(item)) {
        invalid <- TRUE
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
      }
    }
  } else {
    invalid <- TRUE
    if (raise) {
      stop(paste(param_nm, " expected a list. Got ", toString(typeof(list_)), sep = ""))
    }
      }
  return (invalid)
}
