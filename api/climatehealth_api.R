library(plumber)
library(climatehealth)

#* @apiTitle Climatehealth regression
#* @apiDescription An API that computes Quasi-Poission regression
#* @post /regression
#* @param input_csv_path_:str Path to a CSV contain
#* daily time series of death and temperature per region.
#' @param output_folder_path_:str Path to folder for storing outputs.
#' @param save_fig_:str Boolean (TRUE or FALSE). Whether to save output figure.
#' @param save_csv_:str Boolean (TRUE or FALSE). Whether to save output CSVs.
#' @param meta_analysis:bool Boolean (TRUE or FALSE). Whether to include
#' meta-analysis. Must be TRUE if by_region argument is FALSE.
#' @param by_region:bool Boolean (TRUE or FALSE). Whether to disaggregate by region.
#' Must be TRUE if meta-analysis is FALSE.
#' @param time_range_start_:str Start of time range over which to run the analysis.  'NONE' if over full range.
#' @param time_range_end_:str End of time range over which to run the analysis.  'NONE' if over full range.
#' @param dependent_col_:str the column name of the
#' dependent variable of interest e.g. deaths
#' @param indepedent_col1_:str column name of first extra independent
#' variable to include in regression (excluding temperature,
#' see config file for formula structure). 'None' if none.
#' @param indepedent_col2_:str column name of second independent
#' variable to include in regression (excluding temperature,
#' see config file for formula structure). 'None' if none.
#' @param indepedent_col3_:str column name of third independent
#' variable to include in regression (excluding temperature,
#' see config file for formula structure). 'None' if none.
#' @param time_col_:str The column name of column containing dates (e.g date, year).
#' @param region_col_:str The column name of the column containing regions.
#' @param temp_col_:str the column name of the column containing the exposure.
#' @param varfun_:str Exposure function
#' (see dlnm::crossbasis)
#' @param vardegree_:int Degrees of freedom in exposure function
#' (see dlnm:crossbasis)
#' @param lag_:int Lag length in time
#' (see dlnm::logknots)

climatehealth_func <- climatehealth::do_analysis
