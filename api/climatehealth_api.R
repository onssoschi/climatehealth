library(climatehealth)
library(plumber)

#* @apiTitle Climatehealth regression
#* @apiDescription An API that computes Quasi-Poission regression
#* @get /regression_results
#* @param input_csv_path_ Path to a CSV contain
#* daily time series of death and temperature per region.
#* @param output_folder_path_ Path to folder for storing outputs.
#* @param save_fig_ Boolean (TRUE or FALSE). Whether to save output figure.
#* @param save_csv_ Boolean (TRUE or FALSE). Whether to save output CSVs.
#* @param meta_analysis Boolean (TRUE or FALSE). Whether to include
#* meta-analysis. Must be TRUE if by_region argument is FALSE.
#* @param by_region Boolean (TRUE or FALSE). Whether to disaggregate by region.
#* Must be TRUE if meta-analysis is FALSE.
#* @param time_range_ Time range over which to run the analysis.
#* @param dependent_col_ the column name of the
#* dependent variable of interest e.g. deaths
#* @param indepedent_col_ column names of independent
#* variables to include in regression (excluding temperature,
#* see config file for formula structure)
#* @param time_col_ The column name of column containing dates (e.g date, year).
#* @param region_col_ The column name of the column containing regions.
#* @param temp_col_ the column name of the column containing the exposure.
#* @param varfun_ Exposure function
#* (see dlnm::crossbasis)
#* @param varper_ Internal knot positions in exposure function
#* (see dlnm::crossbasis)
#* @param vardegree_ Degrees of freedom in exposure function
#* (see dlnm:crossbasis)
#* @param lag_ Lag length in time
#* (see dlnm::logknots)
#* @param lagnk_ Number of knots in lag function
#* (see dlnm::logknots)
#* @param dfseas_ Degrees of freedom for seasonality

climatehealth_func <- climatehealth::do_analysis

