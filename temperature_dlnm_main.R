devtools::load_all()
library(dlnm)
library(mvmeta)
library(splines)
library(tsModel)
library(config)
library(zeallot)

###############################################################################
# IMPORTANT: Before running this script update the following in config.yml:   #
#             input_csv_path [input dataset]                                  #
#             output_folder_path [folder that stores data and figure outputs] #
###############################################################################

c(output_df, tmean_df, anregions_bind, attrdl_yr_all,
  attr_fractions_yr) %<-%
  do_analysis(input_csv_path_ = config$input_csv_path,
            output_folder_path_ = config$output_folder_path,
            save_fig_ = config$save_fig,
            save_csv_ = config$save_csv,
            meta_analysis = config$meta_analysis,
            by_region = config$by_region,
            time_range_ = config$time_range,
            dependent_col_ = config$dependent_col,
            independent_col_ = config$independent_col,
            time_col_ = config$time_col,
            region_col_ = config$region_col,
            temp_col_ = config$temp_col,
            varfun_ = config$varfun,
            varper_ = config$varper,
            vardegree_ = config$vardegree,
            lag_ = config$lag,
            lagnk_ = config$lagnk,
            dfseas_ = config$dfseas
            )


