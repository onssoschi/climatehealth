setwd("C:/My_RStudio/Workspace/Git projects/climatehealth")
devtools::load_all()
library(dlnm)
library(mvmeta)
library(splines)
library(tsModel)
library(config)
library(zeallot)
library(climatehealth)

###############################################################################
# IMPORTANT: Before running this script update the following in config.yml:   #
#             input_csv_path [input dataset]                                  #
#             output_folder_path [folder that stores data and figure outputs] #
###############################################################################
config <- config::get()

c(output_df, temp_df, anregions_publication, antot_bind, arregions_publication, artot_bind) %<-%
  climatehealth::do_analysis(input_csv_path_ = config$input_csv_path,
                             output_folder_path_ = config$output_folder_path,
                             save_fig_ = config$save_fig,
                             save_csv_ = config$save_csv,
                             meta_analysis_ = config$meta_analysis,
                             by_region_ = config$by_region,
                             RR_distribution_length_ = config$RR_distribution_length,
                             output_year_ = config$output_year,
                             dependent_col_ = config$dependent_col,
                             independent_col1_ = config$independent_col1,
                             independent_col2_ = config$independent_col2,
                             independent_col3_ = config$independent_col3,
                             independent_col4_ = config$independent_col4,
                             time_col_ = config$time_col,
                             region_col_ = config$region_col,
                             temp_col_ = config$temp_col,
                             population_col_ = config$population_col,
                             varfun_ = config$varfun,
                             vardegree_ = config$vardegree,
                             lag_ = config$lag,
                             lagnk_ = config$lagnk,
                             dfseas_ = config$dfseas
                             )

