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

do_analysis(config$input_csv_path, config$output_folder_path, meta_analysis = FALSE)
