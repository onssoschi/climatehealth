devtools::load_all()
library(dlnm)
library(mvmeta)
library(splines)
library(tsModel)
library(config)
library(zeallot)

# 1. Load data, get metadata, define model, run model
do_analysis(config$input_csv_path, config$output_csv_path)
