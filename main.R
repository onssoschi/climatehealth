library(dlnm)
library(mvmeta)
library(splines)
library(tsModel)
library(config)
library(zeallot)

# 1. Load parameters from config file --------------------------------------------------

# Load config file
config <- config::get()

# Input data
input_csv_path <- config$input_csv_path

# Output data
output_csv_path <- config$output_csv_path

# Specification of the exposure function
varfun <- config$varfun
vardegree <- config$vardegree
varper <- c(10,75,90)

# Specification of the lag function
lag <- config$lag
lagnk <- config$lagnk

# Degree of freedom for seasonality
dfseas <- config$dfseas

# 1. Load parameters from config file --------------------------------------------------
