library(dlnm)
library(mvmeta)
library(splines)
library(tsModel)
library(config)
library(zeallot)
library(indicator_functions)

# 1. Load data, get metadata, define model, run model
indicator_functions::run_all()

# 2. Meta-analysis
# code

# 3. Attributable deaths
c(totdeath, arraysim, matsim) %<-%
  indicator_functions::compute_attributable_deaths(
  dlist, cities, coef, vcov, varfun, argvar, bvar, blup, mintempcity
  )

# 3. Plot data
indicator_functions::plot_outputs(dlist, argvar, bvar,
                                  blup, cities, mintempcity,
                                  output_folder_path = config$output_folder_path
                                  )

# 3. Write data
indicator_functions::write_outputs(cities, matsim, arraysim,
                                   totdeath,
                                   output_folder_path = config$output_folder_path
                                   )





