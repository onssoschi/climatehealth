# Run lines below to run API server
library(plumber)

plumber::pr("api/climatehealth_api.R") %>%
  plumber::pr_run(port=8000)
