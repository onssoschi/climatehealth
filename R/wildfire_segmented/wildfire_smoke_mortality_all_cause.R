# Load required libraries
library(logger)
library(zeallot)

# Load local scripts
source("scripts/config_utils.R")

# Setup Logger
log_threshold(INFO)
log_appender(appender_console)

# Store pipeline start time
start_time <- Sys.time()

# Read in config file
log_info("Reading in the wilfire pipeline config...")
config <- read_config("wildfire_config")

# Configure developer options
dev_config <- read_config("developer_config")

if (tolower(dev_config$log_level) == "debug"){
  log_threshold(DEBUG)
}

if (dev_config$save_logs == TRUE) {
  # Parse the current run ID for logging
  run_id <- get_current_run_id(dev_config$logs_folder, "wildfire")
  logs_path <- paste(dev_config$logs_folder, "wildfire_", run_id, ".log", sep="")
  # Set up the log file to write to
  log_appender(appender_tee(logs_path))
  log_info(paste("Writing logs to path:", logs_path))
} else {
  log_info("Skipping writing logs to a file since save_logs==FALSE.")
}

# Load the climatehealth package
load_climatehealth(
  use_library = dev_config$use_library, path = dev_config$climatehealth_path
)

# Begin pipeline
path_config <- config$paths
model_config <- config$configuration
output_config <- config$output
log_info("Pipeline config successfully read.")
log_debug(glue::glue("Found {length(path_config)} config items for 'path'."))
log_debug(glue::glue("Found {length(model_config)} config items for 'configuration'."))
log_debug(glue::glue("Found {length(output_config)} config items for 'output'."))

# Check expected outputs
# Checks if the relative risk is split by region. This can be updated in wildfire_config
RR_by_region <- model_config$relative_risk_by_region
output_AF_AN <- model_config$output_AF_AN


if (identical(RR_by_region, FALSE) && isTrue(output_AF_AN)){
  output_AF_AN <- FALSE
  log_info("relative_risk_by_region must be TRUE to calculate AF and AN")
}


log_info("Loading wildfire data...")
if (model_config$join_wildfire_data) {
  log_info("Skipping NetCDF and SHP file joins since join_wildfire_data==TRUE...")
}
data <- load_wildfire_data(health_path = path_config$health_path,
                           join_wildfire_data = model_config$join_wildfire_data,
                           ncdf_path = path_config$nc_path,
                           shp_path = path_config$shp_path,
                           date_col = model_config$date_col,
                           region_col = model_config$region_col,
                           mean_temperature_col = model_config$mean_temperature_col,
                           #rh_col = model_config$rh_col,
                           #wind_speed_col = model_config$wind_speed_col,
                           health_outcome_col = model_config$health_outcome_col,
                           pm_2_5_col = model_config$pm_2_5_col)



log_debug(paste("Wildfire data size:", dim(data)[1], dim(data)[2]))
log_info("Successfully loaded wildfire data.")

log_info("Creating lagged variables for wildfire data...")
data <- create_lagged_variables(data = data,
                                wildfire_lag = model_config$wildfire_lag,
                                temperature_lag = model_config$temperature_lag)
log_info("Lagged variables created.")

log_info("Creating temperature splines for wildfire data...")
data <- create_temperature_splines(data = data,
                                   temperature_lag = model_config$spline_temperature_lag,
                                   degrees_freedom = model_config$spline_temperature_degrees_freedom)
log_info("Temperature splines created...")

log_info("Stratifying data by region and time period...")
data <- time_stratify(data = data)
log_info("Additional columns created.")

log_info("Checking variance inflation factors...")
tryCatch(
  expr = {
    vif_mod <- check_vif(
      data = data,
      predictors = model_config$predictors_vif,
      print_vif = output_config$print_vif
    )
  },
  warning = function(msg) {
    log_warn(as.character(conditionMessage(msg)))
  }
)

cat("VIF for model:", round(vif_mod,2), "\n")
str(vif_mod)
log_info("VIF check complete.")


log_info("Fitting wildfire model and obtaining relative risk results...")
relative_risk_results <- relative_risk_by_region(data = data,
                         scale_factor = model_config$scale_factor_wildfire_pm,
                         wildfire_lag = model_config$wildfire_lag,
                         calc_relative_risk_by_region = model_config$relative_risk_by_region,
                         save_fig = output_config$save_fig,
                         output_folder_path = path_config$output_folder_path,
                         print_model_summaries = output_config$print_model_summaries)

write.csv(relative_risk_results,
          file = file.path(path_config$output_folder_path, "relative_risk_results_by_region.csv"),
          row.names = FALSE)
log_info("Relative risk results successfully calculated.")


#QAIC calculation
log_info("Starting QAIC calculation...")
#Compute QAIC
qaic_val <- calculate_qaic(data = data,
                           wildfire_lag = 3)
print(qaic_val)
log_info("QAIC calculation complete")

log_info("Calculating Relative risk overall...")
relative_risk_overall <- relative_risk_by_region(data = data,
                                            scale_factor = model_config$scale_factor_wildfire_pm,
                                            wildfire_lag = model_config$wildfire_lag,
                                            calc_relative_risk_by_region = FALSE,
                                            save_fig = output_config$save_fig,
                                            output_folder_path = path_config$output_folder_path,
                                            print_model_summaries = output_config$print_model_summaries)


write.csv(relative_risk_overall,
          file = file.path(path_config$output_folder_path, "rr_results.csv"),
          row.names = FALSE)
log_info("Relative risk overall calculations done")

rr_pm_region_all <- generate_rr_pm_overall(relative_risk_overall,
                                           scale_factor_wildfire_pm = 10,
                                           wildfire_lag = 0)


write.csv(rr_pm_region_all,
          file = file.path(path_config$output_folder_path, "rr_pm_all.csv"),
          row.names = FALSE)
log_info("Relative risk results as PM2.5 levels changes done")

log_info("Calculating daily attributable number and attributable fractions...")
daily_AF_AN <- calculate_daily_AF_AN(data = data,
                              rr_data = relative_risk_results)
log_info("Daily attributable number and attributable fraction calculated.")


log_info("Summarising AN and AF data...")
af_an_results <- summarise_AF_AN(daily_AF_AN)
log_info("Attributable fractions and numbers summary created.")

log_info("Plotting results...")
if (output_config$save_fig) {
  log_info(paste0("Saving plots of results to ", path_config$output_folder_path), "...")
}
plot_RR_by_region(results = relative_risk_results,
                   output_folder_path = path_config$output_folder_path,
                   wildfire_lag = model_config$wildfire_lag,
                   save_fig = output_config$save_fig,
                   relative_risk_by_region = model_config$relative_risk_by_region)
log_info("Results successfully plotted.")


if (output_config$save_csv == TRUE) {
  log_info(paste0("Writing results as a csv file in ", path_config$output_folder_path, "..."))
  save_results(rr_results = relative_risk_results,
               an_ar_results = af_an_results,
               output_folder_path = path_config$output_folder_path)
  log_info("Results written to csv.")
}

time_taken <- difftime(Sys.time(), start_time, units = "secs")
log_success("Wildfire pipeline completed in: {round(time_taken, 2)}s")


plot_RR_overall <- function(results,
                            save_fig = FALSE,
                            wildfire_lag = 3,
                            relative_risk_by_region = FALSE,
                            output_folder_path = NULL){
  if (save_fig && is.null(output_folder_path)) {
    save_fig = FALSE
    warning("Unable to output wildfire RR plots as no output folder path was specified.")
  }
  plot <- plot_RR(results = results,
                  output_folder_path = output_folder_path,
                  wildfire_lag = wildfire_lag,
                  save_fig = save_fig)
  print(plot)
  return(plot)
}



plot_RR_overall(results = relative_risk_overall,
                output_folder_path = path_config$output_folder_path,
                wildfire_lag = model_config$wildfire_lag,
                save_fig = output_config$save_fig,
                relative_risk_by_region = model_config$relative_risk_by_region)
log_info("Results overall successfully plotted.")


# Specify the path where you want to save the CSV file
#save_path <- "data/inputs/input_data_england_wales_card_withPM.csv"

# Save the data to CSV
#write.csv(data, file = save_path, row.names = FALSE)
