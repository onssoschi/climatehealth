#-------------------------------------------------------------------------------
#' @title R-code for Diarrhea disease cases attributable to extreme precipitation
#' and extreme temperature
#-------------------------------------------------------------------------------


#' Read in and format country Map data
#'
#' @description: Read in a map shape file data, rename columns and create the
#' adjacency matrix for spatiotemporal analysis.
#'
#' @param map_path is the path to the country's map shape file "sf" data
#' @param Region_col is the region column in the country's map shape file data
#' @param District_col is the district column in the map data
#' @param geometry_col is the Name of the geometry column in the shapefile
#' (usually "geometry").
#' @param output_dir Path to folder where the process map data should be
#' saved.
#'
#' @return list includes processed map, and the adjacent matrix created.

load_and_process_map <- function(map_path,
                                 Region_col,
                                 District_col,
                                 geometry_col,
                                 output_dir = NULL){
  # Validate output_dir
  if (is.null(output_dir)) {
    stop("output_dir must be provided to save the map.graph file.")
  }
  # Load and process map data
  map <- read_sf(map_path) %>%
    select(Region = Region_col,
           District = District_col,
           geometry = geometry_col)

  # Create adjacency matrix
  if (!file.exists(file.path(output_dir, "nbfile"))){
    nb.map <- poly2nb(as_Spatial(map$geometry))
    write.nb.gal(nb.map, file = file.path(output_dir, "nbfile"))
  } else {
    nb.map <- read.gal(file.path(output_dir, "nbfile"))
  }

  g.file <- file.path(output_dir, "map.graph")
  if (!file.exists(g.file)) {
    nb2INLA(g.file, nb.map)
  }

  return(list(map = map, nb.map = nb.map))
}


#' Read in and format health data - Diarrhea diseases case
#'
#' @description Read in a csv file of monthly time series of health and the
#' population data, rename columns and create time variables for spatiotemporal
#' analysis
#'
#' @param data_path Path to a csv file containing a monthly time series of data
#' for Diarrhea outcome, which may be disaggregated by sex (under five case or
#' above five case), and by Region and District.
#' @param Region_col Character. Name of the column in the dataframe that contains
#' the region names.
#' @param District_col Character. Name of the column in the dataframe that
#' contains the region names.
#' @param Date_col Character. Name of the column in the dataframe that contains
#' the date. Defaults to NULL.
#' @param Year_col Character. Name of the column in the dataframe that contains
#' the Year.
#' @param Month_col Character. Name of the column in the dataframe that contains
#' the Month.
#' @param Diarrhea_case_col Character. Name of the column in the dataframe
#' that contains the Diarrhea cases to be considered.
#' @param tot_pop_col Character. Name of the column in the dataframe that contains
#' the total population.
#' @param Male_counts_pop Character. Name of the column in the dataframe that contains
#' the Male total population. Defaults to NULL.
#' @param Female_counts_pop Character. Name of the column in the dataframe that contains
#' the date. Defaults to NULL.
#'
#' @return A dataframe with formatted and renamed columns.

load_and_process_data <- function(data_path,
                                  Region_col,
                                  District_col,
                                  Date_col = NULL,
                                  Year_col,
                                  Month_col,
                                  Diarrhea_case_col,
                                  tot_pop_col,
                                  Male_counts_pop = NULL,
                                  Female_counts_pop = NULL) {
  # Load health and climate data
  data <- read.csv(data_path)

  # check if the column of dates exists otherwise check for year and months
  if (!is.null(Date_col)){
    if (!"Year" %in% names(data)){
      data  <- data %>%
        mutate(Year = year(Date_col))
    } else if (!"Month" %in% names(data)){
      data  <- data %>%
        mutate(Month = month(Date_col))
    } else {
      stop("Neither the Date_col, Month_col and Year_col do not exist in the data.")
    }
  }

  data <- data %>%
    select(Region = Region_col, District = District_col, Year, Month,
           Diarrhea = Diarrhea_case_col,
           tot_pop = tot_pop_col,
           Male_counts_pop = Male_counts_pop,
           Female_counts_pop = Female_counts_pop
    )
  return(data)
}


#' Read in and format climate data
#'
#' @description Read in a csv file of monthly time series of climate data, rename
#' columns and create lag variable for spatiotemporal and DLNM analysis. The
#' climate data should start a Year before a start year in the health data to
#' allow the lag variables calculation.
#'
#' @param data_path Path to a csv file containing a monthly time series of data
#' for climate variables, which may be disaggregated by district.
#' @param District_col Character. Name of the column in the dataframe that
#' contains the region names.
#' @param Year_col Character. Name of the column in the dataframe that
#' contains the Year.
#' @param Month_col Character. Name of the column in the dataframe that
#' contains the Month.
#' @param tmin_col Character. Name of the column in the dataframe that
#' contains the minimum temperature data.
#' @param tmean_col Character. Name of the column in the dataframe that
#' contains the average temperature.
#' @param tmax_col Character. Name of the column in the dataframe that
#' contains the maximum temperature.
#' @param rainfall_col Character. Name of the column in the dataframe that
#' contains the cumulative monthly rainfall.
#' @param r_humidity_col Character. Name of the column in the dataframe that
#' contains the relative humidity.
#' @param spi_col Character. Name of the column in the dataframe that
#' contains the standardized precipitation index. Defaults to NULL.
#' @param max_lag Character. Number corresponding to the maximum lag to be
#' considered for the delay effect. It should be between 2 an 4. Defaults to 2
#'
#' @return climate dataframe with formatted and renamed columns, and the lag
#' variables

load_and_process_climatedata <- function(data_path,
                                         District_col,
                                         Year_col,
                                         Month_col,
                                         tmin_col,
                                         tmean_col,
                                         tmax_col,
                                         rainfall_col,
                                         r_humidity_col,
                                         spi_col = NULL,
                                         max_lag = 2){

  climate_data <- read.csv(data_path) %>%
    select(District = District_col, Year = Year_col, Month = Month_col,
           tmin = tmin_col, tmean = tmean_col, tmax = tmax_col,
           rainfall = rainfall_col, r_humidity = r_humidity_col, spi = spi_col)

  # lagged data for tmin
  tmin_data <- climate_data %>% select(tmin)
  tmean_data <- climate_data %>% select(tmean)
  tmax_data <- climate_data %>% select(tmax)
  rf_data <- climate_data %>% select(rainfall)
  rh_data <- climate_data %>% select(r_humidity)

  for (i in 1:max_lag){
    tmin_data[paste0("tmin_lag", i)] <- lag(tmin_data$tmin, n = i, default = NA)
    tmean_data[paste0("tmean_lag", i)] <- lag(tmean_data$tmean, n = i, default = NA)
    tmax_data[paste0("tmax_lag", i)] <- lag(tmax_data$tmax, n = i, default = NA)
    rf_data[paste0("rf_lag", i)] <- lag(rf_data$rainfall, n = i, default = NA)
    rh_data[paste0("rh_lag", i)] <- lag(rh_data$r_humidity, n = i, default = NA)
  }

  climate_data <- bind_cols(climate_data %>% select(District, Year, Month),
                            tmin_data, tmean_data,
                            tmax_data, rf_data, rh_data)
  return(climate_data)
}


#' Read in combine climate and health data
#'
#' @description Read in a combine climate and health data prepared for the
#' spatiotemporal and DLNM analysis.
#'
#' @param health_data_path A data frame containing the processed health data
#' @param climate_data_path A data frame containing the processed climate data
#' @param map_path A data frame containing the processed map data
#' @param Region_col Character. Name of the column in the dataframe that contains
#' the region names.
#' @param District_col Character. Name of the column in the dataframe that
#' contains the region names.
#' @param Date_col Character. Name of the column in the dataframe that contains
#' the date. Defaults to NULL.
#' @param Year_col Character. Name of the column in the dataframe that contains
#' the Year.
#' @param Month_col Character. Name of the column in the dataframe that contains
#' the Month.
#' @param Diarrhea_case_col Character. Name of the column in the dataframe
#' that contains the Diarrhea cases to be considered.
#' @param tot_pop_col Character. Name of the column in the dataframe that
#' contains the total population.
#' @param Male_counts_pop Character. Name of the column in the dataframe that
#' contains the Male total population. Defaults to NULL.
#' @param Female_counts_pop Character. Name of the column in the dataframe that
#' contains the date. Defaults to NULL.
#' @param tmin_col Character. Name of the column in the dataframe that
#' contains the minimum temperature data.
#' @param tmean_col Character. Name of the column in the dataframe that
#' contains the average temperature.
#' @param tmax_col Character. Name of the column in the dataframe that
#' contains the maximum temperature.
#' @param rainfall_col Character. Name of the column in the dataframe that
#' contains the cumulative monthly rainfall.
#' @param r_humidity_col Character. Name of the column in the dataframe that
#' contains the relative humidity.
#' @param spi_col Character. Name of the column in the dataframe that
#' contains the standardized precipitation index. Defaults to NULL.
#' @param max_lag Character. Number corresponding to the maximum lag to be
#' considered for the delay effect. It should be between 2 an 4. Defaults to 2
#' @param geometry_col is the Name of the geometry column in the shapefile
#' (usually "geometry").
#' @param output_dir Path to folder where the process map data should be
#' saved.
#'
#' @returns list of dataframes for the map, nb.map, data, grid_data, summary

combine_health_climate_data <- function(health_data_path,
                                        climate_data_path,
                                        map_path,
                                        Region_col,
                                        District_col,
                                        Date_col,
                                        Year_col,
                                        Month_col,
                                        Diarrhea_case_col,
                                        tot_pop_col,
                                        Male_counts_pop = NULL,
                                        Female_counts_pop = NULL,
                                        tmin_col,
                                        tmean_col,
                                        tmax_col,
                                        rainfall_col,
                                        r_humidity_col,
                                        spi_col = NULL,
                                        max_lag = 2,
                                        geometry_col,
                                        output_dir = NULL
){

  health_data <- load_and_process_data(health_data_path, Region_col,
                                       District_col, Date_col,
                                       Year_col, Month_col,
                                       Diarrhea_case_col, tot_pop_col,
                                       Male_counts_pop, Female_counts_pop
  )
  climate_data <- load_and_process_climatedata(climate_data_path, District_col,
                                               Year_col, Month_col, tmin_col,
                                               tmean_col, tmax_col, rainfall_col,
                                               r_humidity_col,
                                               max_lag)
  map_data <- load_and_process_map(map_path, Region_col, District_col,
                                   geometry_col, output_dir)

  data <- health_data %>%
    left_join(climate_data, by = join_by(District, Year, Month)) %>%
    group_by(Region, District) %>%
    unique() %>%
    mutate(time = (Year - min(Year)) * 12 + Month) %>%
    ungroup()

  # Generate region and district codes
  grid_data <- data %>%
    select(Region, District) %>%
    distinct() %>%
    group_by(Region) %>%
    mutate(region_code = cur_group_id(),
           district_number = row_number(),
           district_code = sprintf("%d%01d", region_code, district_number)) %>%
    ungroup()

  # Merge grid data with the main dataset and map
  data <- data %>%
    left_join(grid_data, by = c("Region", "District")) %>%
    arrange(region_code, district_code)

  map <- map_data$map %>%
    left_join(grid_data, by = c("Region", "District")) %>%
    arrange(region_code, district_code)

  grid_data <- grid_data %>%
    rename(name = Region, Code_num = region_code)

  # descriptive statistics
  summary_stats <- list(
    tmin = summary(data$tmin),
    tmax = summary(data$tmax),
    rainfall = summary(data$rainfall),
    rhumidity = summary(data$r_humidity)
  )

  # Return processed data as a list
  return(list(map = map,  nb.map = map_data$nb.map, data = data,
              grid_data = grid_data, summary = summary_stats))
}


#' Read in cross-basis matrix set for DLNM analysis
#'
#' @description creates cross-basis matrix for each climate variables
#'
#' @param data is the dataset containing district_code, region_code, and Year
#' columns from the combine_health_climate_data() function.
#'
#' @return list of cross-basis matrices including the basis matrix for maximum
#' temperature, minimun temperature, cumulative rainfall, and relative humidity.


set_cross_basis <- function(data) {

  nlag <- (data %>% select(contains("tmax")) %>% ncol()) - 1

  # Define lag knots
  lagknot <- equalknots(0:nlag, 2)

  # Tmax
  vars <- list(tmax = data %>% select("tmax", paste0("tmax_lag", 1:nlag)),
               tmin = data %>% select("tmin", paste0("tmin_lag", 1:nlag)),
               tmean = data %>% select("tmean", paste0("tmean_lag", 1:nlag)),
               rainfall = data %>% select("rainfall", paste0("rf_lag", 1:nlag)),
               r_humidity = data %>% select("r_humidity", paste0("rh_lag", 1:nlag)))

  # list of basis matrices
  basis_matrices <- vars %>%
    lapply(function(var_){
      cb <- crossbasis(var_,
                       argvar = list(fun = "ns", knots = equalknots(var_[1], 2)),
                       arglag = list(fun = "ns", knots = nlag/2))

      colnames(cb) <- paste0(paste0("basis_", names(var_)[1], "."), colnames(cb))
      return(cb)
    })

  # Return a list of cross-basis matrices
  return(basis_matrices)
}


#' create indices for INLA models
#'
#' @description: for the INLA model, there is a need to set-up regions index,
#' district index, and year index. This function create these indices using the
#' dataset, ndistrict and nregion defined above.
#'
#' @param data is the dataset containing district_code, region_code, and Year
#' columns from the combine_health_climate_data() function.
#'
#' @returns the modified data with the created indices

create_inla_indices <- function(data) {
  ntime <- length(unique(data$time))       # Total number of months
  nyear <- length(unique(data$Year))       # Total number of years
  ndistrict <- length(unique(data$district_code))  # Total number of districts
  nregion <- length(unique(data$region_code))  # Total number of regions

  # define the offset variable based on the population data
  data$E <- data$tot_pop/10^5
  data$SIR <- data$Diarrhea / data$E  # Standardized Incidence Ratio

  # Create district index
  data$district_index <- rep(1:ndistrict, length.out = nrow(data))  # Ensure correct length

  # Assign district indices based on unique district codes
  unique_districts <- unique(data$district_code)
  for (j in 1:ndistrict) {
    data$district_index[data$district_code == unique_districts[j]] <- j
  }

  # Create region index
  data$region_index <- NA  # Initialize

  # Assign region indices based on unique region codes
  unique_regions <- unique(data$region_code)
  for (j in 1:nregion) {
    data$region_index[data$region_code == unique_regions[j]] <- j
  }

  # Create year index (first_year is the First year in the data set, is set to 1)
  data$year_index <- data$Year - (min(data$Year)-1)

  return(data)
}


#' Function to fit an INLA model
#'
#' @description fit_inla_model() function fits an INLA (Integrated Nested Laplace
#' Approximation) model using the prepared dataset
#'
#' @param data is the dataset containing INLA indices.
#' @param formula is a model defining predictors and random effects.
#' @param family A character string specifying the probability distribution for
#' the response variable. Default is "poisson". The user may also have the
#' possibility to choose "nbinomial" for a negative binomial distribution.
#' @param config is a Boolean flag to enable additional model configurations.
#'
#' @return the fitted INLA model

fit_inla_model <- function(data,
                           formula,
                           family = "poisson",
                           config = FALSE) {

  # Fit model using INLA
  model <- inla(
    formula = formula,
    data = data,
    family = family,
    offset = log(data$E),
    control.inla = list(strategy = 'adaptive'),
    control.compute = list(dic = TRUE, config = config,
                           cpo = TRUE, return.marginals = FALSE),
    control.fixed = list(correlation.matrix = TRUE,
                         prec.intercept = 1, prec = 1),
    control.predictor = list(link = 1, compute = TRUE),
    verbose = FALSE
  )

  # Re-run the model for better estimates
  model <- inla.rerun(model)

  return(model)
}


#' Run models of increasing complexity in INLA: Fit a baseline model including
#' spatiotemporal random effects.
#'
#' @description: create and run multiple INLA (Integrated Nested
#' Laplace Approximation) models to the dataset, evaluates them using
#' DIC (Deviance Information Criterion), and identifies the best-fitting model.
#'
#' @param data is the dataset coming from combine_health_climate_data() function.
#' @param basis_matrices_choices A character vector specifying the basis matrix
#' parameters to be included in the model. Possible values are "tmax", "tmin",
#' "rainfall", "r_humidity", and "spi".  Default to "tmax", and users can select
#' one or more of these parameters for inclusion in the model.
#' @param output_dir is the path to save model output
#' @param save_csv Boolean. Whether to save the results as a CSV. Defaults to
#' FALSE.
#'
#' @returns list of the model, the baseline_model, and the dic_table.


run_inla_models <- function(data,
                            basis_matrices_choices= c("tmax"),
                            output_dir = NULL,
                            save_csv = FALSE) {
  # Validate output_dir if saving
  if (save_csv && is.null(output_dir)) {
    stop("output_dir must be provided if save_fig = TRUE")
  }

  # getting inla indices
  data <- create_inla_indices(data)

  # Define priors for INLA model
  precision_prior <- list(prec = list(prior = "pc.prec", param = c(0.5, 0.01)))

  # Define baseline model formula
  baseformula <- Diarrhea ~ 1 +
    f(Month, replicate = region_index, model = "rw1", cyclic = TRUE, constr = TRUE,
      scale.model = TRUE, hyper = precision_prior) +
    f(district_index, model = "bym2", replicate = year_index,
      graph = file.path(output_dir, "map.graph"),
      scale.model = TRUE, hyper = precision_prior)

  # the basis matrices
  basis_matrices <- set_cross_basis(data)

  # Define extended formulas
  formula <- update.formula(baseformula,
                            as.formula(paste0("~ . + ",
                                              paste0("basis_matrices$",
                                                     basis_matrices_choices,
                                                     collapse = " + "))))

  baseline_model <- fit_inla_model(baseformula, data = as.data.frame(data))
  model <- fit_inla_model(formula, data = as.data.frame(data))

  # Save model if requested
  param_model_name <- paste0(basis_matrices_choices, collapse = "_")
  if (save_csv) {
    model_name <- paste0("model_with_", param_model_name, ".csv")
    save(model, file = file.path(output_dir, model_name))
  } else {
    model_name <- NULL
  }

  # Create a table to store DIC values
  table0 <- data.table(Model = paste0(basis_matrices_choices, collapse = " + "),
                       DIC = NA, logscore = NA)

  # Populate DIC values from saved models
  table0$DIC <- round(model$dic$dic, 0)
  table0$logscore <- round(-mean(log(model$cpo$cpo), na.rm = TRUE), 3)

  return(list(model = model, baseline_model = baseline_model, dic_table = table0))
}


#' Visualise monthly random effects for selected INLA model
#'
#' @description generates and saves a plot of monthly random effects for different
#' regions, visualizing their contribution to Diarrhea Incidence Rate.
#'
#' @param combined_data Data list from combine_health_climate_data() function.
#' @param model The fitted model object
#' @param save_fig Boolean. Whether to save the plot as an output. Defaults to
#' FALSE.
#' @param output_dir is the path to save model output.
#'
#' @return monthly random effects plot.

plot_monthly_random_effects <- function(combined_data,
                                        save_fig = FALSE,
                                        model,
                                        output_dir= NULL) {
  # Validate output_dir if saving
  if (save_fig & is.null(output_dir)) {
    stop("output_dir must be provided if save_fig = TRUE")
  }

  data <- combined_data$data
  grid_data <- combined_data$grid_data
  map <- combined_data$map

  model <- model$model

  # Create data frame for monthly random effects per region
  month_effects <- data.frame(region_code = rep(unique(data$region_code), each = 12),
                              Month = model$summary.random$Month)

  # Merge with predefined state grid
  month_effects <- month_effects %>%
    left_join(grid_data %>% select(-District, -district_code) %>% unique(),
              by = c("region_code" = "Code_num"))

  month_effects <- map %>% select(-District) %>% unique() %>%
    left_join(month_effects, by = c("Region" = "name"))

  # Generate plot
  p <- month_effects %>%
    ggplot() +
    geom_ribbon(aes(x = Month.ID, ymin = `Month.0.025quant`, ymax = `Month.0.975quant`),
                fill = "cadetblue4", alpha = 0.5) +
    geom_line(aes(x = Month.ID, y = Month.mean), col = "cadetblue4") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
    xlab("Month") +
    ylab("Contribution to log(DIR)") +
    scale_y_continuous() +
    scale_x_continuous(breaks = c(1,4,7,10), labels = c("Jan", "Apr", "Jul", "Oct")) +
    theme_bw() +
    facet_wrap(~Region)

  # Save plot
  if (save_fig){
    ggsave(file.path(output_dir), plot = p, height = 30, width = 25, units = "cm")
  }
  return(p)
}


#' Visualize yearly spatial random effect
#'
#' @description generates and saves plots of yearly spatial random effect at
#' District level.
#'
#' @param combined_data Data list from combine_health_climate_data() function.
#' @param model The fitted model from run_inla_models() function.
#' @param save_fig Boolean. Whether to save the plot as an output. Defaults to
#' FALSE.
#' @param output_dir Path to save model output
#'
#' @return yearly space random effect plot

yearly_spatial_random_effect <- function(combined_data,
                                         model,
                                         save_fig = FALSE,
                                         output_dir= NULL){
  # Validate output_dir if saving
  if (save_fig && is.null(output_dir)) {
    stop("output_dir must be provided if save_fig = TRUE")
  }
  # make maps of spatial random effects per year (Appendix Fig S8)
  # extract posterior mean estimates for combined unstructured and structured random effects
  data <- create_inla_indices(combined_data$data)
  grid_data <- combined_data$grid_data
  map <- combined_data$map

  model <- model$model

  ntime <- length(unique(data$time))       # Total number of months
  nyear <- length(unique(data$Year))       # Total number of years
  ndistrict <- length(unique(data$district_code))  # Total number of districts
  nregion <- length(unique(data$region_code))  # Total number of regions

  space <- data.table(model$summary.random$district_index)
  space$Year <- rep(min(data$Year):max(data$Year), each = 2*ndistrict)
  space$re <- rep(c(rep(1,ndistrict),rep(2,ndistrict)),nyear)
  space <- space[space$re == 1,]
  space$District_code <- rep(unique(data$district_code), nyear)
  mn <- min(space$mean)
  mx <- max(space$mean)

  # Add the map geometry to the space dataframe
  space <- left_join(map, space, by = c("district_code" = "District_code"))
  space_effects <- ggplot() +
    geom_sf(data = space, aes(fill = mean), lwd = 0, color = NA) +
    scale_fill_distiller(palette = "PRGn", direction = -1,
                         limits = c(min(mn,-mx),max(mx,-mn))) +
    labs(fill = "Contribution to \n log(DIR)") +
    theme_void() +
    facet_wrap(~space$Year, ncol = 5)

  if (save_fig){
    ggsave(file.path(output_dir),
           plot = space_effects, height = 30, width = 25, units = "cm")
  }

  return(space_effects)
}


#-------------------------------------------------------------------------------
# DNLM FUNCTION FOR RELATIVE RISK PLOT
#-------------------------------------------------------------------------------


#' Create function to predict relative risk at Country, Region, and District level
#'
#' @description Produces cumulative relative risk at country, region and
#' district level from analysis.
#'
#' @param data Data list from combine_health_climate_data() function.
#' @param param_terms A character vector or list containing parameter terms such
#' as "tmax" (maximum temperature) and "rainfall" (precipitation).
#' Default to "tmax"
#' @param model The fitted model from run_inla_models() function.
#' @param level A character vector specifying the spatial disaggregation level.
#' Can take one of the following values: "country", "Region", or "District".
#' Default value is "country".
#'
#' @return Dataframe containing cumulative relative risk at country, Region, or
#' District level.

get_predictions <- function(data,
                            param_terms = "tmax",
                            model,
                            level = "country"){

  # loading the best model
  data <- create_inla_indices(data)
  model <- model$model

  # getting basis matrices
  basis_matrices <- set_cross_basis(data)

  # Extract full coef and vcov for the region
  coef <- model$summary.fixed$mean
  vcov <- model$misc$lincomb.derived.covariance.matrix

  # Find positions of terms associated with tmax crossbasis
  indt <- grep(paste("basis", param_terms, sep = "_"), model$names.fixed)

  if (level == "country"){
    # Extract predictions from the tmax DLNM centered on overall mean Tmax
    predt <- crosspred(basis_matrices[[param_terms]], coef = coef[indt],
                       vcov = vcov[indt, indt], model.link = "log",
                       bylag = 0.25, cen = round(mean(data[[param_terms]],
                                                      na.rm = TRUE), 0))

  } else if (level == "Region"){
    # Iterate over unique regions
    regions <- unique(data$Region)
    predt <- regions %>%
      lapply(function(region){
        region_data <- subset(data, Region == region)
        # Extract predictions from the tmax DLNM centered on overall mean Tmax
        mean_param <- round(mean(region_data[[param_terms]], na.rm = TRUE), 0)
        predt <- crosspred(basis_matrices[[param_terms]], coef = coef[indt],
                           vcov = vcov[indt, indt],
                           model.link = "log", bylag = 0.25, cen = mean_param)
        return(predt)
      })
    names(predt) <- regions
  } else if (level == "District"){
    # Iterate over unique districts
    districts <- unique(data$District)
    predt <- districts %>%
      lapply(function(district){
        # Filter data for the current district
        district_data <- subset(data, District == district)
        # Extract predictions from the tmax DLNM centered on overall mean Tmax
        mean_param <- round(mean(district_data[[param_terms]], na.rm = TRUE), 0)
        predt <- crosspred(basis_matrices[[param_terms]], coef = coef[indt],
                           vcov = vcov[indt, indt],
                           model.link = "log", bylag = 0.25, cen = mean_param)
      })
    names(predt) <- districts
  }
  return(predt)
}


#' read in contour plot at country, Region, District level.
#'
#' @description: Generates a contour plot showing the lag exposure effect  of
#' maximum temperature (tmax) and cumulative rainfall on diarrhea cases.
#'
#' @param data Data list from combine_health_climate_data() function.
#' @param param_terms A character vector or list containing parameter terms such
#' as "tmax" (maximum temperature) and "rainfall" (precipitation).
#' Default to "tmax"
#' @param model The fitted model from run_inla_models() function.
#' @param level A character vector specifying the spatial disaggregation level.
#' Can take one of the following values: "country", "Region", or "District".
#' Default value is "country".
#' @param save_fig Boolean. Whether to save the plot as an output. Defaults to
#' FALSE.
#' @param output_dir Path to save model output. Default to NULL
#'
#' @return contour plot at country, Region and District level

contour_plot <- function(data,
                         param_terms = "tmax",
                         model,
                         level,
                         save_fig = FALSE,
                         output_dir = NULL) {
  # Validate output_dir if saving
  if (save_fig && is.null(output_dir)) {
    stop("output_dir must be provided if save_fig = TRUE")
  }
  # customize the output name
  output_file = paste0("contour_plot_", param_terms, "_", level, ".pdf")
  # getting the best model
  data <- create_inla_indices(data)

  # Extract predictions from the tmax DLNM centered on overall mean Tmax
  predt <- get_predictions(data, param_terms = param_terms, model, level = level)

  if (level == "country"){
    # Generate contour plot
    if (save_fig){
      pdf(file.path(output_dir, output_file), width = 6.5, height = 6)

      nlag <- data %>% select(contains("tmax")) %>% ncol() - 1
      y <- predt$predvar
      x <- seq(0, nlag, 0.25)
      z <- t(predt$matRRfit)

      pal <- rev(brewer.pal(11, "PRGn"))
      levels <- pretty(range(z, na.rm = TRUE), 20)
      col1 <- colorRampPalette(pal[1:6])
      col2 <- colorRampPalette(pal[6:11])
      cols <- c(col1(sum(levels <= 1)), col2(sum(levels > 1)))

      filled.contour(
        x, y, z,
        xlab = "Lag",
        ylab = ifelse(param_terms == "tmax", "Temperature (°C)",
                      ifelse(param_terms == "rainfall", "Rainfall (mm)", NA)),
        main = "Diarrhea cases",
        col = cols,
        levels = levels,
        plot.axes = {
          axis(1, at = 0:nlag, labels = 0:nlag)
          axis(2)
        }
      )
      dev.off()
    }
  } else if (level == "Region"){
    # Open a single PDF file for all regions
    if (save_fig){
      pdf(file.path(output_dir, output_file), width = 12, height = 12)

      # Set up a consistent layout for the plots (1 per page)
      par(mfrow = c(2, 1))  # One plot per page

      # Iterate over unique regions
      regions <- unique(data$Region)

      for (region in regions) {
        # Variables for plotting
        nlag <- data %>% select(contains("tmax")) %>% ncol() - 1
        y <- predt[[region]]$predvar
        x <- seq(0, nlag, 0.25)
        z <- t(predt[[region]]$matRRfit)

        # Define color palettes and levels
        pal <- rev(brewer.pal(11, "PRGn"))
        levels <- pretty(range(z, na.rm = TRUE), 20)
        col1 <- colorRampPalette(pal[1:6])
        col2 <- colorRampPalette(pal[6:11])
        cols <- c(col1(sum(levels <= 1)), col2(sum(levels > 1)))

        # Generate filled contour plot
        filled.contour(
          x, y, z,
          xlab = "Lag month",
          ylab = expression(paste("Temperature (", degree, "C)")),
          main = paste("Contour Plot for", region),
          col = cols,
          levels = levels,
          plot.axes = {
            axis(1, at = 0:nlag, labels = 0:nlag)  # Customize x-axis
            axis(2)  # Customize y-axis
          }
        )
      }

      # Close the PDF device after all plots
      dev.off()
    }
  } else if (level == "District"){
    # Open a single PDF file for all districts
    if (save_fig){
      pdf(file.path(output_dir, output_file),
          width = 12, height = 12)

      # Set up a 2x2 grid for four plots per page
      par(mfrow = c(2, 2))

      # Initialize plot counter
      plot_count <- 0

      # Iterate over unique districts
      districts <- unique(data$District)
      for (district in districts) {
        # Filter data for the current district
        district_data <- subset(data, District == district)

        # Variables for plotting
        nlag <- data %>% select(contains("tmax")) %>% ncol() - 1
        y <- predt[[district]]$predvar
        x <- seq(0, nlag, 0.25)
        z <- t(predt[[district]]$matRRfit)

        # Define color palettes and levels
        pal <- rev(brewer.pal(11, "PRGn"))
        levels <- pretty(range(z, na.rm = TRUE), 20)
        col1 <- colorRampPalette(pal[1:6])
        col2 <- colorRampPalette(pal[6:11])
        cols <- c(col1(sum(levels <= 1)), col2(sum(levels > 1)))

        # Generate filled contour plot
        filled.contour(
          x, y, z,
          xlab = "Lag",
          ylab = expression(paste("Temperature (", degree, "C)")),
          main = paste("Contour Plot for", district),
          col = cols,
          levels = levels,
          plot.axes = {
            axis(1, at = 0:nlag, labels = 0:nlag)  # Customize x-axis
            axis(2)  # Customize y-axis
          }
        )
        # Increment plot counter
        plot_count <- plot_count + 1

        # Start a new page after every four plots
        if (plot_count %% 4 == 0) {
          par(mfrow = c(2, 2))  # Reset 2x2 grid for the next page
        }
      }
      # Close the PDF device after all plots
      dev.off()
    }
  }
}


#' Read in Relative Risk plot at country, Region, and District level
#'
#' @description Plots the relative risk of diarrhea cases by the maximum
#' temperature and cumulative rainfall at country, Region and District level
#'
#' @param data Data list from combine_health_climate_data() function.
#' @param save_fig Boolean. Whether to save the plot as an output. Defaults to
#' FALSE.
#' @param param_terms A character vector or list containing parameter terms such
#' as "tmax" (maximum temperature) and "rainfall" (precipitation).
#' Default to "tmax".
#' @param model The fitted model from run_inla_models() function.
#' @param level A character vector specifying the spatial disaggregation level.
#' Can take one of the following values: "country", "Region", or "District".
#' Default to "country".
#' @param output_dir is the path where the pdf file will be saved. Default to NULL
#'
#' @return relative risk plot at country, Region, and District level.

plot_relative_risk <- function(data,
                               save_fig = FALSE,
                               param_terms = "tmax",
                               model,
                               level = "country",
                               output_dir = NULL) {

  # Validate output_dir if saving
  if (save_fig && is.null(output_dir)) {
    stop("output_dir must be provided if save_fig = TRUE")
  }

  # customizing the output file name
  output_file = paste0("plot_relative_risk_", param_terms, "_", level, ".pdf")

  # getting predictions
  predt <- get_predictions(data, param_terms = param_terms, model, level = level)

  if (level == "country"){
    # Output plot as a high-resolution PNG
    if (save_fig){
      pdf(file.path(output_dir, output_file), width = 6.5, height = 6)

      # Extract exposure values and relative risk data
      vars <- predt$predvar
      rr <- predt$allRRfit  # Use aggregated relative risks
      rr.lci <- predt$allRRlow  # Lower bound of confidence interval
      rr.uci <- predt$allRRhigh  # Upper bound of confidence interval

      # Ensure exposure and RR data are finite
      if (anyNA(c(vars, rr, rr.lci, rr.uci))) {
        stop("Missing values found in input data. Check the 'predt' object.")
      }

      # Set y-axis limits dynamically based on RR range
      r1 <- min(c(rr, rr.lci, rr.uci), na.rm = TRUE)
      r2 <- max(c(rr, rr.lci, rr.uci), na.rm = TRUE)

      # Plot Relative Risk by the parameter Range
      plot(vars, rr, type = "l", col = "red", lwd = 2,
           xlab = paste(ifelse(param_terms == "tmax", "Temperature (°C)",
                               ifelse(param_terms == "rainfall", "Rainfall (mm)", ""))),
           ylab = "Relative Risk",
           main = paste("Diarrhea Relative Risk by",
                        tolower(ifelse(param_terms == "tmax", "Temperature",
                                       ifelse(param_terms == "rainfall", "rainfall", NA))),
                        "range"),
           ylim = c(r1, r2 * 1.1), frame.plot = TRUE)

      # Add shaded confidence interval
      polygon(c(vars, rev(vars)),
              c(rr.lci, rev(rr.uci)),
              col = adjustcolor("red", alpha.f = 0.3), border = NA)

      # Add horizontal reference line at RR = 1
      abline(h = 1, lty = 2, col = "gray")

      # Add legend
      legend("topright",
             legend = paste("Relative Risk by",
                            tolower(ifelse(param_terms == "tmax", "Temperature",
                                           ifelse(param_terms == "rainfall",
                                                  "rainfall", NA)))),
             col = "red", lwd = 2, bty = "n")

      # Save and close the PNG device
      dev.off()
    }
  } else if (level == "Region"){
    # Open a single PDF file for all regions
    if (save_fig){
      pdf(file.path(output_dir, output_file), width = 10, height = 10)

      # Set up a consistent layout for multiple plots
      regions <- unique(data$Region)
      nrow = ceiling(length(regions)/2)
      layout(matrix(1:(2*nrow), nrow = nrow))
      par(mfrow = c(3, 2))

      for (region in regions) {
        # Filter data for the current region
        region_data <- subset(data, Region == region)

        # Extract exposure values and relative risk data
        vars <- predt[[region]]$predvar
        rr <- predt[[region]]$allRRfit  # Use aggregated relative risks
        rr.lci <- predt[[region]]$allRRlow  # Lower bound of confidence interval
        rr.uci <- predt[[region]]$allRRhigh  # Upper bound of confidence interval

        # Ensure exposure and RR data are finite
        if (anyNA(c(vars, rr, rr.lci, rr.uci))) {
          next  # Skip the region if data contains missing values
        }

        # Set y-axis limits dynamically based on RR range
        r1 <- min(c(rr, rr.lci, rr.uci), na.rm = TRUE)
        r2 <- max(c(rr, rr.lci, rr.uci), na.rm = TRUE)

        # Plot Relative Risk by Temperature Range
        plot(vars, rr, type = "l", col = "red", lwd = 2,
             xlab = paste(ifelse(param_terms == "tmax", "Temperature (°C)",
                                 ifelse(param_terms == "rainfall", "Rainfall (mm)", ""))),
             ylab = "Relative Risk",
             main = paste("Diarrhea Relative Risk by",
                          tolower(ifelse(param_terms == "tmax", "Temperature",
                                         ifelse(param_terms == "rainfall", "rainfall", NA))),
                          "\nrange for ", region),
             ylim = c(r1, r2 * 1.1), frame.plot = TRUE)

        # Add shaded confidence interval
        polygon(c(vars, rev(vars)),
                c(rr.lci, rev(rr.uci)),
                col = adjustcolor("red", alpha.f = 0.3), border = NA)

        # Add horizontal reference line at RR = 1
        abline(h = 1, lty = 2, col = "gray")

        # Add legend
        legend("topright",
               legend = paste("Relative Risk by",
                              tolower(ifelse(param_terms == "tmax", "Temperature",
                                             ifelse(param_terms == "rainfall",
                                                    "rainfall", NA)))),
               col = "red", lwd = 2, bty = "n")
      }

      # Close the PDF device
      dev.off()
    }
  } else if (level == "District"){
    # Set up a PDF output device
    if (save_fig){
      pdf(file.path(output_dir, output_file), width = 10, height = 10)

      # Extract unique districts
      districts <- unique(data$District)

      # Set up a 2x2 grid for four plots per page
      par(mfrow = c(2, 2))  # Two rows and two columns per page

      # Counter for plot tracking
      plot_count <- 0

      # Loop through each district
      for (district in districts) {
        # Filter data for the current district
        district_data <- subset(data, District == district)

        # Extract exposure values and relative risk data
        vars <- predt[[district]]$predvar
        rr <- predt[[district]]$allRRfit  # Use aggregated relative risks
        rr.lci <- predt[[district]]$allRRlow  # Lower bound of confidence interval
        rr.uci <- predt[[district]]$allRRhigh  # Upper bound of confidence interval

        # Ensure exposure and RR data are finite
        if (anyNA(c(vars, rr, rr.lci, rr.uci))) {
          stop("Missing values found in input data. Check the 'predt' object.")
        }

        # Set y-axis limits dynamically based on RR range
        r1 <- min(c(rr, rr.lci, rr.uci), na.rm = TRUE)
        r2 <- max(c(rr, rr.lci, rr.uci), na.rm = TRUE)

        # Plot Relative Risk by Temperature Range
        plot(vars, rr, type = "l", col = "red", lwd = 2,
             xlab = paste(ifelse(param_terms == "tmax", "Temperature (°C)",
                                 ifelse(param_terms == "rainfall", "Rainfall (mm)", ""))),
             ylab = "Relative Risk",
             main = paste("Diarrhea Relative Risk by",
                          tolower(ifelse(param_terms == "tmax", "Temperature",
                                         ifelse(param_terms == "rainfall", "rainfall", NA))),
                          "\nrange for ", district),
             ylim = c(r1, r2 * 1.1), frame.plot = TRUE)

        # Add shaded confidence interval
        polygon(c(vars, rev(vars)),
                c(rr.lci, rev(rr.uci)),
                col = adjustcolor("red", alpha.f = 0.3), border = NA)

        # Add horizontal reference line at RR = 1
        abline(h = 1, lty = 2, col = "gray")

        # Add legend
        legend("topright",
               legend = paste("Relative Risk by",
                              tolower(ifelse(param_terms == "tmax", "Temperature",
                                             ifelse(param_terms == "rainfall",
                                                    "rainfall", NA)))),
               col = "red", lwd = 2, bty = "n")

        # Increment plot counter
        plot_count <- plot_count + 1

        # Start a new page after every four plots
        if (plot_count %% 4 == 0) {
          par(mfrow = c(2, 2))  # Reset the 2x2 grid for the next page
        }
      }

      # Close the PDF device
      dev.off()
    }
  } else {
    stop("The level argument must take one of 'country', 'Region', or 'District' values.")
  }
}


#-------------------------------------------------------------------------------
# Attributable number and fraction calculation
#-------------------------------------------------------------------------------

#' attribution calculation for maximum temperature
#'
#' @description the attribution calculation request the attrdl function from
#' Gasparini and DNLM package
#'
#' @param data Data list from combine_health_climate_data() function.
#' @param param_terms A character vector or list containing parameter terms such
#' as "tmax" (maximum temperature) and "rainfall" (precipitation).
#' Default to "tmax"
#' @param model The fitted model from run_inla_models() function.
#' @param attrdl_path is the path to the attrdl function.
#' @param param_threshold Numeric. Threshold above which exposure is considered,
#' "attributable". Default is 1. can take decimal value.
#' @param level A character vector specifying the spatial disaggregation level.
#' Can take one of the following values: "country", "Region", or "District".
#' Default value is "country".
#'
#' @return results which contain the attribution number and fraction at country,
#'  region and district level.

attribution_calculation <- function(data,
                                    param_terms,
                                    model,
                                    param_threshold = 1,
                                    attrdl_path = "attrdl.R",
                                    level = "country"){
  # Load best-fitting model with climate DLNMs
  model <- model$model

  # Extract full coefficients and covariance matrix
  coef <- model$summary.fixed$mean
  vcov <- model$misc$lincomb.derived.covariance.matrix

  # Find the position of terms associated with the temperature crossbasis
  indt <- grep(paste0("basis_", param_terms), model$names.fixed)

  # Validate model data and inputs
  if (is.null(coef) || is.null(vcov)) stop("Model coefficients or covariance matrix are missing.")
  if (length(indt) == 0) stop("No terms associated with 'basis_tmax' found in the model.")

  # Recreate the temperature crossbasis (cb) using the original structure
  nlag <- (data %>% select(contains("tmax")) %>% ncol()) - 1
  if (nlag == 0) stop("Lagged variables not found in the data.")

  basis_matrices <- set_cross_basis(data)

  if (level == "country"){
    # Calculate center temperature for predictions
    cen <- mean(data[[param_terms]], na.rm = TRUE)

    # Validate temperature data
    if (is.na(cen)) stop("Center temperature (cen) calculation failed due to missing data.")

    # Define minimum and maximum temperatures from the data
    min_param_terms <- min(data[[param_terms]], na.rm = TRUE)
    max_param_terms <- max(data[[param_terms]], na.rm = TRUE)

    # Predict relative risks over a range of temperatures using crosspred
    param_terms_range <- seq(min_param_terms, max_param_terms, by = 0.1)  # Define temperature range

    # Generate predictions using the crossbasis, coefficients, and covariance matrix
    predt <- crosspred(basis_matrices[[param_terms]], coef = coef[indt], vcov = vcov[indt, indt], model.link = "log", at = param_terms_range, cen = cen)

    # Find the Minimum Risk Temperature (MRT)
    mr <- predt$predvar[which.min(predt$allRRfit)]
    mrt_ci <- quantile(predt$allRRfit, probs = c(0.025, 0.975))  # Confidence interval for MRT

    # Define the temperature range where RR > 1 (i.e., the RR exceeds 1)
    high_pt_range <- param_terms_range[predt$allRRfit > param_threshold]

    # If the high_temp_range is empty, there are no temperatures where RR > param_threshold
    if (length(high_pt_range) == 0) {
      return(list(MRT = mr, MRT_CI = mrt_ci, High_pt_Range = NULL, Attributable_Risk = NULL))
    }

    # Find the temperature bounds where RR > param_threshold
    high_pt_bounds <- range(high_pt_range)  # Lower and upper bounds

    # Attributable risk calculations for temperatures where RR > param_threshold
    source(attrdl_path)
    an_risk_number_hot <- attrdl(
      data[[param_terms]], basis_matrices[[param_terms]], data$Diarrhea,
      coef = coef[indt], vcov = vcov[indt, indt],
      type = "an", cen = mr, range = high_pt_bounds
    ) %>% round()

    an_risk_fraction_hot <- attrdl(
      data[[param_terms]], basis_matrices[[param_terms]], data$Diarrhea,
      coef = coef[indt], vcov = vcov[indt, indt],
      type = "af", cen = mr, range = high_pt_bounds
    ) * 100

    # Return results as a list
    results <- list(
      MRT = mr,
      MRT_CI = mrt_ci,
      High_pt_Range = high_pt_bounds,
      Attributable_Risk_Number = an_risk_number_hot,
      Attributable_Risk_Fraction = an_risk_fraction_hot
    )
  } else if (level == "Region"){
    results <- data.frame(
      Region = character(),
      # District = character(),
      Year = integer(),
      MRT = numeric(),
      High_Rainfall_Lower = numeric(),
      High_Rainfall_Upper = numeric(),
      Attributable_Risk_Number = numeric(),
      Attributable_Risk_Fraction = numeric(),
      stringsAsFactors = FALSE
    )

    # Loop through each region and year
    results <- unique(data$Region) %>%
      lapply(function(region){
        yearly_res <- unique(data$Year) %>%
          lapply(function(year){
            # Subset data for the current region and year
            data_region_year <- subset(data, Region == region & Year == year)

            # Recreate the temperature crossbasis for the region and year
            cb_region_year <- set_cross_basis(data_region_year)

            # Calculate center temperature for predictions
            cen <- mean(data_region_year[[param_terms]], na.rm = TRUE)

            # Validate temperature data
            if (is.na(cen)) cat("Cen is missing\n")

            # Define minimum and maximum temperatures from the data
            min_param_terms <- min(data_region_year[[param_terms]], na.rm = TRUE)
            max_param_terms <- max(data_region_year[[param_terms]], na.rm = TRUE)
            param_terms_range <- seq(min_param_terms, max_param_terms, by = 0.1)

            # Generate predictions using the crossbasis, coefficients, and covariance matrix
            predt <- crosspred(cb_region_year[[param_terms]], coef = coef[indt],
                               vcov = vcov[indt, indt], model.link = "log",
                               at = param_terms_range, cen = cen)

            # Find the Minimum Risk Temperature (MRT)
            mr <- predt$predvar[which.min(predt$allRRfit)]

            # Define the temperature range where RR > 1.1 (i.e., the RR exceeds 1.1)
            high_pt_range <- c()

            if (sum(predt$allRRfit > param_threshold) == 0){
              high_pt_range = NA
            } else {
              high_pt_range <- param_terms_range[predt$allRRfit > param_threshold]
            }

            # Find the temperature bounds where RR > param_threshold
            high_pt_bounds <- range(high_pt_range)  # Lower and upper bounds

            # Attributable risk calculations for temperatures where RR > param_threshold
            an_risk_number_hot <- attrdl(
              data_region_year[[param_terms]], cb_region_year[[param_terms]],
              data_region_year$Diarrhea,
              coef = coef[indt], vcov = vcov[indt, indt],
              type = "an", cen = mr, dir = "forw", range = high_pt_bounds
            ) %>% round()

            an_risk_fraction_hot <- attrdl(
              data_region_year[[param_terms]], cb_region_year[[param_terms]],
              data_region_year$Diarrhea,
              coef = coef[indt], vcov = vcov[indt, indt],
              type = "af", cen = mr, dir = "forw", range = high_pt_bounds
            ) * 100

            # Store results for the current region and year
            list(
              Region = region,
              Year = year,
              MRT = round(mr, 2),
              High_Temperature_Lower = round(high_pt_bounds[1], 2),
              High_Temperature_Upper = round(high_pt_bounds[2], 2),
              Attributable_Risk_Number = an_risk_number_hot,
              Attributable_Risk_Fraction = round(an_risk_fraction_hot, 2)
            )
          })
      }) %>% bind_rows() %>% na.omit()
  } else if (level == "District"){
    # Loop through each region, district, and year
    results <- unique(data$Region) %>%
      lapply(
        function(region){
          res_dist <- unique(data$District) %>%
            lapply(function(district){
              res_year <- unique(data$Year) %>%
                lapply(function(year){
                  # Subset data for the current region, district, and year
                  cat("Region=", region, ", District=", district, ", year=", year, "\n")
                  data_group <- subset(data, Region == region & District == district & Year == year)

                  # Recreate the parameter crossbasis for the group
                  cb_group <- set_cross_basis(data_group)

                  # Calculate center parameter for predictions
                  cen <- mean(data_group[[param_terms]], na.rm = TRUE)

                  # Validate rainfall data
                  if (is.na(cen)) cat("The cen of the parameter is NA\n")

                  # Define minimum and maximum rainfall from the data
                  min_param_terms <- min(data_group[[param_terms]], na.rm = TRUE)
                  max_param_terms <- max(data_group[[param_terms]], na.rm = TRUE)
                  param_terms_range <- seq(min_param_terms, max_param_terms, by = 0.1)

                  # Generate predictions using the crossbasis, coefficients, and covariance matrix
                  predt <- crosspred(cb_group[[param_terms]], coef = coef[indt], vcov = vcov[indt, indt],
                                     model.link = "log", at = param_terms_range, cen = cen)

                  # Find the Minimum Risk Rainfall (MRR)
                  mr <- predt$predvar[which.min(predt$allRRfit)]

                  # Define the rainfall range where RR > param_threshold (i.e., the RR exceeds param_threshold)
                  high_pt_range <- param_terms_range[predt$allRRfit > param_threshold]

                  # If the high_rain_range is empty, there are no rainfall levels where RR > param_threshold
                  if (length(high_pt_range) == 0) cat(paste0("high_pt_range == 0 for region = ", region,
                                                             ", district = ", district, " and year = ", year, ".\n"))

                  # Find the rainfall bounds where RR > param_threshold
                  high_pt_bounds <- range(high_pt_range)  # Lower and upper bounds

                  # Attributable risk calculations for rainfall where RR > param_threshold
                  an_risk_number <- attrdl(
                    data_group[[param_terms]], cb_group[[param_terms]], data_group$Diarrhea,
                    coef = coef[indt], vcov = vcov[indt, indt],
                    type = "an", cen = mr, dir = "forw", range = high_pt_bounds
                  ) %>% round()

                  an_risk_fraction <- attrdl(
                    data_group[[param_terms]], cb_group[[param_terms]], data_group$Diarrhea,
                    coef = coef[indt], vcov = vcov[indt, indt],
                    type = "af", cen = mr, dir = "forw", range = high_pt_bounds
                  ) * 100

                  # Store results for the current group
                  list(
                    Region = region,
                    District = district,
                    Year = year,
                    MRT = round(mr, 2),
                    High_Rainfall_Lower = high_pt_bounds[1],
                    High_Rainfall_Upper = high_pt_bounds[2],
                    Attributable_Risk_Number = round(an_risk_number),
                    Attributable_Risk_Fraction = round(an_risk_fraction, 2)
                  )
                }) %>% bind_rows()
            })  %>% bind_rows()
        })
  } else {
    stop("The level parameter should be one c('country', 'Region', 'District') values")
  }

  return(results)
}


################################################################################

#' Read in the Combine function for doing all the analysis once
#'
#' @description the function diarrhea_do_analysis() combine all functions
#' previously sated.

#' @param health_data_path A data frame containing the processed health data
#' @param climate_data_path A data frame containing the processed climate data
#' @param map_path A data frame containing the processed map data
#' @param Region_col Character. Name of the column in the dataframe that contains
#' the region names.
#' @param District_col Character. Name of the column in the dataframe that
#' contains the region names.
#' @param Date_col Character. Name of the column in the dataframe that contains
#' the date. Defaults to NULL.
#' @param Year_col Character. Name of the column in the dataframe that contains
#' the Year.
#' @param Month_col Character. Name of the column in the dataframe that contains
#' the Month.
#' @param Diarrhea_case_col Character. Name of the column in the dataframe
#' that contains the Diarrhea cases to be considered.
#' @param tot_pop_col Character. Name of the column in the dataframe that
#' contains the total population.
#' @param Male_counts_pop Character. Name of the column in the dataframe that
#' contains the Male total population. Defaults to NULL.
#' @param Female_counts_pop Character. Name of the column in the dataframe that
#' contains the date. Defaults to NULL.
#' @param tmin_col Character. Name of the column in the dataframe that
#' contains the minimum temperature data.
#' @param tmean_col Character. Name of the column in the dataframe that
#' contains the average temperature.
#' @param tmax_col Character. Name of the column in the dataframe that
#' contains the maximum temperature.
#' @param rainfall_col Character. Name of the column in the dataframe that
#' contains the cumulative monthly rainfall.
#' @param r_humidity_col Character. Name of the column in the dataframe that
#' contains the relative humidity.
#' @param spi_col Character. Name of the column in the dataframe that
#' contains the standardized precipitation index. Defaults to NULL.
#' @param max_lag Character. Number corresponding to the maximum lag to be
#' considered for the delay effect. It should be between 2 an 4. Defaults to 2
#' @param geometry_col is the Name of the geometry column in the shapefile
#' (usually "geometry").
#' @param basis_matrices_choices A character vector specifying the basis matrix
#' parameters to be included in the model. Possible values are "tmax", "tmin",
#' "rainfall", "r_humidity", and "spi".  Default to "tmax", and users can select
#' one or more of these parameters for inclusion in the model.
#' @param param_terms A character vector or list containing parameter terms such
#' as "tmax" (maximum temperature) and "rainfall" (precipitation).
#' Default to "tmax"
#' @param level A character vector specifying the spatial disaggregation level.
#' Can take one of the following values: "country", "Region", or "District".
#' Default value is "country".
#' @param save_fig Boolean. Whether to save the plot as an output. Defaults to
#' FALSE.
#' @param param_threshold Numeric. Threshold above which exposure is considered,
#' "attributable". Default is 1. can take decimal value.
#' @param output_dir Path to folder where the process map data should be
#' saved.

#' @return list of results including the monthly random effect, yearly random
#' effect, the attributable number and fraction.


diarrhea_do_analysis <- function(health_data_path,
                                 climate_data_path,
                                 map_path,
                                 Region_col,
                                 District_col,
                                 Date_col,
                                 Year_col,
                                 Month_col,
                                 Diarrhea_case_col,
                                 tot_pop_col,
                                 Male_counts_pop = NULL,
                                 Female_counts_pop = NULL,
                                 tmin_col,
                                 tmean_col,
                                 tmax_col,
                                 rainfall_col,
                                 r_humidity_col,
                                 spi_col = NULL,
                                 max_lag = 2,
                                 geometry_col,
                                 basis_matrices_choices = "tmax",
                                 param_terms = "tmax",
                                 param_level = "country",
                                 save_fig = TRUE,
                                 param_threshold = 1,
                                 output_dir = NULL){

  # get combined data
  combined_data <- combine_health_climate_data(health_data_path,
                                               climate_data_path,
                                               map_path,
                                               Region_col,
                                               District_col,
                                               Date_col,
                                               Year_col,
                                               Month_col,
                                               Diarrhea_case_col,
                                               tot_pop_col,
                                               Male_counts_pop,
                                               Female_counts_pop,
                                               tmin_col,
                                               tmean_col,
                                               tmax_col,
                                               rainfall_col,
                                               r_humidity_col,
                                               spi_col,
                                               max_lag,
                                               geometry_col,
                                               output_dir)

  # data for INLA
  data_inla <- create_inla_indices(combined_data$data)

  # fitting the model
  model <- run_inla_models(data_inla, basis_matrices_choices)

  #
  reff_plot_monthly <- plot_monthly_random_effects(combined_data, model = model, output_dir = output_dir)

  #
  reff_plot_yearly <- yearly_spatial_random_effect(combined_data, model = model, output_dir = output_dir)

  # contour plots
  contour_plot(combined_data$data,  param_terms, model,  level = param_level, output_dir = output_dir, save_fig)

  # relative ristk plot
  plot_relative_risk(combined_data$data, save_fig = save_fig, param_terms = param_terms, model = model, level = param_level, output_dir = output_dir)

  # attribution fraction and number
  attr_frac_num <- attribution_calculation(combined_data$data, param_terms = param_terms, model = model, param_threshold = param_threshold, level = param_level)

  res <- list(reff_plot_monthly, reff_plot_yearly, attr_frac_num)

  return(res)
}
