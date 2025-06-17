#-------------------------------------------------------------------------------
#' @title R-code for Malaria cases attributable to extreme precipitation
#' and extreme temperature
#'
#' You will need to load the following package under R for the code to work:
#' library(tidyverse), library(INLA), library(splines), library(stats),
#' library(sf), library(sp), library(dlnm), library(tsModel), library(hydroGOF),
#' library(RColorBrewer), library(openxlsx), library(readxl),library(geofacet)
#' library(ggpubr),library(ggthemes), library(here), library(data.table).
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
  # make valid geomtry
  map$geometry <- st_make_valid(map$geometry)

  # Create adjacency matrix
  if (!file.exists(file.path(output_dir, "nbfile"))){
    nb.map <- poly2nb(as_Spatial(map$geometry), snap = 1e-4)
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


#' Read in and format health data - Malaria cases
#'
#' @description Read in a csv file of monthly time series of health and the
#' population data, rename columns and create time variables for spatiotemporal
#' analysis
#'
#' @param data_path Path to a csv file containing a monthly time series of data
#' for Malaria cases, which may be disaggregated by sex (under five case or
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
#' @param Malaria_case_col Character. Name of the column in the dataframe
#' that contains the Malaria cases to be considered.
#' @param tot_pop_col Character. Name of the column in the dataframe that contains
#' the total population.
#'
#' @return A dataframe with formatted and renamed columns.


load_and_process_data <- function(data_path,
                                  Region_col,
                                  District_col,
                                  Date_col = NULL,
                                  Year_col,
                                  Month_col,
                                  Malaria_case_col,
                                  tot_pop_col) {
  # Load health and climate data
  data <- read_csv(data_path)

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
    select(Region = Region_col, District = District_col, Year= Year_col,
           Month= Month_col,
           Malaria = Malaria_case_col,
           tot_pop = tot_pop_col
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
#' @param runoff_col Character. Name of the column in the dataframe that
#' contains the monthly runoff water data. Defaults to NULL.
#' @param cvh_col Character. Name of the column in the dataframe that
#' contains the vegetation index. This can be the Normalized Difference
#' Vegetation Index (NDVI). Defaults to NULL.
#' @param spi_col Character. Name of the column in the dataframe that
#' contains the standardized precipitation index. Defaults to NULL.
#' @param maxlag Character. Number corresponding to the maximum lag to be
#' considered for the delay effect. It should be between 3 to 6. Defaults to 4
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
                                         runoff_col = NULL,
                                         cvh_col = NULL,
                                         Spi = NULL,
                                         maxlag = 4) {

  # Select required columns
  base_cols <- c(District_col, Year_col, Month_col,
                 tmin_col, tmean_col, tmax_col,
                 rainfall_col, r_humidity_col)

  optional_cols <- c( if (!is.null(runoff_col)) runoff_col,
                      if (!is.null(cvh_col)) cvh_col,
                      if (!is.null(Spi)) Spi )

  all_cols <- c(base_cols, optional_cols)

  # Read and rename data
  climate_data <- read_csv(data_path) %>%
    select(all_of(all_cols)) %>%
    rename(District = !!District_col, Year = !!Year_col, Month = !!Month_col,
           tmin = !!tmin_col, tmean = !!tmean_col, tmax = !!tmax_col,
           rainfall = !!rainfall_col, r_humidity = !!r_humidity_col,
           runoff = if (!is.null(runoff_col)) !!runoff_col else NULL,
           cvh = if (!is.null(cvh_col)) !!cvh_col else NULL,
           spi = if (!is.null(Spi)) !!Spi else NULL)

  # Create lagged variables
  tmin_data <- climate_data %>% select(tmin)
  tmean_data <- climate_data %>% select(tmean)
  tmax_data <- climate_data %>% select(tmax)
  rf_data <- climate_data %>% select(rainfall)
  rh_data <- climate_data %>% select(r_humidity)
  runoff_data <- if (!is.null(runoff_col)) climate_data %>% select(runoff) else NULL
  cvh_data <- if (!is.null(cvh_col)) climate_data %>% select(cvh) else NULL
  spi_data <- if (!is.null(Spi)) climate_data %>% select(spi) else NULL

  for (i in 1:maxlag) {
    tmin_data[paste0("tmin_lag", i)] <- lag(tmin_data$tmin, n = i, default = NA)
    tmean_data[paste0("tmean_lag", i)] <- lag(tmean_data$tmean, n = i, default = NA)
    tmax_data[paste0("tmaxlag", i)] <- lag(tmax_data$tmax, n = i, default = NA)
    rf_data[paste0("rf_lag", i)] <- lag(rf_data$rainfall, n = i, default = NA)
    rh_data[paste0("rh_lag", i)] <- lag(rh_data$r_humidity, n = i, default = NA)
    if (!is.null(runoff_data)) {
      runoff_data[paste0("runoff_lag", i)] <- lag(runoff_data$runoff, n = i, default = NA)
    }
    if (!is.null(cvh_data)) {
      cvh_data[paste0("cvh_lag", i)] <- lag(cvh_data$cvh, n = i, default = NA)
    }
    if (!is.null(spi_data)) {
      spi_data[paste0("spi_lag", i)] <- lag(spi_data$spi, n = i, default = NA)
    }
  }

  # Combine final dataset
  climate_data <- bind_cols(
    climate_data %>% select(District, Year, Month), tmin_data, tmean_data,
    tmax_data, rf_data, rh_data,
    if (!is.null(runoff_data)) runoff_data else NULL,
    if (!is.null(cvh_data)) cvh_data else NULL,
    if (!is.null(spi_data)) spi_data else NULL
  )

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
#' @param runoff_col Character. Name of the column in the dataframe that
#' contains the monthly runoff water data. Defaults to NULL.
#' @param cvh_col Character. Name of the column in the dataframe that
#' contains the vegetation index. This can be the Normalized Difference
#' Vegetation Index (NDVI). Defaults to NULL.
#' @param spi_col Character. Name of the column in the dataframe that
#' contains the standardized precipitation index. Defaults to NULL.
#' @param maxlag Character. Number corresponding to the maximum lag to be
#' considered for the delay effect. It should be between 3 to 6. Defaults to 4
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
                                        Malaria_case_col,
                                        tot_pop_col,
                                        tmin_col,
                                        tmean_col,
                                        tmax_col,
                                        rainfall_col,
                                        r_humidity_col,
                                        runoff_col = NULL,
                                        cvh_col = NULL,
                                        spi_col = NULL,
                                        maxlag = 4,
                                        geometry_col,
                                        output_dir= NULL ) {

  health_data <- load_and_process_data(health_data_path,
                                       Region_col = Region_col,
                                       District_col = District_col,
                                       Date_col = Date_col,
                                       Year_col = Year_col,
                                       Month_col = Month_col,
                                       Malaria_case_col = Malaria_case_col,
                                       tot_pop_col = tot_pop_col)

  climate_data <- load_and_process_climatedata(data_path = climate_data_path,
                                               District_col = District_col,
                                               Year_col = Year_col,
                                               Month_col = Month_col,
                                               tmin_col = tmin_col,
                                               tmean_col = tmean_col,
                                               tmax_col = tmax_col,
                                               rainfall_col = rainfall_col,
                                               r_humidity_col = r_humidity_col,
                                               runoff_col = runoff_col,
                                               cvh_col = cvh_col,
                                               Spi = spi_col,
                                               maxlag = maxlag)

  map_data <- load_and_process_map(map_path = map_path,
                                   Region_col = Region_col,
                                   District_col = District_col,
                                   geometry_col = geometry_col,
                                   output_dir = output_dir)

  data <- health_data %>%
    left_join(climate_data, by = join_by(District, Year, Month)) %>%
    group_by(Region, District) %>%
    unique() %>%
    mutate(time = (Year - min(Year)) * 12 + Month) %>%
    ungroup()

  grid_data <- data %>%
    select(Region, District) %>%
    distinct() %>%
    group_by(Region) %>%
    mutate(region_code = cur_group_id(),
           district_number = row_number(),
           district_code = sprintf("%d%01d", region_code, district_number)) %>%
    ungroup()

  data <- data %>%
    left_join(grid_data, by = c("Region", "District")) %>%
    arrange(region_code, district_code)

  map <- map_data$map %>%
    left_join(grid_data, by = c("Region", "District")) %>%
    arrange(region_code, district_code)

  grid_data <- grid_data %>%
    rename(name = Region, Code_num = region_code)

  summary_stats <- list( tmin = summary(data$tmin), tmax = summary(data$tmax),
    rainfall = summary(data$rainfall), rhumidity = summary(data$r_humidity)
  )

  return(list(map = map, nb.map = map_data$nb.map, data = data,
              grid_data = grid_data, summary = summary_stats))
}


#' Read in cross-basis matrix set for DLNM analysis
#'
#' @description creates cross-basis matrix for each climate variables
#'
#' @param data is the dataset containing district_code, region_code, and Year
#' columns from the combine_health_climate_data() function.
#' @param maxlag Character. Number corresponding to the maximum lag to be
#' considered for the delay effect.
#'
#' @return list of cross-basis matrices of the climate variables.

set_cross_basis <- function(data, maxlag) {
  # Internal helper: extract variable + lag columns if present
  lag_definitions <- list( tmax = "tmaxlag", tmin = "tmin_lag",
                           tmean = "tmean_lag", rainfall = "rf_lag",
                           r_humidity = "rh_lag", runoff = "runoff_lag",
                           cvh = "cvh_lag")

  vars <- list()
  for (var in names(lag_definitions)) {
    lagged_names <- paste0(lag_definitions[[var]], 1:maxlag)
    all_names <- c(var, lagged_names)
    if (all(all_names %in% names(data))) {
      vars[[var]] <- data %>% dplyr::select(dplyr::all_of(all_names))
    }
  }

  # Define lag knots
  lagknot <- equalknots(0:maxlag, 2)

  # Apply crossbasis to each variable group
  basis_matrices <- lapply(names(vars), function(varname) {
    var_data <- vars[[varname]]
    cb <- crossbasis(var_data,
      argvar = list(fun = "ns", knots = equalknots(var_data[[1]], 2)),
      arglag = list(fun = "ns", knots = maxlag / 2)
    )
    colnames(cb) <- paste0("basis_", varname, ".", colnames(cb))
    return(cb)
  })

  names(basis_matrices) <- names(vars)
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
  data$Ep <- data$tot_pop/10^5
  data$E<- (sum(data$Malaria) / sum(data$tot_pop))*(data$tot_pop) # Expected counts
  data$SIR <- data$Malaria / data$E  # Standardized Incidence Ratio

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
#' "rainfall", "r_humidity", "runoff", "cvh" and "spi". Users can select
#'  one or more of these parameters for inclusion in the model.
#' @param output_dir is the path to save model output. Default to NULL
#' @param save_csv Boolean. Whether to save the results as a CSV. Defaults to
#' FALSE.
#'
#' @returns list of the model, the baseline_model, and the dic_table.

run_inla_models <- function(data,
                            basis_matrices_choices,
                            output_dir= NULL,
                            save_csv = FALSE) {
  # Validate output_dir if saving
  if (save_csv && is.null(output_dir)) {
    stop("output_dir must be provided if save_csv = TRUE")
  }

  # Add INLA indices to the data
  data <- create_inla_indices(data)

  # Define priors
  precision_prior <- list(prec = list(prior = "pc.prec", param = c(0.5, 0.01)))

  # Define base formula
  baseformula <- Malaria ~ 1 +
    f(Month, replicate = region_index, model = "rw1", cyclic = TRUE, constr = TRUE,
      scale.model = TRUE, hyper = precision_prior) +
    f(district_index, model = "bym2", replicate = year_index,
      graph = file.path(output_dir, "map.graph"),
      scale.model = TRUE, hyper = precision_prior)

  # Get cross-basis matrices
  basis_matrices <- set_cross_basis(data)

  # Filter basis_matrices_choices to only those available in the matrices
  valid_vars <- basis_matrices_choices[basis_matrices_choices %in% names(basis_matrices)]

  if (length(valid_vars) == 0) {
    stop("No valid variables found in basis_matrices for the specified choices.")
  }

  # Dynamically update formula
  formula <- update.formula(baseformula,
                            as.formula(paste0("~ . + ",
                                              paste0("basis_matrices$", valid_vars, collapse = " + "))))

  # Fit models
  baseline_model <- fit_inla_model(baseformula, data = as.data.frame(data))
  model <- fit_inla_model(formula, data = as.data.frame(data))

  # Save model if requested
  param_model_name <- paste0(valid_vars, collapse = "_")
  if (save_csv) {
    model_name <- paste0("model_with_", param_model_name, ".csv")
    save(model, file = file.path(output_dir, model_name))
  } else {
    model_name <- NULL
  }

  # Create summary DIC/logscore table
  table0 <- data.table(Model = paste0(valid_vars, collapse = " + "),
                       DIC = round(model$dic$dic, 0),
                       logscore = round(-mean(log(model$cpo$cpo), na.rm = TRUE), 3))

  return(list(model = model,
              baseline_model = baseline_model,
              dic_table = table0))
}


#' Visualise Monthly Random Effects from INLA Model by Region
#'
#' @description
#' Generates and optionally saves a faceted line plot of monthly random effects
#' from a fitted INLA model, showing each region's temporal contribution to the
#' log-transformed Malaria Incidence Rate (log(MIR)).
#'
#' The plot illustrates seasonal variations in disease risk across regions.
#' - Positive values indicate months with above-average contribution to log(MIR).
#' - Negative values indicate months with below-average risk.
#'
#' This visualization helps identify:
#' - Peak malaria transmission periods by region
#' - Regional differences in seasonality strength
#' - Temporal targeting opportunities for interventions.
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
    ggsave(file.path(output_dir, "monthly_random_effects.pdf"), plot = p, height = 30, width = 25, units = "cm")
  }
  return(p)
}


#' Visualize Yearly Spatial Structured Random Effects
#'
#' @description
#' Generates and optionally saves maps of the yearly **structured spatial random effects**
#' from a Bayesian spatial-temporal model fitted using INLA. These maps show the
#' district-level contributions to the log of malaria incidence rates (log(MIR))
#' after adjusting for observed covariates. Each district's effect reflects residual
#' spatial variation attributable to spatial dependence (structured effect),
#' helping identify persistent hotspots or areas with unexpectedly high or low risk.
#'
#' The output is a faceted map by year, allowing visual comparison of spatial patterns
#' and trends across time.
#'
#' @param combined_data A list returned by the \code{combine_health_climate_data()} function,
#' containing spatial (map), modeling, and covariate data.
#' @param model A fitted INLA model object from the \code{run_inla_models()} function.
#' @param save_fig Logical. If \code{TRUE}, saves the generated plot to
#' the specified directory.
#' @param output_dir A character string specifying the path where the plot
#' should be saved if \code{save_fig = TRUE}.
#'
#' @return A \code{ggplot} object showing yearly district-level structured
#' spatial random effects. Red indicates districts with higher-than-expected
#' malaria incidence; green indicates lower-than-expected incidence,
#' relative to model predictions.

yearly_spatial_random_effect <- function(combined_data,
                                         model,
                                         save_fig = FALSE,
                                         output_dir = NULL) {

  if (save_fig && is.null(output_dir)) {
    stop("output_dir must be provided if save_fig = TRUE")
  }

  # Extract necessary components
  data <- create_inla_indices(combined_data$data)
  grid_data <- combined_data$grid_data
  map <- combined_data$map

  nyear <- length(unique(data$Year))
  ndistrict <- length(unique(data$district_code))

  # Extract spatial random effects (unstructured + structured)
  space <- data.table(model$summary.random$district_index)
  space$Year <- rep(min(data$Year):max(data$Year), each = 2 * ndistrict)
  space$re <- rep(c(rep(1, ndistrict), rep(2, ndistrict)), nyear)
  space <- space[space$re == 1, ]
  space$District_code <- rep(unique(data$district_code), nyear)

  # Merge with spatial data
  space <- left_join(map, space, by = c("district_code" = "District_code"))

  # Define color scale limits symmetrically
  lim <- max(abs(space$mean), na.rm = TRUE)

  # Create map
  space_effects <- ggplot() +
    geom_sf(data = space, aes(fill = mean), lwd = 0.1, color = "grey60") +
    scale_fill_gradient2(
      name = "Contribution to\nlog(MIR)",
      low = "darkgreen", mid = "white", high = "darkred",
      midpoint = 0, limits = c(-lim, lim), breaks = c(-lim, 0, lim)
    ) +
    facet_wrap(~Year, ncol = 5) +
    theme_void(base_size = 12) +
    theme(
      strip.text = element_text(size = 12, face = "bold"),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9),
      legend.position = "right"
    )

  # Save figure
  if (save_fig) {
    ggsave(
      file.path(output_dir, "Yearly_random_effects_improved.pdf"),
      plot = space_effects, height = 30, width = 25, units = "cm", dpi = 300
    )
  }

  return(space_effects)
}


#-------------------------------------------------------------------------------
# DNLM FUNCTION FOR RELATIVE RISK PLOT
#-------------------------------------------------------------------------------


#' Generate Exposure-Lag-Response Predictions at Country, Region, or District Level
#'
#' @description
#' Computes cumulative relative risk predictions from a distributed lag nonlinear
#' model (DLNM) fitted using INLA. The function uses exposure-lag-response
#' relationships (e.g., for climate variables like temperature or rainfall)
#' to estimate the effect of an exposure variable on the outcome (e.g., malaria
#' incidence) across time lags.
#'
#' Predictions are generated using the fitted model coefficients and the DLNM
#' cross-basis, and can be produced at different geographic aggregation levels:
#' national, regional, or district. This allows comparison of how exposure-response
#' relationships vary across spatial units.
#'
#' @param data A dataset returned by the \code{combine_health_climate_data()} function,
#' containing health, climate, and geographic data.
#' @param param_terms A character string specifying the name of the exposure variable
#' (e.g., "tmax" for maximum temperature or "rainfall" for precipitation). Default is "tmax".
#' @param model A fitted INLA model object containing DLNM terms, typically returned
#' from the \code{run_inla_models()} function.
#' @param level A character string specifying the spatial level at which predictions should be made.
#' Can be one of: \code{"country"} (default), \code{"Region"}, or \code{"District"}.
#'
#' @return A \code{crosspred} object (if \code{level = "country"}) or a named list of
#' \code{crosspred} objects (if \code{level = "Region"} or \code{"District"}), each containing:
#' \itemize{
#'   \item Estimated relative risks across values of the exposure and lag periods
#'   \item Centering at the mean exposure for the respective unit
#'   \item Can be used for plotting and interpretation of cumulative and lag-specific effects
#' }

get_predictions <- function(data,
                            param_terms,
                            model,
                            level = "country"){

  # loading the best model
  data <- create_inla_indices(data)

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


#' Generate Contour Plots of Lagged Climate Effects on Health Outcomes
#'
#' @description Produces contour plots showing the joint lagged and exposure effects
#' of climate variables (e.g., maximum temperature or cumulative rainfall) on health outcomes
#' (e.g., diarrhea or malaria cases) using distributed lag nonlinear models (DLNMs).
#' The plots can be generated at the country, region, or district level and illustrate
#' how relative risk varies over different exposure levels and time lags.
#'
#' @param data A dataframe of merged health and climate data, typically output from
#' `combine_health_climate_data()`.
#' @param param_terms A character string specifying the climate parameter to analyze,
#' such as `"tmax"` for maximum temperature or `"rainfall"` for precipitation.
#' @param model A fitted INLA model object from `run_inla_models()` containing
#' DLNM terms for the specified climate variable.
#' @param level A character string specifying the spatial level of analysis:
#' `"country"`, `"Region"`, or `"District"`. Defaults to `"country"`.
#' @param save_fig Logical. If `TRUE`, saves the resulting contour plot(s) to the specified directory.
#' Defaults to `TRUE`.
#' @param nlag An integer specifying the maximum number of lag months to include in the plot.
#' Defaults to 4.
#' @param output_dir A character string specifying the directory path where output plots
#' should be saved. Required if `save_fig = TRUE`.
#'
#' @return Saves and/or returns contour plots of the relative risk associated with the selected
#' climate variable over lags and exposure levels, stratified by spatial level.

contour_plot <- function(data,
                         param_terms,
                         model,
                         level= "country",
                         save_fig = FALSE,
                         nlag= 4,
                         output_dir= NULL) {
  # Validate output_dir if saving
  if (save_fig && is.null(output_dir)) {
    stop("output_dir must be provided if save_fig = TRUE")
  }
  # customize the output name
  output_file = paste0("contour_plot_", param_terms, "_", level, ".pdf")
  # getting the best model
  data <- create_inla_indices(data)
  # model_ <- model$model
  # Extract full coefficients and covariance matrix
  # coef <- model_$summary.fixed$mean
  # vcov <- model_$misc$lincomb.derived.covariance.matrix
  # Extract predictions from the tmax DLNM centered on overall mean Tmax
  predt <- get_predictions(data, param_terms = param_terms, model, level = level)

  if (level == "country"){
    # Generate contour plot
    if (save_fig){
      pdf(file.path(output_dir, output_file), width = 6.5, height = 6)
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
        main = "Malaria Under five cases",
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


#' Plot Relative Risk of Malaria by Climate Exposure
#'
#' @description
#' Generates and optionally saves relative risk plots of malaria cases as a function
#' of exposure to maximum temperature (`tmax`) or cumulative rainfall (`rainfall`)
#' across different spatial levels: country, Region, or District. The function visualizes
#' the estimated relative risks and their 95% confidence intervals derived from the
#' fitted INLA model.
#'
#' @param data A data frame or list returned from the `combine_health_climate_data()` function,
#' containing harmonized health and climate data.
#' @param save_fig Logical. Whether to save the plots to PDF. Defaults to `FALSE`.
#' @param param_terms A character string specifying the climate exposure variable to plot.
#' Accepts `"tmax"` for maximum temperature or `"rainfall"` for cumulative rainfall.
#' @param model The fitted model object returned from the `run_inla_models()` function,
#' containing posterior estimates used to compute relative risks.
#' @param level A character string specifying the spatial level for plotting.
#' Acceptable values are `"country"`, `"Region"`, or `"District"`. Defaults to `"country"`.
#' @param output_dir A character string specifying the path where the output PDF file
#' should be saved. Required if `save_fig = TRUE`. Defaults to `NULL`.
#'
#' @return A line plot (or set of plots) showing the estimated relative risk of malaria
#' across the range of the specified climate variable. If `save_fig = TRUE`, the plots
#' are saved as PDF files in the specified `output_dir`.

plot_relative_risk <- function(data,
                               save_fig = FALSE,
                               param_terms,
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
           main = paste("Malaria Relative Risk by",
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
             main = paste("Malaria Relative Risk by",
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
             main = paste("Malaria Relative Risk by",
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


#' Attribution Calculation for Climate Parameters
#'
#' @description
#' Computes the Minimum Risk Temperature (MRT), high-risk exposure range, and
#' attributable risk (number and fraction) for a given climate parameter
#' (e.g., maximum temperature, minimum temperature, or precipitation).
#' This function uses the crossbasis approach and distributed lag non-linear models (DLNM),
#' requiring the `attrdl` function from Gasparrini's DLNM framework.
#'
#' @param data A data frame or list containing the merged health and climate data.
#'
#' @param param_terms Character string specifying the climate variable of interest,
#' such as \code{"tmax"}, \code{"tmin"}, or \code{"rainfall"}. Default is \code{"tmax"}.
#'
#' @param model The fitted INLA model output from \code{run_inla_models()},
#' which includes fixed effect estimates and the crossbasis structure.
#'
#' @param attrdl_path Character string specifying the path to the custom
#' \code{attrdl.R} file. This file should define the \code{attrdl()} function
#' used to compute attributable risk.
#'
#' @param param_threshold Numeric value specifying the threshold for relative risk (RR)
#' above which exposure is considered harmful (attributable). Default is \code{1},
#' but can accept decimal values such as \code{1.1}.
#'
#' @param level Character string specifying the spatial level of analysis.
#' Can take one of the following values: \code{"country"}, or \code{"Region"}.
#'
#' @param nlag Integer. Number of lags used in constructing the crossbasis matrix.
#' Used to validate the lag structure. Default is \code{4}.
#'
#' @return
#' If \code{level = "country"}, returns a named list containing:
#' \itemize{
#'   \item \code{MRT}: Minimum Risk Temperature (or value for the given parameter).
#'   \item \code{MRT_CI}: Confidence interval for MRT based on predicted RR.
#'   \item \code{High_pt_Range}: Range of values where RR > threshold.
#'   \item \code{Attributable_Risk_Number}: Number of cases attributable to exposure above threshold.
#'   \item \code{Attributable_Risk_Fraction}: Fraction (%) of cases attributable.
#' }
#'
#' If \code{level = "Region"} (or similar finer levels), returns a \code{data.frame} with the following columns:
#' \itemize{
#'   \item \code{Region}, \code{Year}
#'   \item \code{MRT}, \code{High_Temperature_Lower}, \code{High_Temperature_Upper}
#'   \item \code{Attributable_Risk_Number}, \code{Attributable_Risk_Fraction}
#' }

attribution_calculation <- function(data,
                                    param_terms,
                                    model,
                                    param_threshold = 1,
                                    attrdl_path = "attrdl.R",
                                    level,
                                    nlag=4){

  # Extract full coefficients and covariance matrix
  coef <- model$summary.fixed$mean
  vcov <- model$misc$lincomb.derived.covariance.matrix

  # Find the position of terms associated with the temperature crossbasis
  indt <- grep(paste0("basis_", param_terms), model$names.fixed)

  # Validate model data and inputs
  if (is.null(coef) || is.null(vcov)) stop("Model coefficients or covariance matrix are missing.")
  if (length(indt) == 0) stop("No terms associated with 'basis_tmax' found in the model.")

  # Recreate the temperature crossbasis (cb) using the original structure
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
    predt <- crosspred(basis_matrices[[param_terms]], coef = coef[indt], vcov = vcov[indt, indt],
                       model.link = "log", at = param_terms_range, cen = cen)

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
      data[[param_terms]], basis_matrices[[param_terms]], data$Malaria,
      coef = coef[indt], vcov = vcov[indt, indt],
      type = "an", cen = mr, range = high_pt_bounds
    ) %>% round()

    an_risk_fraction_hot <- attrdl(
      data[[param_terms]], basis_matrices[[param_terms]], data$Malaria,
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
              data_region_year$Malaria,
              coef = coef[indt], vcov = vcov[indt, indt],
              type = "an", cen = mr, dir = "forw", range = high_pt_bounds
            ) %>% round()

            an_risk_fraction_hot <- attrdl(
              data_region_year[[param_terms]], cb_region_year[[param_terms]],
              data_region_year$Malaria,
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
  } else {
    stop("The level parameter should be one c('country', 'Region') values")
  }

  return(results)
}


#' Read in the Combine function for doing all the analysis once
#'
#' @description the function Malaria_do_analysis() combine all functions
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
#' @param Malaria_case_col Character. Name of the column in the dataframe
#' that contains the Diarrhea cases to be considered.
#' @param tot_pop_col Character. Name of the column in the dataframe that
#' contains the total population.
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
#' @param runoff_col Character. Name of the column in the dataframe that
#' contains the monthly runoff water data. Defaults to NULL.
#' @param cvh_col Character. Name of the column in the dataframe that
#' contains the vegetation index. This can be the Normalized Difference
#' Vegetation Index (NDVI). Defaults to NULL.
#' @param spi_col Character. Name of the column in the dataframe that
#' contains the standardized precipitation index. Defaults to NULL.
#' @param maxlag Character. Number corresponding to the maximum lag to be
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
#' effect, relative risk plot the attributable number and fraction.


Malaria_do_analysis <- function(health_data_path,
                                climate_data_path,
                                map_path,
                                Region_col,
                                District_col,
                                Date_col= NULL,
                                Year_col,
                                Month_col,
                                Malaria_case_col,
                                tot_pop_col,
                                tmin_col,
                                tmean_col,
                                tmax_col,
                                rainfall_co,
                                r_humidity_col,
                                runoff_col= NULL,
                                cvh_col= NULL,
                                spi_col = NULL,
                                maxlag = 4,
                                geometry_col,
                                basis_matrices_choices,
                                param_terms,
                                param_level,
                                save_fig = FALSE,
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
                                               Malaria_case_col,
                                               tot_pop_col,
                                               tmin_col,
                                               tmean_col,
                                               tmax_col,
                                               rainfall_col,
                                               r_humidity_col,
                                               runoff_col,
                                               cvh_col,
                                               spi_col,
                                               maxlag,
                                               geometry_col,
                                               output_dir)

  # data for INLA
  data_inla <- create_inla_indices(combined_data$data)

  # fitting the model
  a <- run_inla_models(data_inla, basis_matrices_choices)

  #
  reff_plot_monthly <- plot_monthly_random_effects(combined_data, model = a$model,
                                                   output_dir = output_dir)

  #
  reff_plot_yearly <- yearly_spatial_random_effect(combined_data, model = a$model,
                                                   output_dir = output_dir)

  # contour plots
  contour_plot(combined_data$data,  param_terms, model=a$model,  level = param_level,
               output_dir = output_dir, save_fig)

  # relative ristk plot
  plot_relative_risk(combined_data$data, save_fig = save_fig,
                     param_terms = param_terms, model = a$model,
                     level = param_level, output_dir = output_dir)

  # attribution fraction and number
  attr_frac_num <- attribution_calculation(combined_data$data, param_terms = param_terms,
                                           model =a$ model, param_threshold = param_threshold,
                                           level = param_level, nlag = maxlag)

  res <- list(reff_plot_monthly, reff_plot_yearly, plot_relative_risk, attr_frac_num)

  return(res)
}
