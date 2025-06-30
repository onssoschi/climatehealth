#-------------------------------------------------------------------------------
#' @title R-code for Diarrhea disease cases attributable to extreme precipitation
#' and extreme temperature
#'
#' You will need to load the following package under R for the code to work:
#'
required_packages <- c("tidyverse", "INLA", "stats", "data.table", "here",
"sf", "sp", "spdep", "dlnm", "tsModel", "hydroGOF", "RColorBrewer", "openxlsx",
 "readxl", "splines", "geofacet", "patchwork", "tools")
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
  # Load and process map
  map <- read_sf(map_path) %>%
    select(Region = !!sym(Region_col),
           District = !!sym(District_col),
           geometry = !!sym(geometry_col)) %>%
    mutate(geometry = st_make_valid(geometry))

  # Create adjacency matrix
  nb_file <- if (!is.null(output_dir)) file.path(output_dir, "nbfile") else NULL
  g_file <- if (!is.null(output_dir)) file.path(output_dir, "map.graph")
  else tempfile(pattern = "map", fileext = ".graph")

  if (!is.null(nb_file) && file.exists(nb_file)) {
    nb.map <- read.gal(nb_file)
  } else {
    nb.map <- poly2nb(as_Spatial(map$geometry), snap = 1e-4)
    if (!is.null(nb_file)) write.nb.gal(nb.map, nb_file)
  }

  if (is.null(output_dir) || !file.exists(g_file)) {
    nb2INLA(g_file, nb.map)
  }

  return(list(map = map, nb.map = nb.map, graph_file = g_file))
}


#' Read in and format health data - Diarrhea diseases case
#'
#' @description Read in a csv file of monthly time series of health and the
#' population data, rename columns and create time variables for spatiotemporal
#' analysis
#'
#' @param health_data_path Path to a csv file containing a monthly time series of data
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
#'
#' @return A dataframe with formatted and renamed columns.

load_and_process_data <- function(health_data_path,
                                  Region_col,
                                  District_col,
                                  Date_col = NULL,
                                  Year_col,
                                  Month_col,
                                  Diarrhea_case_col,
                                  tot_pop_col) {
  # Load health and climate data
  ext <- tolower(file_ext(health_data_path))
  # Load data based on file extension
  data <- switch(ext,
                 "rds" = read_rds(health_data_path),
                 "csv" = read_csv(health_data_path, show_col_types = FALSE),
                 "xlsx" = read_excel(health_data_path),
                 stop("Unsupported file type: must be .rds, .csv, or .xlsx")
                 )

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
#' @param climate_data_path Path to a csv file containing a monthly time series of data
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
#' @param spi_col Character. Name of the column in the dataframe that
#' contains the standardized precipitation index. Defaults to NULL.
#' @param max_lag Character. Number corresponding to the maximum lag to be
#' considered for the delay effect. It should be between 2 an 4. Defaults to 2
#'
#' @return climate dataframe with formatted and renamed columns, and the lag
#' variables

load_and_process_climatedata <- function(climate_data_path,
                                         District_col,
                                         Year_col,
                                         Month_col,
                                         tmin_col,
                                         tmean_col,
                                         tmax_col,
                                         rainfall_col,
                                         r_humidity_col,
                                         runoff_col = NULL,
                                         spi_col = NULL,
                                         max_lag = 4) {
  # Detect file extension
  ext <- tolower(tools::file_ext(climate_data_path))

  # Read file
  data <- switch(ext,
                 "rds" = readr::read_rds(climate_data_path),
                 "csv" = readr::read_csv(climate_data_path, show_col_types = FALSE),
                 "xlsx" = readxl::read_excel(climate_data_path),
                 stop("Unsupported file type: must be .rds, .csv, or .xlsx")
                 )

  # Map columns to standard names, excluding NULLs
  var_map <- list(District = District_col, Year = Year_col, Month = Month_col,
                  tmin = tmin_col, tmean = tmean_col, tmax = tmax_col,
                  rainfall = rainfall_col, r_humidity = r_humidity_col,
                  runoff = runoff_col, spi = spi_col)

  var_map <- var_map[!sapply(var_map, is.null)]
  selected_cols <- as.character(var_map)
  rename_vec <- setNames(selected_cols, names(var_map))

  # Select and rename
  climate_data <- data %>%
    dplyr::select(all_of(selected_cols)) %>%
    dplyr::rename(!!!rename_vec)

  # Function to create lagged variables
  create_lags <- function(df, var, max_lag) {
    for (i in 1:max_lag) {
      df[[paste0(var, "_lag", i)]] <- dplyr::lag(df[[var]], i)
    }
    return(df)
  }

  # Determine variables to lag
  vars_to_lag <- intersect(names(rename_vec),
                           c("tmin", "tmean", "tmax", "rainfall",
                             "r_humidity", "runoff", "spi"))

  # Create lagged data
  lagged_data <- lapply(vars_to_lag,
                        function(var) create_lags(climate_data[var], var, max_lag))

  # Bind all
  final_data <- dplyr::bind_cols(climate_data[c("District", "Year", "Month")],
                                 lagged_data)

  return(final_data)
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
                                        tmin_col,
                                        tmean_col,
                                        tmax_col,
                                        rainfall_col,
                                        r_humidity_col,
                                        runoff_col=NULL,
                                        spi_col = NULL,
                                        max_lag = 2,
                                        geometry_col,
                                        output_dir = NULL){

  # Load data
  health_data <- load_and_process_health_data(health_data_path, Region_col,
                                              District_col,Date_col, Year_col,
                                              Month_col, Diarrhea_case_col,
                                              tot_pop_col)

  climate_data <- load_and_process_climatedata(climate_data_path, District_col,
                                               Year_col, Month_col, tmin_col,
                                               tmean_col, tmax_col, rainfall_col,
                                               r_humidity_col, runoff_col,
                                               spi_col, max_lag)

  map_data <- load_and_process_map(map_path, Region_col, District_col,
                                   geometry_col, output_dir)

  # Merge health + climate
  data <- health_data %>%
    left_join(climate_data, by = join_by(District, Year, Month)) %>%
    distinct() %>%
    group_by(Region, District) %>%
    mutate(time = (Year - min(Year)) * 12 + Month) %>%
    ungroup()

  # Build grid codes
  grid_data <- data %>%
    select(Region, District) %>%
    distinct() %>%
    group_by(Region) %>%
    mutate(region_code = cur_group_id(),
           district_number = row_number(),
           district_code = as.integer(paste0(region_code, district_number))) %>%
    ungroup()

  # Attach codes
  data <- left_join(data, grid_data, by = c("Region", "District")) %>%
    arrange(region_code, district_code)

  map <- left_join(map_data$map, grid_data, by = c("Region", "District")) %>%
    arrange(region_code, district_code)

  grid_data <- rename(grid_data, name = Region, Code_num = region_code)

  # Summary stats
  summary_stats <- list(tmin = summary(data$tmin),
                        tmax = summary(data$tmax),
                        rainfall = summary(data$rainfall),
                        rhumidity = summary(data$r_humidity)
                        )

  # Return combined
  return(list(map = map, nb.map = map_data$nb.map,
              graph_file = map_data$graph_file,
              data = data, grid_data = grid_data,
              summary = summary_stats))
}


#' Plot Time Series of Health and Climate Variables
#'
#' @description
#' Generate time series plots for combined health and climate data
#' prepared for spatiotemporal and DLNM analysis. Supports aggregation
#' at the country, region, or district level.
#'
#' @param data A data frame containing the combined health and climate data.
#' @param param_terms A character vector specifying variables to plot (tmax,
#' tmean, tmin, Diarrhea). Use "all" to include all available variables.
#' @param level Aggregation level: one of "country", "region", or "district".
#' @param year Optional numeric vector to filter data by year(s).
#' @param save_fig Logical. Whether to save the figure as a PDF.
#' @param output_dir Directory path to save the figure. Default to NULL
#'
#' @return A ggplot object.

plot_health_climate_timeseries <- function(data,
                                           param_terms,
                                           level = "country",
                                           year = NULL,
                                           save_fig = TRUE,
                                           output_dir = NULL) {

  vars_all <- c("Diarrhea", "tmin", "tmean", "tmax", "rainfall")
  vars_to_plot <- if (param_terms == "all") vars_all else param_terms

  if (!is.null(year)) data <- data %>% filter(Year %in% year)

  data <- data %>% mutate(Date = as.Date(paste(Year, Month, 1, sep = "-")))

  missing <- setdiff(vars_to_plot, names(data))
  if (length(missing)) stop("Missing columns: ", paste(missing, collapse = ", "))

  group_var <- switch(tolower(level),
                      country = NULL,
                      region = "Region",
                      district = "District",
                      stop("Invalid level"))

  group_cols <- c("Date", group_var)

  agg <- data %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(across(all_of(vars_to_plot),
                     ~ if (level == "country" && "Diarrhea" %in% cur_column()) {
                       if (cur_column() == "Diarrhea") sum(.x, na.rm = TRUE) else mean(.x, na.rm = TRUE)
                     } else {
                       mean(.x, na.rm = TRUE)
                     }),
              .groups = "drop") %>%
    pivot_longer(cols = all_of(vars_to_plot), names_to = "variable", values_to = "value")

  if (!is.null(group_var)) agg <- agg %>% rename(Group = all_of(group_var))

  title_text <- if (length(vars_to_plot) == 1) {
    paste("Time Series of", vars_to_plot)
  } else if (length(vars_to_plot) == length(vars_all)) {
    "Time Series of All Health & Climate Variables"
  } else {
    paste("Time Series of", paste(vars_to_plot, collapse = ", "))
  }

  p <- ggplot(agg, aes(x = Date, y = value)) +
    geom_line(aes(color = if (!is.null(group_var)) Group), linewidth = 1) +
    facet_wrap(~variable, scales = "free_y", ncol = 1) +
    scale_x_date(date_breaks = "6 month", date_labels = "%Y-%m") +
    labs(title = title_text, x = "Date", y = "Value") +
    theme_minimal() +
    theme(legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))

  if (save_fig) {
    fname <- paste0("timeseries_",
                    paste(vars_to_plot, collapse = "_"), "_", level, ".pdf")
    ggsave(file.path(output_dir, fname), p, width = 12, height = 7)
  }

  return(p)
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

  nlag <- ncol(dplyr::select(data, all_of(grep("^tmax_lag", names(data),
                                               value = TRUE))))
  var_defs <- list( tmax = "tmax_lag", tmin = "tmin_lag", tmean = "tmean_lag",
    rainfall = "rf_lag", r_humidity = "rh_lag",
    runoff = "runoff_lag", spi = "spi_lag")

  vars <- lapply(names(var_defs), function(var) {
    cols <- c(var, paste0(var_defs[[var]], 1:nlag))
    if (all(cols %in% names(data))) dplyr::select(data, all_of(cols)) else NULL
  })
  names(vars) <- names(var_defs)
  vars <- vars[!sapply(vars, is.null)]

  lagknot <- equalknots(0:nlag, 2)

  basis_matrices <- lapply(names(vars), function(var) {
    x <- vars[[var]]
    cb <- crossbasis(
      x,
      argvar = list(fun = "ns", knots = equalknots(x[[1]], 2)),
      arglag = list(fun = "ns", knots = nlag / 2)
    )
    colnames(cb) <- paste0("basis_", var, ".", colnames(cb))
    cb
  })

  names(basis_matrices) <- names(vars)
  basis_matrices
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
  overall_rate <- sum(data$Diarrhea, na.rm = TRUE) / sum(data$tot_pop, na.rm = TRUE)
  data$E <- overall_rate * data$tot_pop # Expected counts
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


#' Run models of increasing complexity in INLA: Fit a baseline model including
#' spatiotemporal random effects.
#'
#' @description: create and run multiple INLA (Integrated Nested
#' Laplace Approximation) models to the dataset, evaluates them using
#' DIC (Deviance Information Criterion), and identifies the best-fitting model.
#'
#' @param combined_data is the dataset coming from combine_health_climate_data() function.
#' @param basis_matrices_choices A character vector specifying the basis matrix
#' parameters to be included in the model. Possible values are "tmax", "tmin",
#' "rainfall", "r_humidity", and "spi".  Default to "tmax", and users can select
#' one or more of these parameters for inclusion in the model.
#' @param output_dir is the path to save model output
#' @param save_csv Boolean. Whether to save the results as a CSV. Defaults to
#' FALSE.
#' @param family A character string specifying the probability distribution for
#' the response variable. Default is "poisson". The user may also have the
#' possibility to choose "nbinomial" for a negative binomial distribution.
#' @param config is a Boolean flag to enable additional model configurations.
#'
#' @returns list of the model, the baseline_model, and the dic_table.

run_inla_models <- function(combined_data,
                            basis_matrices_choices,
                            output_dir= NULL,
                            save_csv = FALSE,
                            family = "poisson",
                            config = FALSE) {
  if (save_csv && is.null(output_dir))
    stop("output_dir must be provided if save_csv = TRUE")

  data <- create_inla_indices(combined_data$data)
  basis <- set_cross_basis(combined_data$data)
  graph_file <- combined_data$graph_file

  prior <- list(prec = list(prior = "pc.prec", param = c(0.5 / 0.31, 0.01)))

  base_formula <- Diarrhea ~ 1 +
    f(Month, replicate = region_index, model = "rw1", cyclic = TRUE,
      constr = TRUE, scale.model = TRUE, hyper = prior) +
    f(district_index, model = "bym2", replicate = year_index,
      graph = graph_file, scale.model = TRUE, hyper = prior)

  # Filter out basis_matrices_choices elements that don't exist or are NULL in basis
  valid_choices <- Filter(function(x)
    !is.null(basis[[x]]), basis_matrices_choices)

  if (length(valid_choices) > 0) {
    full_formula <- update(base_formula,
                           as.formula(paste("~ . +",
                                            paste0("basis$", valid_choices,
                                                   collapse = " + "))))
  } else {
    full_formula <- base_formula
  }

  fit <- function(formula) inla.rerun(inla(
    formula, data = data, family = family, offset = log(data$E),
    control.inla = list(strategy = "adaptive"),
    control.compute = list(dic = TRUE, config = config, cpo = TRUE, return.marginals = FALSE),
    control.fixed = list(correlation.matrix = TRUE, prec.intercept = 1, prec = 1),
    control.predictor = list(link = 1, compute = TRUE),
    verbose = FALSE))

  baseline_model <- fit(base_formula)
  model <- fit(full_formula)

  if (save_csv) {
    save(model,
         file = file.path(output_dir,
                          paste0("model_with_",paste(valid_choices, collapse = "_"), ".RData")))
  }

  dic_table <- data.table(
    Model = paste(valid_choices, collapse = " + "),
    DIC = round(model$dic$dic, 0),
    LogScore = round(mean(-log(model$cpo$cpo[model$cpo$failure == 0]), na.rm = TRUE), 3),
    LPML = round(sum(log(model$cpo$cpo[model$cpo$failure == 0]), na.rm = TRUE), 2)
  )

  list(model = model, baseline_model = baseline_model, dic_table = dic_table)
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

yearly_spatial_random_effect <- function(combined_data ,
                                         model,
                                         save_fig = FALSE,
                                         output_dir = NULL) {
  # Validate output_dir if saving
  if (save_fig && is.null(output_dir)) {
    stop("output_dir must be provided if save_fig = TRUE")
  }
  # Prepare data
  data <- create_inla_indices(combined_data$data)
  grid_data <- combined_data$grid_data
  map <- combined_data$map
  ntime <- length(unique(data$time))
  nyear <- length(unique(data$Year))
  ndistrict <- length(unique(data$district_code))
  # Extract spatial random effects
  space <- data.table(model$summary.random$district_index)
  space$Year <- rep(min(data$Year):max(data$Year), each = 2 * ndistrict)
  space$re <- rep(c(rep(1, ndistrict), rep(2, ndistrict)), nyear)
  space <- space[space$re == 1, ]
  space$District_code <- rep(unique(data$district_code), nyear)
  # Merge with spatial map
  space <- left_join(map, space, by = c("district_code" = "District_code"))
  # Plot
  space_effects <- ggplot() +
    geom_sf(data = space, aes(fill = mean), color = "black", size = 0.1) +
    scale_fill_gradient2(
      low = "green", mid = "white", high = "purple",
      midpoint = 0,
      limits = c(min(space$mean, na.rm = TRUE), max(space$mean, na.rm = TRUE)),
      name = "Contribution to\nlog(DIR)"
    ) +
    theme_void() +
    facet_wrap(~Year, ncol = 5)

  # Save if required
  if (save_fig) {
    ggsave(file.path(output_dir, "spatial_random_effects_per_year.pdf"),
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
#' @param model The fitted model from run_inla_models() function.
#' @param level A character vector specifying the spatial disaggregation level.
#' Can take one of the following values: "country", "Region", or "District".
#'
#' @return Dataframe containing cumulative relative risk at country, Region, or
#' District level.

get_predictions <- function(data,
                            param_terms,
                            model,
                            level){
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
#' @param save_fig Boolean. Whether to save the plot as an output. Defaults to
#' FALSE.
#' @param output_dir Path to save model output. Default to NULL
#'
#' @return contour plot at country, Region and District level

contour_plot <- function(data,
                         param_terms,
                         model,
                         level,
                         year = NULL,
                         save_fig = FALSE,
                         output_dir = NULL) {

  if (save_fig && is.null(output_dir)) {
    stop("output_dir must be provided if save_fig = TRUE")
  }

  if (!is.null(year)) {
    if (!"Year" %in% names(data)) stop("'Year' column not found in data.")
    data <- filter(data, Year %in% year)
  }

  data <- create_inla_indices(data)
  predt <- get_predictions(data, param_terms = param_terms, model, level = level)

  plot_contour <- function(x, y, z, title) {
    nlag <- max(x)
    pal <- rev(brewer.pal(11, "PRGn"))
    levels <- pretty(range(z, na.rm = TRUE), 20)
    col1 <- colorRampPalette(pal[1:6])
    col2 <- colorRampPalette(pal[6:11])
    cols <- c(col1(sum(levels <= 1)), col2(sum(levels > 1)))

    filled.contour(
      x, y, z,
      xlab = "Lag",
      ylab = ifelse(param_terms == "tmax", "Temperature (°C)",
                    ifelse(param_terms == "rainfall", "Rainfall (mm)", param_terms)),
      main = title,
      col = cols,
      levels = levels,
      plot.axes = {
        axis(1, at = 0:nlag, labels = 0:nlag)
        axis(2)
      }
    )
  }

  nlag <- data %>% select(contains(param_terms)) %>% ncol() - 1
  lag_seq <- seq(0, nlag, 0.25)

  if (save_fig) {
    output_file <- file.path(
      output_dir,
      paste0("contour_plot_", param_terms, "_", level,
             if (!is.null(year)) paste0("_", paste(year, collapse = "_")), ".pdf")
    )
    pdf(output_file, width = 8, height = 8)
  }

  if (level == "country") {
    plot_contour(lag_seq, predt$predvar, t(predt$matRRfit), title = "Contour Plot for Country")
  } else {
    groups <- if (level == "Region") unique(data$Region) else unique(data$District)
    for (grp in groups) {
      plot_contour(lag_seq, predt[[grp]]$predvar, t(predt[[grp]]$matRRfit),
                   title = paste("Contour Plot for", grp))
    }
  }

  if (save_fig) dev.off()
}


#' Plot Relative Risk Map at District or Region Level
#'
#' @description
#' Generates a map of the relative risk of diarrhea cases associated with climate
#' hazards, including extreme temperature and cumulative rainfall, at a specified
#' spatial level (District or Region).
#'
#' @param combined_data A list returned from the `combine_health_climate_data()`
#' function. This list should include both the health-climate data and the map data.
#' @param model The fitted model object returned from the `run_inla_models()` function.
#' @param param_terms A character vector or list specifying the climate parameters
#' (e.g., "tmax" for maximum temperature, "rainfall" for precipitation) to include in the map.
#' Defaults to "tmax".
#' @param level A character string indicating the spatial aggregation level.
#' Options are "Region" or "District". Defaults to "District".
#' @param year A character string specifying the year for which the map should be generated.
#' Defaults to NULL.
#' @param output_dir A string specifying the directory path where the output PDF
#' file should be saved. Defaults to NULL.
#' @param save_fig Logical. If TRUE, saves the plot to the specified directory.
#' Defaults to FALSE.
#'
#' @return relative risk map at District level.

plot_rr_map <- function(combined_data,
                        model,
                        param_terms = "tmax",
                        level = "District",
                        year = NULL,
                        output_dir = NULL,
                        save_fig = FALSE) {
  data <- combined_data$data
  map <- combined_data$map
  stopifnot("Year" %in% names(data))

  if (save_fig && is.null(output_dir)) {
    stop("output_dir must be provided if save_fig = TRUE")
  }

  years <- if (is.null(year)) sort(unique(data$Year)) else year
  grouping_var <- ifelse(level == "District", "District", "Region")

  # Get RR data for each year
  get_rr_df <- function(yr) {
    pred <- get_predictions(filter(data, Year == yr), param_terms, model, level)
    map_dfr(names(pred), function(name) {
      vals <- pred[[name]]
      if (anyNA(vals$allRRfit)) return(NULL)
      tibble(!!grouping_var := name, RR = median(vals$allRRfit, na.rm = TRUE))
    })
  }

  rr_list <- lapply(years, get_rr_df)
  rr_all <- bind_rows(rr_list)
  rr_range <- range(rr_all$RR, na.rm = TRUE)

  # Create plots for each year
  plots <- lapply(seq_along(years), function(i) {
    map_rr <- left_join(map, rr_list[[i]], by = grouping_var)
    ggplot(map_rr) +
      geom_sf(aes(fill = RR), color = "black", size = 0.2) +
      scale_fill_gradient2(
        low = "blue", mid = "white", high = "red",
        midpoint = 1, limits = rr_range, na.value = "grey80", name = "RR"
      ) +
      theme_minimal() +
      labs(title = paste("Year:", years[i]), subtitle = paste("Exposure:", param_terms)) +
      theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())
  })

  # Combine all plots
  combined_plot <- wrap_plots(plots) +
    plot_annotation(
      title = paste("Relative Risk of Diarrhea by", level),
      subtitle = paste("Exposure:", param_terms),
      theme = theme(plot.title = element_text(size = 16, face = "bold"))
    )

  # Save to file if needed
  if (save_fig) {
    ggsave(file.path(output_dir,
                     paste0("RR_map_", param_terms, "_", level, "_all_years.pdf")),
           combined_plot, width = 14, height = 10)
  }

  return(combined_plot)
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
#' @param year A character string specifying the year for which the map should be generated.
#' Defaults to NULL.
#' @param output_dir is the path where the pdf file will be saved. Default to NULL
#'
#' @return relative risk plot at country, Region, and District level.

plot_relative_risk <- function(data,
                               model,
                               param_terms,
                               level = "country",
                               year = NULL,
                               output_dir = NULL,
                               save_fig = FALSE) {

  if (!"Year" %in% names(data)) stop("'Year' column not found in data.")
  if (is.null(year)) year <- sort(unique(data$Year))
  if (save_fig) {
    if (is.null(output_dir)) stop("output_dir must be provided if save_fig = TRUE")
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  }

  output_pdf <- if (save_fig)
    file.path(output_dir, paste0("RR_", param_terms, "_",
                                 tolower(level), "_all_plots.pdf")) else NULL

  build_plot <- function(pred, yr) {
    if (anyNA(pred$allRRfit)) return(NULL)
    ggplot(tibble(x = pred$predvar, y = pred$allRRfit, ymin = pred$allRRlow,
                  ymax = pred$allRRhigh), aes(x, y)) +
      geom_line(color = "red", linewidth = 1) +
      geom_ribbon(aes(ymin = ymin, ymax = ymax), fill = "red", alpha = 0.3) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "gray",
                 linewidth = 0.5) +
      labs(title = yr, x = param_terms, y = "Relative Risk") +
      theme_minimal() + theme(plot.title = element_text(size = 9))
  }

  if (level == "country") {
    plots <- lapply(year, function(yr) {
      build_plot(get_predictions(filter(data, Year == yr), param_terms,
                                 model, level), yr)
    }) %>% Filter(Negate(is.null), .)

    if (save_fig) {
      pdf(output_pdf, width = 14, height = 10)
      walk(plots, print)
      dev.off()
    }

    return(wrap_plots(plots) + plot_annotation(
      title = "Exposure-Response Curves by Country",
      subtitle = paste(param_terms, "Years:", paste(year, collapse = ", "))
    ))
  }

  # Region or District
  group_plots <- list()
  for (yr in year) {
    preds <- get_predictions(filter(data, Year == yr), param_terms, model, level)
    for (grp in names(preds)) {
      p <- build_plot(preds[[grp]], yr)
      if (!is.null(p)) group_plots[[grp]] <- c(group_plots[[grp]], list(p))
    }
  }

  if (save_fig) {
    pdf(output_pdf, width = 10, height = 8)
    for (grp in names(group_plots)) {
      print(wrap_plots(group_plots[[grp]]) + plot_annotation(
        title = paste("Exposure-Response Curves:", grp),
        subtitle = paste(param_terms, "Years:", paste(year, collapse = ", "))
      ))
    }
    dev.off()
  }

  return(group_plots)
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
#' @param year A character string specifying the year for which the map should
#' be generated. Defaults to NULL.
#' @return results which contain the attribution number and fraction at country,
#'  region and district level.

attribution_calculation <- function(data,
                                    param_terms,
                                    model,
                                    param_threshold = 1,
                                    attrdl_path = "attrdl.R",
                                    level,
                                    year = NULL) {
  source(attrdl_path)

  coef <- model$summary.fixed$mean
  vcov <- model$misc$lincomb.derived.covariance.matrix
  indt <- grep(paste0("basis_", param_terms), model$names.fixed)
  if (length(indt) == 0) stop("No terms for 'basis_", param_terms, "' found in model.")

  if (!is.null(year)) {
    stopifnot("Year" %in% names(data), all(year %in% unique(data$Year)))
    data <- dplyr::filter(data, Year %in% year)
  }

  compute_metrics <- function(df) {
    if (nrow(df) == 0 || all(is.na(df[[param_terms]]))) return(NULL)
    basis <- set_cross_basis(df)[[param_terms]]
    cen <- mean(df[[param_terms]], na.rm = TRUE)
    at_vals <- seq(min(df[[param_terms]], na.rm = TRUE),
                   max(df[[param_terms]], na.rm = TRUE), 0.1)

    pred <- crosspred(basis, coef = coef[indt], vcov = vcov[indt, indt],
                      model.link = "log", at = at_vals, cen = cen)

    high_vals <- at_vals[pred$allRRfit > param_threshold]
    if (length(high_vals) == 0) return(NULL)
    bounds <- range(high_vals)
    ref_temp <- pred$predvar[which.min(pred$allRRfit)]

    an_num <- round(attrdl(df[[param_terms]], basis, df$Diarrhea,
                           coef = coef[indt], vcov = vcov[indt, indt],
                           type = "an", cen = ref_temp, dir = "forw", range = bounds))
    an_frac <- round(attrdl(df[[param_terms]], basis, df$Diarrhea,
                            coef = coef[indt], vcov = vcov[indt, indt],
                            type = "af", cen = ref_temp, dir = "forw", range = bounds) * 100, 2)
    pop <- sum(df$tot_pop, na.rm = TRUE)
    cases_100k <- if (pop > 0) round((an_num / pop) * 1e5, 0) else NA

    list(MRT = round(ref_temp, 2), High_Lower = round(bounds[1], 2),
         High_Upper = round(bounds[2], 2), AR_Number = an_num,
         AR_Fraction = an_frac, AR_per_100k = cases_100k)
  }

  group_and_compute <- function(grp_vars) {
    data %>%
      dplyr::group_by(across(all_of(grp_vars))) %>%
      dplyr::group_split() %>%
      purrr::map_dfr(~{
        res <- compute_metrics(.x)
        if (is.null(res)) return(NULL)
        dplyr::tibble(!!!.x[1, grp_vars], !!!res)
      })
  }

  switch(level,
         "country" = compute_metrics(data),
         "Region" = group_and_compute(c("Region", "Year")),
         "District" = group_and_compute(c("Region", "District", "Year")),
         stop("Invalid level. Choose 'country', 'Region', or 'District'."))
}


################################################################################

#' Run Full Diarrhea-Climate Analysis Pipeline
#'
#' @description
#' The `diarrhea_do_analysis()` function runs the complete analysis workflow
#' by combining multiple functions to analyze the association between diarrhea
#' cases and climate variables. It processes health, climate, and spatial data,
#' fits models, generates plots, and calculates attributable risk.
#'
#' @param health_data_path Data frame containing the processed health data.
#' @param climate_data_path Data frame containing the processed climate data.
#' @param map_path Data frame containing the spatial map data (shapefile or equivalent).
#' @param Region_col Character. Name of the column containing region names.
#' @param District_col Character. Name of the column containing district names.
#' @param Date_col Character. Name of the column containing the date. Defaults to NULL.
#' @param Year_col Character. Name of the column containing the year.
#' @param Month_col Character. Name of the column containing the month.
#' @param Diarrhea_case_col Character. Name of the column containing diarrhea case counts.
#' @param tot_pop_col Character. Name of the column containing total population.
#' @param tmin_col Character. Name of the column containing minimum temperature.
#' @param tmean_col Character. Name of the column containing mean temperature.
#' @param tmax_col Character. Name of the column containing maximum temperature.
#' @param rainfall_col Character. Name of the column containing cumulative
#' monthly rainfall.
#' @param r_humidity_col Character. Name of the column containing relative humidity.
#' @param runoff_col Character. Name of the column containing monthly runoff
#' data. Defaults to NULL.
#' @param spi_col Character. Name of the column containing the Standardized
#' Precipitation Index. Defaults to NULL.
#' @param max_lag Numeric. Maximum lag to consider in the model
#' (typically 2 to 4). Defaults to 2.
#' @param geometry_col Character. Name of the geometry column in the shapefile
#' (usually "geometry").
#' @param basis_matrices_choices Character vector specifying basis matrix
#' parameters to include in the model (e.g., "tmax", "tmin", "rainfall",
#' "r_humidity", "spi"). Defaults to "tmax".
#' @param param_terms Character vector specifying the exposure variables of interest
#' (e.g., "tmax", "rainfall"). Defaults to "tmax".
#' @param level Character. Spatial disaggregation level: "country", "Region", or "District".
#' Defaults to "country".
#' @param save_fig Logical. If TRUE, saves the generated plots. Defaults to FALSE.
#' @param param_threshold Numeric. Exposure threshold above which risk is
#' considered attributable. Defaults to 1. Can be a decimal value.
#' @param year Character. The year for which the analysis is performed. Defaults to NULL.
#' @param output_dir String. Path to the directory where outputs (e.g., plots, maps) should be saved.
#'
#' @return A list containing:
#' \itemize{
#'   \item Model output from INLA
#'   \item Monthly random effects plot
#'   \item Yearly random effects plot
#'   \item Contour plot
#'   \item Relative risk map
#'   \item Relative risk plot
#'   \item Attributable fraction and number summary
#' }

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
                                 tmin_col,
                                 tmean_col,
                                 tmax_col,
                                 rainfall_col,
                                 r_humidity_col,
                                 runoff_col,
                                 spi_col = NULL,
                                 max_lag = 2,
                                 geometry_col,
                                 basis_matrices_choices,
                                 param_terms,
                                 level,
                                 save_fig = FALSE,
                                 param_threshold = 1,
                                 year= NULL,
                                 family = "poisson",
                                 config = FALSE,
                                 save_csv = FALSE,
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
                                               tmin_col,
                                               tmean_col,
                                               tmax_col,
                                               rainfall_col,
                                               r_humidity_col,
                                               runoff_col,
                                               spi_col,
                                               max_lag,
                                               geometry_col,
                                               output_dir)

  #Base matrice
  basis <- set_cross_basis(combined_data$data)

  # fitting the model

  a <- run_inla_models(combined_data, basis_matrices_choices,output_dir = output_dir,
                       save_csv= save_csv, family = family, config =config )

  #
  reff_plot_monthly <- plot_monthly_random_effects(combined_data, model = a$model,
                                                   output_dir = output_dir)

  #
  reff_plot_yearly <- yearly_spatial_random_effect(combined_data, model = a$model,
                                                   output_dir = output_dir)

  # contour plots
  Contour_plot <- contour_plot(combined_data$data, param_terms, model=a$model,
                               level = level, output_dir = output_dir,
                               save_fig=save_fig, year=year)
  # rr map plots
  rr_map_plot <- plot_rr_map(combined_data, param_terms, model=a$model,
                             level = "District", year=year, output_dir = output_dir,
                             save_fig=save_fig)

  # relative ristk plot
  rr_plot <- plot_relative_risk(combined_data$data, param_terms = param_terms,
                                model = a$model,level = level, year=year,
                                output_dir = output_dir, save_fig = save_fig)

  # attribution fraction and number
  attr_frac_num <- attribution_calculation(combined_data$data,
                                           param_terms = param_terms,
                                           model = a$model,
                                           param_threshold = param_threshold,
                                           level = level, year=year)

  res <- list(a, reff_plot_monthly, reff_plot_yearly, Contour_plot, rr_map_plot,
              rr_plot, attr_frac_num)

  return(res)
}

