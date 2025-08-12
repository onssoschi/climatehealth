#' R-code for Diarrhea disease cases attributable to extreme precipitation
#' and extreme temperature

#' Read in and format country map data
#'
#' @description: Read in a shape file, rename columns and create the
#' adjacency matrix for spatiotemporal analysis.
#'
#' @param map_path The path to the country's shape file "sf" data.
#' @param region_col Character. The region column in the dataset.
#' @param district_col Character. The district column in the dataset.
#' @param geometry_col  Character. The geometry column in the dataset.
#' @param output_dir  Character. The path to output the processed map data to.
#'
#' @return
#' \itemize{
#'  \item 'map' The processed map
#'  \item 'nb.map'
#'  \item 'graph_file'
#'  }
#'
#' @export
load_and_process_map <- function(map_path,
                                 region_col,
                                 district_col,
                                 geometry_col,
                                 output_dir = NULL){
  # Load and process map
  map <- sf::read_sf(map_path) %>%
    select(region = !!sym(region_col),
           district = !!sym(district_col),
           geometry = !!sym(geometry_col)) %>%
    mutate(geometry = sf::st_make_valid(geometry))

  # Create adjacency matrix
  nb_file <- if (!is.null(output_dir)) file.path(output_dir, "nbfile") else NULL
  g_file <- if (!is.null(output_dir)) file.path(output_dir, "map.graph")
  else tempfile(pattern = "map", fileext = ".graph")

  if (!is.null(nb_file) && file.exists(nb_file)) {
    nb.map <- spdep::read.gal(nb_file)
  } else {
    nb.map <- spdep::poly2nb(sf::as_Spatial(map$geometry), snap = 1e-4)
    if (!is.null(nb_file)) spdep::write.nb.gal(nb.map, nb_file)
  }

  if (is.null(output_dir) || !file.exists(g_file)) {
    spdep::nb2INLA(g_file, nb.map)
  }

  return(list(map = map, nb.map = nb.map, graph_file = g_file))
}


#' Read in and format health data - Diarrhea diseases case
#'
#' @description Reads in a csv file containing a monthly time series of health
#' outcomes and population data. Renames columns and creates time variables for
#' spatiotemporal analysis.
#'
#' @param health_data_path Path to a csv file containing a monthly time series of data
#' for Diarrhea outcome, which may be disaggregated by sex (under five case or
#' above five case), and by Region and District.
#' @param region_col Character. Name of the column in the dataframe that contains
#' the region names.
#' @param district_col Character. Name of the column in the dataframe that
#' contains the district names.
#' @param date_col Character. Name of the column in the dataframe that contains
#' the date. Defaults to NULL.
#' @param year_col Character. Name of the column in the dataframe that contains
#' the year.
#' @param month_col Character. Name of the column in the dataframe that contains
#' the month.
#' @param diarrhea_case_col Character. Name of the column in the dataframe
#' that contains the Diarrhea cases to be considered.
#' @param tot_pop_col Character. Name of the column in the dataframe that contains
#' the total population.
#'
#' @return A dataframe with formatted and renamed columns.
#'
#' @export
load_and_process_data <- function(health_data_path,
                                  region_col,
                                  district_col,
                                  date_col = NULL,
                                  year_col = NULL,
                                  month_col = NULL,
                                  diarrhea_case_col,
                                  tot_pop_col) {
  # Load health and climate data
  ext <- tolower(xfun::file_ext(health_data_path))
  # Load data based on file extension
  data <- switch(ext,
                 "rds" = read_rds(health_data_path),
                 "csv" = read_csv(health_data_path, show_col_types = FALSE),
                 "xlsx" = readxl::read_excel(health_data_path),
                 stop("Unsupported file type: must be .rds, .csv, or .xlsx")
  )

  # create date columns if needed
  if (is.null(date_col) & (is.null(month_col) | is.null(year_col))) {
    stop("If no date column is provided, you must provide both the year and the month columns.")
  }
  if (!is.null(date_col)) {
    data <- data %>% rename(
      date = date_col
    ) %>%
      mutate(
        year = lubridate::year(date),
        month = lubridate::month(date)
      )
  }
  data <- data %>% rename(
    year = year_col,
    month = month_col,
    region = region_col,
    district = district_col,
    diarrhea = diarrhea_case_col,
    tot_pop = tot_pop_col
  ) %>% select(
    all_of(c("region", "district", "year", "month", "diarrhea", "tot_pop"))
  )
  return(data)
}


#' Read in and format climate data
#'
#' @description Read in a monthly time series of climate data, rename
#' columns and create lag variable for spatiotemporal and DLNM analysis. The
#' climate data should start a year before a start year in the health data to
#' allow the lag variables calculation.
#'
#' @param climate_data_path Path to a csv file containing a monthly time series of data
#' for climate variables, which may be disaggregated by district.
#' @param district_col Character. Name of the column in the dataframe that
#' contains the region names.
#' @param year_col Character. Name of the column in the dataframe that
#' contains the Year.
#' @param month_col Character. Name of the column in the dataframe that
#' contains the month.
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
#' considered for the delay effect. It should be between 2 an 4. Defaults to 4.
#'
#' @return climate dataframe with formatted and renamed columns, and the lag
#' variables
#'
#' @export
load_and_process_climatedata <- function(climate_data_path,
                                         district_col,
                                         year_col,
                                         month_col,
                                         tmin_col,
                                         tmean_col,
                                         tmax_col,
                                         rainfall_col,
                                         r_humidity_col,
                                         runoff_col= NULL,
                                         spi_col = NULL,
                                         max_lag = 4){
  # Detect file extension
  ext <- tolower(xfun::file_ext(climate_data_path))

  # Read file
  data <- switch(ext,
                 "rds" = readr::read_rds(climate_data_path),
                 "csv" = readr::read_csv(climate_data_path, show_col_types = FALSE),
                 "xlsx" = readxl::read_excel(climate_data_path),
                 stop("Unsupported file type: must be .rds, .csv, or .xlsx")
  )

  # Map columns to standard names, excluding NULLs
  var_map <- list(district = district_col, year = year_col, month = month_col,
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
  final_data <- dplyr::bind_cols(climate_data[c("district", "year", "month")],
                                 lagged_data)

  return(final_data)
}


#' Read in and combine climate and health data
#'
#' @description Read and combine climate and health data prepared for the
#' spatiotemporal and DLNM analysis.
#'
#' @param health_data_path The path to the health data.
#' @param climate_data_path The path to the climate data.
#' @param map_path The path to the relevant map data.
#' @param region_col Character. Name of the column in the dataframe that contains
#' the region names.
#' @param district_col Character. Name of the column in the dataframe that
#' contains the region names.
#' @param date_col Character. Name of the column in the dataframe that contains
#' the date. Defaults to NULL.
#' @param year_col Character. Name of the column in the dataframe that contains
#' the Year.
#' @param month_col Character. Name of the column in the dataframe that contains
#' the Month.
#' @param diarrhea_case_col Character. Name of the column in the dataframe
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
#' @param geometry_col is the Name of the geometry column in the shapefile
#' (usually "geometry").
#' @param runoff_col Character. Name of the column in the dataframe that
#' contains the monthly runoff water data. Defaults to NULL.
#' @param spi_col Character. Name of the column in the dataframe that
#' contains the standardized precipitation index. Defaults to NULL.
#' @param max_lag Character. Number corresponding to the maximum lag to be
#' considered for the delay effect. It should be between 2 an 4. Defaults to 2.
#' @param output_dir Path to folder where the processed map data should be
#' saved. Defaults to NULL.
#'
#' @returns A list of dataframes containing the map, nb.map, data, grid_data, summary
#'
#' @export
combine_health_climate_data <- function(health_data_path,
                                        climate_data_path,
                                        map_path,
                                        region_col,
                                        district_col,
                                        date_col,
                                        year_col,
                                        month_col,
                                        diarrhea_case_col,
                                        tot_pop_col,
                                        tmin_col,
                                        tmean_col,
                                        tmax_col,
                                        rainfall_col,
                                        r_humidity_col,
                                        geometry_col,
                                        runoff_col = NULL,
                                        spi_col = NULL,
                                        max_lag = 2,
                                        output_dir = NULL){

  # Load data
  health_data <- load_and_process_data(health_data_path, region_col,
                                       district_col, date_col, year_col,
                                       month_col, diarrhea_case_col,
                                       tot_pop_col)

  climate_data <- load_and_process_climatedata(climate_data_path, district_col,
                                               year_col, month_col, tmin_col,
                                               tmean_col, tmax_col, rainfall_col,
                                               r_humidity_col, runoff_col,
                                               spi_col, max_lag)

  map_data <- load_and_process_map(map_path, region_col, district_col,
                                   geometry_col, output_dir)

  # Merge health + climate
  data <- health_data %>%
    left_join(climate_data, by = join_by(district, year, month)) %>%
    distinct() %>%
    group_by(region, district) %>%
    mutate(time = (year - min(year)) * 12 + month) %>%
    ungroup()

  # Build grid codes
  grid_data <- data %>%
    select(region, district) %>%
    distinct() %>%
    group_by(region) %>%
    mutate(region_code = cur_group_id(),
           district_number = row_number(),
           district_code = as.integer(paste0(region_code, district_number))) %>%
    ungroup()

  # Attach codes
  data <- left_join(data, grid_data, by = c("region", "district")) %>%
    arrange(region_code, district_code)

  map <- left_join(map_data$map, grid_data, by = c("region", "district")) %>%
    arrange(region_code, district_code)

  grid_data <- rename(grid_data, name = region, code_num = region_code)

  # Summary stats
  summary_stats <- list(tmin = summary(data$tmin),
                        tmax = summary(data$tmax),
                        rainfall = summary(data$rainfall),
                        rhumidity = summary(data$r_humidity)
  )

  # Return combined
  return_list <- list(
    map = map,
    nb.map = map_data$nb.map,
    graph_file = map_data$graph_file,
    data = data, grid_data = grid_data,
    summary = summary_stats
  )
  return(return_list)
}


#' Plot Time Series of Health and Climate Variables
#'
#' @description Generate time series plots for combined health and climate data
#' prepared for spatiotemporal and DLNM analysis. Supports aggregation
#' at the country, region, or district level.
#'
#' @param data A data frame containing the combined health and climate data.
#' @param param_term Character. The variable to plot (e.g., tmax,
#' tmean, tmin, Diarrhea). Use "all" to include all available variables.
#' @param level Character. Aggregation level: one of "country", "region", or "district".
#' Defaults to "country".
#' @param year Optional numeric vector to filter data by year(s). Defaults to NULL.
#' @param save_fig Boolean. Whether to save the figure as a PDF. Defaults to FALSE.
#' @param output_dir Character. Directory path to save the figure. Default to NULL
#'
#' @return A ggplot object.
#'
#' @export
plot_health_climate_timeseries <- function(data,
                                           param_term,
                                           level = "country",
                                           filter_year = NULL,
                                           save_fig = FALSE,
                                           output_dir = NULL) {

  vars_all <- c("diarrhea", "tmin", "tmean", "tmax", "rainfall")
  vars_to_plot <- if (param_term == "all") vars_all else param_term

  if (!is.null(filter_year)) data <- data %>% filter(year %in% filter_year)

  data <- data %>% mutate(date = as.Date(paste(year, month, 1, sep = "-")))

  missing <- setdiff(vars_to_plot, names(data))
  if (length(missing)) stop("Missing columns: ", paste(missing, collapse = ", "))

  group_var <- switch(tolower(level),
                      country = NULL,
                      region = "region",
                      district = "district",
                      stop("Invalid level"))

  group_cols <- c("date", group_var)

  agg <- data %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(across(all_of(vars_to_plot),
                     ~ if (level == "country" && "diarrhea" %in% cur_column()) {
                       if (cur_column() == "diarrhea") sum(.x, na.rm = TRUE) else mean(.x, na.rm = TRUE)
                     } else {
                       mean(.x, na.rm = TRUE)
                     }),
              .groups = "drop") %>%
    pivot_longer(cols = all_of(vars_to_plot), names_to = "variable", values_to = "value")

  if (!is.null(group_var)) agg <- agg %>% rename(group = all_of(group_var))

  title_text <- if (length(vars_to_plot) == 1) {
    paste("Time Series of", vars_to_plot)
  } else if (length(vars_to_plot) == length(vars_all)) {
    "Time Series of All Health & Climate Variables"
  } else {
    paste("Time Series of", paste(vars_to_plot, collapse = ", "))
  }

  p <- ggplot2::ggplot(agg, ggplot2::aes(x = date, y = value)) +
    ggplot2::geom_line(ggplot2::aes(color = if (!is.null(group_var)) group), linewidth = 1) +
    ggplot2::facet_wrap(~variable, scales = "free_y", ncol = 1) +
    ggplot2::scale_x_date(date_breaks = "6 month", date_labels = "%Y-%m") +
    ggplot2::labs(title = title_text, x = "date", y = "Value") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.title = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  if (save_fig) {
    fname <- paste0("timeseries_",
                    paste(vars_to_plot, collapse = "_"), "_", level, ".pdf")
    ggplot2::ggsave(file.path(output_dir, fname), p, width = 12, height = 7)
  }

  return(p)
}


#' Create a cross-basis matrix set for DLNM analysis
#'
#' @description Creates cross-basis matrix for each climate variable.
#'
#' @param data The dataset containing district_code, region_code and year
#' columns from the combine_health_climate_data() function.
#'
#' @return A list of cross-basis matrices including the basis matrix for maximum
#' temperature, minimun temperature, cumulative rainfall, and relative humidity.
#'
#' @export
set_cross_basis <- function(data) {

  nlag <- ncol(dplyr::select(data, all_of(grep("^tmax_lag", names(data),
                                               value = TRUE))))
  var_defs <- list(tmax = "tmax_lag", tmin = "tmin_lag", tmean = "tmean_lag",
                   rainfall = "rainfall_lag", r_humidity = "r_humidity_lag",
                   runoff = "runoff_lag", spi = "spi_lag")

  vars <- lapply(names(var_defs), function(var) {
    cols <- c(var, paste0(var_defs[[var]], 1:nlag))
    if (all(cols %in% names(data))) dplyr::select(data, all_of(cols)) else NULL
  })
  names(vars) <- names(var_defs)
  vars <- vars[!sapply(vars, is.null)]

  lagknot <- dlnm::equalknots(0:nlag, 2)

  basis_matrices <- lapply(names(vars), function(var) {
    x <- vars[[var]]
    cb <- dlnm::crossbasis(
      x,
      argvar = list(fun = "ns", knots = dlnm::equalknots(x[[1]], 2)),
      arglag = list(fun = "ns", knots = nlag / 2)
    )
    colnames(cb) <- paste0("basis_", var, ".", colnames(cb))
    cb
  })

  names(basis_matrices) <- names(vars)
  return(basis_matrices)
}


#' Create indices for INLA models
#'
#' @description: For the INLA model, there is a need to set-up regions index,
#' district index, and year index. This function create these indices using the
#' dataset, ndistrict and nregion defined above.
#'
#' @param data is the dataset containing district_code, region_code, and year
#' columns from the combine_health_climate_data() function.
#'
#' @returns The modified data with the created indices.
#'
#' @export
create_inla_indices <- function(data) {

  ntime <- length(unique(data$time))       # Total number of months
  nyear <- length(unique(data$year))       # Total number of years
  ndistrict <- length(unique(data$district_code))  # Total number of districts
  nregion <- length(unique(data$region_code))  # Total number of regions

  # define the offset variable based on the population data
  overall_rate <- sum(data$diarrhea, na.rm = TRUE) / sum(data$tot_pop, na.rm = TRUE)
  data$E <- overall_rate * data$tot_pop # Expected counts
  data$SIR <- data$diarrhea / data$E  # Standardized Incidence Ratio

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
  data$year_index <- data$year - (min(data$year)-1)

  return(data)
}


#' Check multicollinearity using VIF and Condition Number
#'
#' @description
#' This function checks for multicollinearity among DLNM basis variables
#' (specifically tmax) and selected confounders (rainfall, r_humidity, runoff, tmin)
#' using the Variance Inflation Factor (VIF) and condition number (Kappa).
#'
#' @param data A data frame from `combined_health_climate_data()` function,
#' containing the columns: `rainfall`, `r_humidity`, `runoff`, `tmin`,
#' and must be compatible with `set_cross_basis()` for generating DLNM matrices.
#' @param inla_param Character vector of parameter names representing all
#' climate variables to consider.
#' @param basis_matrices_choices Character vector specifying which variables
#' should be included as DLNM basismatrices from `inla_param`.
#' It might be `tmax`for temperature and `rainfall` if rainfall.
#'
#' @return A list with:
#' \describe{
#'   \item{vif}{Named numeric vector of VIF values.}
#'   \item{condition_number}{The condition number (Kappa) of the design matrix.}
#'   \item{interpretation}{A qualitative interpretation of collinearity level.}
#' }
#'
#' @export
check_vif <- function(data,
                      inla_param,
                      basis_matrices_choices) {

  data  <- create_inla_indices(combined_data$data)
  basis <- set_cross_basis(combined_data$data)

  vars_basis <- Filter(Negate(is.null), basis[basis_matrices_choices])
  vars_data  <- setdiff(inla_param, basis_matrices_choices)

  miss_basis <- setdiff(basis_matrices_choices, names(vars_basis))
  miss_data  <- setdiff(vars_data, names(data))
  if (length(miss_basis)) stop("Missing in basis: ", paste(miss_basis, collapse = ", "))
  if (length(miss_data))  stop("Missing in data: ", paste(miss_data, collapse = ", "))

  X <- cbind(do.call(cbind, vars_basis), data[vars_data])
  X <- as.data.frame(X[complete.cases(X), ])
  colnames(X) <- make.names(colnames(X), unique = TRUE)

  vif_vals <- car::vif(lm(rep(1, nrow(X)) ~ ., data = X))
  cond_num <- kappa(scale(X), exact = TRUE)

  list(
    vif = vif_vals,
    condition_number = cond_num,
    interpretation = if (cond_num < 10) {
      "Low collinearity ✅"
    } else if (cond_num < 30) {
      "Moderate collinearity"
    } else {
      "High collinearity ⚠️"
    }
  )
}


#' Run models of increasing complexity in INLA: Fit a baseline model including
#' spatiotemporal random effects.
#'
#' @description: Create and run multiple INLA (Integrated Nested
#' Laplace Approximation) models to the dataset, evaluates them using
#' DIC (Deviance Information Criterion), and identifies the best-fitting model.
#'
#' @param combined_data A dataframe resulting from combine_health_climate_data() function.
#' @param basis_matrices_choices A character vector specifying the basis matrix
#' parameters to be included in the model. Possible values are "tmax", and "rainfall".
#' @param inla_param A character vector specifying the confounding exposures to
#' be included in the model. Possible values are "tmax","tmin", "rainfall",
#' "r_humidity", and "runoff".
#' @param output_dir Character. The path to save model output to.  Defaults to NULL.
#' @param save_csv Boolean. Whether to save the results as a CSV. Defaults to
#' FALSE.
#' @param family Character. The probability distribution for the response
#' variable. The user may also have thepossibility to choose "nbinomial" for a
#' negative binomial distribution. Defaults to "poisson".
#' @param config Boolean. Enable additional model configurations. Defaults to FALSE.
#'
#' @returns A list containing the model, baseline_model, and the dic_table.
#'
#' @export
run_inla_models <- function(combined_data,
                            basis_matrices_choices,
                            inla_param,
                            output_dir = NULL,
                            save_csv = FALSE,
                            family = "poisson",
                            config = FALSE) {
  if (save_csv && is.null(output_dir)) stop("output_dir must be provided if save_csv = TRUE")

  if (!requireNamespace("INLA", quietly = TRUE)) {
    INLA_pth <- system.file("packages", "INLA_24.12.11.zip", package = "climatehealth")
    install.packages(INLA_pth, repos = NULL, type = "win.binary", lib = .libPaths()[1])
  }

  data <- create_inla_indices(combined_data$data)
  basis <- set_cross_basis(combined_data$data)
  graph_file <- combined_data$graph_file

  prior <- list(prec = list(prior = "pc.prec", param = c(0.5 / 0.31, 0.01)))
  base_formula <- diarrhea ~ 1 +
    f(month, replicate = region_index, model = "rw1", cyclic = TRUE,
      constr = TRUE, scale.model = TRUE, hyper = prior) +
    f(district_index, model = "bym2", replicate = year_index,
      graph = graph_file, scale.model = TRUE, hyper = prior)

  if (is.null(basis_matrices_choices)) basis_matrices_choices <- character(0)

  valid_basis <- Filter(function(x) !is.null(basis[[x]]), basis_matrices_choices)
  basis_terms <- if (length(valid_basis) > 0) paste0("basis$", valid_basis)
  else character()
  raw_vars <- intersect(inla_param, names(data))
  all_terms <- c(basis_terms, raw_vars)

  full_formula <- if (length(all_terms) > 0)
    update(base_formula, as.formula(paste("~ . +",
                                          paste(all_terms,
                                                collapse = " + "))))
  else base_formula

  fit <- function(f) INLA::inla.rerun(INLA::inla(
    f, data = data, family = family, offset = log(data$E),
    control.inla = list(strategy = "adaptive"),
    control.compute = list(dic = TRUE, config = config,
                           cpo = TRUE, return.marginals = FALSE),
    control.fixed = list(correlation.matrix = TRUE, prec.intercept = 1, prec = 1),
    control.predictor = list(link = 1, compute = TRUE), verbose = FALSE))

  baseline_model <- fit(base_formula)
  model <- fit(full_formula)

  if (save_csv) {
    save(model,
         file = file.path(output_dir,
                          paste0("model_with_",
                                 paste(c(valid_basis, raw_vars),
                                       collapse = "_"), ".csv")))
  }

  dic_table <- data.table::data.table(
    Model = paste(c(valid_basis, raw_vars), collapse = " + "),
    DIC = round(model$dic$dic, 0),
    LogScore = round(mean(-log(model$cpo$cpo[model$cpo$failure == 0]), na.rm = TRUE), 3),
    LPML = round(sum(log(model$cpo$cpo[model$cpo$failure == 0]), na.rm = TRUE), 2)
  )

  list(model = model, baseline_model = baseline_model, dic_table = dic_table)
}


#' Visualise monthly random effects for selected INLA model
#'
#' @description Generates and saves a plot of monthly random effects for different
#' regions, visualizing their contribution to Diarrhea Incidence Rate.
#'
#' @param combined_data Data list from combine_health_climate_data() function.
#' @param model The fitted model object.
#' @param save_fig Boolean. Whether to save the plot as an output. Defaults to
#' FALSE.
#' @param output_dir Character. The path to save the visualisation to. Defaults to NULL.
#'
#' @return THe monthly random effects plot.
#'
#' @export
plot_monthly_random_effects <- function(combined_data,
                                        model,
                                        save_fig = FALSE,
                                        output_dir = NULL) {
  # Validate output_dir if saving
  if (save_fig & is.null(output_dir)) {
    stop("output_dir must be provided if save_fig = TRUE")
  }

  data <- combined_data$data
  grid_data <- combined_data$grid_data
  map <- combined_data$map

  # Create data frame for monthly random effects per region
  month_effects <- data.frame(region_code = rep(unique(data$region_code), each = 12),
                              month = model$summary.random$month)

  # Merge with predefined state grid
  month_effects <- month_effects %>%
    left_join(grid_data %>% select(-district, -district_code) %>% unique(),
              by = c("region_code" = "code_num"))

  month_effects <- map %>% select(-district) %>% unique() %>%
    left_join(month_effects, by = c("region" = "name"))

  # Generate plot
  p <- month_effects %>%
    ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      ggplot2::aes(x = month.ID, ymin = `month.0.025quant`, ymax = `month.0.975quant`),
      fill = "cadetblue4", alpha = 0.5) +
    ggplot2::geom_line(ggplot2::aes(x = month.ID, y = month.mean), col = "cadetblue4") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
    ggplot2::xlab("Month") +
    ggplot2::ylab("Contribution to log(DIR)") +
    ggplot2::scale_y_continuous() +
    ggplot2::scale_x_continuous(breaks = c(1,4,7,10), labels = c("Jan", "Apr", "Jul", "Oct")) +
    ggplot2::theme_bw() +
    ggplot2::facet_wrap(~region)

  # Save plot
  if (save_fig){
    ggplot2::ggsave(file.path(output_dir, "monthly_random_effects.pdf"),
                    plot = p, height = 30, width = 25, units = "cm")
  }
  return(p)
}


#' Visualize yearly spatial random effect
#'
#' @description Generates and saves plots of yearly spatial random effect at
#' district level.
#'
#' @param combined_data Data list from combine_health_climate_data() function.
#' @param model The fitted model from run_inla_models() function.
#' @param save_fig Boolean. Whether to save the plot as an output. Defaults to
#' FALSE.
#' @param output_dir Character. The path to save the fitted model to. Defaults
#' to NULL.
#'
#' @return The yearly space random effect plot
#'
#' @export

plot_yearly_spatial_random_effect <- function(combined_data ,
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
  nyear <- length(unique(data$year))
  ndistrict <- length(unique(data$district_code))
  # Extract spatial random effects
  space <- data.table::data.table(model$summary.random$district_index)
  space$year <- rep(min(data$year):max(data$year), each = 2 * ndistrict)
  space$re <- rep(c(rep(1, ndistrict), rep(2, ndistrict)), nyear)
  space <- space[space$re == 1, ]
  space$district_code <- rep(unique(data$district_code), nyear)
  # Merge with spatial map
  space <- left_join(map, space, by = c("district_code" = "district_code"))
  # Plot
  space_effects <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = space, ggplot2::aes(fill = mean), color = "black", size = 0.1) +
    ggplot2::scale_fill_gradient2(
      low = "green", mid = "white", high = "purple",
      midpoint = 0,
      limits = c(min(space$mean, na.rm = TRUE), max(space$mean, na.rm = TRUE)),
      name = "Contribution to\nlog(DIR)"
    ) +
    ggplot2::theme_void() +
    ggplot2::facet_wrap(~year, ncol = 5)

  # Save if required
  if (save_fig) {
    ggplot2::ggsave(file.path(output_dir, "spatial_random_effects_per_year.pdf"),
                    plot = space_effects, height = 30, width = 25, units = "cm")
  }

  return(space_effects)
}


#' A function to predict relative risk at country, region, and district level
#'
#' @description Produces cumulative relative risk at country, region and
#' district level from analysis.
#'
#' @param data Data list from combine_health_climate_data() function.
#' @param param_term A character vector or list containing parameter terms such
#' as "tmax" (maximum temperature) and "rainfall" (precipitation).
#' @param model The fitted model from run_inla_models() function.
#' @param level Character. The spatial disaggregation level.
#' Can take one of the following values: "country", "region", or "district".
#'
#' @return A dataframe containing cumulative relative risk at the chosen level.
#'
#' @export
get_predictions <- function(data,
                            param_term,
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
  indt <- grep(paste("basis", param_term, sep = "_"), model$names.fixed)

  if (level == "country"){
    # Extract predictions from the tmax DLNM centered on overall mean Tmax
    predt <- dlnm::crosspred(basis_matrices[[param_term]], coef = coef[indt],
                             vcov = vcov[indt, indt], model.link = "log",
                             bylag = 0.25, cen = round(mean(data[[param_term]],
                                                            na.rm = TRUE), 0))

  } else if (tolower(level) == "region"){
    # Iterate over unique regions
    regions <- unique(data$region)
    predt <- regions %>%
      lapply(function(regi){
        region_data <- subset(data, region == regi)
        # Extract predictions from the tmax DLNM centered on overall mean Tmax
        mean_param <- round(mean(region_data[[param_term]], na.rm = TRUE), 0)
        predt <- dlnm::crosspred(basis_matrices[[param_term]], coef = coef[indt],
                                 vcov = vcov[indt, indt],
                                 model.link = "log", bylag = 0.25, cen = mean_param)
        return(predt)
      })
    names(predt) <- regions
  } else if (tolower(level) == "district"){
    # Iterate over unique districts
    districts <- unique(data$district)
    predt <- districts %>%
      lapply(function(dist){
        # Filter data for the current district
        district_data <- subset(data, district == dist)
        # Extract predictions from the tmax DLNM centered on overall mean Tmax
        mean_param <- round(mean(district_data[[param_term]], na.rm = TRUE), 0)
        predt <- dlnm::crosspred(basis_matrices[[param_term]], coef = coef[indt],
                                 vcov = vcov[indt, indt],
                                 model.link = "log", bylag = 0.25, cen = mean_param)
      })
    names(predt) <- districts
  }
  return(predt)
}


#' Create a contour plot at country, region or district level.
#'
#' @description: Generates a contour plot showing the lag exposure effect of
#' maximum temperature (tmax) and cumulative rainfall on diarrhea cases.
#'
#' @param data Data list from combine_health_climate_data() function.
#' @param param_term A character vector or list containing parameter terms such
#' as "tmax" (maximum temperature) and "rainfall" (precipitation).
#' Default to "tmax"
#' @param model The fitted model from run_inla_models() function.
#' @param level A character vector specifying the spatial disaggregation level.
#' Can take one of the following values: "country", "region", or "district".
#' @param filter_year Integer. The year to filter to data to. Defaults to NULL.
#' @param save_fig Boolean. Whether to save the outputted plot. Defaults to
#' FALSE.
#' @param output_dir The path to save the visualisation to. Defaults to NULL
#'
#' @return contour plot at country, Region and District level
#'
#' @export
contour_plot <- function(data,
                         param_term,
                         model,
                         level,
                         filter_year = NULL,
                         save_fig = FALSE,
                         output_dir = NULL) {

  if (save_fig && is.null(output_dir)) {
    stop("'output_dir' must be provided if save_fig = TRUE")
  }

  if (!is.null(filter_year)) {
    if (!"year" %in% names(data)) stop("'year' column not found in data.")
    data <- filter(data, year %in% filter_year)
  }
  predt <- get_predictions(data, param_term=param_term, model, level=level)

  plot_contour <- function(x, y, z, title) {
    nlag <- max(x)
    pal <- rev(RColorBrewer::brewer.pal(11, "PRGn"))
    levels <- pretty(range(z, na.rm = TRUE), 20)
    col1 <- colorRampPalette(pal[1:6])
    col2 <- colorRampPalette(pal[6:11])
    cols <- c(col1(sum(levels <= 1)), col2(sum(levels > 1)))

    filled.contour(
      x, y, z,
      xlab = "Lag",
      ylab = ifelse(param_term == "tmax", "Temperature (°C)",
                    ifelse(param_term == "rainfall", "Rainfall (mm)", param_term)),
      main = title,
      col = cols,
      levels = levels,
      plot.axes = {
        axis(1, at = 0:nlag, labels = 0:nlag)
        axis(2)
      }
    )
  }

  nlag <- data %>% select(contains(param_term)) %>% ncol() - 1
  lag_seq <- seq(0, nlag, 0.25)

  if (save_fig) {
    output_file <- file.path(
      output_dir,
      paste0("contour_plot_", param_term, "_", level,
             if (!is.null(filter_year)) paste0("_", paste(filter_year, collapse = "_")), ".pdf")
    )
    pdf(output_file, width = 8, height = 8)
  }

  if (tolower(level) == "country") {
    plot_contour(lag_seq, predt$predvar, t(predt$matRRfit), title = "Contour Plot for Country")
  } else {
    groups <- if (tolower(level) == "region") unique(data$region) else unique(data$district)
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
#' @param param_term A character vector or list specifying the climate parameters
#' (e.g., "tmax" for maximum temperature, "rainfall" for precipitation) to include in the map.
#' Defaults to "tmax".
#' @param level A character string indicating the spatial aggregation level.
#' Options are "region" or "district". Defaults to "District".
#' @param filter_year Integer. The year to filter to data to. Defaults to NULL.
#' @param output_dir Character. The directory path where the output PDF file
#' should be saved. Defaults to NULL.
#' @param save_fig Boolean. If TRUE, saves the plot to the specified directory.
#' Defaults to FALSE.
#'
#' @return Relative risk map at the chosen level.
#'
#' @export
plot_rr_map <- function(combined_data,
                        model,
                        param_term = "tmax",
                        level = "District",
                        filter_year = NULL,
                        output_dir = NULL,
                        save_fig = FALSE) {
  data <- combined_data$data
  map <- combined_data$map
  stopifnot("year" %in% names(data))

  if (save_fig && is.null(output_dir)) {
    stop("output_dir must be provided if save_fig = TRUE")
  }

  level <- tolower(level)
  years <- if (is.null(filter_year)) sort(unique(data$year)) else filter_year
  grouping_var <- ifelse(level == "district", "district", "region")

  # Get RR data for each year
  get_rr_df <- function(yr) {
    pred <- get_predictions(filter(data, year == yr), param_term, model, level)
    purrr::map_dfr(names(pred), function(name) {
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
    ggplot2::ggplot(map_rr) +
      ggplot2::geom_sf(ggplot2::aes(fill = RR), color = "black", size = 0.2) +
      ggplot2::scale_fill_gradient2(
        low = "blue", mid = "white", high = "red",
        midpoint = 1, limits = rr_range, na.value = "grey80", name = "RR"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = paste("Year:", years[i]), subtitle = paste("Exposure:", param_term)) +
      ggplot2::theme(axis.text = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(), panel.grid = ggplot2::element_blank())
  })

  # Combine all plots
  combined_plot <- patchwork::wrap_plots(plots) +
    patchwork::plot_annotation(
      title = paste("Relative Risk of Diarrhea by", level),
      subtitle = paste("Exposure:", param_term),
      theme = ggplot2::theme(plot.title = ggplot2::element_text(size = 16, face = "bold"))
    )

  # Save to file if needed
  if (save_fig) {
    ggplot2::ggsave(file.path(output_dir,
                              paste0("RR_map_", param_term, "_", level, "_all_years.pdf")),
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
#' @param model The fitted model from run_inla_models() function.
#' @param param_term A character vector or list containing parameter terms such
#' as "tmax" (maximum temperature) and "rainfall" (precipitation).
#' Default to "tmax".
#' @param level A character vector specifying the spatial disaggregation level.
#' Can take one of the following values: "country", "region", or "district".
#' Default to "country".
#' @param filter_year Integer. The year to filter to data to. Defaults to NULL.
#' @param output_dir Character. The path where the PDF file will be saved. Default to NULL.
#' @param save_csv Boolean. If TRUE, saves the RR data to the specified directory.
#' Defaults to FALSE.
#' @param save_fig Boolean. If TRUE, saves the plot to the specified directory.
#' Defaults to FALSE.
#'
#' @return Relative risk plot at country, region, and district levels.
#'
#' @export
plot_relative_risk <- function(data,
                               model,
                               param_term,
                               level = "country",
                               filter_year = NULL,
                               output_dir = NULL,
                               save_csv = FALSE,
                               save_fig = FALSE) {
  if (!"year" %in% names(data)) stop("'year' column not found in data.")
  level <- tolower(level)
  if (save_fig) {
    if (is.null(output_dir)) stop("output_dir must be provided if save_fig = TRUE")
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  }
  output_pdf <- if (save_fig) {
    file.path(output_dir, paste0("RR_", param_term, "_", level, "_all_plots.pdf"))
  } else {
    NULL
  }
  csv_output_path <- if (save_csv) {
    file.path(output_dir, paste0("RR_", param_term, "_", level, "_all_plots.csv"))
  } else {
    NULL
  }
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("Package 'patchwork' is required but not installed.")
  }
  build_plot <- function(pred, title) {
    if (anyNA(pred$allRRfit)) return(NULL)
    ggplot2::ggplot(
      dplyr::tibble(
        x = pred$predvar,
        y = pred$allRRfit,
        ymin = pred$allRRlow,
        ymax = pred$allRRhigh
      ),
      ggplot2::aes(x, y)
    ) +
      ggplot2::geom_line(color = "red", linewidth = 1) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = ymin, ymax = ymax), fill = "red", alpha = 0.3) +
      ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "gray", linewidth = 0.5) +
      ggplot2::labs(title = title, x = param_term, y = "Relative Risk") +
      ggplot2::theme_minimal() +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 9))
  }

  all_predictions <- list()

  if (level == "country") {
    if (is.null(filter_year)) {
      data_all <- data
      pred <- get_predictions(data_all, param_term, model, level)
      if (is.list(pred) && !is.null(names(pred)) && length(pred) == 1) {
        pred <- pred[[1]]
      }
      all_predictions[["All Years"]] <- pred

      x_breaks <- pretty(range(pred$predvar, na.rm = TRUE), n = 6)
      x_limits <- range(x_breaks)
      param_sym <- rlang::sym(param_term)

      # Get range of predvar where RR >= 1
      rr_above_1 <- pred$predvar[which(pred$allRRfit >= 1)]
      rr_range <- range(rr_above_1, na.rm = TRUE)

      rr_plot <- ggplot2::ggplot() +
        ggplot2::geom_line(
          data = dplyr::tibble(x = pred$predvar, y = pred$allRRfit),
          ggplot2::aes(x = x, y = y), color = "red", linewidth = 1) +
        ggplot2::geom_ribbon(
          data = dplyr::tibble(x = pred$predvar, ymin = pred$allRRlow, ymax = pred$allRRhigh),
          ggplot2::aes(x = x, ymin = ymin, ymax = ymax), fill = "red", alpha = 0.3) +
        ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "gray", linewidth = 0.5) +
        ggplot2::geom_vline(xintercept = rr_range, linetype = "dotted", color = "blue", linewidth = 0.8) +
        ggplot2::scale_x_continuous(limits = x_limits, breaks = x_breaks) +
        ggplot2::labs(title = "Relative Risk Curve", y = "Relative Risk") +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.title.x = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank(),
          axis.title.y = ggplot2::element_text(color = "gray"),
          plot.title = ggplot2::element_text(size = 11)
        )

      hist_counts <- ggplot2::ggplot_build(
        ggplot2::ggplot(data_all, ggplot2::aes(x = !!param_sym)) +
          ggplot2::geom_histogram(binwidth = 1, boundary = 0)
      )$data[[1]]
      max_count <- max(hist_counts$count, na.rm = TRUE)
      y_breaks <- pretty(c(0, max_count), n = 3)
      y_limits <- range(y_breaks)

      hist_plot <- ggplot2::ggplot(data_all, ggplot2::aes(x = !!param_sym)) +
        ggplot2::geom_histogram(binwidth = 1, boundary = 0, fill = "skyblue",
                                color = "black", alpha = 0.6) +
        ggplot2::scale_x_continuous(limits = x_limits, breaks = x_breaks) +
        ggplot2::scale_y_continuous(name = "Frequency", limits = y_limits,
                                    breaks = y_breaks, position = "right") +
        ggplot2::labs(x = param_term) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.title.y = ggplot2::element_text(color = "skyblue"))

      plot_all_years <- rr_plot / hist_plot +
        patchwork::plot_layout(heights = c(2, 1))

      if (save_fig && !is.null(output_pdf)) {
        grDevices::pdf(output_pdf, width = 10, height = 6)
        print(plot_all_years)
        grDevices::dev.off()
      }

      if (save_csv && !is.null(csv_output_path)) {
        utils::write.csv(
          dplyr::tibble(
            predvar = pred$predvar,
            allRRfit = pred$allRRfit,
            allRRlow = pred$allRRlow,
            allRRhigh = pred$allRRhigh
          ),
          csv_output_path, row.names = FALSE
        )
      }

      return(list(plots = plot_all_years, RR = all_predictions))
    }

    filter_year <- sort(unique(filter_year))
    plots <- lapply(filter_year, function(yr) {
      pred <- get_predictions(dplyr::filter(data, year == yr), param_term, model, level)
      all_predictions[[as.character(yr)]] <- pred
      build_plot(pred, as.character(yr))
    }) %>% purrr::keep(~ !is.null(.))

    if (save_fig && !is.null(output_pdf)) {
      grDevices::pdf(output_pdf, width = 14, height = 10)
      purrr::walk(plots, print)
      grDevices::dev.off()
    }

    if (save_csv && !is.null(csv_output_path)) {
      flat_df <- dplyr::bind_rows(lapply(names(all_predictions), function(yr) {
        df <- all_predictions[[yr]]
        dplyr::tibble(
          year = as.numeric(yr),
          predvar = df$predvar,
          allRRfit = df$allRRfit,
          allRRlow = df$allRRlow,
          allRRhigh = df$allRRhigh
        )
      }))
      utils::write.csv(flat_df, csv_output_path, row.names = FALSE)
    }
    return(list(
      plots = patchwork::wrap_plots(plots) +
        patchwork::plot_annotation(
          title = "Exposure-Response Curves by Country",
          subtitle = paste(param_term, "Years:", paste(filter_year, collapse = ", "))
        ),
      RR = all_predictions
    ))
  }
  if (level %in% c("region", "district")) {
    group_plots <- list()

    if (is.null(filter_year)) {
      preds <- get_predictions(data, param_term, model, level)
      all_predictions[["All Years"]] <- preds
      for (grp in names(preds)) {
        p <- build_plot(preds[[grp]], grp)
        if (!is.null(p)) {
          group_plots[[grp]] <- list(p)
        }
      }
    } else {
      for (yr in filter_year) {
        preds <- get_predictions(dplyr::filter(data, year == yr), param_term, model, level)
        all_predictions[[as.character(yr)]] <- preds
        for (grp in names(preds)) {
          p <- build_plot(preds[[grp]], paste0(grp, " (", yr, ")"))
          if (!is.null(p)) {
            group_plots[[grp]] <- c(group_plots[[grp]], list(p))
          }
        }
      }
    }
    if (save_fig && !is.null(output_pdf)) {
      grDevices::pdf(output_pdf, width = 12, height = 9)
      all_plots <- unlist(group_plots, recursive = FALSE)
      if (length(all_plots) > 0) {
        plot_pages <- split(all_plots, ceiling(seq_along(all_plots) / 6))
        for (page in plot_pages) {
          print(
            patchwork::wrap_plots(page, ncol = 2, nrow = 3) +
              patchwork::plot_annotation(
                title = paste("Exposure-Response Curves by", tools::toTitleCase(level)),
                subtitle = if (is.null(filter_year)) "All Years Combined" else paste(param_term, "Years:", paste(filter_year, collapse = ", "))
              )
          )
        }
      }
      grDevices::dev.off()
    }
    if (save_csv && !is.null(csv_output_path)) {
      flat_df <- dplyr::bind_rows(lapply(names(all_predictions), function(yr) {
        preds <- all_predictions[[yr]]
        dplyr::bind_rows(lapply(names(preds), function(grp) {
          df <- preds[[grp]]
          dplyr::tibble(
            year = yr,
            group = grp,
            predvar = df$predvar,
            allRRfit = df$allRRfit,
            allRRlow = df$allRRlow,
            allRRhigh = df$allRRhigh
          )
        }))
      }))
      utils::write.csv(flat_df, csv_output_path, row.names = FALSE)
    }

    return(list(plots = group_plots, RR = all_predictions))
  }
}


#' Attribution calculation for maximum temperature
#'
#' @description The attribution calculation uses the attrdl function from
#' Gasparini and DNLM package
#'
#' @param data Data list from combine_health_climate_data() function.
#' @param param_term A character vector or list containing parameter terms such
#' as "tmax" (maximum temperature) and "rainfall" (precipitation).
#' Default to "tmax"
#' @param model The fitted model from run_inla_models() function.
#' @param level Character. The spatial disaggregation level.
#' Can take one of the following values: "country", "region", or "district".
#' @param param_threshold Numeric. Threshold above which exposure is considered,
#' "attributable". Can take floats. Defaults to 1.
#' @param filter_year Integer. The year to filter to data to. Defaults to NULL.
#' @param save_csv Logical. Whether to save the generated attribution metrics to file.
#' Default is `FALSE`.
#' @param output_dir Optional. Directory path to save the output metrics if
#' `save_fig = TRUE`.
#'
#' @return Results containing the attributable number and fraction at the chosen
#' dissagregation level.
#'
#' @export
attribution_calculation <- function(data,
                                    param_term,
                                    model,
                                    level,
                                    param_threshold = 1,
                                    filter_year = NULL,
                                    output_dir = NULL,
                                    save_csv = FALSE) {

  level <- tolower(level)
  # Get model coefficients
  coef_mean <- model$summary.fixed$mean
  vcov_full <- model$misc$lincomb.derived.covariance.matrix
  indt <- grep(paste0("basis_", param_term), model$names.fixed)
  if (length(indt) == 0) stop("No terms for 'basis_", param_term, "' found in model.")

  # Filter years if needed
  if (!is.null(filter_year)) {
    stopifnot("year" %in% names(data), all(filter_year %in% unique(data$year)))
    data <- dplyr::filter(data, year %in% filter_year)
  }

  # Create INLA indices and basis matrices
  data <- create_inla_indices(data)
  basis_matrices <- set_cross_basis(data)

  # Function to compute metrics given prediction object
  compute_metrics_from_pred <- function(df, pred) {
    df <- df[!is.na(df[[param_term]]) & !is.na(df$diarrhea) & !is.na(df$tot_pop), ]
    if (nrow(df) == 0) return(NULL)

    ref_temp <- pred$predvar[which.min(abs(pred$allRRfit - 1))]
    rr_obs_fit  <- approx(pred$predvar, pred$allRRfit,  xout = df[[param_term]], rule = 2)$y
    rr_obs_low  <- approx(pred$predvar, pred$allRRlow,  xout = df[[param_term]], rule = 2)$y
    rr_obs_high <- approx(pred$predvar, pred$allRRhigh, xout = df[[param_term]], rule = 2)$y

    total_cases <- df$diarrhea
    tot_pop <- sum(df$tot_pop, na.rm = TRUE)

    get_metrics <- function(rr_obs) {
      valid <- which(rr_obs > param_threshold & !is.na(rr_obs))
      if (length(valid) == 0 || tot_pop == 0 || is.na(tot_pop)) return(c(0, 0, 0))  # Changed NA to 0
      af <- 1 - 1 / mean(rr_obs[valid])
      an <- af * sum(total_cases[valid], na.rm = TRUE)
      ar <- (an / tot_pop) * 1e5
      c(af, an, ar)
    }

    res_fit <- get_metrics(rr_obs_fit)
    res_low <- get_metrics(rr_obs_low)
    res_high <- get_metrics(rr_obs_high)

    list(
      MRT = round(ref_temp, 2),
      AR_Number = round(res_fit[2], 0),
      AR_Number_LCI = round(res_low[2], 0),
      AR_Number_UCI = round(res_high[2], 0),
      AR_Fraction = round(res_fit[1] * 100, 2),
      AR_Fraction_LCI = round(res_low[1] * 100, 2),
      AR_Fraction_UCI = round(res_high[1] * 100, 2),
      AR_per_100k = ceiling(res_fit[3]),
      AR_per_100k_LCI = ceiling(res_low[3]),
      AR_per_100k_UCI = ceiling(res_high[3])
    )
  }

  # Grouping variables
  grp_vars <- switch(level,
                     "country" = c("year", "month"),
                     "region"  = c("region", "year", "month"),
                     "district"= c("region", "district", "year", "month"),
                     stop("Invalid level. Choose 'country', 'region', or 'district'."))

  # Compute predictions and metrics
  res <- data %>%
    dplyr::group_by(across(all_of(grp_vars))) %>%
    dplyr::group_split() %>%
    purrr::map_dfr(~{
      df_group <- .x

      mean_param <- switch(level,
                           "country"  = round(mean(data[[param_term]], na.rm = TRUE), 0),
                           "region"   = round(mean(df_group[[param_term]], na.rm = TRUE), 0),
                           "district" = round(mean(df_group[[param_term]], na.rm = TRUE), 0))

      pred <- tryCatch({
        dlnm::crosspred(basis_matrices[[param_term]],
                        coef = coef_mean[indt],
                        vcov = vcov_full[indt, indt],
                        model.link = "log",
                        bylag = 0.25,
                        cen = mean_param)
      }, error = function(e) NULL)

      if (is.null(pred)) return(NULL)

      r <- compute_metrics_from_pred(df_group, pred)
      if (is.null(r)) return(NULL)

      tibble::tibble(!!!df_group[1, grp_vars],
                     MRT = r$MRT,
                     AR_Number = r$AR_Number,
                     AR_Number_LCI = r$AR_Number_LCI,
                     AR_Number_UCI = r$AR_Number_UCI,
                     AR_Fraction = r$AR_Fraction,
                     AR_Fraction_LCI = r$AR_Fraction_LCI,
                     AR_Fraction_UCI = r$AR_Fraction_UCI,
                     AR_per_100k = r$AR_per_100k,
                     AR_per_100k_LCI = r$AR_per_100k_LCI,
                     AR_per_100k_UCI = r$AR_per_100k_UCI)
    })

  # Save if needed
  if (save_csv && !is.null(output_dir)) {
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
    readr::write_csv(res, file.path(output_dir,
                                    paste0("attribution_", level, "_", param_term, ".csv")))
  }

  return(res)
}


#' Plot Attributable Metrics
#'
#' @description
#' This function generates plots of climate-attributable health metrics—such as
#' the attributable number, fraction, and rate of diarrhea cases—at different spatial
#' levels (country, region, or district). It supports filtering by year and can
#' optionally save the plots as PDF files.
#'
#' The function produces time series plots (if `filter_year` is specified) or aggregate
#' bar charts (if `filter_year` is `NULL`), depending on the level and user input.
#'
#' @param attr_data A data frame containing climate-health attribution results,
#' including columns such as `year`, `AR_Number`, `AR_Fraction`, `AR_per_100k`,
#' and their respective confidence intervals.
#' @param level Character. The spatial level at which to plot the results.
#' Must be one of `"country"`, `"region"`, or `"district"`.
#' @param metrics Character vector. Specifies which metrics to plot.
#' Options include `"AR_Number"` (attributable number),
#' `"AR_Fraction"` (attributable fraction), and
#' `"AR_per_100k"` (attributable rate per 100,000 population).
#' Multiple values can be passed.
#' @param filter_year Optional. Integer or vector of integers specifying the year(s)
#' to filter the dataset. If `NULL`, all years are aggregated.
#' @param param_term Character. The climate variable term used in the attribution
#' analysis (e.g., `"tmax"`, `"rainfall"`). This is used for labeling the plot titles.
#' @param save_fig Logical. Whether to save the generated plots to file.
#' Default is `FALSE`.
#' @param output_dir Optional. Directory path to save the output plots if
#' `save_fig = TRUE`.
#'
#' @return
#' A list of ggplot objects (or nested lists if the level is `"region"` or
#' `"district"` with no `filter_year`). If `save_fig = TRUE`,
#' the plots are also saved as PDF files in the specified `output_dir`.
#'
#' @details
#' - When `level = "country"` and `filter_year = NULL`, data is aggregated by
#' year and plotted as a time series.
#' - When `level = "region"` or `"district"` and `filter_year = NULL`,
#' the function returns a list of faceted bar plots showing top-level units,
#' split across multiple pages if necessary.
#' - When `filter_year` is provided, trends over time are shown for the specified
#' regions or districts.
#'
#' @export
plot_attribution_metric <- function(attr_data,
                                    level = c("country", "region", "district"),
                                    metrics = c("AR_Number", "AR_Fraction", "AR_per_100k"),
                                    filter_year = NULL,
                                    param_term,
                                    save_fig = FALSE,
                                    output_dir = NULL) {
  level <- tolower(level)
  metrics <- match.arg(metrics, several.ok = TRUE)

  if (is.null(param_term)) stop("'param_term' must be provided.")

  param_label <- switch(tolower(param_term),
                        tmax = "Extreme Temperature",
                        rainfall = "Extreme Rainfall",
                        param_term)

  if (!is.null(filter_year)) {
    if (!"year" %in% names(attr_data)) stop("'year' column not found in data.")
    attr_data <- dplyr::filter(attr_data, year %in% filter_year)
  }

  y_title_lookup <- c(
    AR_per_100k = "AR (per 100,000 population)",
    AR_Fraction = "Attributable Fraction (%)",
    AR_Number = "Attributable Number"
  )

  title_lookup <- c(
    AR_per_100k = paste0("Diarrhea cases per 100,000 attributable to ", param_label, " (95% CI)"),
    AR_Fraction = paste0("Diarrhea  Attributable Fraction (%) due to ", param_label, " (95% CI)"),
    AR_Number = paste0("Number of Diarrhea  cases attributable to ", param_label, " (95% CI)")
  )

  formatter_lookup <- list(
    AR_per_100k = scales::label_comma(),
    AR_Fraction = scales::label_comma(),
    AR_Number = scales::label_comma()
  )

  aggregate_attr_data <- function(data, group_var) {
    dplyr::group_by(data, .data[[group_var]]) %>%
      dplyr::summarise(
        across(matches("^AR_Number(_LCI|_UCI)?$"), ~ sum(.x, na.rm = TRUE)),
        across(matches("^AR_(Fraction|per_100k)(_LCI|_UCI)?$"), ~ mean(.x, na.rm = TRUE)),
        .groups = "drop"
      )
  }

  if (is.null(filter_year)) {
    if (level == "country") {
      attr_data <- aggregate_attr_data(attr_data, "year")
    } else if (level %in% c("region", "district")) {
      if (!(level %in% names(attr_data))) stop(paste0("'", level, "' column not found in data."))
      attr_data <- aggregate_attr_data(attr_data, level)
    }
  }

  plots <- purrr::map(metrics, function(metric) {
    lci_col <- paste0(metric, "_LCI")
    uci_col <- paste0(metric, "_UCI")
    x_var <- if (level %in% c("region", "district") && is.null(filter_year)) level else "year"

    required_cols <- c(x_var, metric, lci_col, uci_col)
    if (level %in% c("region", "district") && !is.null(filter_year)) {
      required_cols <- c(required_cols, level)
    }

    missing_cols <- setdiff(required_cols, names(attr_data))
    if (length(missing_cols) > 0) {
      warning("Skipping '", metric, "' due to missing columns: ", paste(missing_cols, collapse = ", "))
      return(NULL)
    }

    attr_data_plot <- attr_data %>%
      dplyr::filter(!is.na(.data[[metric]]),
                    !is.na(.data[[lci_col]]),
                    !is.na(.data[[uci_col]]))

    title <- title_lookup[[metric]]
    y_formatter <- formatter_lookup[[metric]]
    y_label <- y_title_lookup[[metric]]

    # Country-level time series when filter_year is NULL
    if (level == "country" && is.null(filter_year)) {
      attr_data_plot$year <- factor(attr_data_plot$year)
      p <- ggplot(attr_data_plot, aes(x = year, y = .data[[metric]], group = 1)) +
        geom_line(color = "steelblue", linewidth = 1) +
        geom_point(color = "steelblue", size = 2) +
        geom_ribbon(aes(ymin = .data[[lci_col]], ymax = .data[[uci_col]]),
                    alpha = 0.2, fill = "steelblue") +
        labs(title = title, y = y_label, x = "Year") +
        scale_y_continuous(labels = y_formatter) +
        theme_minimal(base_size = 10) +
        theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))

      if (save_fig && !is.null(output_dir)) {
        if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
        ggsave(filename = file.path(output_dir, paste0("plot_", metric, "_", param_term, "_country.pdf")),
               plot = p, width = 8, height = 5)
      }
      return(p)
    }

    # region/district bar plot logic
    if (level %in% c("region", "district") && is.null(filter_year)) {
      attr_data_plot <- attr_data_plot %>%
        dplyr::arrange(dplyr::desc(.data[[metric]])) %>%
        dplyr::mutate(!!level := factor(.data[[level]], levels = unique(.data[[level]])))

      max_y <- max(attr_data_plot[[uci_col]], na.rm = TRUE)

      district_plots <- attr_data_plot %>%
        split(ceiling(seq_along(attr_data_plot[[level]]) / 30)) %>%
        purrr::map(~ {
          ggplot(.x, aes(x = .data[[level]], y = .data[[metric]])) +
            geom_col(fill = "steelblue", width= 0.6) +
            geom_errorbar(aes(ymin = .data[[lci_col]], ymax = .data[[uci_col]], color = "95% CI"),
                          width = 0.2) +
            coord_flip() +
            labs(x = tools::toTitleCase(level), y = y_label) +
            scale_y_continuous(labels = y_formatter, limits = c(0, max_y)) +
            scale_color_manual(name = "", values = c("95% CI" = "black")) +
            theme_minimal(base_size = 10) +
            theme(axis.text.y = element_text(size = 7),
                  plot.title = element_text(hjust = 0.5, size = 9))
        })

      if (save_fig && !is.null(output_dir)) {
        if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
        pdf_file <- file.path(output_dir, paste0("plot_", metric, "_", param_term, "_", level, ".pdf"))
        pdf(pdf_file, width = 11, height = 8)
        for (i in seq_along(district_plots)) {
          merged_plot <- patchwork::wrap_plots(district_plots[i], ncol = 1) +
            patchwork::plot_annotation(
              title = paste(title, "by", tools::toTitleCase(level)),
              theme = theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5))
            )
          print(merged_plot)
        }
        dev.off()
      }
      return(district_plots)
    }

    # Region/district with multi-year grouped bar plot logic
    if (!is.null(filter_year) && length(filter_year) > 2 && level %in% c("region", "district")) {
      attr_data_plot <- attr_data_plot %>%
        group_by(.data[[level]], year) %>%
        summarise(
          across(matches("^AR_Number(_LCI|_UCI)?$"), ~ sum(.x, na.rm = TRUE)),
          across(matches("^AR_(Fraction|per_100k)(_LCI|_UCI)?$"), ~ mean(.x, na.rm = TRUE)),
          .groups = "drop")

      level_vals <- attr_data_plot %>%
        group_by(.data[[level]]) %>%
        summarise(avg = mean(.data[[metric]], na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(avg)) %>%
        pull(.data[[level]])

      attr_data_plot[[level]] <- factor(attr_data_plot[[level]], levels = level_vals)
      y_min <- min(attr_data_plot[[paste0(metric, "_LCI")]], na.rm = TRUE)
      y_max <- max(attr_data_plot[[paste0(metric, "_UCI")]], na.rm = TRUE)
      split_levels <- split(level_vals, ceiling(seq_along(level_vals) / 30))

      group_plots <- purrr::map(split_levels, function(subset_levels) {
        df <- filter(attr_data_plot, .data[[level]] %in% subset_levels)
        ggplot(df, aes(x = .data[[level]], y = .data[[metric]], fill = factor(year))) +
          geom_col(position = position_dodge(width = 0.8)) +
          geom_errorbar(
            aes(ymin = .data[[paste0(metric, "_LCI")]], ymax = .data[[paste0(metric, "_UCI")]],
                color = "95% CI"), position = position_dodge(0.8), width = 0.25) +
          scale_color_manual(name = "", values = c("95% CI" = "black")) +
          labs( x = tools::toTitleCase(level), y = y_label, fill = "Year") +
          theme_minimal(base_size = 8) +
          theme(axis.text.x = element_text(angle = 70, hjust = 1, size = 8),
                axis.text.y = element_text(size = 8),
                plot.margin = margin(t = 5, r = 5, b = 50, l = 5)) +
          coord_cartesian(ylim = c(y_min, y_max))
      })

      if (save_fig && !is.null(output_dir)) {
        if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
        pdf_file <- file.path(output_dir, paste0("plot_", metric, "_", param_term, "_", "Year_",level, ".pdf"))
        pdf(pdf_file, width = 11, height = 8)
        for (i in seq_along(group_plots)) {
          merged <- patchwork::wrap_plots(group_plots[i], ncol = 1) +
            patchwork::plot_annotation(
              title = paste(title, "by Year and", tools::toTitleCase(level)),
              theme = theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5))
            )
          print(merged)
        }
        dev.off()
      }
      return(group_plots)
    }
  })

  return(plots)
}


#' Run Full Diarrhea-Climate Analysis Pipeline
#'
#' @description
#' The `diarrhea_do_analysis` function runs the complete analysis workflow
#' by combining multiple functions to analyze the association between diarrhea
#' cases and climate variables. It processes health, climate, and spatial data,
#' fits models, generates plots, and calculates attributable risk.
#'
#' @param health_data_path Data frame containing the processed health data.
#' @param climate_data_path Data frame containing the processed climate data.
#' @param map_path Data frame containing the spatial map data (shapefile or equivalent).
#' @param region_col Character. Name of the column containing region names.
#' @param district_col Character. Name of the column containing district names.
#' @param date_col Character. Name of the column containing the date. Defaults to NULL.
#' @param year_col Character. Name of the column containing the year.
#' @param month_col Character. Name of the column containing the month.
#' @param diarrhea_case_col Character. Name of the column containing diarrhea case counts.
#' @param tot_pop_col Character. Name of the column containing total population.
#' @param tmin_col Character. Name of the column containing minimum temperature.
#' @param tmean_col Character. Name of the column containing mean temperature.
#' @param tmax_col Character. Name of the column containing maximum temperature.
#' @param rainfall_col Character. Name of the column containing cumulative
#' monthly rainfall.
#' @param r_humidity_col Character. Name of the column containing relative humidity.
#' @param runoff_col Character. Name of the column containing monthly runoff
#' data.
#' @param geometry_col Character. Name of the geometry column in the shapefile
#' (usually "geometry").
#' @param spi_col Character. Name of the column containing the Standardized
#' Precipitation Index. Defaults to NULL.
#' @param max_lag Numeric. Maximum lag to consider in the model
#' (typically 2 to 4). Defaults to 2.
#' @param basis_matrices_choices Character vector specifying basis matrix
#' parameters to include in the model (e.g., "tmax", "tmin", "rainfall",
#' "r_humidity", "spi").
#' @param param_term Character vector specifying the exposure variables of interest
#' (e.g., "tmax", "rainfall").
#' @param level Character. Spatial disaggregation level: "country", "region", or "district".
#' @param param_threshold Numeric. Threshold above which exposure is considered,
#' "attributable". Can take floats. Defaults to 1.
#' @param filter_year Integer. The year to filter to data to. Defaults to NULL.
#' @param family Character. The probability distribution for the response
#' variable. The user may also have thepossibility to choose "nbinomial" for a
#' negative binomial distribution. Defaults to "poisson".
#' @param config Boolean. Enable additional model configurations. Defaults to FALSE.
#' @param save_fig Boolean. If TRUE, saves the generated plots. Defaults to FALSE.
#' @param save_fig Boolean. If TRUE, saves the resultant datasets. Defaults to FALSE.
#' @param output_dir Character. The path to the directory where outputs
#' (e.g., plots, maps, datasets) should be saved.
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
#'
#' @export
diarrhea_do_analysis <- function(health_data_path,
                                 climate_data_path,
                                 map_path,
                                 region_col,
                                 district_col,
                                 date_col= NULL,
                                 year_col,
                                 month_col,
                                 diarrhea_case_col,
                                 tot_pop_col,
                                 tmin_col,
                                 tmean_col,
                                 tmax_col,
                                 rainfall_col,
                                 r_humidity_col,
                                 runoff_col,
                                 geometry_col,
                                 spi_col = NULL,
                                 max_lag = 2,
                                 inla_param,
                                 basis_matrices_choices,
                                 param_term,
                                 level,
                                 param_threshold = 1,
                                 filter_year = NULL,
                                 family = "poisson",
                                 config = FALSE,
                                 save_csv = FALSE,
                                 save_fig = FALSE,
                                 output_dir = NULL){

  # Simple output validation
  if (is.null(output_dir) & (save_fig | save_csv)) {
    stop("'output_dir' must be provided is 'save_fig' or save_csv' are TRUE.")
  }
  #check_file_exists(output_dir, TRUE)

  # level validation
  level <- tolower(level)
  acceptable_levels = c("country", "region", "district")
  if (!(level %in% acceptable_levels)) {
    stop(paste0("Level must be one of ", paste0(acceptable_levels, collapse=", ")))
  }

  # Input validation
  check_file_exists(health_data_path, TRUE)
  check_file_exists(climate_data_path, TRUE)
  check_file_exists(map_path, TRUE)

  # get combined data
  combined_data <- combine_health_climate_data(health_data_path,
                                               climate_data_path,
                                               map_path,
                                               region_col,
                                               district_col,
                                               date_col,
                                               year_col,
                                               month_col,
                                               diarrhea_case_col,
                                               tot_pop_col,
                                               tmin_col,
                                               tmean_col,
                                               tmax_col,
                                               rainfall_col,
                                               r_humidity_col,
                                               geometry_col,
                                               runoff_col,
                                               spi_col,
                                               max_lag,
                                               output_dir)

  #plot time seris
  plot_diarrhea<-plot_health_climate_timeseries(combined_data$data,
                                                param_term= "diarrhea",
                                                level = "country",
                                                filter_year = filter_year,
                                                save_fig = save_fig,
                                                output_dir = output_dir)
  plot_tmax<-plot_health_climate_timeseries(combined_data$data,
                                            param_term= "tmax",
                                            level = "district",
                                            filter_year = filter_year,
                                            save_fig = save_fig,
                                            output_dir = output_dir)

  plot_rainfall<-plot_health_climate_timeseries(combined_data$data,
                                                param_term= "rainfall",
                                                level = "country",
                                                filter_year = filter_year,
                                                save_fig = save_fig,
                                                output_dir = output_dir)

  # create base matrice
  basis <- set_cross_basis(combined_data$data)

  #check for multicolinearity
  VIF<- check_vif(combined_data$data, inla_param, basis_matrices_choices)

  # fitting the model
  inla_result <- run_inla_models(combined_data,
                                 basis_matrices_choices,
                                 inla_param=inla_param,
                                 output_dir=output_dir,
                                 save_csv=save_csv,
                                 family=family,
                                 config=config)

  # seasonality plot
  reff_plot_monthly <- plot_monthly_random_effects(combined_data,
                                                   model=inla_result$model,
                                                   output_dir=output_dir,
                                                   save_fig=save_fig )

  # spatial random effect
  reff_plot_yearly <- plot_yearly_spatial_random_effect(combined_data,
                                                        model=inla_result$model,
                                                        output_dir=output_dir,
                                                        save_fig=save_fig)
  # contour plots
  contour_plot <- contour_plot(combined_data$data,
                               param_term,
                               model=inla_result$model,
                               level=level,
                               output_dir=output_dir,
                               save_fig=save_fig,
                               filter_year=filter_year)

  # rr map plots
  rr_map_plot <- plot_rr_map(combined_data,
                             param_term,
                             model=inla_result$model,
                             level="district",
                             filter_year=filter_year,
                             output_dir=output_dir,
                             save_fig=save_fig)

  # relative rist plot
  rr_data <- plot_relative_risk( combined_data$data,
                                 param_term=param_term,
                                 model=inla_result$model,
                                 level=level,
                                 filter_year=filter_year,
                                 output_dir=output_dir,
                                 save_csv=save_csv,
                                 save_fig=save_fig)
  rr_plot <- rr_data[["plots"]]
  rr_df <- rr_data[["RR"]]

  # attribution fraction and number
  attr_frac_num <- attribution_calculation(combined_data$data,
                                           param_term=param_term,
                                           model=inla_result$model,
                                           param_threshold=param_threshold,
                                           level= level,
                                           filter_year=filter_year,
                                           save_csv=save_csv,
                                           output_dir=output_dir)

  #AN, AF, and AR plot
  plot_AR_Num <-plot_attribution_metric(attr_data = attr_frac_num,
                                        level= level,
                                        metrics = "AR_Number",
                                        filter_year = filter_year,
                                        param_term=param_term,
                                        save_fig =save_fig,
                                        output_dir = output_dir)

  plot_AR_Fr <-plot_attribution_metric(attr_data = attr_frac_num,
                                       level= level,
                                       metrics = "AR_Fraction",
                                       filter_year = filter_year,
                                       param_term=param_term,
                                       save_fig =save_fig,
                                       output_dir = output_dir)

  plot_AR_per_100k <-plot_attribution_metric(attr_data = attr_frac_num,
                                             level= level,
                                             filter_year = filter_year,
                                             param_term=param_term,
                                             metrics = "AR_per_100k",
                                             save_fig =save_fig,
                                             output_dir = output_dir)

  res <- list(plot_diarhea, plot_tmax, plot_rainfall, VIF, inla_result,
              reff_plot_monthly, reff_plot_yearly, contour_plot, rr_map_plot,
              rr_plot, rr_df, attr_frac_num, plot_AR_Num,plot_AR_Fr,
              plot_AR_per_100k)

  return(res)
}
