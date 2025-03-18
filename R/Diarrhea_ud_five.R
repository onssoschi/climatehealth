
#' @title R-code for Diarrhea disease cases attributable to extreme precipitation
#' and extreme temperature

#' Required_package

#' @description: Read in the requested package for the indicator calculation
#' including INLA package for the spatiotemporal modeling, and dlnm package for
#' the distributed lag nonlinear modeling.
#' @return loaded all required package

load_required_packages <- function() {
  # Load necessary packages
  required_packages <- c(
    "INLA", "stats", "data.table", "tidyverse", "here", "sf", "sp", "spdep",
    "dlnm", "tsModel", "hydroGOF", "RColorBrewer", "openxlsx", "splines",
    "geofacet", "ggpubr", "ggthemes"
  )

  # Load packages or install if missing
  for (pkg in required_packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }
}

#' load data
#' @description: the load_and_process_data() function loads, processes, and
#' prepares spatial and health data for further analysis.
#' @param map_path File path to the shapefile (.shp) containing geographical
#' boundary data for regions and districts. map file must contain a column of
#' region name, district name, and a column of geometry points. Make sure to
#' rename columns to match the expected
#'
#' @param data_path File path to the CSV file containing monthly recorded
#' health and climate data. data file must contain a column of region name,
#' district name, Date, Year, Month, Diarrhea Monthly record, climate variables
#' (tmin, tmean, tmax,rainfall, rhumidity), the lag variables for each climate
#' variables (tminlag,tmeanlag, tmaxlag, rlag), and the Population data column.
#'
#' @param graph_output_path File path for the adjacency matrix, used in
#' spatial analysis.
#' @returns a list containing map Processed spatial data with region and district
#' codes, data Health and climate dataset with corresponding codes, grid_data
#' Lookup table containing region and district codes, nb.map the adjacency matrix,
#' and the summary statistics

load_and_process_data <- function(map_path,
                                  data_path,
                                  graph_Output_path) {
  # Load and process map data
  map <- read_sf(map_path,
                 region_column,
                 district_column,
                 geometry_column)
  # Create adjacency matrix
  nb.map <- poly2nb(as_Spatial(map$geometry))
  if (!file.exists(graph_output_path)) {
    nb2INLA(graph_output, nb.map)
  }

  # Load health and climate data
  data <- read.csv(data_path,
                   date_column,
                   Region_column,
                   District_column,
                   Year_column,
                   Month_column,
                   Diarrheal_column,
                   Climate_column,
                   lag_climate_column
                   ) %>%
    group_by(Region_column,
             District_column) %>%
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

  map <- map %>%
    left_join(grid_data, by = c("Region", "District")) %>%
    arrange(region_code, district_code)

  grid_data <- grid_data %>%
    rename(name = Region, Code_num = region_code)

  # descriptive statistics
  summary_stats <- list(
    tmin = summary(data$tmin),
    tmax = summary(data$tmax),
    rainfall = summary(data$rainfall),
    rhumidity = summary(data$rhumidity)
  )

  # Return processed data as a list
  return(list(map = map,  nb.map = nb.map, data = data, grid_data = grid_data, summary = summary_stats))
}

#' set key variables
#' @description: Read in key variables needed to set the INLA and DLNM model.
#' @param data is the climate and health data previously prepared.
#' @param nlag define the maximum number of lag month to be included in the DLNM model
#' @returns A list of the defined key variables(ntime, nyear, ndistrict, nregion)
#' ntime define the total number of months in the dataset,
#' nyear the total number of year in the dataset,
#' ndistrict the total number of unique districts in the dataset
#' nregion the total number of unique regions in the dataset

set_variables <- function(data, nlag=2) {
  # Calculate key variables
  ntime <- length(unique(data$time))       # Total number of months
  nyear <- length(unique(data$Year))       # Total number of years
  ndistrict <- length(unique(data$district_code))  # Total number of districts
  nregion <- length(unique(data$region_code))  # Total number of regions

  # Return results as a list
  return(list(
    nlag = nlag,
    ntime = ntime,
    nyear = nyear,
    ndistrict = ndistrict,
    nregion = nregion
  ))
}

#' set cross-basis matrix for DLNM
#' @description: The set_cross_basis() function constructs cross-basis matrices
#' for use in a Distributed Lag Non-Linear Model (DLNM), capturing the delayed
#' effects of climate variables (temperature and rainfall) on health outcomes.
#' @param data a dataset containing temperature (tmax, tmin, tmean) and rainfall
#' variables.
#' @param nlag the maximum number of lagged months to consider in the analysis.
#' @param lagknot defines knot locations for the lag structure using equally
#' spaced knots over the lag range [0, nlag]
#' @param basis_tmax is the crossbasis() matrix which capture both: Exposure-response
#' relationship (argvar) Models how maximum temperature affects the outcome, and the
#' Lag-response relationship (arglag) Models how past exposure (lagged values)
#' influences the outcome.
#' @param basis_tmin is the crossbasis() matrix which capture both: Exposure-response
#' relationship of minimum temperature and diarrhea cases
#' @param basis_tmean is the crossbasis() matrix which capture both: Exposure-response
#' relationship of the average temperature and diarrhea cases.
#' @param basis_rainfall is the crossbasis() matrix which capture both: Exposure-response
#' relationship of the cumulative rainfall and diarrhea cases.
#' @returns a list of the defined cross_basis matrix for minimum, mean, and
#' maximum temperature, and rainfall.

set_cross_basis <- function(data, nlag=2, lagknot, basis_tmax, basis_tmin, basis_tmean, basis_rainfall ) {

  # Define lag knots
  lagknot <- equalknots(0:nlag, 2)

  # Tmax
  var_tmax <- subset(data, select = c("tmax", paste0("tmaxlag", 1:nlag)))
  basis_tmax <- crossbasis(var_tmax,
                           argvar = list(fun = "ns", knots = equalknots(data$tmax, 2)),
                           arglag = list(fun = "ns", knots = nlag/2))

  # Tmin
  var_tmin <- subset(data, select = c("tmin", paste0("tminlag", 1:nlag)))
  basis_tmin <- crossbasis(var_tmin,
                           argvar = list(fun = "ns", knots = equalknots(data$tmin, 2)),
                           arglag = list(fun = "ns", knots = nlag/2))

  # Tmean
  var_tmean <- subset(data, select = c("tmean", paste0("tmeanlag", 1:nlag)))
  basis_tmean <- crossbasis(var_tmean,
                            argvar = list(fun = "ns", knots = equalknots(data$tmean, 2)),
                            arglag = list(fun = "ns", knots = nlag/2))

  # Rainfall
  var_rainfall <- subset(data, select = c("rainfall", paste0("rlag", 1:nlag)))
  basis_rainfall <- crossbasis(var_rainfall,
                               argvar = list(fun = "ns", knots = equalknots(data$rainfall, 2)),
                               arglag = list(fun = "ns", knots = nlag/2))

  # Assign unique column names to cross-basis matrices (required for INLA)
  colnames(basis_tmax) <- paste0("basis_tmax.", colnames(basis_tmax))
  colnames(basis_tmin) <- paste0("basis_tmin.", colnames(basis_tmin))
  colnames(basis_tmean) <- paste0("basis_tmean.", colnames(basis_tmean))
  colnames(basis_rainfall) <- paste0("basis_rainfall.", colnames(basis_rainfall))

  # Return a list of cross-basis matrices
  return(list(
    basis_tmax = basis_tmax,
    basis_tmin = basis_tmin,
    basis_tmean = basis_tmean,
    basis_rainfall = basis_rainfall
  ))
}

#' create indices for INLA models
#' @description: for the INLA model, there is a need to set-up regions index,
#'  district index, and year index. This function create these indices using
#'  the dataset, ndistrict and nregion defined above.
#' @param data is the dataset containing district_code, region_code, and Year columns.
#' @param ndistrict is the total number of unique districts in the dataset.
#' @param nregion is the total number of unique regions in the dataset.
#' @param district_index is district index that repeats numbers from 1 to
#' ndistrict to match the total number of rows in data. Ensures that the index
#' starts at 1 and follows INLA’s requirement.
#' @param unique_districts extracts unique district codes and stores them in
#' unique_districts, loops over each district and assigns a unique index.
#' @param region_index is the initialized region index
#' @param unique_regions extracts unique region codes and stores them in
#' unique_regions, loops over each region and assigns a unique index.
#' @param year_index converts the calendar year into an index.
#' @returns the modified data with the new indices

create_inla_indices <- function(data, ndistrict, nregion) {
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
  data$year_index <- data$Year - (min(Year)-1)

  return(data)
}

# Setup priors for INLA model
#' @description: setup_inla_priors(data) function prepares the dataset for INLA
#' modeling by defining response variables, computing offsets, and setting up
#' random effects.
#' @param data is the dataset containing health data, climate data,
#' district_code, region_code, and Year columns.
#' Y extracts the dependent variable (number of diarrhea cases) from
#' the dataset.
#' N counts the total number of Diarrhea observations in the dataset.
#' E is the expected cases and computes an expected number of diarrhea
#' cases using the total population (pop_tot), ensuring the response variable is
#' modeled as an incidence rate per 100,000 people.
#' SIR is the Standardized Incidence Ratio and calculates the ratio of
#' observed cases (Diarrhea) to expected cases (E), which helps assess disease
#' risk in different regions.
#' T1 is the Seasonal effect (monthly variation)
#' T2 is the Inter-annual variability (yearly trends)
#' S1 is the District-level spatial random effect
#' S2 is the Region-level spatial-temporal interaction
#' rh extracts the confounding variable relative humidity
#' tmin extracts the confounding variable minimum temperature
#' df is a dataframe created for the model testing. it includes
#' the following selected variables: Y, E, T1, T2, S1, S2
#' precision.prior specifies prior distributions for model parameters.
#' pc.prec is a penalized complexity prior, with parameters 0.5 and
#' 0.01 controlling the precision level.
#' @returns a list of the prepared dataset (df)

setup_inla_priors <- function(data) {

  # Set response variable
  Y  <- data$Diarrhea  # Response variable
  N  <- length(Y)      # Total number of data points

  # Compute expected cases (E) as an offset for incidence rate modeling
  data$E  <- round(data$pop_tot * sum(data$Diarrhea, na.rm = TRUE) / sum(data$pop_tot, na.rm = TRUE))
  data$SIR <- data$Diarrhea / data$E  # Standardized Incidence Ratio
  E <- data$E

  # Define random effect variables
  T1 <- data$Month         # Seasonality (monthly random effect)
  T2 <- data$year_index    # Inter-annual variability
  S1 <- data$district_index # District-level spatial effect
  S2 <- data$region_index   # Region interaction with month random effect
  rh <- data$rhumidity
  tmin <- data$tmin
  rainfall <- data$rainfall

  # Create dataframe for model testing
  df <- data.frame(Y, E, T1, T2, S1, S2, rh, tmin, rainfall)

  # Define priors for INLA model
  precision.prior <- list(prec = list(prior = "pc.prec", param = c(0.5, 0.01)))

  return(list(df = df, precision_prior = precision.prior))
}

#' Function to fit an INLA model
#' @description fit_inla_model() function fits an INLA (Integrated Nested Laplace
#' Approximation) model using the prepared dataset
#' @param formula is a model defining predictors and random effects.
#' @param data dataset prepared by setup_inla_data() (df)
#' @param family The probability distribution for the response variable
#' (default: "poisson", it can also be "nbinomial")
#' @param config is a Boolean flag to enable additional model configurations.
#' @return the fitted INLA model

fit_inla_model <- function(formula, data=df, family = "poisson", config = FALSE) {

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
#' spatiotemporal random effects
#' @description: run_inla_models() function is used to define the baseline model
#' formula and the extended formulas.
#' @param data dataset prepared by setup_inla_data() (df) baseformula defined the baseline formula including an intercept,
#' confounders variable, and the spatiotemporal parameters.
#' @param precision_prior
#' @param output_dir
#'
#' @return list the different models, dic_table, and the best fit model.

run_inla_models <- function(data = df, precision_prior, output_dir = output_path) {

  # Ensure output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Define baseline model formula
  baseformula <- Y ~ 1 + rh + tmin +
    f(T1, replicate = S2, model = "rw1", cyclic = TRUE, constr = TRUE,
      scale.model = TRUE, hyper = precision_prior) +
    f(S1, model = "bym2", replicate = T2, graph = file.path(output_dir, graph_Output_path),
      scale.model = TRUE, hyper = precision_prior)

  # Define extended formulas
  formula0.1 <- update.formula(baseformula, ~ . + basis_tmax)
  formula0.2 <- update.formula(baseformula, ~ . + basis_rainfall)
  formula0.3 <- update.formula(baseformula, ~ . + basis_tmax + basis_rainfall)

  # List of formulas and corresponding labels
  formulas <- list(baseformula, formula0.1, formula0.2, formula0.3)
  labels <- c("basemodel", "model0.1", "model0.2", "model0.3")

  # Fit models and save results
  models <- lapply(seq_along(formulas), function(i) {
    model <- mymodel(formulas[[i]], df)
    save(model, file = file.path(output_dir, paste0(labels[i], ".RData")))
    return(model)
  })

  # Create a table to store DIC values
  table0 <- data.table(Model = c("base", "tmax", "rainfall", "tmax + rainfall"), DIC = NA)

  # Populate DIC values from saved models
  for (i in seq_along(labels)) {
    file_path <- file.path(output_dir, paste0(labels[i], ".RData"))
    if (file.exists(file_path)) {
      load(file_path)
      if (!is.null(model$dic$dic)) {
        table0$DIC[i] <- round(model$dic$dic, 0)
      }
    }
  }

  # Identify the best fitting model
  best_fit <- which.min(table0$DIC)

  return(list(models = models, dic_table = table0, best_fit = best_fit))
}

#' best fit model adequacy results
#' @description

Fit_inla_models_adequacy <- function(df, precision_prior, output_dir = "Outputgdia") {

  # Ensure output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Assign formula for best fitting model to the new baseformula
  baseformula <- formulas[[best.fit]]

  # The baseline model formula could also be redefined as follows
  baseformula <- Y ~ 1 + rh + tmin +
    f(T1, replicate = S2, model = "rw1", cyclic = TRUE, constr = TRUE,
      scale.model = TRUE, hyper = precision_prior) +
    f(S1, model = "bym2", replicate = T2, graph = file.path(output_dir, graph_Output_path),
      scale.model = TRUE, hyper = precision_prior) + basis_tmax + basis_rainfall

  # Model names
  mod.name <- c("basemodel", "model0.1", "model0.2", "model0.3")

  # Create a table to store model adequacy results (DIC, log score)
  table1 <- data.table(Model = mod.name, DIC = NA, logscore = NA)

  # Loop through model labels, load models, and extract fit statistics
  for (i in seq_along(mod.name)) {
    model_path <- file.path(output_dir, paste0(mod.name[i], ".RData"))

    if (file.exists(model_path)) {
      load(model_path)
      table1$DIC[i] <- round(model$dic$dic, 0)
      table1$logscore[i] <- round(-mean(log(model$cpo$cpo), na.rm = TRUE), 3)
    }
  }

  # Save model adequacy results
  fwrite(table1, file = file.path("figsgdia", "table_S01.csv"), quote = FALSE, row.names = FALSE)

  return(table1)
}


#' load baseline and selected model
#' @description

load_inla_models <- function(output_dir = Output_path, selected_model = Best_fit_model_path) {
  # Load baseline model
  baseline_path <- file.path(output_dir, "basemodel.RData")
  if (file.exists(baseline_path)) {
    load(baseline_path)
    basemodel <- model
  } else {
    stop("Baseline model file not found.")
  }

  # Load selected model
  selected_path <- file.path(output_dir, paste0(selected_model, ".RData"))
  if (file.exists(selected_path)) {
    load(selected_path)
  } else {
    stop("Selected model file not found.")
  }

  return(list(baseline_model = basemodel, selected_model = model))
}


#'  Visualise random effects for selected model
#'  @description
#'

plot_monthly_random_effects <- function(data, grid_data, map, model, output_file) {
  # Create data frame for monthly random effects per state
  month_effects <- data.frame(region_code = rep(unique(data$region_code), each = 12),
                              Month = model$summary.random$T1)

  # Merge with predefined state grid
  month_effects <- month_effects %>%
    left_join(grid_data %>% select(-District, -district_code) %>% unique(),
              by = c("region_code" = "Code_num"))

  month_effects <- map %>% select(-District) %>% unique() %>% left_join(month_effects, by = c("Region" = "name"))

  # Generate plot
  p <- month_effects %>%
    ggplot() +
    geom_ribbon(aes(x = Month.ID, ymin = `Month.0.025quant`, ymax = `Month.0.975quant`),
                fill = "cadetblue4", alpha = 0.5) +
    geom_line(aes(x = Month.ID, y = Month.mean), col = "cadetblue4") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
    xlab("Month") +
    ylab("Contribution to log(Diarrhea cases)") +
    scale_y_continuous() +
    scale_x_continuous(breaks = c(1,4,7,10), labels = c("Jan", "Apr", "Jul", "Oct")) +
    theme_bw() +
    facet_wrap(~Region)

  # Save plot
  ggsave(output_file, plot = p, height = 30, width = 25, units = "cm")
}

#' Relative Risk Plot at country level.
#' @description: contour_plot_tmax() function generates a contour plot showing
#' the lag exposure effect of maximum temperature (tmax) on diarrhea cases.
#' @param model_file path to the saved best fit model
#' @param output_pdf: File path for saving the plot.
#' @param coef extracts mean estimates of fixed-effect coefficients from the model.
#' @param vcov extracts the covariance matrix of linear combinations from the model.
#' @param indt identifies indices of terms in the model related to the maximum
#' temperature cross-basis variable
#' @param predt uses crosspred (from the dlnm package) to generate predictions
#' from a distributed lag non-linear model (DLNM)
#' @param pdf opens a PDF device to save the plot.
#' @param y extracts predicted values of temperature.
#' @param x creates a sequence from 0 to nlag in steps of 0.25 for the x-axis
#' @param z extracts and transposes the matrix of relative risk predictions.
#' @return contour plot of maximum temperature

contour_plot_tmax <- function(selected_model = Best_fit_model_path, output_dir = Output_path, nlag=2, data) {
  # Load best fitting model

  if (file.exists(model_file)) {
    load(model_file)
    model0 <- model
  } else {
    stop("Model file not found.")
  }

  model <- model0

  # Extract full coefficients and covariance matrix
  coef <- model$summary.fixed$mean
  vcov <- model$misc$lincomb.derived.covariance.matrix

  # Find positions of terms associated with tmax crossbasis
  indt <- grep("basis_tmax", model$names.fixed)

  # Extract predictions from the tmax DLNM centered on overall mean Tmax
  predt <- crosspred(basis_tmax, coef = coef[indt], vcov = vcov[indt, indt],
                     model.link = "log", bylag = 0.25, cen = round(mean(data$tmax, na.rm = TRUE), 0))

  # Generate contour plot
  pdf(output_pdf, width = 6.5, height = 6)

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
    ylab = expression(paste("Temperature (", degree, "C)")),
    main = "Diarrhea cases",
    col = cols,
    levels = levels,
    plot.axes = {
      axis(1, at = 0:nlag, labels = 0:nlag)
      axis(2)
      mtext(side = 2, at = max(y) * 0.95, text = "a", las = 2, cex = 1.2, line = 2)
    }
  )

  if (dev.cur() > 1) dev.off()
}

#' Relative Risk plot by Temperature Range
#' @description plot_relative_risk_temperature() function plots the relative
#' risk of the maximum temperature and diarrhea association at country level.
#' @param output_file is the path where the PNG file will be saved
#' @param predt is the required input object (likely from crosspred(),
#' containing temperature exposure and relative risk estimates).
#' @param rr is the aggregated relative risks
#' @param rr.lci is the lower bound of confidence interval
#' @param rr.uci is the upper bound of confidence interval
#' @return the relative risk plot of diarrhea risk by maximum temperature

plot_relative_risk_temperature <- function(output_file, predt) {
  # Output plot as a high-resolution PNG
  png(output_file, width = 800, height = 800, res = 150)

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

  # Plot Relative Risk by Temperature Range
  plot(vars, rr, type = "l", col = "red", lwd = 2,
       xlab = "Temperature (°C)", ylab = "Relative Risk",
       main = "Diarrhea Relative Risk by Temperature Range",
       ylim = c(r1, r2 * 1.1), frame.plot = TRUE)

  # Add shaded confidence interval
  polygon(c(vars, rev(vars)),
          c(rr.lci, rev(rr.uci)),
          col = adjustcolor("red", alpha.f = 0.3), border = NA)

  # Add horizontal reference line at RR = 1
  abline(h = 1, lty = 2, col = "gray")

  # Add legend
  legend("topright", legend = "Relative Risk by Temperature", col = "red", lwd = 2, bty = "n")

  # Save and close the PNG device
  dev.off()
}

#' Contour Plot at region level
#' @description plot_tmax_contours_by_region() function generates a contour plot
#' showing the lag exposure effect of maximum temperature (tmax) on diarrhea cases
#' for each region.
#' @param model_file path to the saved best fit model
#' @param output_pdf: File path for saving the plot.
#' @param regions Extracts unique regions from the dataset and iterates through
#' each region to create individual plots
#' @param coef extracts mean estimates of fixed-effect coefficients from the model.
#' @param vcov extracts the covariance matrix of linear combinations from the model.
#' @param indt identifies indices of terms in the model related to the maximum
#' temperature cross-basis variable
#' @param predt uses crosspred (from the dlnm package) to generate predictions
#' from a distributed lag non-linear model (DLNM)
#' @param pdf opens a PDF device to save the plot.
#' @param y extracts predicted values of temperature.
#' @param x creates a sequence from 0 to nlag in steps of 0.25 for the x-axis
#' @param z extracts and transposes the matrix of relative risk predictions.
#' @return contour plot of maximum temperature

plot_tmax_contours_by_region <- function(model_file_path, output_path, data, nlag) {
  # Load model output
  if (file.exists(model_file)) {
    load(model_file)
    model0 <- model
  } else {
    stop("Model file not found.")
  }

  # Open a single PDF file for all regions
  pdf(output_pdf, width = 12, height = 12)

  # Set up a consistent layout for the plots (1 per page)
  par(mfrow = c(2, 1))  # One plot per page

  # Iterate over unique regions
  regions <- unique(data$Region)
  for (region in regions) {
    # Filter data for the current region
    region_data <- subset(data, Region == region)

    # Set the model for the region
    model <- model0

    # Extract full coef and vcov for the region
    coef <- model$summary.fixed$mean
    vcov <- model$misc$lincomb.derived.covariance.matrix

    # Find position of terms associated with tmax crossbasis
    indt <- grep("basis_tmax", model$names.fixed)

    # Extract predictions from the tmax DLNM centered on overall mean Tmax
    mean_temp <- round(mean(region_data$tmax, na.rm = TRUE), 0)
    predt <- crosspred(basis_tmax, coef = coef[indt], vcov = vcov[indt, indt],
                       model.link = "log", bylag = 0.25, cen = mean_temp)

    # Variables for plotting
    y <- predt$predvar
    x <- seq(0, nlag, 0.25)
    z <- t(predt$matRRfit)

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

#'  Relative Risk plot by Temperature Range for Each Region
#' @description plot_relative_risk_by_region() function generates the relative
#' risk plot of diarrhea risk by temperature for each region in the dataset
#' @param model_file path to the saved best fit model
#' @param output_pdf: File path for saving the plot
#' @return relative risk plot for each region

plot_relative_risk_by_region <- function(model_file_path, output_path, data) {
  # Load model output
  if (file.exists(model_file)) {
    load(model_file)
    model0 <- model
  } else {
    stop("Model file not found.")
  }

  # Open a single PDF file for all regions
  pdf(output_pdf, width = 10, height = 10)

  # Set up a consistent layout for multiple plots
  regions <- unique(data$Region)
  layout(matrix(1:length(regions), nrow = ceiling(sqrt(length(regions)))))
  par(mfrow = c(3, 2))

  for (region in regions) {
    # Filter data for the current region
    region_data <- subset(data, Region == region)

    # Set the model for the region
    model <- model0

    # Extract full coef and vcov for the region
    coef <- model$summary.fixed$mean
    vcov <- model$misc$lincomb.derived.covariance.matrix

    # Find position of terms associated with tmax crossbasis
    indt <- grep("basis_tmax", model$names.fixed)

    # Extract predictions from the tmax DLNM centered on overall mean Tmax
    mean_temp <- round(mean(region_data$tmax, na.rm = TRUE), 0)
    predt <- crosspred(basis_tmax, coef = coef[indt], vcov = vcov[indt, indt],
                       model.link = "log", bylag = 0.25, cen = mean_temp)

    # Extract exposure values and relative risk data
    vars <- predt$predvar
    rr <- predt$allRRfit  # Use aggregated relative risks
    rr.lci <- predt$allRRlow  # Lower bound of confidence interval
    rr.uci <- predt$allRRhigh  # Upper bound of confidence interval

    # Ensure exposure and RR data are finite
    if (anyNA(c(vars, rr, rr.lci, rr.uci))) {
      next  # Skip the region if data contains missing values
    }

    # Set y-axis limits dynamically based on RR range
    r1 <- min(c(rr, rr.lci, rr.uci), na.rm = TRUE)
    r2 <- max(c(rr, rr.lci, rr.uci), na.rm = TRUE)

    # Plot Relative Risk by Temperature Range
    plot(vars, rr, type = "l", col = "red", lwd = 2,
         xlab = "Temperature (°C)", ylab = "Relative Risk",
         main = paste("Relative Risk by Temperature for", region),
         ylim = c(r1, r2 * 1.1), frame.plot = TRUE)

    # Add shaded confidence interval
    polygon(c(vars, rev(vars)),
            c(rr.lci, rev(rr.uci)),
            col = adjustcolor("red", alpha.f = 0.3), border = NA)

    # Add horizontal reference line at RR = 1
    abline(h = 1, lty = 2, col = "gray")

    # Add legend
    legend("topright", legend = "Relative Risk by Temperature", col = "red", lwd = 2, bty = "n")
  }

  # Close the PDF device
  dev.off()
}

#' Contours plot at district level
#' @description plot_tmax_contours_by_district() function generates a contour plot
#' showing the lag exposure effect of maximum temperature (tmax) on diarrhea cases
#' for each district
#' @param model_file path to the saved best fit model
#' @param output_path: File path for saving the plot
#' @return relative risk plot for each region

plot_tmax_contours_by_district <- function(model_file, output_path, data, nlag) {
  # Load model output
  if (file.exists(model_file)) {
    load(model_file)
    model0 <- model
  } else {
    stop("Model file not found.")
  }

  # Open a single PDF file for all districts
  pdf(output_path, width = 12, height = 12)

  # Set up a 2x2 grid for four plots per page
  par(mfrow = c(2, 2))

  # Initialize plot counter
  plot_count <- 0

  # Iterate over unique districts
  districts <- unique(data$District)
  for (district in districts) {
    # Filter data for the current district
    district_data <- subset(data, District == district)

    # Set the model for the district
    model <- model0

    # Extract full coef and vcov for the district
    coef <- model$summary.fixed$mean
    vcov <- model$misc$lincomb.derived.covariance.matrix

    # Find position of terms associated with tmax crossbasis
    indt <- grep("basis_tmax", model$names.fixed)

    # Extract predictions from the tmax DLNM centered on overall mean Tmax
    mean_temp <- round(mean(district_data$tmax, na.rm = TRUE), 0)
    predt <- crosspred(basis_tmax, coef = coef[indt], vcov = vcov[indt, indt],
                       model.link = "log", bylag = 0.25, cen = mean_temp)

    # Variables for plotting
    y <- predt$predvar
    x <- seq(0, nlag, 0.25)
    z <- t(predt$matRRfit)

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

#' Relative Risk plot by Temperature Range for Each district
#' @description
#' @param

plot_relative_risk_temperature <- function(data, basis_tmax, coef, vcov,
                                           output_file) {
  # Set up a PDF output device
  pdf(output_file, width = 10, height = 10)

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

    # Calculate the mean temperature for centering (if applicable)
    mean_temp <- round(mean(district_data$tmax, na.rm = TRUE), 0)

    # Extract predictions for the current district
    predt <- crosspred(basis_tmax, coef = coef[indt], vcov = vcov[indt, indt],
                       model.link = "log", bylag = 0.25, cen = mean_temp)

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

    # Plot Relative Risk by Temperature Range
    plot(vars, rr, type = "l", col = "red", lwd = 2,
         xlab = "Temperature (°C)", ylab = "Relative Risk",
         main = paste("Diarrhea Relative Risk by Temperature for", district),
         ylim = c(r1, r2 * 1.1), frame.plot = TRUE)

    # Add shaded confidence interval
    polygon(c(vars, rev(vars)),
            c(rr.lci, rev(rr.uci)),
            col = adjustcolor("red", alpha.f = 0.3), border = NA)

    # Add horizontal reference line at RR = 1
    abline(h = 1, lty = 2, col = "gray")

    # Add legend
    legend("topright", legend = "Relative Risk by Temperature", col = "red", lwd = 2, bty = "n")

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

####################### Rainfall ###############################################

#' rainfall contour plot of exposure-lag-response associations at Country level
#' @description
#' @param

plot_rainfall_output <- function(model, data, basis_rainfall, nlag, output_path) {
  # Select the climate model
  model <- model0

  # Extract full coef and vcov and create indicators for each term
  coef <- model$summary.fixed$mean
  vcov <- model$misc$lincomb.derived.covariance.matrix

  # Find position of the terms associated with tmax crossbasis
  indt <- grep("basis_rainfall", model$names.fixed)

  # Extract predictions from the rainfall DLNM
  predt <- crosspred(basis_rainfall, coef = coef[indt], vcov = vcov[indt, indt],
                     model.link = "log", bylag = 0.25, cen = round(mean(data$rainfall, na.rm = TRUE), 0))

  # Contour plot of exposure-lag-response associations
  pdf(output_path, width = 6.5, height = 6)

  # Variables for plotting
  y <- predt$predvar
  x <- seq(0, nlag, 0.25)
  z <- t(predt$matRRfit)

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
    ylab = expression(paste("Rainfall (", millimeter, "mm)")),
    main = "Diarrhea Under Five relative risk by rainfall",
    col = cols,
    levels = levels,
    plot.axes = {
      axis(1, at = 0:nlag, labels = 0:nlag)  # Customize x-axis
      axis(2)  # Customize y-axis
      mtext(side = 2, at = max(y) * 0.95, text = "a", las = 2, cex = 1.2, line = 2)
    }
  )

  # Close PDF device
  if (dev.cur() > 1) dev.off()
}

#' Relative Risk plot by cumulative rainfall at country level
#' @description
#' @param

plot_relative_risk_rainfall <- function(output_file, predt) {
  # Output plot as a high-resolution PNG
  png(output_file, width = 800, height = 800, res = 150)

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

  # Plot Relative Risk by Rainfall Range
  plot(vars, rr, type = "l", col = "blue", lwd = 2,
       xlab = "Rainfall (mm)", ylab = "Relative Risk",
       main = "Diarrhea Relative Risk by Rainfall Range",
       ylim = c(r1, r2 * 1.1), frame.plot = TRUE)

  # Add shaded confidence interval
  polygon(c(vars, rev(vars)),
          c(rr.lci, rev(rr.uci)),
          col = adjustcolor("blue", alpha.f = 0.3), border = NA)

  # Add horizontal reference line at RR = 1
  abline(h = 1, lty = 2, col = "gray")

  # Add legend
  legend("topright", legend = "Relative Risk by Rainfall", col = "blue", lwd = 2, bty = "n")

  # Save and close the PNG device
  dev.off()
}

#' contour plot rainfall for each region
#' @description
#' @param


plot_rainfall_contours_by_region <- function(model_file_path, output_path, data, nlag) {
  # Load model output
  if (file.exists(model_file_path)) {
    load(model_file_path)
    model0 <- model
  } else {
    stop("Model file not found.")
  }

  # Open a single PDF file for all regions
  pdf(output_path, width = 12, height = 12)

  # Set up a consistent layout for the plots (1 per page)
  par(mfrow = c(2, 1))  # One plot per page

  # Iterate over unique regions
  regions <- unique(data$Region)
  for (region in regions) {
    # Filter data for the current region
    region_data <- subset(data, Region == region)

    # Set the model for the region
    model <- model0

    # Extract full coef and vcov for the region
    coef <- model$summary.fixed$mean
    vcov <- model$misc$lincomb.derived.covariance.matrix

    # Find position of terms associated with rainfall crossbasis
    indt <- grep("basis_rainfall", model$names.fixed)

    # Extract predictions from the rainfall DLNM centered on overall mean Rainfall
    mean_rainfall <- round(mean(region_data$rainfall, na.rm = TRUE), 0)
    predt <- crosspred(basis_rainfall, coef = coef[indt], vcov = vcov[indt, indt],
                       model.link = "log", bylag = 0.25, cen = mean_rainfall)

    # Variables for plotting
    y <- predt$predvar
    x <- seq(0, nlag, 0.25)
    z <- t(predt$matRRfit)

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
      ylab = expression(paste("Rainfall (mm)")),
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

#' Relative risk for each region
#' @description
#' @param

plot_relative_risk_rainfall_by_region <- function(model_file_path, output_path, data) {
  # Load model output
  if (file.exists(model_file_path)) {
    load(model_file_path)
    model0 <- model
  } else {
    stop("Model file not found.")
  }

  # Open a single PDF file for all regions
  pdf(output_path, width = 10, height = 10)

  # Set up a consistent layout for multiple plots
  regions <- unique(data$Region)
  layout(matrix(1:length(regions), nrow = ceiling(sqrt(length(regions)))))
  par(mfrow = c(3, 2))

  for (region in regions) {
    # Filter data for the current region
    region_data <- subset(data, Region == region)

    # Set the model for the region
    model <- model0

    # Extract full coef and vcov for the region
    coef <- model$summary.fixed$mean
    vcov <- model$misc$lincomb.derived.covariance.matrix

    # Find position of terms associated with rainfall crossbasis
    indt <- grep("basis_rainfall", model$names.fixed)

    # Extract predictions from the rainfall DLNM centered on overall mean Rainfall
    mean_rainfall <- round(mean(region_data$rainfall, na.rm = TRUE), 0)
    predt <- crosspred(basis_rainfall, coef = coef[indt], vcov = vcov[indt, indt],
                       model.link = "log", bylag = 0.25, cen = mean_rainfall)

    # Extract exposure values and relative risk data
    vars <- predt$predvar
    rr <- predt$allRRfit  # Use aggregated relative risks
    rr.lci <- predt$allRRlow  # Lower bound of confidence interval
    rr.uci <- predt$allRRhigh  # Upper bound of confidence interval

    # Ensure exposure and RR data are finite
    if (anyNA(c(vars, rr, rr.lci, rr.uci))) {
      next  # Skip the region if data contains missing values
    }

    # Set y-axis limits dynamically based on RR range
    r1 <- min(c(rr, rr.lci, rr.uci), na.rm = TRUE)
    r2 <- max(c(rr, rr.lci, rr.uci), na.rm = TRUE)

    # Plot Relative Risk by Rainfall Range
    plot(vars, rr, type = "l", col = "blue", lwd = 2,
         xlab = "Rainfall (mm)", ylab = "Relative Risk",
         main = paste("Relative Risk by Rainfall for", region),
         ylim = c(r1, r2 * 1.1), frame.plot = TRUE)

    # Add shaded confidence interval
    polygon(c(vars, rev(vars)),
            c(rr.lci, rev(rr.uci)),
            col = adjustcolor("blue", alpha.f = 0.3), border = NA)

    # Add horizontal reference line at RR = 1
    abline(h = 1, lty = 2, col = "gray")

    # Add legend
    legend("topright", legend = "Relative Risk by Rainfall", col = "blue", lwd = 2, bty = "n")
  }

  # Close the PDF device
  dev.off()
}

#' contour plot district
#' @description
#'
#' @param
#'

plot_rainfall_contours_by_district <- function(model_file, output_path, data, nlag) {
  # Load model output
  if (file.exists(model_file)) {
    load(model_file)
    model0 <- model
  } else {
    stop("Model file not found.")
  }

  # Open a single PDF file for all districts
  pdf(output_path, width = 12, height = 12)

  # Set up a 2x2 grid for four plots per page
  par(mfrow = c(2, 2))

  # Initialize plot counter
  plot_count <- 0

  # Iterate over unique districts
  districts <- unique(data$District)
  for (district in districts) {
    # Filter data for the current district
    district_data <- subset(data, District == district)

    # Set the model for the district
    model <- model0

    # Extract full coef and vcov for the district
    coef <- model$summary.fixed$mean
    vcov <- model$misc$lincomb.derived.covariance.matrix

    # Find position of terms associated with rainfall crossbasis
    indt <- grep("basis_rainfall", model$names.fixed)

    # Extract predictions from the rainfall DLNM centered on overall mean Rainfall
    mean_rainfall <- round(mean(district_data$rainfall, na.rm = TRUE), 0)
    predt <- crosspred(basis_rainfall, coef = coef[indt], vcov = vcov[indt, indt],
                       model.link = "log", bylag = 0.25, cen = mean_rainfall)

    # Variables for plotting
    y <- predt$predvar
    x <- seq(0, nlag, 0.25)
    z <- t(predt$matRRfit)

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
      ylab = "Rainfall (mm)",
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

#' relative risk plot district
#' @description
#'
#' @param
#'

plot_relative_risk_rainfall <- function(data, basis_rainfall, coef, vcov,
                                        output_file) {
  # Set up a PDF output device
  pdf(output_file, width = 10, height = 10)

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

    # Calculate the mean rainfall for centering (if applicable)
    mean_rainfall <- round(mean(district_data$rainfall, na.rm = TRUE), 0)

    # Extract predictions for the current district
    predt <- crosspred(basis_rainfall, coef = coef[indt], vcov = vcov[indt, indt],
                       model.link = "log", bylag = 0.25, cen = mean_rainfall)

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

    # Plot Relative Risk by Rainfall Range
    plot(vars, rr, type = "l", col = "blue", lwd = 2,
         xlab = "Rainfall (mm)", ylab = "Relative Risk",
         main = paste("Diarrhea Relative Risk by Rainfall for", district),
         ylim = c(r1, r2 * 1.1), frame.plot = TRUE)

    # Add shaded confidence interval
    polygon(c(vars, rev(vars)),
            c(rr.lci, rev(rr.uci)),
            col = adjustcolor("blue", alpha.f = 0.3), border = NA)

    # Add horizontal reference line at RR = 1
    abline(h = 1, lty = 2, col = "gray")

    # Add legend
    legend("topright", legend = "Relative Risk by Rainfall", col = "blue", lwd = 2, bty = "n")

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

############### Attributable number and fraction calculation ##################

#' attribution calculation for maximum temperature
#' @description the attribution calculation request the attrdl function and
#' DNLM package
#' @param attrdl_path is the path to the attrdl function.
#' @return attrdl function.

load_attrdl_function <- function(attrdl_path) {
  # Read attrdl function
  source(attrdl_path)
}

########### attributable risk for extreme temperature #########################
#'
#' @description
#'

attribution_calculation <- function(model_path, data) {
  # Load best-fitting model with climate DLNMs
  load(model_path)
  model <- model0  # Ensure model0 is loaded correctly

  # Extract full coefficients and covariance matrix
  coef <- model$summary.fixed$mean
  vcov <- model$misc$lincomb.derived.covariance.matrix

  # Find the position of terms associated with the temperature crossbasis
  indt <- grep("basis_tmax", model$names.fixed)

  # Validate model data and inputs
  if (is.null(coef) || is.null(vcov)) stop("Model coefficients or covariance matrix are missing.")
  if (length(indt) == 0) stop("No terms associated with 'basis_tmax' found in the model.")

  # Recreate the temperature crossbasis (cb) using the original structure
  nlag <- 2  # Infer the number of lags
  if (nlag == 0) stop("Lagged variables not found in the data.")

  cb <- crossbasis(
    subset(data, select = c("tmax", paste0("tmaxlag", 1:nlag))),
    argvar = list(fun = "ns", knots = equalknots(data$tmax, 2)),  # Use equalknots with reproducibility
    arglag = list(fun = "ns", knots = nlag / 2)
  )

  # Calculate center temperature for predictions
  cen <- round(mean(data$tmax, na.rm = TRUE), 0)

  # Validate temperature data
  if (is.na(cen)) stop("Center temperature (cen) calculation failed due to missing data.")

  # Define minimum and maximum temperatures from the data
  min_temp <- min(data$tmax, na.rm = TRUE)
  max_temp <- max(data$tmax, na.rm = TRUE)

  # Predict relative risks over a range of temperatures using crosspred
  temp_range <- seq(min_temp, max_temp, by = 0.1)  # Define temperature range

  # Generate predictions using the crossbasis, coefficients, and covariance matrix
  predt <- crosspred(cb, coef = coef[indt], vcov = vcov[indt, indt], model.link = "log", at = temp_range, cen = cen)

  # Find the Minimum Risk Temperature (MRT)
  mrt <- predt$predvar[which.min(predt$allRRfit)]
  mrt_ci <- quantile(predt$allRRfit, probs = c(0.025, 0.975))  # Confidence interval for MRT

  # Define the temperature range where RR > 1 (i.e., the RR exceeds 1)
  high_temp_range <- temp_range[predt$allRRfit > 1.1]

  # If the high_temp_range is empty, there are no temperatures where RR > 1
  if (length(high_temp_range) == 0) {
    return(list(MRT = mrt, MRT_CI = mrt_ci, High_Temp_Range = NULL, Attributable_Risk = NULL))
  }

  # Find the temperature bounds where RR > 1.1
  high_temp_bounds <- range(high_temp_range)  # Lower and upper bounds

  # Attributable risk calculations for temperatures where RR > 1.1
  an_risk_number_hot <- attrdl(
    data$tmax, cb, data$Diarrhea,
    coef = coef[indt], vcov = vcov[indt, indt],
    type = "an", cen = mrt, range = high_temp_bounds
  )

  an_risk_fraction_hot <- attrdl(
    data$tmax, cb, data$Diarrhea,
    coef = coef[indt], vcov = vcov[indt, indt],
    type = "af", cen = mrt, range = high_temp_bounds
  )

  # Return results as a list
  return(list(
    MRT = mrt,
    MRT_CI = mrt_ci,
    High_Temp_Range = high_temp_bounds,
    Attributable_Risk_Number = an_risk_number_hot,
    Attributable_Risk_Fraction = an_risk_fraction_hot * 100
  ))
}

#' Attributable Risk of extreme hot temperatures for each region and year
#' @description
#' @param
#' @return

attribution_risk_by_region <- function(model_path, data) {
  # Load best-fitting model with climate DLNMs
  load(model_path)
  model <- model0  # Ensure model0 is loaded correctly

  # Extract full coefficients and covariance matrix
  coef <- model$summary.fixed$mean
  vcov <- model$misc$lincomb.derived.covariance.matrix

  # Find the position of terms associated with the temperature crossbasis
  indt <- grep("basis_tmax", model$names.fixed)

  # Validate model data and inputs
  if (is.null(coef) || is.null(vcov)) stop("Model coefficients or covariance matrix are missing.")
  if (length(indt) == 0) stop("No terms associated with 'basis_tmax' found in the model.")

  # Recreate the temperature crossbasis (cb) using the original structure
  nlag <- 2  # Infer the number of lags
  if (nlag == 0) stop("Lagged variables not found in the data.")

  # Initialize results storage
  results <- data.frame(
    Region = character(),
    Year = integer(),
    MRT = numeric(),
    High_Temperature_Lower = numeric(),
    High_Temperature_Upper = numeric(),
    Attributable_Risk_Number = numeric(),
    Attributable_Risk_Fraction = numeric(),
    stringsAsFactors = FALSE
  )

  # Loop through each region and year
  for (region in unique(data$Region)) {
    for (year in unique(data$Year)) {
      # Subset data for the current region and year
      data_region_year <- subset(data, Region == region & Year == year)

      # Skip if insufficient data for the region and year
      if (nrow(data_region_year) < 10) next

      # Recreate the temperature crossbasis for the region and year
      cb_region_year <- crossbasis(
        subset(data_region_year, select = c("tmax", paste0("tmaxlag", 1:nlag))),
        argvar = list(fun = "ns", knots = equalknots(data_region_year$tmax, 2)),
        arglag = list(fun = "ns", knots = nlag / 2)
      )

      # Calculate center temperature for predictions
      cen <- round(mean(data_region_year$tmax, na.rm = TRUE), 0)

      # Validate temperature data
      if (is.na(cen)) next

      # Define minimum and maximum temperatures from the data
      min_temp <- min(data_region_year$tmax, na.rm = TRUE)
      max_temp <- max(data_region_year$tmax, na.rm = TRUE)
      temp_range <- seq(min_temp, max_temp, by = 0.1)

      # Generate predictions using the crossbasis, coefficients, and covariance matrix
      predt <- crosspred(cb_region_year, coef = coef[indt], vcov = vcov[indt, indt], model.link = "log", at = temp_range, cen = cen)

      # Find the Minimum Risk Temperature (MRT)
      mrt <- predt$predvar[which.min(predt$allRRfit)]

      # Define the temperature range where RR > 1.1 (i.e., the RR exceeds 1.1)
      high_temp_range <- temp_range[predt$allRRfit > 1.1]

      # If the high_temp_range is empty, there are no temperatures where RR > 1.1
      if (length(high_temp_range) == 0) {
        next
      }

      # Find the temperature bounds where RR > 1
      high_temp_bounds <- range(high_temp_range)  # Lower and upper bounds

      # Attributable risk calculations for temperatures where RR > 1.1
      an_risk_number_hot <- attrdl(
        data_region_year$tmax, cb_region_year, data_region_year$Diarrhea,
        coef = coef[indt], vcov = vcov[indt, indt],
        type = "an", cen = mrt, range = high_temp_bounds
      )

      an_risk_fraction_hot <- attrdl(
        data_region_year$tmax, cb_region_year, data_region_year$Diarrhea,
        coef = coef[indt], vcov = vcov[indt, indt],
        type = "af", cen = mrt, range = high_temp_bounds
      )

      # Store results for the current region and year
      results <- rbind(results, data.frame(
        Region = region,
        Year = year,
        MRT = round(mrt, 2),
        High_Temperature_Lower = round(high_temp_bounds[1], 2),
        High_Temperature_Upper = round(high_temp_bounds[2], 2),
        Attributable_Risk_Number = round(an_risk_number_hot),
        Attributable_Risk_Fraction = round(an_risk_fraction_hot * 100, 2)
      ))
    }
  }

  return(results)
}

#' attribution calculation by district
#' @description
#' @param
#'
#'


attribution_calculation_by_district <- function(model_path, data) {
  # Load best-fitting model with climate DLNMs
  load(model_path)
  model <- model0  # Ensure model0 is loaded correctly

  # Extract full coefficients and covariance matrix
  coef <- model$summary.fixed$mean
  vcov <- model$misc$lincomb.derived.covariance.matrix

  # Find the position of terms associated with the temperature crossbasis
  indt <- grep("basis_tmax", model$names.fixed)

  # Validate model data and inputs
  if (is.null(coef) || is.null(vcov)) stop("Model coefficients or covariance matrix are missing.")
  if (length(indt) == 0) stop("No terms associated with 'basis_tmax' found in the model.")

  # Recreate the temperature crossbasis (cb) using the original structure
  nlag <- 2  # Infer the number of lags
  if (nlag == 0) stop("Lagged variables not found in the data.")

  # Initialize results storage
  results <- data.frame(
    Region = character(),
    District = character(),
    Year = integer(),
    MRT = numeric(),
    High_Temperature_Lower = numeric(),
    High_Temperature_Upper = numeric(),
    Attributable_Risk_Number = numeric(),
    Attributable_Risk_Fraction = numeric(),
    stringsAsFactors = FALSE
  )

  # Loop through each region, district, and year
  for (region in unique(data$Region)) {
    for (district in unique(data$District)) {
      for (year in unique(data$Year)) {
        # Subset data for the current region, district, and year
        data_group <- subset(data, Region == region & District == district & Year == year)

        # Skip if insufficient data for the group
        if (nrow(data_group) < 10) next

        # Recreate the temperature crossbasis for the group
        cb_group <- crossbasis(
          subset(data_group, select = c("tmax", paste0("tmaxlag", 1:nlag))),
          argvar = list(fun = "ns", knots = equalknots(data_group$tmax, 2)),
          arglag = list(fun = "ns", knots = nlag / 2)
        )

        # Calculate center temperature for predictions
        cen <- round(mean(data_group$tmax, na.rm = TRUE), 0)

        # Validate temperature data
        if (is.na(cen)) next

        # Define minimum and maximum temperatures from the data
        min_temp <- min(data_group$tmax, na.rm = TRUE)
        max_temp <- max(data_group$tmax, na.rm = TRUE)
        temp_range <- seq(min_temp, max_temp, by = 0.1)

        # Generate predictions using the crossbasis, coefficients, and covariance matrix
        predt <- crosspred(cb_group, coef = coef[indt], vcov = vcov[indt, indt], model.link = "log", at = temp_range, cen = cen)

        # Find the Minimum Risk Temperature (MRT)
        mrt <- predt$predvar[which.min(predt$allRRfit)]

        # Define the temperature range where RR > 1.1 (i.e., the RR exceeds 1.1)
        high_temp_range <- temp_range[predt$allRRfit > 1.1]

        # If the high_temp_range is empty, there are no temperatures where RR > 1.1
        if (length(high_temp_range) == 0) {
          next
        }

        # Find the temperature bounds where RR > 1
        high_temp_bounds <- range(high_temp_range)  # Lower and upper bounds

        # Attributable risk calculations for temperatures where RR > 1.1
        an_risk_number_hot <- attrdl(
          data_group$tmax, cb_group, data_group$Diarrhea,
          coef = coef[indt], vcov = vcov[indt, indt],
          type = "an", cen = mrt, range = high_temp_bounds
        )

        an_risk_fraction_hot <- attrdl(
          data_group$tmax, cb_group, data_group$Diarrhea,
          coef = coef[indt], vcov = vcov[indt, indt],
          type = "af", cen = mrt, range = high_temp_bounds
        )

        # Store results for the current group
        results <- rbind(results, data.frame(
          Region = region,
          District = district,
          Year = year,
          MRT = round(mrt, 2),
          High_Temperature_Lower = round(high_temp_bounds[1], 2),
          High_Temperature_Upper = round(high_temp_bounds[2], 2),
          Attributable_Risk_Number = round(an_risk_number_hot),
          Attributable_Risk_Fraction = round(an_risk_fraction_hot * 100, 2)
        ))
      }
    }
  }

  return(results)
}

###########  attribution risk rainfall ###################

# attribution risk rainfall
#' @description
#' A short description...
#' @param
#'

attribution_calculation <- function(model_path, data) {
  # Load best-fitting model with climate DLNMs
  load(model_path)
  model <- model0  # Ensure model0 is loaded correctly

  # Extract full coefficients and covariance matrix
  coef <- model$summary.fixed$mean
  vcov <- model$misc$lincomb.derived.covariance.matrix

  # Find the position of terms associated with the rainfall crossbasis
  indr <- grep("basis_rainfall", model$names.fixed)

  # Validate model data and inputs
  if (is.null(coef) || is.null(vcov)) stop("Model coefficients or covariance matrix are missing.")
  if (length(indr) == 0) stop("No terms associated with 'basis_rainfall' found in the model.")

  # Recreate the rainfall crossbasis (cb) using the original structure
  nlag <- 2  # Infer the number of lags
  if (nlag == 0) stop("Lagged variables not found in the data.")

  cb <- crossbasis(
    subset(data, select = c("rainfall", paste0("rainfalllag", 1:nlag))),
    argvar = list(fun = "ns", knots = equalknots(data$rainfall, 2)),  # Use equalknots with reproducibility
    arglag = list(fun = "ns", knots = nlag / 2)
  )

  # Calculate center rainfall for predictions
  cen <- round(mean(data$rainfall, na.rm = TRUE), 0)

  # Validate rainfall data
  if (is.na(cen)) stop("Center rainfall (cen) calculation failed due to missing data.")

  # Define minimum and maximum rainfall from the data
  min_rain <- min(data$rainfall, na.rm = TRUE)
  max_rain <- max(data$rainfall, na.rm = TRUE)

  # Predict relative risks over a range of rainfall values using crosspred
  rain_range <- seq(min_rain, max_rain, by = 0.1)  # Define rainfall range

  # Generate predictions using the crossbasis, coefficients, and covariance matrix
  predr <- crosspred(cb, coef = coef[indr], vcov = vcov[indr, indr], model.link = "log", at = rain_range, cen = cen)

  # Find the Minimum Risk Rainfall (MRR)
  mrr <- predr$predvar[which.min(predr$allRRfit)]
  mrr_ci <- quantile(predr$allRRfit, probs = c(0.025, 0.975))  # Confidence interval for MRR

  # Define the rainfall range where RR > 1 (i.e., the RR exceeds 1)
  high_rain_range <- rain_range[predr$allRRfit > 1.1]

  # If the high_rain_range is empty, there are no rainfall levels where RR > 1
  if (length(high_rain_range) == 0) {
    return(list(MRR = mrr, MRR_CI = mrr_ci, High_Rain_Range = NULL, Attributable_Risk = NULL))
  }

  # Find the rainfall bounds where RR > 1.1
  high_rain_bounds <- range(high_rain_range)  # Lower and upper bounds

  # Attributable risk calculations for rainfall levels where RR > 1.1
  an_risk_number_wet <- attrdl(
    data$rainfall, cb, data$Diarrhea,
    coef = coef[indr], vcov = vcov[indr, indr],
    type = "an", cen = mrr, range = high_rain_bounds
  )

  an_risk_fraction_wet <- attrdl(
    data$rainfall, cb, data$Diarrhea,
    coef = coef[indr], vcov = vcov[indr, indr],
    type = "af", cen = mrr, range = high_rain_bounds
  )

  # Return results as a list
  return(list(
    MRR = mrr,
    MRR_CI = mrr_ci,
    High_Rain_Range = high_rain_bounds,
    Attributable_Risk_Number = an_risk_number_wet,
    Attributable_Risk_Fraction = an_risk_fraction_wet * 100
  ))
}

#'
#' @description
#' A short description...
#' @param

attribution_risk_by_region <- function(model_path, data) {
  # Load best-fitting model with climate DLNMs
  load(model_path)
  model <- model0  # Ensure model0 is loaded correctly

  # Extract full coefficients and covariance matrix
  coef <- model$summary.fixed$mean
  vcov <- model$misc$lincomb.derived.covariance.matrix

  # Find the position of terms associated with the rainfall crossbasis
  indr <- grep("basis_rainfall", model$names.fixed)

  # Validate model data and inputs
  if (is.null(coef) || is.null(vcov)) stop("Model coefficients or covariance matrix are missing.")
  if (length(indr) == 0) stop("No terms associated with 'basis_rainfall' found in the model.")

  # Recreate the rainfall crossbasis (cb) using the original structure
  nlag <- 2  # Infer the number of lags
  if (nlag == 0) stop("Lagged variables not found in the data.")

  # Initialize results storage
  results <- data.frame(
    Region = character(),
    Year = integer(),
    MRR = numeric(),
    High_Rainfall_Lower = numeric(),
    High_Rainfall_Upper = numeric(),
    Attributable_Risk_Number = numeric(),
    Attributable_Risk_Fraction = numeric(),
    stringsAsFactors = FALSE
  )

  # Loop through each region and year
  for (region in unique(data$Region)) {
    for (year in unique(data$Year)) {
      # Subset data for the current region and year
      data_region_year <- subset(data, Region == region & Year == year)

      # Skip if insufficient data for the region and year
      if (nrow(data_region_year) < 10) next

      # Recreate the rainfall crossbasis for the region and year
      cb_region_year <- crossbasis(
        subset(data_region_year, select = c("rainfall", paste0("rainfalllag", 1:nlag))),
        argvar = list(fun = "ns", knots = equalknots(data_region_year$rainfall, 2)),
        arglag = list(fun = "ns", knots = nlag / 2)
      )

      # Calculate center rainfall for predictions
      cen <- round(mean(data_region_year$rainfall, na.rm = TRUE), 0)

      # Validate rainfall data
      if (is.na(cen)) next

      # Define minimum and maximum rainfall from the data
      min_rain <- min(data_region_year$rainfall, na.rm = TRUE)
      max_rain <- max(data_region_year$rainfall, na.rm = TRUE)
      rain_range <- seq(min_rain, max_rain, by = 0.1)

      # Generate predictions using the crossbasis, coefficients, and covariance matrix
      predr <- crosspred(cb_region_year, coef = coef[indr], vcov = vcov[indr, indr], model.link = "log", at = rain_range, cen = cen)

      # Find the Minimum Risk Rainfall (MRR)
      mrr <- predr$predvar[which.min(predr$allRRfit)]

      # Define the rainfall range where RR > 1.1 (i.e., the RR exceeds 1.1)
      high_rain_range <- rain_range[predr$allRRfit > 1.1]

      # If the high_rain_range is empty, there are no rainfall levels where RR > 1.1
      if (length(high_rain_range) == 0) {
        next
      }

      # Find the rainfall bounds where RR > 1
      high_rain_bounds <- range(high_rain_range)  # Lower and upper bounds

      # Attributable risk calculations for rainfall levels where RR > 1.1
      an_risk_number_wet <- attrdl(
        data_region_year$rainfall, cb_region_year, data_region_year$Diarrhea,
        coef = coef[indr], vcov = vcov[indr, indr],
        type = "an", cen = mrr, range = high_rain_bounds
      )

      an_risk_fraction_wet <- attrdl(
        data_region_year$rainfall, cb_region_year, data_region_year$Diarrhea,
        coef = coef[indr], vcov = vcov[indr, indr],
        type = "af", cen = mrr, range = high_rain_bounds
      )

      # Store results for the current region and year
      results <- rbind(results, data.frame(
        Region = region,
        Year = year,
        MRR = round(mrr, 2),
        High_Rainfall_Lower = round(high_rain_bounds[1], 2),
        High_Rainfall_Upper = round(high_rain_bounds[2], 2),
        Attributable_Risk_Number = round(an_risk_number_wet),
        Attributable_Risk_Fraction = round(an_risk_fraction_wet * 100, 2)
      ))
    }
  }

  return(results)
}

#'
#' @description
#' A short description...
#' @param
#'
#' @return
#'
attribution_risk_by_district <- function(model_path, data) {
  # Load best-fitting model with climate DLNMs
  load(model_path)
  model <- model0  # Ensure model0 is loaded correctly

  # Extract full coefficients and covariance matrix
  coef <- model$summary.fixed$mean
  vcov <- model$misc$lincomb.derived.covariance.matrix

  # Find the position of terms associated with the rainfall crossbasis
  indt <- grep("basis_rain", model$names.fixed)

  # Validate model data and inputs
  if (is.null(coef) || is.null(vcov)) stop("Model coefficients or covariance matrix are missing.")
  if (length(indt) == 0) stop("No terms associated with 'basis_rain' found in the model.")

  # Recreate the rainfall crossbasis (cb) using the original structure
  nlag <- 2  # Infer the number of lags
  if (nlag == 0) stop("Lagged variables not found in the data.")

  # Initialize results storage
  results <- data.frame(
    Region = character(),
    District = character(),
    Year = integer(),
    MRT = numeric(),
    High_Rainfall_Lower = numeric(),
    High_Rainfall_Upper = numeric(),
    Attributable_Risk_Number = numeric(),
    Attributable_Risk_Fraction = numeric(),
    stringsAsFactors = FALSE
  )

  # Loop through each region, district, and year
  for (region in unique(data$Region)) {
    for (district in unique(data$District)) {
      for (year in unique(data$Year)) {
        # Subset data for the current region, district, and year
        data_group <- subset(data, Region == region & District == district & Year == year)

        # Skip if insufficient data for the group
        if (nrow(data_group) < 10) next

        # Recreate the rainfall crossbasis for the group
        cb_group <- crossbasis(
          subset(data_group, select = c("rainfall", paste0("rlag", 1:nlag))),
          argvar = list(fun = "ns", knots = equalknots(data_group$rainfall, 2)),
          arglag = list(fun = "ns", knots = nlag / 2)
        )

        # Calculate center rainfall for predictions
        cen <- round(mean(data_group$rainfall, na.rm = TRUE), 0)

        # Validate rainfall data
        if (is.na(cen)) next

        # Define minimum and maximum rainfall from the data
        min_rain <- min(data_group$rainfall, na.rm = TRUE)
        max_rain <- max(data_group$rainfall, na.rm = TRUE)
        rain_range <- seq(min_rain, max_rain, by = 0.1)

        # Generate predictions using the crossbasis, coefficients, and covariance matrix
        predt <- crosspred(cb_group, coef = coef[indt], vcov = vcov[indt, indt],
                           model.link = "log", at = rain_range, cen = cen)

        # Find the Minimum Risk Rainfall (MRR)
        mrr <- predt$predvar[which.min(predt$allRRfit)]

        # Define the rainfall range where RR > 1.1 (i.e., the RR exceeds 1.1)
        high_rain_range <- rain_range[predt$allRRfit > 1.1]

        # If the high_rain_range is empty, there are no rainfall levels where RR > 1.1
        if (length(high_rain_range) == 0) {
          next
        }

        # Find the rainfall bounds where RR > 1
        high_rain_bounds <- range(high_rain_range)  # Lower and upper bounds

        # Attributable risk calculations for rainfall where RR > 1.1
        an_risk_number_wet <- attrdl(
          data_group$rainfall, cb_group, data_group$Diarrhea,
          coef = coef[indt], vcov = vcov[indt, indt],
          type = "an", cen = mrr, range = high_rain_bounds
        )

        an_risk_fraction_wet <- attrdl(
          data_group$rainfall, cb_group, data_group$Diarrhea,
          coef = coef[indt], vcov = vcov[indt, indt],
          type = "af", cen = mrr, range = high_rain_bounds
        )

        # Store results for the current group
        results <- rbind(results, data.frame(
          Region = region,
          District = district,
          Year = year,
          MRT = round(mrr, 2),
          High_Rainfall_Lower = round(high_rain_bounds[1], 2),
          High_Rainfall_Upper = round(high_rain_bounds[2], 2),
          Attributable_Risk_Number = round(an_risk_number_wet),
          Attributable_Risk_Fraction = round(an_risk_fraction_wet * 100, 2)
        ))
      }
    }
  }

  return(results)
}
