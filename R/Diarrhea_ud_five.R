
# Read in and format data: R-code for Diarrheal disease cases attributable to extreme precipitation and extreme temperature


##Required_package

# @description: Read in the requested package for the indicator calculation including INLA package for the spatiotemporal modeling, and dlnm package for the distributed lag nonlinear modeling.

load_required_packages <- function() {
  # Clear the environment
  rm(list = ls())

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




##load data
# @description: the load_and_process_data() function loads, processes, and prepares spatial and health data for further analysis.
# @param map_path File path to the shapefile (.shp) containing geographical boundary data for regions and districts.
# @param data_path File path to the rds or CSV file containing monthly recorded health and climate data.
# @param graph_output File path for the adjacency matrix, used in spatial analysis.
# @param map file must contain a column of region name, district name, and a column of geometry points. Make sure to rename columns to match the expected format (e.g. rename(Region = ADM1_EN, District = ADM2_EN))
# @param nb.map define the adjacency matrix for the spatial analysis.
# @param data file must contain a column of region name, district name, Date, Year, Month, Diarrhea Monthly record, climate variables (tmin, tmean, tmax, rainfall, rhumidity), the lag variables for each climate variables, and the Population data column.
# @param time Creates a time index by converting Year and Month into a continuous numeric variable.
# @param grid_data call the region and district column from the data file, and used them to create region code and district code. Merge the region code and district code with the climate and health data and the map data.
# @returns A list containing map Processed spatial data with region and district codes, data Health and climate dataset with corresponding codes, grid_data Lookup table containing region and district codes, nb.map the adjacency matrix, and the summary statistics

load_and_process_data <- function(map_path = ".shp",
                                  data_path = "data_path",
                                  graph_output = "storname/map.graph") {


  # Load and process map data
  map <- read_sf(map_path) %>%
    rename(Region = ADM1_EN, District = ADM2_EN)

  # Create adjacency matrix
  nb.map <- poly2nb(as_Spatial(map$geometry))
  if (!file.exists(graph_output)) {
    nb2INLA(graph_output, nb.map)
  }

  # Load health and climate data
  data <- read_rds(data_path) %>%
    group_by(Region, District) %>%
    unique() %>%
    mutate(time = (Year - min(data$Year, na.rm = TRUE)) * 12 + Month) %>%
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

  # Print summary statistics for climate variables
  cat("Summary statistics for climate variables:\n")
  summary_stats <- list(
    tmin = summary(data$tmin),
    tmax = summary(data$tmax),
    rainfall = summary(data$rainfall),
    rhumidity = summary(data$rhumidity)
  )

  # Return processed data as a list
  return(list(map = map,  nb.map = nb.map, data = data, grid_data = grid_data, summary = summary_stats))
}


##set key variables
# @description: Read in key variables needed to set the INLA and DLNM model.
# @param data is the climate and health data previously prepared.
# @param first_year is the first year in the dataset
# @param nlag define the maximum number of lag month to be included in the DLNM model
# @param ntime define the total number of months in the dataset
# @param nyear the total number of year in the dataset
# @param ndistrict the total number of unique districts in the dataset
# @param nregion the total number of unique regions in the dataset
# @returns A list of the defined key variables

set_variables <- function(data, nlag = 2) {
  # Calculate key variables
  first_year <- min(data$Year, na.rm = TRUE) # the fist year in the dataset
  ntime <- length(unique(data$time))       # Total number of months
  nyear <- length(unique(data$Year))       # Total number of years
  ndistrict <- length(unique(data$district_code))  # Total number of districts
  nregion <- length(unique(data$region_code))  # Total number of regions

  # Return results as a list
  return(list(
    first_year = first_year
    nlag = nlag,
    ntime = ntime,
    nyear = nyear,
    ndistrict = ndistrict,
    nregion = nregion
  ))
}


##set cross-basis matrix for DLNM
# @description: The set_cross_basis() function constructs cross-basis matrices for use in a Distributed Lag Non-Linear Model (DLNM), capturing the delayed effects of climate variables (temperature and rainfall) on health outcomes.
# @param data a dataset containing temperature (tmax, tmin, tmean) and rainfall variables.
# @param nlag the maximum number of lagged months to consider in the analysis.
# @param lagknot defines knot locations for the lag structure using equally spaced knots over the lag range [0, nlag]
# @param var_tmax Extracts tmax and its lagged versions (e.g., lag1, lag2, ..., lagN) from the dataset
# @param basis_tmax is the crossbasis() matrix which capture both: Exposure-response relationship (argvar) Models how temperature or rainfall affects the outcome, and the Lag-response relationship (arglag) Models how past exposure (lagged values) influences the outcome.
# @param argvar uses a natural spline (ns) function with two knots to model the nonlinear relationship between temperature and health outcomes.
# @param uses a natural spline (ns) function to model the impact of past temperature values over time
# The cross_basis matrix were defined for each climate variables in the dataset.
# @returns a list of the defined cross_basis matrix for minimum, mean, and maximum temperature, and rainfall.


set_cross_basis <- function(data, nlag) {
  library(dlnm)  # Ensure the required package is loaded

  # Define lag knots
  lagknot <- equalknots(0:nlag, 2)

  # Tmax
  var_tmax <- subset(data, select = c("tmax", paste0("lag", 1:nlag)))
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

## create indices for INLA models
# @description: for the INLA model, there is a need to set-up regions index, district index, and year index. This function create these indices using the dataset, ndistrict and nregion defined above.
# @param data is the dataset containing district_code, region_code, and Year columns.
# @param ndistrict is the total number of unique districts in the dataset.
# @param nregion is the total number of unique regions in the dataset.
# @param district_index is district index that repeats numbers from 1 to ndistrict to match the total number of rows in data. Ensures that the index starts at 1 and follows INLA’s requirement.
# @param unique_districts extracts unique district codes and stores them in unique_districts, loops over each district and assigns a unique index.
# @param region_index is the initialized region index
# @param unique_regions extracts unique region codes and stores them in unique_regions, loops over each region and assigns a unique index.
# @param year_index converts the calendar year into an index.
# @returns the modified data with the new indices

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
  data$year_index <- data$Year - (first_year-1)

  return(data)
}


## Setup priors for INLA model
# @description: setup_inla_priors(data) function prepares the dataset for INLA modeling by defining response variables, computing offsets, and setting up random effects.
# @param Y extracts the dependent variable (number of diarrhea cases) from the dataset.
# @param N counts the total number of Diarrhea observations in the dataset.
# @param E is the expected cases and computes an expected number of diarrhea cases using the total population (pop_tot), ensuring the response variable is modeled as an incidence rate per 100,000 people.
# @param SIR is the Standardized Incidence Ratio and calculates the ratio of observed cases (Diarrhea) to expected cases (E), which helps assess disease risk in different regions.
# @random effect variables:T1, T2, S1, S2
# @param T1 is the Seasonal effect (monthly variation)
# @param T2 is the Inter-annual variability (yearly trends)
# @param S1 is the District-level spatial random effect
# @param S2 is the Region-level spatial-temporal interaction
# @param df is a dataframe created for the model testing. it includes the following selected variables: Y, E, T1, T2, S1, S2
# @param precision.prior specifies prior distributions for model parameters.
# @param pc.prec is a penalized complexity prior, with parameters 0.5 and 0.01 controlling the precision level.
# @returns a list of the prepared dataset (df)


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

  # Create dataframe for model testing
  df <- data.frame(Y, E, T1, T2, S1, S2)

  # Define priors for INLA model
  precision.prior <- list(prec = list(prior = "pc.prec", param = c(0.5, 0.01)))

  return(list(df = df, precision_prior = precision.prior))
}


## Function to fit an INLA model
# @description fit_inla_model() function fits an INLA (Integrated Nested Laplace Approximation) model using the prepared dataset
# @param formula is a model defining predictors and random effects.
# @param data dataset prepared by setup_inla_data() (df)
# @param family The probability distribution for the response variable (default: "poisson", it can also be "nbinomial")
# @param config is a Boolean flag to enable additional model configurations.
# @param model is the fited model
# @param offset incorporates expected cases as an offset, ensuring the model represents incidence rates.
# @param control.inla controls how INLA approximates the posterior distribution.
# @param control.compute for model fit statistics. dic = TRUE: Computes Deviance Information Criterion (DIC), a model selection metric. cpo = TRUE: Computes Conditional Predictive Ordinate (CPO), useful for model comparison.
# @param control.fixed for Fixed Effects Settings.
# @param control.predictor for predictor control.
# @return the fitted INLA model

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











# run models of increasing complexity in INLA

# Step 1: Fit a baseline model including spatiotemporal random effects
# Base model includes:
# - State-specific monthly random effects (random walk cyclic prior)
# - Year-specific spatial random effects (BYM2 prior)

# Define baseline formula
baseformula <- Y ~ 1 +
  f(T1, replicate = S2, model = "rw2", cyclic = TRUE, constr = TRUE,
    scale.model = TRUE, hyper = precision.prior) +
  f(S1, model = "bym2", replicate = T2, graph = "output4/map.graph",
    scale.model = TRUE, hyper = precision.prior)

# test baseline model with Negative Binomial distribution
# model <- mymodel(baseformula, family = "nbinomial")
# model$dic$dic

# Define formulas by adding Tmax and rainfall cross-basis functions
formula0.1 <- update.formula(baseformula, ~ . + basis_tmax)
formula0.2 <- update.formula(baseformula, ~ . + basis_tprainfall)
formula0.3 <- update.formula(baseformula, ~ . + basis_tmax + basis_tprainfall)

# Create a list of formulas and corresponding labels
formulas <- list(baseformula, formula0.1, formula0.2, formula0.3)
lab <- c("basemodel", "model0.1", "model0.2", "model0.3")

# Fit models and save results
models <- lapply(seq_along(formulas), function(i) {
  cat("Fitting model:", lab[i], "\n")  # Progress message
  model <- mymodel(formulas[[i]], df)
  save(model, file = file.path("output4", paste0(lab[i], ".RData")))
  return(model)
})

# Create a table to store DIC values
table0 <- data.table(Model = c("base", "tmax", "rainfall", "tmax + rainfall"), DIC = NA)

# Populate DIC values from saved models
for (i in seq_along(lab)) {
  file_path <- file.path("output4", paste0(lab[i], ".RData"))
  if (file.exists(file_path)) {
    load(file_path)
    if (!is.null(model$dic$dic)) {
      table0$DIC[i] <- round(model$dic$dic, 0)
    } else {
      cat("Warning: DIC not found for", lab[i], "\n")
    }
  } else {
    cat("Warning: File not found for", lab[i], "\n")
  }
}

# View the resulting DIC table
print(table0)
# define position of best fitting model
best.fit <- which.min(table0$DIC)

# assign formula for best fitting model to the new baseformula
#baseformula <- formulas[[best.fit]]

# redefine baseformula as best fitting model from above
baseformula <- Y ~ 1 + f(T1, replicate = S2, model = "rw2", cyclic = TRUE, constr = TRUE,
                         scale.model = TRUE,  hyper = precision.prior) +
  f(S1, model = "bym2", replicate = T2, graph = "output4/map.graph",
    scale.model = TRUE, hyper = precision.prior) + basis_tmax + basis_tprainfall

# Step 1: compare models using goodness of fit statistics

# create a table to store model adequacy results (DIC, CV log score, MAE difference results)
# create model label string
mod.name <- c("basemodel", "model0.1", "model0.2", "model0.3")

table1 <- data.table(Model = mod.name,
                     DIC = NA,
                     logscore = NA)

# create loop to read in model object and extract DIC and CV log score
for (i in 1:length(mod.name))
{
  load(paste0("Output4/", mod.name[i],".RData"))
  # add model fit statistics from each model to table
  table1$DIC[i] <- round(model$dic$dic,0)
  table1$logscore[i] <- round(-mean(log(model$cpo$cpo), na.rm = T), 3)
}

# save model adequacy results for all models (Appendix Table S1)
fwrite(table1, file = "figs4/table_S01.csv", quote = FALSE,
       row.names = FALSE)

# load baseline and selected model
load("output4/basemodel.RData")
basemodel <- model
load("output4/model0.3.RData")



# Visualise random effects for selected model

# explore spatial and temporal random effects

# plot monthly random effects per state (Appendix Fig S7)
month_effects <- data.frame(Province_code = rep(unique(data$Province_code), each = 12),
                            Month = model$summary.random$T1)

# plot monthly random effects per province
month_effects <- month_effects %>%
  # add the predefined state grid by state code
  left_join(grid_data %>% select(-District_name, -District_code) %>% unique,
            by = c("Province_code" = "code_num"))

month_effects <- map %>% select(-District) %>% unique() %>% left_join(month_effects, by = c("Province" = "name"))

month_effects %>%
  ggplot() +
  geom_ribbon(aes(x = Month.ID, ymin = `Month.0.025quant`, ymax = `Month.0.975quant`),
              fill = "cadetblue4", alpha = 0.5) +
  geom_line(aes(x = Month.ID, y = Month.mean), col = "cadetblue4") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
  xlab("Month") +
  ylab("Contribution to log(Diarrhe_cases)") +
  scale_y_continuous() +
  scale_x_continuous(breaks = c(1,4,7,10), labels = c("Jan", "Apr", "Jul", "Oct")) +
  theme_bw() +
  # organise by Province name in grid file
  facet_wrap(~Province)
# facet_geo( ~name, grid = grid_data)

ggsave("figs4/fig_S06_month_effect.pdf", height = 30, width = 25, units = "cm")

#DNLM result

# Country level
# load model output

# Load best fitting model with climate DLNMs
load("output4/model0.3.RData")
model0 <- model

# Step 1: plot tmax output
# Select the climate best model
model <- model0

# Extract full coef and vcov and create indicators for each term
coef <- model$summary.fixed$mean
vcov <- model$misc$lincomb.derived.covariance.matrix

# Find position of the terms associated with tmax crossbasis
indt <- grep("basis_tmax", model$names.fixed)

# Extract predictions from the tmax DLNM centred on overall mean Tmax (26 deg C)
predt <- crosspred(basis_tmax, coef = coef[indt], vcov = vcov[indt, indt],
                   model.link = "log", bylag = 0.25, cen = round(mean(data$temp, na.rm = TRUE), 0))

# Contour and scenario plots for tmax (Main text Fig 3)
# Contour plot of exposure-lag-response associations (Main text Fig 3a)
pdf("figs4/fig_03a_tmax_contour.pdf", width = 6.5, height = 6)

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
  main = "Diarrhea Under Five",
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

## Relative Risk plot by Temperature Range Using allRRfit
# Output plot as a high-resolution PNG
png("figs4/fig_relative_risk_temperature23.png", width = 800, height = 800, res = 150)

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

### Relative Risk plot by Temperature Range Using matRRfit
# Output plot as a high-resolution PNG
png("figs4/fig_relative_risk_temperature.png", width = 800, height = 800, res = 150)

# Get exposure values
vars <- predt$predvar

# Ensure exposure values and RR data are finite
rr <- predt$matRRfit
rr.lci <- predt$matRRlow
rr.uci <- predt$matRRhigh

# Ensure there are no NA values in the data
if (anyNA(c(vars, rr, rr.lci, rr.uci))) {
  stop("Missing values found in input data. Check 'predt' object.")
}

# Set colors for different temperature ranges
col <- colorRampPalette(brewer.pal(11, "RdBu"))(length(vars))

# Set y-axis limits dynamically based on RR range
r1 <- min(c(rr, rr.lci, rr.uci), na.rm = TRUE)
r2 <- max(c(rr, rr.lci, rr.uci), na.rm = TRUE)

# Plot Relative Risk by Temperature Range
plot(vars, rr[, 1], type = "l", col = col[1], lwd = 2,
     xlab = "Temperature (°C)", ylab = "Relative Risk",
     main = "Diarrhea Under Five Relative Risk by Temperature Range",
     ylim = c(r1, r2 * 1.1), frame.plot = TRUE, axes = TRUE)

polygon(c(vars, rev(vars)),
        c(rr.lci[, 1], rev(rr.uci[, 1])),
        col = adjustcolor(col[1], alpha.f = 0.3), border = NA)

# Add horizontal reference line at RR = 1
abline(h = 1, lty = 2, col = "gray")

# Add a legend for temperature ranges
legend("topright",
       legend = c("Relative Risk by Temperature Range"),
       col = col[1], lwd = 2, lty = 1, bty = "n")

# Save and close the PNG device
dev.off()

#temp scenario (save plot as image)

## Lag Response Plot for Different Tmax Scenarios

# Output plot as a high-resolution PNG
png("figs4/fig_03b_tmax_scenario.png", width = 800, height = 800, res = 150)

# Get exposure values
vars <- predt$predvar

# Ensure exposure values and RR data are finite
rr <- predt$matRRfit
rr.lci <- predt$matRRlow
rr.uci <- predt$matRRhigh
lagbylag <- seq(0, ncol(rr) - 1) * 0.25

# Ensure there are no NA values in the data
if (anyNA(c(vars, rr, rr.lci, rr.uci))) {
  stop("Missing values found in input data. Check 'predt' object.")
}

# Get indices for selected temperature scenarios
mn <- which.min(abs(vars - 20))  # Cool scenario (20°C)
mx <- which.min(abs(vars - 25))  # Warm scenario (25°C)
mx2 <- which.min(abs(vars - 30)) # Warmest scenario (30°C)

# Define colors and transparency for scenarios
col1 <- brewer.pal(11, "RdBu")[9]  # Cool scenario
tcol1 <- adjustcolor(col1, alpha.f = 0.3)

col2 <- brewer.pal(11, "RdBu")[3]  # Warm scenario
tcol2 <- adjustcolor(col2, alpha.f = 0.3)

col3 <- brewer.pal(11, "RdBu")[1]  # Warmest scenario
tcol3 <- adjustcolor(col3, alpha.f = 0.3)

# Set y-axis limits dynamically based on RR range
r1 <- min(c(rr, rr.lci, rr.uci), na.rm = TRUE)
r2 <- max(c(rr, rr.lci, rr.uci), na.rm = TRUE)

# Plot the Cool scenario
plot(lagbylag, rr[mn, ], col = col1, type = "l", lwd = 2,
     xlab = "Lag (Month)", ylab = "Relative Risk",
     main = "Lag Response for Different Temperature Scenarios",
     ylim = c(r1, r2 * 1.1), frame.plot = TRUE, axes = FALSE)
axis(1, at = seq(0, max(lagbylag), 1), labels = seq(0, max(lagbylag), 1))
axis(2)
polygon(c(lagbylag, rev(lagbylag)),
        c(rr.lci[mn, ], rev(rr.uci[mn, ])),
        col = tcol1, border = NA)

# Plot the Warm scenario
lines(lagbylag, rr[mx, ], col = col2, lwd = 2)
polygon(c(lagbylag, rev(lagbylag)),
        c(rr.lci[mx, ], rev(rr.uci[mx, ])),
        col = tcol2, border = NA)

# Plot the Warmest scenario
lines(lagbylag, rr[mx2, ], col = col3, lwd = 2)
polygon(c(lagbylag, rev(lagbylag)),
        c(rr.lci[mx2, ], rev(rr.uci[mx2, ])),
        col = tcol3, border = NA)

# Add horizontal reference line at RR = 1
abline(h = 1, lty = 2, col = "gray")

# Add a legend
legend("topright",
       legend = c(paste0("Temp = ", round(vars[mn], 1), "°C"),
                  paste0("Temp = ", round(vars[mx], 1), "°C"),
                  paste0("Temp = ", round(vars[mx2], 1), "°C")),
       col = c(col1, col2, col3), lwd = 2, lty = 1, bty = "n")

# Add subplot label
mtext(side = 2, line = 3, text = "b", las = 2, cex = 1.2, adj = 0.9)

# Save and close the PNG device
dev.off()

#Province level plot

# Load model output
load("output4/model0.3.RData")
model0 <- model

# Open a single PDF file for all provinces
pdf("figs4/fig_03a_tmax_contour_all_provinces.pdf", width = 12, height = 12)

# Set up a consistent layout for the plots (1 per page)
par(mfrow = c(2,1 ))  # One plot per page

# Iterate over unique provinces
provinces <- unique(data$Province)
for (province in provinces) {
  # Filter data for the current province
  province_data <- subset(data, Province == province)

  # Set the model for the province
  model <- model0

  # Extract full coef and vcov for the province
  coef <- model$summary.fixed$mean
  vcov <- model$misc$lincomb.derived.covariance.matrix

  # Find position of terms associated with tmax crossbasis
  indt <- grep("basis_tmax", model$names.fixed)

  # Extract predictions from the tmax DLNM centered on overall mean Tmax
  mean_temp <- round(mean(province_data$temp, na.rm = TRUE), 0)
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
    main = paste("Contour Plot for", province),
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

##Relative Risk plot by Temperature Range for Each Province allRRfit

# Set up a PDF output device for all plots
pdf("figs4/fig_relative_risk_temperature_all_provinces23.pdf", width = 10, height = 10)

provinces <- unique(data$Province)
layout(matrix(1:length(provinces), nrow = ceiling(sqrt(length(provinces))))) # Arrange plots in a grid
par(mfrow = c(3, 2))
for (province in provinces) {
  # Filter data for the current province
  province_data <- subset(data, Province == province)

  # Extract predictions for the current province (assuming predt setup)
  mean_temp <- round(mean(province_data$temp, na.rm = TRUE), 0)
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
       main = paste("Diarrhea Relative Risk by Temperature for", province),
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

##Relative Risk plot by Temperature Range for Each Province matRRfit

# Set up a PDF output device for all plots
pdf("figs4/fig_relative_risk_temperature_all_provinces.pdf", width = 10, height = 10)

provinces <- unique(data$Province)
layout(matrix(1:length(provinces), nrow = ceiling(sqrt(length(provinces))))) # Arrange plots in a grid
par(mfrow = c(3, 2))
for (province in provinces) {
  # Filter data for the current province
  province_data <- subset(data, Province == province)

  # Extract predictions for the current province (assuming predt setup)
  mean_temp <- round(mean(province_data$temp, na.rm = TRUE), 0)
  predt <- crosspred(basis_tmax, coef = coef[indt], vcov = vcov[indt, indt],
                     model.link = "log", bylag = 0.25, cen = mean_temp)

  # Get exposure values
  vars <- predt$predvar

  # Ensure exposure values and RR data are finite
  rr <- predt$matRRfit
  rr.lci <- predt$matRRlow
  rr.uci <- predt$matRRhigh

  # Ensure there are no NA values in the data
  if (anyNA(c(vars, rr, rr.lci, rr.uci))) {
    stop("Missing values found in input data. Check 'predt' object.")
  }

  # Set colors for different temperature ranges
  col <- colorRampPalette(brewer.pal(11, "RdBu"))(length(vars))

  # Set y-axis limits dynamically based on RR range
  r1 <- min(c(rr, rr.lci, rr.uci), na.rm = TRUE)
  r2 <- max(c(rr, rr.lci, rr.uci), na.rm = TRUE)

  # Plot Relative Risk by Temperature Range
  plot(vars, rr[, 1], type = "l", col = col[1], lwd = 2,
       xlab = "Temperature (°C)", ylab = "Relative Risk",
       main = paste("Diarrhea Under Five Relative Risk by Temperature for", province),
       ylim = c(r1, r2 * 1.1), frame.plot = TRUE, axes = TRUE)

  polygon(c(vars, rev(vars)),
          c(rr.lci[, 1], rev(rr.uci[, 1])),
          col = adjustcolor(col[1], alpha.f = 0.3), border = NA)

  # Add horizontal reference line at RR = 1
  abline(h = 1, lty = 2, col = "gray")

  # Add a legend for temperature ranges
  legend("topright",
         legend = c("Relative Risk by Temperature Range"),
         col = col[1], lwd = 2, lty = 1, bty = "n")
}

# Close the PDF device
dev.off()

## contour Plot by district

# Load model output
load("output4/model0.3.RData")
model0 <- model

# Open a single PDF file for all districts
pdf("figs4/fig_03a_tmax_contour_all_districts.pdf", width = 12, height = 12)

# Set up a 2x2 grid for four plots per page
par(mfrow = c(2, 2))  # Two rows and two columns

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
  mean_temp <- round(mean(district_data$temp, na.rm = TRUE), 0)
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

###Relative Risk plot by Temperature Range for Each district allRRfit

# Set up a PDF output device
pdf("figs4/fig_relative_risk_temperature_all_districts23.pdf", width = 10, height = 10)

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
  mean_temp <- round(mean(district_data$temp, na.rm = TRUE), 0)

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

#rainfall Country level

# Step 1: plot rainfall output
# Select the climate model
model <- model0

# Extract full coef and vcov and create indicators for each term
coef <- model$summary.fixed$mean
vcov <- model$misc$lincomb.derived.covariance.matrix

# Find position of the terms associated with tmax crossbasis
indt <- grep("basis_tprainfall", model$names.fixed)

# Extract predictions from the rainfall DLNM
predt <- crosspred(basis_tprainfall, coef = coef[indt], vcov = vcov[indt, indt],
                   model.link = "log", bylag = 0.25, cen = round(mean(data$tprainfall, na.rm = TRUE), 0))

# Contour and scenario plots for rainfall (Main text Fig 3)
# Contour plot of exposure-lag-response associations (Main text Fig 3a)
pdf("figs4/fig_03a_rainfall_contour.pdf", width = 6.5, height = 6)

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

# Plot Relative Risk by rainfall Range Only country level

# Plot Relative Risk by rainfall Range Only

# Output plot as a high-resolution PNG
png("figs4/fig_relative_risk_rainfal.png", width = 800, height = 800, res = 150)

# Get exposure values
vars <- predt$predvar

# Ensure exposure values and RR data are finite
rr <- predt$matRRfit
rr.lci <- predt$matRRlow
rr.uci <- predt$matRRhigh

# Ensure there are no NA values in the data
if (anyNA(c(vars, rr, rr.lci, rr.uci))) {
  stop("Missing values found in input data. Check 'predt' object.")
}

# Set colors for different temperature ranges
col <- colorRampPalette(brewer.pal(11, "RdBu"))(length(vars))

# Set y-axis limits dynamically based on RR range
r1 <- min(c(rr, rr.lci, rr.uci), na.rm = TRUE)
r2 <- max(c(rr, rr.lci, rr.uci), na.rm = TRUE)

# Plot Relative Risk by Rainfall Range
plot(vars, rr[, 1], type = "l", col = col[1], lwd = 2,
     xlab = "Rainfall (mm)", ylab = "Relative Risk",
     main = "Diarrhea Under Five Relative Risk by rainfall Range",
     ylim = c(r1, r2 * 1.1), frame.plot = TRUE, axes = TRUE)

polygon(c(vars, rev(vars)),
        c(rr.lci[, 1], rev(rr.uci[, 1])),
        col = adjustcolor(col[1], alpha.f = 0.3), border = NA)

# Add horizontal reference line at RR = 1
abline(h = 1, lty = 2, col = "gray")

# Add a legend for rainfall ranges
legend("topright",
       legend = c("Relative Risk by rainfall Range"),
       col = col[1], lwd = 2, lty = 1, bty = "n")

# Save and close the PNG device
dev.off()

#Province level Rainfall
# Load model output
load("output4/model0.3.RData")
model0 <- model

# Open a single PDF file for all provinces
pdf("figs4/fig_03a_rainfall_contour.pdf", width = 6.5, height = 6)

# Set up a consistent layout for the plots (1 per page)
par(mfrow = c(2,1 ))  # One plot per page

# Iterate over unique provinces
provinces <- unique(data$Province)
for (province in provinces) {
  # Filter data for the current province
  province_data <- subset(data, Province == province)

  # Set the model for the province
  model <- model0

  # Extract full coef and vcov and create indicators for each term
  coef <- model$summary.fixed$mean
  vcov <- model$misc$lincomb.derived.covariance.matrix

  # Extract predictions for the current province (assuming predt setup)
  mean_rainfall <- round(mean(province_data$tprainfall, na.rm = TRUE), 0)
  predt <- crosspred(basis_tprainfall, coef = coef[indt], vcov = vcov[indt, indt],
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
    xlab = "Lag Month",
    ylab = expression(paste("Rainfall (", millimeter, "mm)")),
    main = paste("Contour Plot for", province),
    col = cols,
    levels = levels,
    plot.axes = {
      axis(1, at = 0:nlag, labels = 0:nlag)  # Customize x-axis
      axis(2)  # Customize y-axis
    }
  )
}
# Close PDF device
if (dev.cur() > 1) dev.off()

# Plot Relative Risk by Precipitation Range Only for Each Province in a Single PDF

# Set up a PDF output device for all plots
pdf("figs4/fig_relative_risk_rainfall_all_provinces.pdf", width = 10, height = 10)

provinces <- unique(data$Province)
layout(matrix(1:length(provinces), nrow = ceiling(sqrt(length(provinces))))) # Arrange plots in a grid
par(mfrow = c(3, 2))
for (province in provinces) {
  # Filter data for the current province
  province_data <- subset(data, Province == province)

  # Extract predictions for the current province (assuming predt setup)
  mean_rainfall <- round(mean(province_data$tprainfall, na.rm = TRUE), 0)
  predt <- crosspred(basis_tprainfall, coef = coef[indt], vcov = vcov[indt, indt],
                     model.link = "log", bylag = 0.25, cen = mean_rainfall)
  # Get exposure values
  vars <- predt$predvar

  # Ensure exposure values and RR data are finite
  rr <- predt$matRRfit
  rr.lci <- predt$matRRlow
  rr.uci <- predt$matRRhigh

  # Ensure there are no NA values in the data
  if (anyNA(c(vars, rr, rr.lci, rr.uci))) {
    stop("Missing values found in input data. Check 'predt' object.")
  }

  # Set colors for different temperature ranges
  col <- colorRampPalette(brewer.pal(11, "RdBu"))(length(vars))

  # Set y-axis limits dynamically based on RR range
  r1 <- min(c(rr, rr.lci, rr.uci), na.rm = TRUE)
  r2 <- max(c(rr, rr.lci, rr.uci), na.rm = TRUE)

  # Plot Relative Risk by Rainfall Range
  plot(vars, rr[, 1], type = "l", col = col[1], lwd = 2,
       xlab = "Rainfall (mm)", ylab = "Relative Risk",
       main = paste("Diarrhea Under Five Relative Risk by rainfall for", province),
       ylim = c(r1, r2 * 1.1), frame.plot = TRUE, axes = TRUE)

  polygon(c(vars, rev(vars)),
          c(rr.lci[, 1], rev(rr.uci[, 1])),
          col = adjustcolor(col[1], alpha.f = 0.3), border = NA)

  # Add horizontal reference line at RR = 1
  abline(h = 1, lty = 2, col = "gray")

  # Add a legend for rainfall ranges
  legend("topright",
         legend = c("Relative Risk by rainfall Range"),
         col = col[1], lwd = 2, lty = 1, bty = "n")

}
# Save and close the PNG device
dev.off()

#District level Rainfall

# Load model output
load("output4/model0.3.RData")
model0 <- model

# Open a single PDF file for all districts
pdf("figs4/fig_03a_rainfall_contour_district.pdf", width = 6.5, height = 6)

# Set up a consistent layout for the plots (1 per page)
par(mfrow = c(2,2 ))  # One plot per page

# Iterate over unique districts
districts <- unique(data$District)
for (district in districts) {
  # Filter data for the current districts
  district_data <- subset(data, District == district)

  # Set the model for the district
  model <- model0

  # Extract full coef and vcov and create indicators for each term
  coef <- model$summary.fixed$mean
  vcov <- model$misc$lincomb.derived.covariance.matrix

  # Find position of terms associated with rainfall crossbasis
  indt <- grep("basis_tprainfall", model$names.fixed)

  # Extract predictions from the rainfall DLNM centered on overall mean Tmax
  mean_rainfall <- round(mean(district_data$tprainfall, na.rm = TRUE), 0)
  predt <- crosspred(basis_tprainfall, coef = coef[indt], vcov = vcov[indt, indt],
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
    xlab = "Lag Month",
    ylab = expression(paste("Rainfall (", millimeter, "mm)")),
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
# Close PDF device
if (dev.cur() > 1) dev.off()

#District level relative risk plot

# Set up a PDF output device for all plots
pdf("figs4/fig_relative_risk_rainfall_all_districts.pdf", width = 10, height = 10)

# Get unique districts
districts <- unique(data$District)

# Initialize counters
plot_count <- 0
total_plots <- length(districts)

# Loop over each district
for (i in seq_along(districts)) {
  district <- districts[i]

  # Filter data for the current district
  district_data <- subset(data, District == district)

  # Extract predictions for the current district
  mean_rainfall <- round(mean(district_data$tprainfall, na.rm = TRUE), 0)
  predt <- crosspred(basis_tprainfall, coef = coef[indt], vcov = vcov[indt, indt],
                     model.link = "log", bylag = 0.25, cen = mean_rainfall)

  # Get exposure values
  vars <- predt$predvar

  # Ensure exposure values and RR data are finite
  rr <- predt$matRRfit
  rr.lci <- predt$matRRlow
  rr.uci <- predt$matRRhigh

  # Ensure there are no NA values in the data
  if (anyNA(c(vars, rr, rr.lci, rr.uci))) {
    stop("Missing values found in input data. Check 'predt' object.")
  }

  # Set colors for different rainfall ranges
  col <- colorRampPalette(brewer.pal(11, "RdBu"))(length(vars))

  # Set y-axis limits dynamically based on RR range
  r1 <- min(c(rr, rr.lci, rr.uci), na.rm = TRUE)
  r2 <- max(c(rr, rr.lci, rr.uci), na.rm = TRUE)

  # Set up new page for every 4 plots
  if (plot_count %% 4 == 0) {
    par(mfrow = c(2, 2))  # Set 2x2 layout for 4 plots per page
  }

  # Plot Relative Risk by Rainfall Range
  plot(vars, rr[, 1], type = "l", col = col[1], lwd = 2,
       xlab = "Rainfall (mm)", ylab = "Relative Risk",
       main = paste("Diarrhea Under Five Relative Risk by Rainfall for", district),
       ylim = c(r1, r2 * 1.1), frame.plot = TRUE, axes = TRUE)

  polygon(c(vars, rev(vars)),
          c(rr.lci[, 1], rev(rr.uci[, 1])),
          col = adjustcolor(col[1], alpha.f = 0.3), border = NA)

  # Add horizontal reference line at RR = 1
  abline(h = 1, lty = 2, col = "gray")

  # Add a legend for rainfall ranges
  legend("topright",
         legend = c("Relative Risk by Rainfall Range"),
         col = col[1], lwd = 2, lty = 1, bty = "n")

  # Increment plot counter
  plot_count <- plot_count + 1

  # Reset layout after the last plot on the current page
  if (plot_count %% 4 == 0 || i == total_plots) {
    par(mfrow = c(1, 1))  # Reset layout after every page or at the end
  }
}

# Save and close the PDF device
dev.off()

#rainfall scenarios
# Lag response for different rainfall scenarios (Main text Fig 3b)
pdf("figs4/fig_03b_rainfall_scenario.pdf", width = 6, height = 6)

# Get exposure values
vars <- predt$predvar

# Debug: Print exposure values to confirm target matches
print(vars)

# Obtain relative risk (RR) fit and upper/lower confidence limits
rr <- predt$matRRfit
rr.lci <- predt$matRRlow
rr.uci <- predt$matRRhigh

# Debug: Print dimensions
print(dim(rr))
print(dim(rr.lci))
print(dim(rr.uci))

# Adjust lagbylag to match rr dimensions
lagbylag <- seq(0, ncol(rr) - 1) * 0.25

# Debug: Print lagbylag
print(length(lagbylag))

# Get selected exposure variable positions with approximate matching
mn <- which.min(abs(vars - 200))
mx <- which.min(abs(vars - 600))
mx2 <- which.min(abs(vars - 800))

# Debug: Print indices
print(mn); print(mx); print(mx2)

# Define colors
col1 <- brewer.pal(11, "RdBu")[9]
tcol1 <- do.call(rgb, c(as.list(col2rgb(col1)), alpha = 255/4, max = 255))

col2 <- brewer.pal(11, "RdBu")[3]
tcol2 <- do.call(rgb, c(as.list(col2rgb(col2)), alpha = 255/4, max = 255))

col3 <- brewer.pal(11, "RdBu")[1]
tcol3 <- do.call(rgb, c(as.list(col2rgb(col3)), alpha = 255/4, max = 255))

# Set relative risk range
r1 <- min(range(rr, rr.lci, rr.uci, na.rm = TRUE))
r2 <- max(range(rr, rr.lci, rr.uci, na.rm = TRUE))
r <- range(rr, rr.lci, rr.uci, na.rm = TRUE)
# Cool scenario
plot(lagbylag, rr[mn,], col = col1, type = "l", lwd = 1,
     xlab = "Lag", ylab = "Relative risk", main = "",
     ylim = c(r1, r2 * 1.1), frame.plot = TRUE, axes = FALSE)
axis(1, at = seq(0, max(lagbylag), 1), labels = seq(0, max(lagbylag), 1))
axis(2)
xx <- c(lagbylag, rev(lagbylag))
yy <- c(rr.lci[mn,], rev(rr.uci[mn,]))
polygon(xx, yy, col = tcol1, border = tcol1)

# Warm scenario
lines(lagbylag, rr[mx,], col = col2, lwd = 1)
xx <- c(lagbylag, rev(lagbylag))
yy <- c(rr.lci[mx,], rev(rr.uci[mx,]))
polygon(xx, yy, col = tcol2, border = tcol2)

# Warmest scenario
lines(lagbylag, rr[mx2,], col = col3, lwd = 1)
xx <- c(lagbylag, rev(lagbylag))
yy <- c(rr.lci[mx2,], rev(rr.uci[mx2,]))
polygon(xx, yy, col = tcol3, border = tcol3)

abline(h = 1, lty = 3)

# Legend
legend("topleft",
       legend = c(paste0("rainfall = ", round(vars[mn], 1), " mm "),
                  paste0("rainfall = ", round(vars[mx], 1), " mm "),
                  paste0("rainfall = ", round(vars[mx2], 1), " mm ")),
       col = c(col1, col2, col3),
       lwd = 2, lty = 1, bty = "n",
       y.intersp = 1.5, horiz = FALSE)

# Add subplot label
mtext(side = 2, at = r2 * 1.1, text = "b", las = 2, cex = 1.2, line = 2)

# Close PDF device
dev.off()

#Attributable number and fraction calculation

# (READ THE ACCOMPANYING PDF FOR DOCUMENTATION)
source("attrdl.R")
#data$Diarrhea_Hosp_OPD_ud_five

# Load best-fitting model with climate DLNMs
load("output4/model0.3.RData")
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
  subset(data, select = c("temp", paste0("tp_lag", 1:nlag))),
  argvar = list(fun = "ns", knots = equalknots(data$temp, 2)),  # Use equalknots with reproducibility
  arglag = list(fun = "ns", knots = nlag / 2)
)

# Calculate center temperature for predictions
cen <- round(mean(data$temp, na.rm = TRUE), 0)

# Validate temperature data
if (is.na(cen)) stop("Center temperature (cen) calculation failed due to missing data.")

# Define minimum and maximum temperatures from the data
min_temp <- min(data$temp, na.rm = TRUE)
max_temp <- max(data$temp, na.rm = TRUE)

# Predict relative risks over a range of temperatures using crosspred
temp_range <- seq(min_temp, max_temp, by = 0.1)  # Define temperature range

# Generate predictions using the crossbasis, coefficients, and covariance matrix
predt <- crosspred(cb, coef = coef[indt], vcov = vcov[indt, indt], model.link = "log", at = temp_range, cen = cen)

# Find the Minimum Risk Temperature (MRT)
mrt <- predt$predvar[which.min(predt$allRRfit)]
mrt_ci <- quantile(predt$allRRfit, probs = c(0.025, 0.975))  # Confidence interval for MRT

# Output MRT range
print(paste("Minimum Risk Temperature (MRT):", mrt))
print(paste("Confidence Interval for MRT:", round(mrt_ci[1], 2), "to", round(mrt_ci[2], 2)))

# Define the temperature range where RR > 1 (i.e., the RR exceeds 1)
high_temp_range <- temp_range[predt$allRRfit > 1.1]

# If the high_temp_range is empty, there are no temperatures where RR > 1
if (length(high_temp_range) == 0) {
  cat("No temperatures found where RR > 1.\n")
  next
}

# Find the temperature bounds where RR > 1.1
high_temp_bounds <- range(high_temp_range)  # Lower and upper bounds

# Attributable risk calculations for temperatures where RR > 1.1
an_risk_number_hot <- attrdl(
  data$temp, cb, data$Diarrhea_Hosp_OPD_ud_five,
  coef = coef[indt], vcov = vcov[indt, indt],
  type = "an", cen = mrt, range = high_temp_bounds
)

an_risk_fraction_hot <- attrdl(
  data$temp, cb, data$Diarrhea_Hosp_OPD_ud_five,
  coef = coef[indt], vcov = vcov[indt, indt],
  type = "af", cen = mrt, range = high_temp_bounds
)

# Output the results for extreme high temperatures
cat("Attributable Risk for High Temperatures (RR > 1.1):\n")
cat("  High Temperature Range:", round(high_temp_bounds[1], 2), "to", round(high_temp_bounds[2], 2), "\n")
cat("  Attributable Risk (Number):", round(an_risk_number_hot), "\n")
cat("  Attributable Risk (Fraction):", round(an_risk_fraction_hot * 100, 2), "%\n")

## Attributable Risk of Extreme Hot Temperatures for each province and year

# Load best-fitting model with climate DLNMs
load("output4/model0.3.RData")
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
  Province = character(),
  Year = integer(),
  MRT = numeric(),
  High_Temperature_Lower = numeric(),
  High_Temperature_Upper = numeric(),
  Attributable_Risk_Number = numeric(),
  Attributable_Risk_Fraction = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each province and year
for (prov in unique(data$Province)) {

  for (year in unique(data$Year)) {

    # Subset data for the current province and year
    data_prov_year <- subset(data, Province == prov & Year == year)

    # Skip if insufficient data for the province and year
    if (nrow(data_prov_year) < 10) next

    # Recreate the temperature crossbasis for the province and year
    cb_prov_year <- crossbasis(
      subset(data_prov_year, select = c("temp", paste0("tp_lag", 1:nlag))),
      argvar = list(fun = "ns", knots = equalknots(data_prov_year$temp, 2)),
      arglag = list(fun = "ns", knots = nlag / 2)
    )

    # Calculate center temperature for predictions
    cen <- round(mean(data_prov_year$temp, na.rm = TRUE), 0)

    # Validate temperature data
    if (is.na(cen)) next

    # Define minimum and maximum temperatures from the data
    min_temp <- min(data_prov_year$temp, na.rm = TRUE)
    max_temp <- max(data_prov_year$temp, na.rm = TRUE)
    temp_range <- seq(min_temp, max_temp, by = 0.1)

    # Generate predictions using the crossbasis, coefficients, and covariance matrix
    predt <- crosspred(cb_prov_year, coef = coef[indt], vcov = vcov[indt, indt], model.link = "log", at = temp_range, cen = cen)

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
      data_prov_year$temp, cb_prov_year, data_prov_year$Diarrhea_Hosp_OPD_ud_five,
      coef = coef[indt], vcov = vcov[indt, indt],
      type = "an", cen = mrt, range = high_temp_bounds
    )

    an_risk_fraction_hot <- attrdl(
      data_prov_year$temp, cb_prov_year, data_prov_year$Diarrhea_Hosp_OPD_ud_five,
      coef = coef[indt], vcov = vcov[indt, indt],
      type = "af", cen = mrt, range = high_temp_bounds
    )

    # Store results for the current province and year
    results <- rbind(results, data.frame(
      Province = prov,
      Year = year,
      MRT = round(mrt, 2),
      High_Temperature_Lower = round(high_temp_bounds[1], 2),
      High_Temperature_Upper = round(high_temp_bounds[2], 2),
      Attributable_Risk_Number = round(an_risk_number_hot),
      Attributable_Risk_Fraction = round(an_risk_fraction_hot * 100, 2)
    ))
  }
}

# Display results for all provinces and years
print(results)
# Save results to CSV
write.csv(results, "output4/attribution_temp_per_province_year.csv", row.names = FALSE)

#Attributable Risk of Extreme Hot Temperatures for each district and year

# Load best-fitting model with climate DLNMs
load("output4/model0.3.RData")
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
  Province = character(),
  District = character(),
  Year = integer(),
  MPR = numeric(),
  High_Temperature_Lower = numeric(),
  High_Temperature_Upper = numeric(),
  Attributable_Risk_Number = numeric(),
  Attributable_Risk_Fraction = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each province, district, and year
for (prov in unique(data$Province)) {
  for (district in unique(data$District)) {
    for (year in unique(data$Year)) {

      # Subset data for the current province, district, and year
      data_group <- subset(data, Province == prov & District == district & Year == year)

      # Skip if insufficient data
      if (nrow(data_group) < 10) next

      # Recreate the temperature crossbasis for the group
      cb_group <- crossbasis(
        subset(data_group, select = c("temp", paste0("tp_lag", 1:nlag))),
        argvar = list(fun = "ns", knots = equalknots(data_group$temp, 2)),
        arglag = list(fun = "ns", knots = nlag / 2)
      )

      # Calculate center temperature for predictions
      cen <- round(mean(data_group$temp, na.rm = TRUE), 0)

      # Validate temperature data
      if (is.na(cen)) next

      # Define minimum and maximum temperatures from the data
      min_temp <- min(data_group$temp, na.rm = TRUE)
      max_temp <- max(data_group$temp, na.rm = TRUE)
      temp_range <- seq(min_temp, max_temp, by = 0.1)

      # Generate predictions using the crossbasis, coefficients, and covariance matrix
      predt <- crosspred(cb_group, coef = coef[indt], vcov = vcov[indt, indt], model.link = "log", at = temp_range, cen = cen)

      # Find the Minimum Risk Temperature (MRT)
      mrt <- predt$predvar[which.min(predt$allRRfit)]

      # Define the temperature range where RR > 1.1 (i.e., the RR exceeds 1.1)
      high_temp_range <- temp_range[predt$allRRfit > 1.1]

      # If the high_temp_range is empty, there are no temperatures where RR > 1
      if (length(high_temp_range) == 0) {
        next
      }

      # Find the temperature bounds where RR > 1
      high_temp_bounds <- range(high_temp_range)  # Lower and upper bounds

      # Attributable risk calculations for temperatures where RR > 1.1
      an_risk_number_hot <- attrdl(
        data_group$temp, cb_group, data_group$Diarrhea_Hosp_OPD_ud_five,
        coef = coef[indt], vcov = vcov[indt, indt],
        type = "an", cen = mrt, range = high_temp_bounds
      )

      an_risk_fraction_hot <- attrdl(
        data_group$temp, cb_group, data_group$Diarrhea_Hosp_OPD_ud_five,
        coef = coef[indt], vcov = vcov[indt, indt],
        type = "af", cen = mrt, range = high_temp_bounds
      )

      # Store results for the current group
      results <- rbind(results, data.frame(
        Province = prov,
        District = district,
        Year = year,
        MPR = round(mrt, 2),
        High_Temperature_Lower = round(high_temp_bounds[1], 2),
        High_Temperature_Upper = round(high_temp_bounds[2], 2),
        Attributable_Risk_Number = round(an_risk_number_hot),
        Attributable_Risk_Fraction = round(an_risk_fraction_hot * 100, 2)
      ))
    }
  }
}

# Display results for all groups
print(results)
# Save results to CSV
write.csv(results, "output4/attribution_temp_per_district_year.csv", row.names = FALSE)

##Country level attribution risk rainfall

# Load best-fitting model with climate DLNMs
load("output4/model0.3.RData")

# Select the model
model <- model0

# Extract coefficients and covariance matrix
coef <- model$summary.fixed$mean
vcov <- model$misc$lincomb.derived.covariance.matrix

# Identify terms related to the rainfall crossbasis
indr <- grep("basis_tprainfall", model$names.fixed)

# Validate model inputs
if (is.null(coef) || is.null(vcov)) stop("Model coefficients or covariance matrix are missing.")
if (length(indr) == 0) stop("No terms associated with 'basis_tprainfall' found in the model.")

# Recreate the rainfall crossbasis for the entire dataset
nlag <- 2  # Number of lags
cb <- crossbasis(
  subset(data, select = c("tprainfall", paste0("rtp_lag", 1:nlag))),
  argvar = list(fun = "ns", knots = equalknots(data$tprainfall, 2)),
  arglag = list(fun = "ns", knots = lagknot)
)

# Center rainfall value for predictions
cen <- round(mean(data$tprainfall, na.rm = TRUE), 0)

# Validate precipitation data
if (is.na(cen)) stop("Center rainfall (cen) calculation failed due to missing data.")

# Define minimum and maximum precipitation from the data
min_rainfall <- min(data$tprainfall, na.rm = TRUE)
max_rainfall <- max(data$tprainfall, na.rm = TRUE)

# Predict relative risks over a range of rainfall using crosspred
rain_range <- seq(min_rainfall, max_rainfall, by = 0.1)  # Define precipitation range

# Generate predictions using the crossbasis, coefficients, and covariance matrix
predt <- crosspred(cb, coef = coef[indr], vcov = vcov[indr, indr], model.link = "log", at = rain_range, cen = cen)

# Find the Minimum Precipitation Risk (MPR)
mpr <- predt$predvar[which.min(predt$allRRfit)]
mpr_ci <- quantile(predt$allRRfit, probs = c(0.025, 0.975))  # Confidence interval for MPR

# Define the precipitation range where RR > 1.1
high_rainfall_range <- rain_range[predt$allRRfit > 1.05]
high_rainfall_bounds <- range(high_rainfall_range)  # Lower and upper bounds

# Attributable risk calculations for the high precipitation range
an_risk_number_rain <- attrdl(
  data$tprainfall, cb, data$Diarrhea_Hosp_OPD_ud_five,
  coef = coef[indr], vcov = vcov[indr, indr],
  type = "an", cen = mpr, range = high_rainfall_bounds
)

an_risk_fraction_rain <- attrdl(
  data$tprainfall, cb, data$Diarrhea_Hosp_OPD_ud_five,
  coef = coef[indr], vcov = vcov[indr, indr],
  type = "af", cen = mpr, range = high_rainfall_bounds
)

# Output the results
cat("\nPrecipitation Risk Summary:\n")
cat("  Minimum Precipitation Risk (MPR):", mpr, "\n")
cat("  95% CI for MPR:", mpr_ci[1], "-", mpr_ci[2], "\n")
cat("  High Precipitation Range (RR > 1):", high_rainfall_bounds[1], "to", high_rainfall_bounds[2], "\n")
cat("  Attributable Risk (Number):", round(an_risk_number_rain), "\n")
cat("  Attributable Risk (Fraction):", round(an_risk_fraction_rain * 100, 2), "%\n")

## attribution risk rainfall provice for each year

# Load best-fitting model with climate DLNMs
load("output4/model0.3.RData")

# Select the model
model <- model0

# Extract coefficients and covariance matrix
coef <- model$summary.fixed$mean
vcov <- model$misc$lincomb.derived.covariance.matrix

# Identify terms related to the rainfall crossbasis
indr <- grep("basis_tprainfall", model$names.fixed)

# Validate model inputs
if (is.null(coef) || is.null(vcov)) stop("Model coefficients or covariance matrix are missing.")
if (length(indr) == 0) stop("No terms associated with 'basis_tprainfall' found in the model.")

# List unique provinces and years
provinces <- unique(data$Province)
years <- unique(data$Year)  # Assuming "Year" is a column in the dataset

# Initialize results storage
results <- data.frame(
  Province = character(),
  Year = integer(),
  MPR = numeric(),
  High_Precipitation_Lower = numeric(),
  High_Precipitation_Upper = numeric(),
  Attributable_Risk_Number = numeric(),
  Attributable_Risk_Fraction = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each province
for (prov in provinces) {

  # Subset data for the province
  data_prov <- subset(data, Province == prov)

  # Skip if insufficient data
  if (nrow(data_prov) < 10) next

  # Loop through each year within the province
  for (yr in years) {

    # Subset data for the specific year
    data_year <- subset(data_prov, Year == yr)

    if (nrow(data_year) < 10) next  # Skip years with insufficient data

    # Recreate the rainfall crossbasis
    nlag <- 2  # Number of lags
    cb_year <- crossbasis(
      subset(data_year, select = c("tprainfall", paste0("rtp_lag", 1:nlag))),
      argvar = list(fun = "ns", knots = equalknots(data_year$tprainfall, 2)),
      arglag = list(fun = "ns", knots = lagknot)
    )

    # Center rainfall value for predictions
    cen <- round(mean(data_year$tprainfall, na.rm = TRUE), 0)

    # Validate precipitation data
    if (is.na(cen)) next

    # Define precipitation range
    min_rainfall <- min(data_year$tprainfall, na.rm = TRUE)
    max_rainfall <- max(data_year$tprainfall, na.rm = TRUE)
    rain_range <- seq(min_rainfall, max_rainfall, by = 0.1)

    # Generate predictions using the crossbasis, coefficients, and covariance matrix
    predt <- crosspred(cb_year, coef = coef[indr], vcov = vcov[indr, indr], model.link = "log", at = rain_range, cen = cen)

    # Find the Minimum Precipitation Risk (MPR)
    mpr <- predt$predvar[which.min(predt$allRRfit)]

    # Define the precipitation range where RR > 1.1
    high_rainfall_range <- rain_range[predt$allRRfit > 1.1]

    if (length(high_rainfall_range) == 0) next  # Skip if no high-risk range found

    high_rainfall_bounds <- range(high_rainfall_range)  # Lower and upper bounds

    # Attributable risk calculations for high precipitation range
    an_risk_number_rain <- attrdl(
      data_year$tprainfall, cb_year, data_year$Diarrhea_Hosp_OPD_ud_five,
      coef = coef[indr], vcov = vcov[indr, indr],
      type = "an", cen = mpr, range = high_rainfall_bounds
    )

    an_risk_fraction_rain <- attrdl(
      data_year$tprainfall, cb_year, data_year$Diarrhea_Hosp_OPD_ud_five,
      coef = coef[indr], vcov = vcov[indr, indr],
      type = "af", cen = mpr, range = high_rainfall_bounds
    )

    # Store results in a data frame
    results <- rbind(results, data.frame(
      Province = prov,
      Year = yr,
      MPR = round(mpr, 2),
      High_Precipitation_Lower = round(high_rainfall_bounds[1], 2),
      High_Precipitation_Upper = round(high_rainfall_bounds[2], 2),
      Attributable_Risk_Number = round(an_risk_number_rain),
      Attributable_Risk_Fraction = round(an_risk_fraction_rain * 100, 2)
    ))
  }
}

# Display results
print(results)

# Save results to CSV
write.csv(results, "output4/attribution_rainfall_province_per_year.csv", row.names = FALSE)

##attribution risk rainfall by district for each year

# Load best-fitting model with climate DLNMs
load("output4/model0.3.RData")

# Select the model
model <- model0

# Extract coefficients and covariance matrix
coef <- model$summary.fixed$mean
vcov <- model$misc$lincomb.derived.covariance.matrix

# Identify terms related to the rainfall crossbasis
indr <- grep("basis_tprainfall", model$names.fixed)

# Validate model inputs
if (is.null(coef) || is.null(vcov)) stop("Model coefficients or covariance matrix are missing.")
if (length(indr) == 0) stop("No terms associated with 'basis_tprainfall' found in the model.")

# List unique provinces, districts, and years
provinces <- unique(data$Province)
districts <- unique(data$District)
years <- unique(data$Year)  # Assuming "Year" is a column in the dataset

# Initialize results storage
results <- data.frame(
  Province = character(),
  District = character(),
  Year = integer(),
  MPR = numeric(),
  High_Precipitation_Lower = numeric(),
  High_Precipitation_Upper = numeric(),
  Attributable_Risk_Number = numeric(),
  Attributable_Risk_Fraction = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each province
for (prov in provinces) {

  # Subset data for the province
  data_prov <- subset(data, Province == prov)

  # Skip if insufficient data
  if (nrow(data_prov) < 10) next

  # Loop through each district within the province
  for (dist in districts) {

    # Subset data for the specific district
    data_dist <- subset(data_prov, District == dist)

    if (nrow(data_dist) < 10) next  # Skip districts with insufficient data

    # Loop through each year within the district
    for (yr in years) {

      # Subset data for the specific year
      data_year <- subset(data_dist, Year == yr)

      if (nrow(data_year) < 10) next  # Skip years with insufficient data

      # Recreate the rainfall crossbasis
      nlag <- 2  # Number of lags
      cb_year <- crossbasis(
        subset(data_year, select = c("tprainfall", paste0("rtp_lag", 1:nlag))),
        argvar = list(fun = "ns", knots = equalknots(data_year$tprainfall, 2)),
        arglag = list(fun = "ns", knots = lagknot)
      )

      # Center rainfall value for predictions
      cen <- round(mean(data_year$tprainfall, na.rm = TRUE), 0)

      # Validate precipitation data
      if (is.na(cen)) next

      # Define precipitation range
      min_rainfall <- min(data_year$tprainfall, na.rm = TRUE)
      max_rainfall <- max(data_year$tprainfall, na.rm = TRUE)
      rain_range <- seq(min_rainfall, max_rainfall, by = 0.1)

      # Generate predictions using the crossbasis, coefficients, and covariance matrix
      predt <- crosspred(cb_year, coef = coef[indr], vcov = vcov[indr, indr], model.link = "log", at = rain_range, cen = cen)

      # Find the Minimum Precipitation Risk (MPR)
      mpr <- predt$predvar[which.min(predt$allRRfit)]

      # Define the precipitation range where RR > 1
      high_rainfall_range <- rain_range[predt$allRRfit > 1]

      if (length(high_rainfall_range) == 0) next  # Skip if no high-risk range found

      high_rainfall_bounds <- range(high_rainfall_range)  # Lower and upper bounds

      # Attributable risk calculations for high precipitation range
      an_risk_number_rain <- attrdl(
        data_year$tprainfall, cb_year, data_year$Diarrhea_Hosp_OPD_ud_five,
        coef = coef[indr], vcov = vcov[indr, indr],
        type = "an", cen = mpr, range = high_rainfall_bounds
      )

      an_risk_fraction_rain <- attrdl(
        data_year$tprainfall, cb_year, data_year$Diarrhea_Hosp_OPD_ud_five,
        coef = coef[indr], vcov = vcov[indr, indr],
        type = "af", cen = mpr, range = high_rainfall_bounds
      )

      # Store results in a data frame
      results <- rbind(results, data.frame(
        Province = prov,
        District = dist,
        Year = yr,
        MPR = round(mpr, 2),
        High_Precipitation_Lower = round(high_rainfall_bounds[1], 2),
        High_Precipitation_Upper = round(high_rainfall_bounds[2], 2),
        Attributable_Risk_Number = round(an_risk_number_rain),
        Attributable_Risk_Fraction = round(an_risk_fraction_rain * 100, 2)
      ))
    }
  }
}

# Display results
print(results)

# Save results to CSV
write.csv(results, "output4/attribution_per_district_year.csv", row.names = FALSE)
