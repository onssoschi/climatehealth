# Synthetic test data generators for Climate Health contract tests
# This file provides minimal but valid test data for all 5 main indicators:
# - Temperature Mortality
# - Wildfire
# - Mental Health (Suicides/Extreme Heat)
# - Malaria
# - Diarrhea

#' Generate synthetic temperature mortality data
#' @param n_days Number of days of data to generate
#' @param regions Character vector of region names
#' @return data.frame with date, deaths, tmean, region, population columns
generate_temp_mortality_data <- function(n_days = 365, regions = c("North", "South")) {
  set.seed(42)
  dates <- seq.Date(from = as.Date("2020-01-01"), by = "day", length.out = n_days)

  data <- expand.grid(date = dates, region = regions, stringsAsFactors = FALSE)
  data$day_of_year <- as.numeric(format(data$date, "%j"))

  # Seasonal temperature pattern (peaks in summer)
  data$tmean <- 10 + 15 * sin((data$day_of_year - 80) * 2 * pi / 365) +
    rnorm(nrow(data), 0, 2)

  # Mortality with U-shaped temperature relationship (more deaths at extremes)
  data$deaths <- rpois(
    nrow(data),
    lambda = 100 + 2 * pmax(0, data$tmean - 20) + 3 * pmax(0, 5 - data$tmean)
  )

  # Population varies by region
  data$population <- ifelse(data$region == regions[1], 500000, 1200000)

  # Clean up helper column

  data$day_of_year <- NULL

  return(data)
}

#' Generate synthetic wildfire health data
#' @param n_days Number of days of data to generate
#' @param regions Character vector of region names
#' @return data.frame with wildfire-relevant columns
generate_wildfire_data <- function(n_days = 200, regions = c("Region_A", "Region_B")) {
  set.seed(42)
  dates <- seq.Date(from = as.Date("2020-01-01"), by = "day", length.out = n_days)

  data <- expand.grid(date = dates, region = regions, stringsAsFactors = FALSE)

  # Add time columns
  data$year <- as.integer(format(data$date, "%Y"))
  data$month <- as.integer(format(data$date, "%m"))
  data$day <- as.integer(format(data$date, "%d"))
  data$dow <- weekdays(data$date, abbreviate = TRUE)

  # Temperature
  data$tmean <- rnorm(nrow(data), mean = 15, sd = 5)

  # PM2.5 with seasonal fire pattern (higher in summer/dry season)
  data$mean_PM <- runif(nrow(data), 5, 30) *
    (1 + 0.5 * sin((data$month - 3) * pi / 6))

  # Health outcomes related to PM exposure
  data$health_outcome <- rpois(nrow(data), lambda = 5 + 0.1 * data$mean_PM)

  # Optional environmental variables
  data$rh <- runif(nrow(data), 40, 90)
  data$wind_speed <- runif(nrow(data), 0, 10)

  # Population
  data$pop <- ifelse(data$region == regions[1], 500000, 1200000)

  return(data)
}

#' Generate synthetic mental health / suicides data
#' @param n_days Number of days of data to generate
#' @param regions Character vector of region names
#' @return data.frame with mental health analysis columns
generate_mental_health_data <- function(n_days = 365, regions = c("National")) {

  set.seed(42)
  dates <- seq.Date(from = as.Date("2020-01-01"), by = "day", length.out = n_days)

  data <- expand.grid(date = dates, region = regions, stringsAsFactors = FALSE)
  data$day_of_year <- as.numeric(format(data$date, "%j"))

  # Seasonal temperature pattern
  data$temp <- 10 + 15 * sin((data$day_of_year - 80) * 2 * pi / 365) +
    rnorm(nrow(data), 0, 3)

  # Suicides with weak heat relationship
  data$suicides <- rpois(
    nrow(data),
    lambda = 5 + 0.05 * pmax(0, data$temp - 25)
  )

  # Population
  data$population <- 5000000

  # Clean up helper column
  data$day_of_year <- NULL

  return(data)
}

#' Generate synthetic disease data (malaria or diarrhea)
#' @param n_months Number of months of data to generate
#' @param districts Character vector of district names
#' @param case_type Either "malaria" or "diarrhea"
#' @return list with health_data and climate_data data.frames
generate_disease_data <- function(
    n_months = 24,
    districts = c("DistrictA", "DistrictB", "DistrictC"),
    case_type = "malaria"
) {
  set.seed(42)

  # Generate time grid
  months <- rep(1:12, ceiling(n_months / 12))[1:n_months]
  years <- rep(2020:(2020 + ceiling(n_months / 12) - 1), each = 12)[1:n_months]

  # Health data
  health_data <- expand.grid(
    district = districts,
    year = unique(years),
    month = 1:12,
    stringsAsFactors = FALSE
  )
  health_data <- health_data[
    paste(health_data$year, health_data$month) %in% paste(years, months),
  ]

  # Assign regions
  health_data$region <- ifelse(
    health_data$district %in% districts[1:min(2, length(districts))],
    "RegionNorth",
    "RegionSouth"
  )

  # Population
  health_data$tot_pop <- sample(50000:200000, nrow(health_data), replace = TRUE)

  # Case counts with seasonal pattern
  if (case_type == "malaria") {
    # Malaria peaks after rainy season (month 3-6)
    health_data$malaria_cases <- rpois(
      nrow(health_data),
      lambda = 500 + 100 * sin((health_data$month - 3) * pi / 6)
    )
  } else {
    # Diarrhea peaks in hot/wet months (month 1-3)
    health_data$diarrhea_cases <- rpois(
      nrow(health_data),
      lambda = 300 + 80 * sin((health_data$month - 1) * pi / 6)
    )
  }

  # Climate data
  climate_data <- health_data[, c("district", "year", "month", "region")]

  # Temperature variables with seasonal patterns
  climate_data$tmin <- 15 + 5 * sin((climate_data$month - 1) * pi / 6) +
    rnorm(nrow(climate_data), 0, 2)
  climate_data$tmean <- 22 + 8 * sin((climate_data$month - 1) * pi / 6) +
    rnorm(nrow(climate_data), 0, 2)
  climate_data$tmax <- 30 + 10 * sin((climate_data$month - 1) * pi / 6) +
    rnorm(nrow(climate_data), 0, 2)

  # Precipitation variables
  climate_data$rainfall <- pmax(
    0,
    100 + 150 * sin((climate_data$month - 4) * pi / 6) +
      rnorm(nrow(climate_data), 0, 30)
  )
  climate_data$r_humidity <- 60 + 20 * sin((climate_data$month - 4) * pi / 6) +
    rnorm(nrow(climate_data), 0, 5)
  climate_data$runoff <- pmax(
    0,
    climate_data$rainfall * 0.3 + rnorm(nrow(climate_data), 0, 10)
  )

  return(list(health_data = health_data, climate_data = climate_data))
}

#' Generate minimal GeoJSON-like sf object for disease indicators
#' @param districts Character vector of district names
#' @return sf object with simple polygon geometries (requires sf package)
generate_simple_geometry <- function(
    districts = c("DistrictA", "DistrictB", "DistrictC")
) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("sf package required for geometry generation")
  }

  # Create simple polygon geometries
  geoms <- lapply(seq_along(districts), function(i) {
    x_offset <- (i - 1) * 2
    sf::st_polygon(list(matrix(c(
      x_offset, 0,
      x_offset + 1, 0,
      x_offset + 1, 1,
      x_offset, 1,
      x_offset, 0
    ), ncol = 2, byrow = TRUE)))
  })

  sf::st_sf(
    region = ifelse(
      districts %in% districts[1:min(2, length(districts))],
      "RegionNorth",
      "RegionSouth"
    ),
    district = districts,
    geometry = sf::st_sfc(geoms, crs = 4326)
  )
}

#' Write test data to temporary CSV files
#' @param data data.frame to write
#' @param filename Name for the temp file (without path)
#' @return Full path to the temporary file
write_temp_csv <- function(data, filename = "test_data.csv") {
  tmp_dir <- tempdir()
  tmp_file <- file.path(tmp_dir, filename)
  write.csv(data, tmp_file, row.names = FALSE)
  return(tmp_file)
}

#' Generate complete wildfire test data with all required columns
#' For use with casecrossover_quasipoisson and related functions
#' Uses the package's naming convention: mean_PM, mean_PM_l{n}_mean
#' @param n Number of rows
#' @param n_strata Number of strata
#' @param n_lags Number of lag periods
#' @param spline_df Degrees of freedom for splines
#' @param start_date Start date for data
#' @return data.frame ready for wildfire analysis
generate_wildfire_test_data <- function(
    n = 1000,
    n_strata = 5,
    n_lags = 2,
    spline_df = 6,
    start_date = as.Date("2020-01-01")
) {
  set.seed(42)

  # Generate strata (categorical identifier for case-crossover design)
  strata <- factor(rep(1:n_strata, length.out = n))

  # Generate base data using package's naming convention
  data <- data.frame(
    stratum = strata,
    ind = 1,  # Indicator for subsetting
    health_outcome = rpois(n, lambda = 5),
    mean_PM = runif(n, 5, 50),  # Base PM column
    tmean = rnorm(n, 20, 5)     # Base temperature column
  )

  # Add lagged PM variables using package convention (mean_PM_l{n}_mean)
  for (lag in 0:n_lags) {
    col_name <- paste0("mean_PM_l", lag, "_mean")
    data[[col_name]] <- runif(n, 5, 50)
  }

  # Add temperature lag columns (tmean_l{n}_mean)
  for (lag in 0:n_lags) {
    col_name <- paste0("tmean_l", lag, "_mean")
    data[[col_name]] <- rnorm(n, 20, 5)
  }

  # Add temperature spline basis columns
  for (i in 1:spline_df) {
    data[[paste0("tmean_basis", i)]] <- rnorm(n)
  }

  # Add date information
  data$date <- start_date + (seq_len(n) - 1)
  data$year <- as.integer(format(data$date, "%Y"))
  data$month <- as.integer(format(data$date, "%m"))
  data$dow <- weekdays(data$date, abbreviate = TRUE)

  return(data)
}
