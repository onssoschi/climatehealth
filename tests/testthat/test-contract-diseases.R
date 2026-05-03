# Contract tests for disease indicators (malaria and diarrhea)
# Tests verify input/output contracts for the external-facing API

# Test combine_health_climate_data input contract - malaria
test_that("combine_health_climate_data accepts valid malaria data", {
  skip_if_not_installed("sf")

  disease_data <- generate_disease_data(n_months = 12, case_type = "malaria")

  health_file <- write_temp_csv(disease_data$health_data, "malaria_health.csv")
  climate_file <- write_temp_csv(disease_data$climate_data, "malaria_climate.csv")

  geo <- generate_simple_geometry(districts = c("DistrictA", "DistrictB", "DistrictC"))
  geo_file <- file.path(tempdir(), "malaria_geo.geojson")
  sf::st_write(geo, geo_file, quiet = TRUE, delete_dsn = TRUE)

  result <- combine_health_climate_data(
    health_data_path = health_file,
    climate_data_path = climate_file,
    map_path = geo_file,
    region_col = "region",
    district_col = "district",
    date_col = NULL,
    year_col = "year",
    month_col = "month",
    case_col = "malaria_cases",
    case_type = "malaria",
    tot_pop_col = "tot_pop",
    tmin_col = "tmin",
    tmean_col = "tmean",
    tmax_col = "tmax",
    rainfall_col = "rainfall",
    r_humidity_col = "r_humidity",
    geometry_col = "geometry",
    runoff_col = "runoff",
    spi_col = NULL,
    ndvi_col = NULL,
    max_lag = 3,
    output_dir = NULL
  )

  # Output contract: returns a list with data and map
  expect_true(is.list(result))
  expect_true("data" %in% names(result))
  expect_true("map" %in% names(result))
  expect_true(is.data.frame(result$data))
})

# Test combine_health_climate_data input contract - diarrhea
test_that("combine_health_climate_data accepts valid diarrhea data", {
  skip_if_not_installed("sf")

  disease_data <- generate_disease_data(n_months = 12, case_type = "diarrhea")

  health_file <- write_temp_csv(disease_data$health_data, "diarrhea_health.csv")
  climate_file <- write_temp_csv(disease_data$climate_data, "diarrhea_climate.csv")

  geo <- generate_simple_geometry(districts = c("DistrictA", "DistrictB", "DistrictC"))
  geo_file <- file.path(tempdir(), "diarrhea_geo.geojson")
  sf::st_write(geo, geo_file, quiet = TRUE, delete_dsn = TRUE)

  result <- combine_health_climate_data(
    health_data_path = health_file,
    climate_data_path = climate_file,
    map_path = geo_file,
    region_col = "region",
    district_col = "district",
    date_col = NULL,
    year_col = "year",
    month_col = "month",
    case_col = "diarrhea_cases",
    case_type = "diarrhea",
    tot_pop_col = "tot_pop",
    tmin_col = "tmin",
    tmean_col = "tmean",
    tmax_col = "tmax",
    rainfall_col = "rainfall",
    r_humidity_col = "r_humidity",
    geometry_col = "geometry",
    runoff_col = "runoff",
    spi_col = NULL,
    ndvi_col = NULL,
    max_lag = 3,
    output_dir = NULL
  )

  # Output contract: returns a list with data and map
  expect_true(is.list(result))
  expect_true("data" %in% names(result))
  expect_true("diarrhea" %in% names(result$data))
})

# Test data structure contract - malaria
test_that("malaria output has required columns", {
  skip_if_not_installed("sf")

  disease_data <- generate_disease_data(n_months = 6, case_type = "malaria")
  health_file <- write_temp_csv(disease_data$health_data, "malaria_health2.csv")
  climate_file <- write_temp_csv(disease_data$climate_data, "malaria_climate2.csv")

  geo <- generate_simple_geometry(districts = c("DistrictA", "DistrictB", "DistrictC"))
  geo_file <- file.path(tempdir(), "malaria_geo2.geojson")
  sf::st_write(geo, geo_file, quiet = TRUE, delete_dsn = TRUE)

  result <- combine_health_climate_data(
    health_data_path = health_file,
    climate_data_path = climate_file,
    map_path = geo_file,
    region_col = "region",
    district_col = "district",
    date_col = NULL,
    year_col = "year",
    month_col = "month",
    case_col = "malaria_cases",
    case_type = "malaria",
    tot_pop_col = "tot_pop",
    tmin_col = "tmin",
    tmean_col = "tmean",
    tmax_col = "tmax",
    rainfall_col = "rainfall",
    r_humidity_col = "r_humidity",
    geometry_col = "geometry",
    runoff_col = "runoff",
    spi_col = NULL,
    ndvi_col = NULL,
    max_lag = 3,
    output_dir = NULL
  )

  df <- result$data

  # Required columns
  required_cols <- c("region", "district", "year", "month", "malaria", "tot_pop")
  expect_true(
    all(required_cols %in% names(df)),
    info = paste("Missing columns:", paste(setdiff(required_cols, names(df)), collapse = ", "))
  )

  # Climate columns
  climate_cols <- c("tmin", "tmean", "tmax", "rainfall", "r_humidity")
  expect_true(
    all(climate_cols %in% names(df)),
    info = paste("Missing climate columns:", paste(setdiff(climate_cols, names(df)), collapse = ", "))
  )
})

# Test lagged variable creation
test_that("disease data includes lagged climate variables", {
  skip_if_not_installed("sf")

  disease_data <- generate_disease_data(n_months = 12, case_type = "malaria")
  health_file <- write_temp_csv(disease_data$health_data, "malaria_health_lag.csv")
  climate_file <- write_temp_csv(disease_data$climate_data, "malaria_climate_lag.csv")

  geo <- generate_simple_geometry(districts = c("DistrictA", "DistrictB", "DistrictC"))
  geo_file <- file.path(tempdir(), "malaria_geo_lag.geojson")
  sf::st_write(geo, geo_file, quiet = TRUE, delete_dsn = TRUE)

  result <- combine_health_climate_data(
    health_data_path = health_file,
    climate_data_path = climate_file,
    map_path = geo_file,
    region_col = "region",
    district_col = "district",
    date_col = NULL,
    year_col = "year",
    month_col = "month",
    case_col = "malaria_cases",
    case_type = "malaria",
    tot_pop_col = "tot_pop",
    tmin_col = "tmin",
    tmean_col = "tmean",
    tmax_col = "tmax",
    rainfall_col = "rainfall",
    r_humidity_col = "r_humidity",
    geometry_col = "geometry",
    runoff_col = "runoff",
    spi_col = NULL,
    ndvi_col = NULL,
    max_lag = 3,
    output_dir = NULL
  )

  df <- result$data

  # Lagged columns should exist (check for any lag pattern)
  lag_cols <- grep("_lag[0-9]+$|_l[0-9]+", names(df), value = TRUE)
  expect_true(length(lag_cols) > 0, info = "Should have lagged variables")
})

# Test case_type validation
test_that("disease functions reject invalid case_type", {
  # The validate_case_type function may be internal, test via the main function
  skip_if_not_installed("sf")

  disease_data <- generate_disease_data(n_months = 6, case_type = "malaria")
  health_file <- write_temp_csv(disease_data$health_data, "invalid_health.csv")
  climate_file <- write_temp_csv(disease_data$climate_data, "invalid_climate.csv")

  geo <- generate_simple_geometry(districts = c("DistrictA", "DistrictB", "DistrictC"))
  geo_file <- file.path(tempdir(), "invalid_geo.geojson")
  sf::st_write(geo, geo_file, quiet = TRUE, delete_dsn = TRUE)

  expect_error(
    combine_health_climate_data(
      health_data_path = health_file,
      climate_data_path = climate_file,
      map_path = geo_file,
      region_col = "region",
      district_col = "district",
      date_col = NULL,
      year_col = "year",
      month_col = "month",
      case_col = "malaria_cases",
      case_type = "invalid_disease",  # Invalid!
      tot_pop_col = "tot_pop",
      tmin_col = "tmin",
      tmean_col = "tmean",
      tmax_col = "tmax",
      rainfall_col = "rainfall",
      r_humidity_col = "r_humidity",
      geometry_col = "geometry",
      runoff_col = "runoff",
      spi_col = NULL,
      ndvi_col = NULL,
      max_lag = 3,
      output_dir = NULL
    )
  )
})

# Test data preservation
test_that("combine_health_climate_data preserves case counts", {
  skip_if_not_installed("sf")

  # Create small test data with known values
  health_data <- data.frame(
    region = rep("RegionNorth", 6),
    district = rep(c("DistrictA", "DistrictB"), 3),
    year = rep(2020, 6),
    month = c(1, 1, 2, 2, 3, 3),
    malaria_cases = c(100, 150, 120, 160, 130, 170),
    tot_pop = rep(100000, 6)
  )

  climate_data <- data.frame(
    region = rep("RegionNorth", 6),
    district = rep(c("DistrictA", "DistrictB"), 3),
    year = rep(2020, 6),
    month = c(1, 1, 2, 2, 3, 3),
    tmin = rep(15, 6),
    tmean = rep(22, 6),
    tmax = rep(30, 6),
    rainfall = rep(100, 6),
    r_humidity = rep(60, 6),
    runoff = rep(30, 6)
  )

  health_file <- write_temp_csv(health_data, "malaria_health_preserve.csv")
  climate_file <- write_temp_csv(climate_data, "malaria_climate_preserve.csv")

  geo <- generate_simple_geometry(districts = c("DistrictA", "DistrictB"))
  geo_file <- file.path(tempdir(), "malaria_geo_preserve.geojson")
  sf::st_write(geo, geo_file, quiet = TRUE, delete_dsn = TRUE)

  result <- combine_health_climate_data(
    health_data_path = health_file,
    climate_data_path = climate_file,
    map_path = geo_file,
    region_col = "region",
    district_col = "district",
    date_col = NULL,
    year_col = "year",
    month_col = "month",
    case_col = "malaria_cases",
    case_type = "malaria",
    tot_pop_col = "tot_pop",
    tmin_col = "tmin",
    tmean_col = "tmean",
    tmax_col = "tmax",
    rainfall_col = "rainfall",
    r_humidity_col = "r_humidity",
    geometry_col = "geometry",
    runoff_col = "runoff",
    spi_col = NULL,
    ndvi_col = NULL,
    max_lag = 1,
    output_dir = NULL
  )

  df <- result$data

  # Case counts should be preserved
  expect_true(all(c(100, 150, 120, 160, 130, 170) %in% df$malaria))
})
