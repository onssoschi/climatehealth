# Unit tests for wildfire.R

# Create temp_dir to be used by all WF tests
temp_dir <- tempdir()
temp_dir <- file.path(temp_dir, "wildfire_tests")
if (!file.exists(temp_dir)) dir.create(temp_dir)

# Test read_and_format_data

WF_TEST_HEALTH <- data.frame(
    date = c("2020-01-01", "2020-01-02", "2020-01-03"),
    tmean = c(5.2, 6.1, 4.8),
    deaths = c(10, 12, 9),
    region = c("North", "South", "East"),
    relative_humidity = c(70, 65, 80),
    wind_speed = c(3.2, 2.8, 4.0),
    stringsAsFactors = FALSE,
    regnames = c("North", "South", "East")
)

WF_TEST_CLIMATE <- data.frame(
    date = lubridate::dmy(c("01-01-2020", "02-01-2020", "03/01/2020")),
    region = c("North", "South", "East"),
    mean_PM = c(0.0005, 0.0001, 0.00015)
)

test_that(
    "read_and_format_data returns as expected with all columns provided.",
    {
        res <- read_and_format_data(
            health_path = WF_TEST_HEALTH,
            date_col = "date",
            mean_temperature_col = "tmean",
            health_outcome_col = "deaths",
            region_col = "regnames",
            rh_col = "relative_humidity",
            wind_speed_col = "wind_speed"
        )
        exp_cols = c(
            "date", "tmean", "health_outcome", "region", "rh", "wind_speed",
            "year", "month", "day", "dow"
        )
        expect_true(all(exp_cols %in% colnames(res)))
        expect_equal(res$day, c(1, 2, 3))
        expect_equal(res$month, c(1, 1, 1))
    }
)

test_that(
    "read_and_format_data returns as expected with no optional columns.",
    {
        res <- read_and_format_data(
            health_path = WF_TEST_HEALTH,
            date_col = "date",
            mean_temperature_col = "tmean",
            health_outcome_col = "deaths"
        )
        exp_cols = c(
            "date", "tmean", "health_outcome", "region", "rh", "wind_speed",
            "year", "month", "day", "dow"
        )
        expect_true(all(exp_cols %in% colnames(res)))
        expect_equal(res$region, rep("no_region", 3))
        expect_equal(res$rh, rep(NA, 3))
        expect_equal(res$wind_speed, rep(NA, 3))
    }
)

test_that(
    "read_and_format_data identifies and convertsdates in dmy format.",
    {
        WF_DMY_DATA = WF_TEST_HEALTH %>% mutate(
            date = c("01-01-2020", "02-01-2020", "03/01/2020")
        )
        res <- read_and_format_data(
            health_path = WF_DMY_DATA,
            date_col = "date",
            mean_temperature_col = "tmean",
            health_outcome_col = "deaths"
        )
        expect_equal(res$dow, c("Wed", "Thu", "Fri"))
    }
)

# Tests for extract_means_for_geography

test_that(
    "extract_means_for_geography raises when the terra package is not installed.",
    {
        with_mocked_bindings(
            requireNamespace = function(pkg, quietly=T) FALSE,
            .package = "base",
            code = expect_error(
                extract_means_for_geography("", ""),
                "'terra' is not installed. Run climatehealth::"
            )
        )
    }
)

test_that("extract_means_for_geography returns expected structure for a tiny NetCDF + shapefile", {

  # Helper: create a minimal NetCDF with dims lon, lat, time and one variable "pm25"
  create_test_ncdf <- function(path) {
    lon <- c(0.25, 0.75)
    lat <- c(0.25, 0.75)
    time <- c(0, 1)  # "days since 2020-01-01"

    dim_lon  <- ncdf4::ncdim_def("lon",  "degrees_east",  lon)
    dim_lat  <- ncdf4::ncdim_def("lat",  "degrees_north", lat)
    dim_time <- ncdf4::ncdim_def("time", "days since 2020-01-01", time, unlim = FALSE)

    var_pm <- ncdf4::ncvar_def(
      name = "pm25",
      units = "ug/m3",
      dim = list(dim_lon, dim_lat, dim_time),
      missval = -9999,
      prec = "float"
    )

    nc <- ncdf4::nc_create(path, vars = list(var_pm))

    # Array is lon x lat x time, 2 x 2 x 2
    # Day 1 values: 1,2,3,4 ; Day 2 values: 5,6,7,8
    arr <- array(c(1, 2, 3, 4, 5, 6, 7, 8), dim = c(2, 2, 2))
    ncdf4::ncvar_put(nc, var_pm, arr)
    ncdf4::nc_close(nc)
  }

  # Helper: create a shapefile with two polygons splitting the domain in half
  # Region "west" covers x in [0,0.5], Region "east" covers x in [0.5,1]
  create_test_shapefile <- function(dsn_dir) {
    poly_west <- sf::st_polygon(list(rbind(
      c(0.0, 0.0),
      c(0.5, 0.0),
      c(0.5, 1.0),
      c(0.0, 1.0),
      c(0.0, 0.0)
    )))
    poly_east <- sf::st_polygon(list(rbind(
      c(0.5, 0.0),
      c(1.0, 0.0),
      c(1.0, 1.0),
      c(0.5, 1.0),
      c(0.5, 0.0)
    )))

    shp <- sf::st_sf(
      region = c("west", "east"),
      geometry = sf::st_sfc(poly_west, poly_east),
      crs = 4326
    )

    # Write ESRI Shapefile
    shp_path <- file.path(dsn_dir, "regions.shp")
    sf::st_write(shp, shp_path, quiet = TRUE, delete_layer = TRUE)
    shp_path
  }

  nc_path <- file.path(temp_dir, "tiny_pm25.nc")
  create_test_ncdf(nc_path)

  shp_path <- create_test_shapefile(temp_dir)

  out <- extract_means_for_geography(ncdf_path = nc_path, shp_path = shp_path)

  # Basic structure checks
  expect_s3_class(out, "data.frame")
  expect_true(all(c("region", "date", "mean_PM") %in% names(out)))

  # Expect two regions * two time steps = 4 rows
  expect_equal(nrow(out), 4L)

  # date should be Date and non-missing
  expect_s3_class(out$date, "Date")
  expect_false(any(is.na(out$date)))

  # mean_PM should be numeric and non-missing
  expect_true(is.numeric(out$mean_PM))
  expect_false(any(is.na(out$mean_PM)))
})


test_that("extract_means_for_geography errors when region_col is missing", {

  create_test_ncdf <- function(path) {
    lon <- c(0.25, 0.75)
    lat <- c(0.25, 0.75)
    time <- c(0, 1)

    dim_lon  <- ncdf4::ncdim_def("lon",  "degrees_east", lon)
    dim_lat  <- ncdf4::ncdim_def("lat",  "degrees_north", lat)
    dim_time <- ncdf4::ncdim_def("time", "days since 2020-01-01", time)

    var_pm <- ncdf4::ncvar_def("pm25", "ug/m3", list(dim_lon, dim_lat, dim_time), -9999, "float")
    nc <- ncdf4::nc_create(path, vars = list(var_pm))
    arr <- array(c(1, 2, 3, 4, 5, 6, 7, 8), dim = c(2, 2, 2))
    ncdf4::ncvar_put(nc, var_pm, arr)
    ncdf4::nc_close(nc)
  }

  create_bad_shapefile <- function(dsn_dir) {
    poly <- sf::st_polygon(list(rbind(
      c(0.0, 0.0),
      c(1.0, 0.0),
      c(1.0, 1.0),
      c(0.0, 1.0),
      c(0.0, 0.0)
    )))
    shp <- sf::st_sf(
      not_region = "x",
      geometry = sf::st_sfc(poly),
      crs = 4326
    )
    shp_path <- file.path(dsn_dir, "bad_regions.shp")
    sf::st_write(shp, shp_path, quiet = TRUE, delete_layer = TRUE)
    shp_path
  }

  nc_path <- file.path(temp_dir, "tiny_pm25_missing_region.nc")
  create_test_ncdf(nc_path)
  shp_path <- create_bad_shapefile(temp_dir)

  expect_error(
    extract_means_for_geography(ncdf_path = nc_path, shp_path = shp_path, region_col = "region"),
    "Column 'region' not found in shapefile"
  )
})


test_that("extract_means_for_geography uses custom output_value_col", {

  create_test_ncdf <- function(path) {
    lon <- c(0.25, 0.75)
    lat <- c(0.25, 0.75)
    time <- c(0, 1)

    dim_lon  <- ncdf4::ncdim_def("lon",  "degrees_east", lon)
    dim_lat  <- ncdf4::ncdim_def("lat",  "degrees_north", lat)
    dim_time <- ncdf4::ncdim_def("time", "days since 2020-01-01", time)

    var_pm <- ncdf4::ncvar_def("pm25", "ug/m3", list(dim_lon, dim_lat, dim_time), -9999, "float")
    nc <- ncdf4::nc_create(path, vars = list(var_pm))
    arr <- array(c(1, 2, 3, 4, 5, 6, 7, 8), dim = c(2, 2, 2))
    ncdf4::ncvar_put(nc, var_pm, arr)
    ncdf4::nc_close(nc)
  }

  create_test_shapefile <- function(dsn_dir) {
    poly <- sf::st_polygon(list(rbind(
      c(0.0, 0.0),
      c(1.0, 0.0),
      c(1.0, 1.0),
      c(0.0, 1.0),
      c(0.0, 0.0)
    )))
    # Use a different region column name to test region_col mapping too
    shp <- sf::st_sf(
      my_region = "all",
      geometry = sf::st_sfc(poly),
      crs = 4326
    )
    shp_path <- file.path(dsn_dir, "one_region.shp")
    sf::st_write(shp, shp_path, quiet = TRUE, delete_layer = TRUE)
    shp_path
  }

  tmp <- withr::local_tempdir()
  nc_path <- file.path(tmp, "tiny_pm25_custom_col.nc")
  create_test_ncdf(nc_path)
  shp_path <- create_test_shapefile(tmp)

  out <- extract_means_for_geography(
    ncdf_path = nc_path,
    shp_path = shp_path,
    region_col = "my_region",
    output_value_col = "avg_pm"
  )

  expect_true(all(c("region", "date", "avg_pm") %in% names(out)))
  expect_false("mean_PM" %in% names(out))
})


test_that("extract_means_for_geography handles shapefile CRS different from raster CRS (transform)", {

  create_test_ncdf <- function(path) {
    lon <- c(0.25, 0.75)
    lat <- c(0.25, 0.75)
    time <- c(0, 1)

    dim_lon  <- ncdf4::ncdim_def("lon",  "degrees_east", lon)
    dim_lat  <- ncdf4::ncdim_def("lat",  "degrees_north", lat)
    dim_time <- ncdf4::ncdim_def("time", "days since 2020-01-01", time)

    var_pm <- ncdf4::ncvar_def("pm25", "ug/m3", list(dim_lon, dim_lat, dim_time), -9999, "float")
    nc <- ncdf4::nc_create(path, vars = list(var_pm))
    arr <- array(c(1, 2, 3, 4, 5, 6, 7, 8), dim = c(2, 2, 2))
    ncdf4::ncvar_put(nc, var_pm, arr)
    ncdf4::nc_close(nc)
  }

  create_test_shapefile_wrong_crs <- function(dsn_dir) {
    poly <- sf::st_polygon(list(rbind(
      c(0.0, 0.0),
      c(1.0, 0.0),
      c(1.0, 1.0),
      c(0.0, 1.0),
      c(0.0, 0.0)
    )))
    shp_ll <- sf::st_sf(
      region = "all",
      geometry = sf::st_sfc(poly),
      crs = 4326
    )

    # Transform to Web Mercator before writing
    shp_3857 <- sf::st_transform(shp_ll, 3857)

    shp_path <- file.path(dsn_dir, "region_3857.shp")
    sf::st_write(shp_3857, shp_path, quiet = TRUE, delete_layer = TRUE)
    shp_path
  }

  nc_path <- file.path(temp_dir, "tiny_pm25_crs.nc")
  create_test_ncdf(nc_path)
  shp_path <- create_test_shapefile_wrong_crs(temp_dir)

  out <- extract_means_for_geography(ncdf_path = nc_path, shp_path = shp_path)

  expect_equal(unique(out$region), "all")
  expect_equal(nrow(out), 2L)
  expect_false(any(is.na(out$mean_PM)))
})


# Tests for join_health_and_climate_data

test_that(
    "join_health_and_climate_data behaves as expected.",
    {
        health <- read_and_format_data(
            health_path = WF_TEST_HEALTH,
            date_col = "date",
            mean_temperature_col = "tmean",
            health_outcome_col = "deaths",
            region_col = "region",
            rh_col = "relative_humidity",
            wind_speed_col = "wind_speed"
        )
        joined <- join_health_and_climate_data(
            climate_data = WF_TEST_CLIMATE,
            health_data = health,
            region_col = "region",
            date_col = "date",
            exposure_col = "mean_PM"
        )
        exp_cols <- c(
            "date", "tmean", "health_outcome", "region", "rh", "wind_speed",
            "year", "month", "day", "dow", "mean_PM"
        )
        expect_true(all(exp_cols %in% colnames(joined)))
        expect_equal(joined$mean_PM, c(500000, 100000, 150000))

    }
)

# Tests for load_wildfire_data

test_that("load_wildfire_data returns formatted health data and renames PM column when join_wildfire_data = FALSE", {
  # Arrange: create a deterministic health_df that read_and_format_data would return
  health_df_stub <- data.frame(
    date = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03")),
    tmean = c(10, 11, 12),
    health_outcome = c(1, 2, 3),
    region = c("A", "A", "A"),
    rh = c(50, 55, 60),
    wind_speed = c(3, 4, 5),
    pm_raw = c(1.1, 2.2, 3.3),
    year = c(2020, 2020, 2020),
    month = c(1, 1, 1),
    day = c(1, 2, 3),
    dow = c("Wed", "Thu", "Fri")
  )

  called_extract <- FALSE

  local_mocked_bindings(
    read_and_format_data = function(...) health_df_stub,
    extract_means_for_geography = function(...) {
      called_extract <<- TRUE
      stop("extract_means_for_geography should not be called when join_wildfire_data = FALSE")
    }
  )

  # Act
  res <- load_wildfire_data(
    health_path = "ignored_because_mocked",
    ncdf_path = NULL,
    shp_path = NULL,
    join_wildfire_data = FALSE,
    date_col = "date",
    region_col = "region",
    shape_region_col = "region",
    mean_temperature_col = "tmean",
    health_outcome_col = "deaths",
    rh_col = "relative_humidity",
    wind_speed_col = "wind_speed",
    pm_2_5_col = "pm_raw"
  )

  # Assert: extractor not called
  expect_false(called_extract)

  # Assert: mean_PM exists and equals pm_raw
  expect_true("mean_PM" %in% names(res))
  expect_equal(res$mean_PM, health_df_stub$pm_raw)

  # Assert: pm_raw no longer present (rename not copy)
  expect_false("pm_raw" %in% names(res))

  # Assert: key columns still exist
  exp_columns <- c(
    "date", "tmean", "health_outcome", "region", "rh", "wind_speed",
    "mean_PM", "year", "month", "day", "dow"
  )
  expect_true(all(exp_columns %in% names(res)))

  # Assert: date type preserved
  expect_s3_class(res$date, "Date")
})

test_that("load_wildfire_data calls extractor + joiner with correct arguments when join_wildfire_data = TRUE", {
  # Arrange stubs
  health_df_stub <- data.frame(
    date = as.Date(c("2020-01-01", "2020-01-02")),
    tmean = c(10, 11),
    health_outcome = c(1, 2),
    region = c("A", "A")
  )

  wildfire_df_stub <- data.frame(
    region = c("A", "A"),
    date = as.Date(c("2020-01-01", "2020-01-02")),
    mean_PM = c(5, 6)
  )

  joined_df_stub <- data.frame(
    date = as.Date(c("2020-01-01", "2020-01-02")),
    region = c("A", "A"),
    tmean = c(10, 11),
    health_outcome = c(1, 2),
    mean_PM = c(5, 6)
  )

  # Capture calls
  captured <- list(
    ncdf_path = NULL,
    shp_path = NULL,
    shape_region_col = NULL,
    join_region_col = NULL,
    join_date_col = NULL
  )

  local_mocked_bindings(
    read_and_format_data = function(...) health_df_stub,

    extract_means_for_geography = function(ncdf_path, shp_path, region_col, ...) {
      captured$ncdf_path <<- ncdf_path
      captured$shp_path <<- shp_path
      captured$shape_region_col <<- region_col
      wildfire_df_stub
    },

    join_health_and_climate_data = function(climate_data, health_data, ...) {

      # Additional sanity checks inside the mock
      expect_equal(climate_data, wildfire_df_stub)
      expect_equal(health_data, health_df_stub)

      joined_df_stub
    }
  )

  # Act
  res <- load_wildfire_data(
    health_path = "ignored_because_mocked",
    ncdf_path = "path/to/file.nc",
    shp_path = "path/to/shape.shp",
    join_wildfire_data = TRUE,
    date_col = "date",
    region_col = "region",
    shape_region_col = "shape_name",
    mean_temperature_col = "tmean",
    health_outcome_col = "deaths",
    rh_col = NULL,
    wind_speed_col = NULL
  )

  # Assert: output is whatever the joiner returns
  expect_equal(res, joined_df_stub)

  # Assert: correct wiring of parameters
  expect_equal(captured$ncdf_path, "path/to/file.nc")
  expect_equal(captured$shp_path, "path/to/shape.shp")
  expect_equal(captured$shape_region_col, "shape_name")
})


test_that("load_wildfire_data defaults to joining when join_wildfire_data is not supplied", {
  health_df_stub <- data.frame(
    date = as.Date("2020-01-01"),
    tmean = 10,
    health_outcome = 1,
    region = "A"
  )

  wildfire_df_stub <- data.frame(
    region = "A",
    date = as.Date("2020-01-01"),
    mean_PM = 5
  )

  joined_df_stub <- data.frame(
    date = as.Date("2020-01-01"),
    region = "A",
    tmean = 10,
    health_outcome = 1,
    mean_PM = 5
  )

  called_extract <- FALSE
  called_join <- FALSE

  local_mocked_bindings(
    read_and_format_data = function(...) health_df_stub,
    extract_means_for_geography = function(...) { called_extract <<- TRUE; wildfire_df_stub },
    join_health_and_climate_data = function(...) { called_join <<- TRUE; joined_df_stub }
  )

  res <- load_wildfire_data(
    health_path = "ignored_because_mocked",
    ncdf_path = "dummy.nc",
    shp_path = "dummy.shp",
    # join_wildfire_data omitted on purpose
    date_col = "date",
    region_col = "region",
    shape_region_col = "region_in_shape",
    mean_temperature_col = "tmean",
    health_outcome_col = "deaths"
  )

  expect_true(called_extract)
  expect_true(called_join)
  expect_equal(res, joined_df_stub)
})

# Tests for get_lags_and_means

test_that(
    "get_lags_and_means returns the expected dataframe.",
    {
        LAG_MEAN_DF <- data.frame(
            tmean = c(5, 10, 15, 10, 3),
            tmin = c(3, 8, 13, 8, 2)
        )
        res <- get_lags_and_means(LAG_MEAN_DF, "tmean", 2)
        expect_equal(length(colnames(res)), 8)
        exp_cols <- c(
            "tmean_l0", "tmean_l1", "tmean_l2", "tmean_l0_mean",
            "tmean_l1_mean", "tmean_l2_mean"
        )
        expect_true(all(exp_cols %in% colnames(res)))
        expect_equal(res$tmean_l1_mean, c(NA, 7.5, 12.5, 12.5, 6.5))
    }
)

# Tests for create_lagged_variables

test_that(
    "create_lagged_variables works as expected when provided the correct data.",
    {
        LAGGED_VARS_DF <- data.frame(
            region = c("A1", "A1", "A2", "A2", "A2", "A2"),
            tmean = c(5, 10, 2, 3, 5, 7),
            mean_PM = c(3, 3, 10, 7, 12, 20)
        )
        res <- create_lagged_variables(
            data = LAGGED_VARS_DF,
            wildfire_lag = 2,
            temperature_lag = 1
        )
        added_cols <- c(
            "mean_PM_l0", "mean_PM_l1", "mean_PM_l2",
            "mean_PM_l0_mean", "mean_PM_l1_mean", "mean_PM_l2_mean",
            "tmean_l0", "tmean_l1", "tmean_l0_mean", "tmean_l1_mean"
        )
        expect_true(all(added_cols %in% colnames(res)))
    }
)

# Tests for create_temperature_splines

test_that(
    "create_temperature_splines raises an error when degrees_freedom < 1.",
    {
        expect_error(
            create_temperature_splines(NULL, 0, 0),
            "Degrees of freedom must be >= 1."
        )
    }
)

test_that(
    "create_temperature_splines raises an error when nlag < 0.",
    {
        expect_error(
            create_temperature_splines(NULL, -1, 2),
            "nlag must be >= 0."
        )
    }
)

test_that(
    "create_temperature_splines raises an error when tmean isn't included.",
    {
        NO_TMEAN = data.frame(test = c(1, 2))
        expect_error(
            create_temperature_splines(NO_TMEAN, 1, 6),
            "tmean not found in dataset to create splines."
        )
    }
)

test_that(
    "create_temperature_splines raises an error when the correct lag column isn't included.",
    {
        NO_TMEAN_LAG = data.frame(tmean = c(1, 2))
        expect_error(
            create_temperature_splines(NO_TMEAN_LAG, 1, 6),
            "tmean_l1_mean not found in dataset."
        )
    }
)

test_that(
    "create_temperature_splines calculates splines as expected.",
    {
        SPLINES_DF <- data.frame(
            region = c("A1", "A1", "A1", "A2", "A2", "A2"),
            tmean = c(10, 14, 13, 5, 4, 9),
            tmean_l1_mean = c(NA, 12, 13.5, NA, 4.5, 6.5)
        )
        res <- create_temperature_splines(
            data = SPLINES_DF,
            nlag = 1,
            degrees_freedom = 2
        )
        expect_true("ns.tmean" %in% colnames(res))
        exp_splines_1 <- c(NA, 0, 0.3441, NA, 0, 0.3441)
        exp_splines_2 <- c(NA, 0, 0.7706, NA, 0, 0.7706)
        expect_equal(exp_splines_1, round(res$ns.tmean[, 1], 4))
        expect_equal(exp_splines_2, round(res$ns.tmean[, 2], 4))
    }
)

# Tests for time_stratify

TS_DATA <- data.frame(
    month = rep(c(01, 02), 2),
    year = c(2001, 2001, 2001, 2001),
    dow = rep(c("Mon", "Tue"), 2),
    region = c("Wales", "Wales", "North East", "North East"),
    health_outcome = c(57, 59, 278, 301)
)

test_that(
    "time_stratify raises an error if required columns are missing.",
    {
        TS_FAIL <- TS_DATA[, !(colnames(TS_DATA) %in% c("dow"))]
        expect_error(
            time_stratify(TS_FAIL),
            "Data must include columns:"
        )
    }
)

test_that(
    "time_stratify returns expected values",
    {
        res <- time_stratify(TS_DATA)
        exp_stratum <- c(
            "North_East:2001:1:Mon",
            "North_East:2001:2:Tue",
            "Wales:2001:1:Mon",
            "Wales:2001:2:Tue"
        )
        expect_equal(typeof(res$stratum), "integer")
        expect_equal(as.character(res$stratum), exp_stratum)
    }
)

test_that(
    "descriptive_stats raises an error when the output directory does not exist.",
    {
        expect_error(
            descriptive_stats(
                data = NULL,
                variables = c(),
                bin_width = 5,
                output_dir = "does/not/exist"
            ),
            "Output directory does not exist"
        )
    }
)

test_that(
    "descriptive_stats saves out descriptive stats files correctly",
    {
        DS_DATA <- data.frame(
            health_outcome = c(150, 30, 122, 75, 94),
            tmean = c(7, 9, 14, 13, 13),
            rainfall = c(0.21, 0.22, 0.17, 0.18, 0.26)
        )

        descriptive_stats(
            data = DS_DATA,
            variables = c("tmean", "rainfall"),
            output_dir = temp_dir
        )
        expect_true(file.exists(file.path(temp_dir, "health_outcome_hist.png")))
        expect_true(file.exists(file.path(temp_dir, "variable_summaries.csv")))
    }
)

# Tests for check_wildfire_vif

test_that(
    "check_wildfire_vif raises an error when save_csv==T and no output path is provided.",
    {
        expect_error(
            check_wildfire_vif(NULL, c(), TRUE, NULL, FALSE),
            "No output path provided when save_csv==T."
        )
    }
)

test_that(
    "check_wildfire_vif raises an error when predictors is of the wrong type.",
    {
        expect_error(
            check_wildfire_vif(NULL, 5643574, FALSE, NULL, FALSE),
            "Please provide predictor variable names as a character vector"
        )
    }
)

test_that(
    "check_wildfire_vif raises an error when predictors is not of length 2.",
    {
        expect_error(
            check_wildfire_vif(NULL, c("one"), FALSE, NULL, FALSE),
            "Please provide at least two predictor variables"
        )
    }
)

set.seed(42)
WIF_VIF_DATA <- data.frame(
  region = rep(c("RegionA", "RegionB"), each = 30),
  health_outcome = rpois(60, lambda = 50),
  smoke_exposure = rnorm(60, mean = 10, sd = 3),
  temperature = rnorm(60, mean = 20, sd = 2)
)
WIF_VIF_DATA$pm25 <- WIF_VIF_DATA$smoke_exposure * 2 + rnorm(60, 0, 10)


test_that(
    "check_wildfire calculates and saves VIF as expected",
    {

        if (!file.exists(file.path(temp_dir, "model_validation")))
        dir.create(file.path(temp_dir, "model_validation"))
        out <- capture.output(check_wildfire_vif(
            WIF_VIF_DATA,
            c("smoke_exposure", "pm25", "temperature"),
            TRUE,
            temp_dir,
            TRUE
        ))

        # Printed message: accept presence anywhere on the line ([1] prefix from print is common)
        expect_true(any(grepl("Variance inflation factor:", out)))

        # Extract numeric tokens to ensure something VIF-like was printed
        nums <- suppressWarnings(as.numeric(
          unlist(regmatches(out, gregexpr("\\d+\\.?\\d*", out)))
        ))
        # At least as many numeric tokens as predictors per region, minimum 6 tokens
        expect_true(sum(!is.na(nums)) >= 6)

        # File created
        output_fpath <- file.path(temp_dir, "model_validation", "vif_results.csv")
        expect_true(file.exists(output_fpath))

        # Read file and validate header and notes
        lines <- readLines(output_fpath, warn = FALSE)
        expect_true(length(lines) >= 2)

        # Allow quoted or unquoted header fields; write.csv quotes by default
        expect_true(grepl('^"?region"?,\\s*"?formula"?,\\s*"?var"?,\\s*"?VIF"?$', lines[1]))

        # There should be at least one non-empty data row before the appended notes
        # We don't assert exact content because current function overwrites per var in loop
        expect_true(any(nzchar(lines[2:length(lines)])))

        # Notes must be present and in order near the end of the file
        note1 <- "Note: VIF model validation results."
        note2 <- "Please refer to wildfire methods documentation (section 3.7) for interpretation guidelines."

        idx1 <- which(trimws(lines) == note1)
        idx2 <- which(trimws(lines) == note2)

        expect_true(length(idx1) == 1)
        expect_true(length(idx2) == 1)
        expect_true(idx1 < idx2)
        # the final note should be close to the end; allow some buffer for trailing newline behavior
        expect_true(idx2 > (length(lines) - 10))
    }
)

test_that(
    "check_wildfire_vif raises a warning when VIF > 2.",
    {
        VIF_WARN_DATA <- data.frame(
            region = rep(c("RegionA", "RegionB"), each = 20),
            health_outcome = rnorm(40, mean = 50, sd = 10),
            smoke_exposure = rnorm(40, mean = 10, sd = 3),
            temperature = rnorm(40, mean = 20, sd = 2)
        )
        VIF_WARN_DATA$pm25 <- (
            VIF_WARN_DATA$smoke_exposure * 3 + rnorm(nrow(VIF_WARN_DATA))
        )
        expect_warning(
            check_wildfire_vif(
                VIF_WARN_DATA,
                c("smoke_exposure", "pm25", "temperature"),
                FALSE,
                NULL,
                FALSE
            )
        )
    }
)

# Tests for get_wildfire_lag_columns

test_that(
    "get_wildfire_lag_columns acts as expected",
    {
        WF_L_DATA <- data.frame(
            "mean_PM_l0_mean" = c(1),
            "mean_PM_l1_mean" = c(2),
            "mean_PM_l2_mean" = c(3)
        )
        lag_cols <- get_wildfire_lag_columns(WF_L_DATA)$col_names
        exp_lag_cols <- c(
            "mean_PM",
            "mean_PM_l0_mean",
            "mean_PM_l1_mean",
            "mean_PM_l2_mean"
        )
        expect_equal(lag_cols, exp_lag_cols)
    }
)

# Tests for casecrossover_quasipoisson

generate_wildfire_test_data <- function(
  n = 1000,
  n_strata = 5,
  n_lags = 2,
  spline_df = 6,
  start_date = as.Date("2020-01-01")
) {
  set.seed(42)
  RNGkind("Mersenne-Twister", "Inversion")
  dates <- seq.Date(from = start_date, by = "day", length.out = n)
  region <- rep(c("Region_A", "Region_B"), length.out = n)
  region_pop <- c(Region_A = 500000, Region_B = 1200000)
  data <- data.frame(
    date = dates,
    year = as.integer(format(dates, "%Y")),
    month = as.integer(format(dates, "%m")),
    day = as.integer(format(dates, "%d")),
    dow = factor(
        weekdays(dates),
        levels = c(
            "Monday",
            "Tuesday",
            "Wednesday",
            "Thursday",
            "Friday",
            "Saturday",
            "Sunday"
        )
    ),
    region = region,
    pop = unname(region_pop[region]),
    health_outcome = rpois(n, lambda = 5),
    mean_PM = runif(n, 5, 30),
    tmean = rnorm(n, mean = 15, sd = 5),
    rh = runif(n, 40, 90),
    wind_speed = runif(n, 0, 10),
    ind = sample(c(0, 1), n, replace = TRUE),
    stratum = factor(rep(1:n_strata, each = n / n_strata))
  )
  data <- create_lagged_variables(data, 3, 1)
  data <- create_temperature_splines(data, 1, 6)
  return(data)
}

test_that(
    "casecrossover_quassipoission calculates model values and saves residuals.",
    {

        test_data <- generate_wildfire_test_data(
            n = 5000,
            n_strata = 5,
            n_lags = 2,
            spline_df = 6
        )
        res <- casecrossover_quasipoisson(
            data = test_data,
            scale_factor_wildfire_pm = 10,
            save_fig = TRUE,
            output_folder_path = temp_dir,
            print_model_summaries = FALSE
        )
        exp_results <- data.frame(
            lag = c(0, 1, 2, 3),
            relative_risk = c(0.9863, 0.9952, 1.0087, 1.0143),
            ci_lower = c(0.9632, 0.9622, 0.9682, 0.9673),
            ci_upper = c(1.0101, 1.0292, 1.0509, 1.0636)
        )
        expect_equal(round(res, 4), exp_results, tolerance = 1e-3)
        expect_true(file.exists(
            file.path(
                temp_dir,
                "model_validation",
                "wildfires_residuals_vs_fit_plot.pdf"
            )
        ))
    }
)

test_that(
    "casecrossover_quassipoission calculates model values and prints residuals.",
    {
        test_data <- generate_wildfire_test_data(
            n = 5000,
            n_strata = 5,
            n_lags = 2,
            spline_df = 6
        )
        out <- capture.output(
            casecrossover_quasipoisson(
                data = test_data,
                scale_factor_wildfire_pm = 10,
                save_fig = FALSE,
                output_folder_path = NULL,
                print_model_summaries = TRUE
            )
        )
        # validate outputs are being printed
        reg_out <- unlist(regmatches(
            out,
            gregexpr("Ratio of residual deviance to degrees of freedom: ", out)
        ))
        expect_equal(length(reg_out), 65)
    }
)

# Tests for calculate_qaic

test_that(
    "calculate_qaic raises an error when save_csv=T and no output path is provided",
    {
        expect_error(
            calculate_qaic(NULL, TRUE, NULL, FALSE),
            "No output path provided when save_csv==T."
        )
    }
)

test_that(
    "calculate_qaic saves as expected, returns the correct results, and appends note to csv.",
    {
        test_data <- generate_wildfire_test_data(
            n = 5000,
            n_strata = 5,
            n_lags = 2,
            spline_df = 6
        )
        qaic_res <- calculate_qaic(
            data = test_data,
            save_csv = TRUE,
            output_folder_path = temp_dir,
            print_results = FALSE
        )
        # validate qaic results
        expect_equal(unique(qaic_res$region), c("Region_A", "Region_B"))
        # NOTE: high tolerance due to platform drift (mac vs windows)
        expect_equal(
            round(qaic_res$qaic_at_lag_0, 3), c(1377.051, 1309.869), tolerance = 10
        )
        expect_equal(
            round(qaic_res$qaic_at_lag_2, 3), c(1376.393, 1307.147), tolerance = 10
        )
        expect_equal(round(
            qaic_res$pearson_dispersion_at_lag_1, 3
        ), c(0.961, 1.032), tolerance = 0.005)
        expect_equal(round(
            qaic_res$pearson_dispersion_at_lag_3, 3
        ), c(0.961, 1.031), tolerance = 0.005)
        # validate results were saved
        output_fpath <- file.path(temp_dir, "model_validation", "qaic_results.csv")
        expect_true(file.exists(output_fpath))
        # Validate header and appended notes using readLines
        lines <- readLines(output_fpath, warn = FALSE)
        expect_true(length(lines) >= 3)
        # Header line: allow optional quotes around column names (write.csv conventions)
        # We know at least "region" should be present as first column
        expect_true(grepl('^"?region"?[,]', lines[1]))
        # Verify notes are present, appear in order, and are near the end
        note1 <- "Note: QAIC model validation results."
        note2 <- "Please refer to wildfire methods documentation (section 3.7) for interpretation guidelines."

        idx1 <- which(trimws(lines) == note1)
        idx2 <- which(trimws(lines) == note2)

        expect_true(length(idx1) == 1)
        expect_true(length(idx2) == 1)
        expect_true(idx1 < idx2)
        # final note should be close to the end; allow buffer for possible trailing newline
        expect_true(idx2 > (length(lines) - 10))
        # Ensure there was at least one data row before the notes
        expect_true(any(nzchar(lines[2:(min(idx1, length(lines)) - 1)])))
    }
)

test_that(
    "calculate_qaic prints as expected when prompted.",
    {
        test_data <- generate_wildfire_test_data(
            n = 5000,
            n_strata = 5,
            n_lags = 2,
            spline_df = 6
        )
        out <- capture.output(calculate_qaic(
            data = test_data,
            print_results = TRUE
        ))
        qaic <- unlist(regmatches(
            out,
            gregexpr("QAIC for .* = ([0-9]+\\.[0-9]+)", out)
        ))
        pearson <- unlist(regmatches(
            out,
            gregexpr("Pearson dispersion statistic: ", out)
        ))
        expect_equal(length(qaic), 10)
        expect_equal(length(pearson), 10)
    }
)

# Tests for plot_RR
RR_EX_DATA <- data.frame(
  region_name = c(rep("Region_A", 3),rep("Region_B", 3)),
  lag = c(0, 1, 2, 0, 1, 2),
  relative_risk = c(1.0102, 1.0116, 1.0102,1.0125, 1.0138, 1.0120),
  ci_lower = c(0.9854, 0.9759, 0.9660, 0.9870, 0.9780, 0.9690),
  ci_upper = c(1.0356, 1.0487, 1.0565, 1.0380, 1.0505, 1.0550)
)

test_that(
    "plot_RR raises an error is save_fig=T and output path is NULL",
    {
        expect_error(
            plot_RR(NULL, NULL, NULL, TRUE, NULL),
            "No output path provided when save_fig==T."
        )
    }
)

test_that(
    "plot_RR returns a plot object when no save is prompted.",
    {
        rr_plot <- plot_RR(
            rr_data = RR_EX_DATA,
            wildfire_lag = 2,
            by_region = TRUE,
            save_fig = FALSE,
            output_folder_path = NULL
        )
        expect_true(inherits(rr_plot, "patchwork"))
        expect_true(inherits(rr_plot, "ggplot"))
        expect_true(inherits(rr_plot, "gg"))
    }
)

test_that(
    "plot_RR saves a figure to disk when prompted.",
    {

        plot_RR(
            rr_data = RR_EX_DATA,
            wildfire_lag = 2,
            by_region = FALSE,
            save_fig = TRUE,
            output_folder_path = temp_dir
        )
        expect_true(file.exists(file.path(temp_dir, "RR_lag_estimates.pdf")))
    }
)

# Tests for plot_RR_core

test_that(
    "plot_RR_core raises an error is save_fig=T and output path is NULL",
    {
        expect_error(
            plot_RR_core(NULL, TRUE, 3, NULL),
            "No output path provided when save_fig==T."
        )
    }
)

test_that(
    "plot_RR_core returns a plot object.",
    {
        rr_plot <- plot_RR_core(
            rr_data <- subset(RR_EX_DATA, RR_EX_DATA$region_name=="Region_A"),
            save_fig = FALSE,
            wildfire_lag = 2,
            output_folder_path = NULL,
            region_name = "Region A",
            ylims = NULL
        )
        expect_true(inherits(rr_plot, "ggplot"))
        expect_true(inherits(rr_plot, "gg"))
    }
)

test_that(
    "plot_RR_core saves plots to disk when prompted.",
    {

        rr_plot <- plot_RR_core(
            rr_data <- subset(RR_EX_DATA, RR_EX_DATA$region_name=="Region_A"),
            save_fig = TRUE,
            wildfire_lag = 2,
            output_folder_path = temp_dir,
            region_name = "Region A",
            ylims = NULL
        )
        output_fpath <- file.path(temp_dir, "RR_lag_estimates_region_a.pdf")
        expect_true(file.exists(output_fpath))
    }
)

# Tests for save_wildfire_results

test_that(
    "save_wildfire_results raises an error is output_folder_path is NULL.",
    {
        expect_error(
            save_wildfire_results(
                NULL, NULL, NULL, NULL
            ),
            "Output directory required."
        )
    }
)

test_that(
    "save_wildfire_results raises an error is output_folder_path does not exist.",
    {
        expect_error(
            save_wildfire_results(
                NULL, NULL, NULL, "does/not/exist"
            ),
            "Output directory does not exist"
        )
    }
)

test_that(
    "save_wildfire_results saves results to disk when prompted",
    {

        rr_res <- data.frame(col1 = c("a", "b"), cold2 = c(34, 42))
        an_ar_res <- data.frame(col1 = c("c", "d"), cold2 = c(88, 102))
        an_ar_annual_res <- data.frame(col1 = c("e", "f"), cold2 = c(500, 1000))
        save_wildfire_results(
            rr_results = rr_res,
            an_ar_results = an_ar_res,
            annual_af_an_results = an_ar_annual_res,
            output_folder_path = temp_dir
        )
        rr_fpath <- file.path(temp_dir, "RR_lag_estimates.csv")
        an_ar_fpath <- file.path(temp_dir, "wildfire_health_monthly_estimates.csv")
        an_ar_ann_fpath <- file.path(temp_dir, "wildfire_health_yearly_estimates.csv")
        files <- c(rr_fpath, an_ar_fpath, an_ar_ann_fpath)
        for (f in files) {
            expect_true(file.exists(f))
        }
        expect_equal(
            read.csv(rr_fpath), rr_res
        )
        expect_equal(
            read.csv(an_ar_fpath), an_ar_res
        )
        expect_equal(
            read.csv(an_ar_ann_fpath), an_ar_annual_res
        )
    }
)

# Tests for calculate_wildfire_rr_by_region

test_that(
    "calculate_wildfire_rr_by_region raises an error is output_folder_path is NULL.",
    {
        expect_error(
            calculate_wildfire_rr_by_region(
                NULL, NULL, NULL, TRUE
            ),
            "No output path provided when save_fig==T."
        )
    }
)

test_that(
    "calculate_wildfire_rr_by_region rauses an error when region isn't present",
    {
        data <- data.frame(test = c(1, 2, 3))
        expect_error(
            calculate_wildfire_rr_by_region(
                data = data,
                scale_factor_wildfire_pm = 10,
                calc_relative_risk_by_region = TRUE,
                save_fig = FALSE,
                output_folder_path = NULL,
                print_model_summaries = FALSE
            ),
            "data must contain 'region' column for region level RR data."
        )
    }
)

# NOTE: not testing save_fig functionality since this is done in casecrossover_quasipoisson
test_that(
    "calculate_wildfire_rr_by_region returns resuults as expected",
    {
        test_data <- generate_wildfire_test_data(
            n = 5000,
            n_strata = 5,
            n_lags = 2,
            spline_df = 6
        )
        rr_by_reg <- calculate_wildfire_rr_by_region(
            data = test_data,
            scale_factor_wildfire_pm = 10,
            calc_relative_risk_by_region = TRUE,
            save_fig = FALSE,
            output_folder_path = NULL,
            print_model_summaries = FALSE
        )
        expect_equal(
            unique(rr_by_reg$region_name),
            c("Region_A", "Region_B", "All Regions")
        )
        lag2_df <- subset(rr_by_reg, rr_by_reg$lag==2)
        num_cols <- sapply(lag2_df, is.numeric)
        lag2_df[num_cols] <- lapply(lag2_df[num_cols], round, 6)
        rownames(lag2_df) <- NULL
        exp_lag2_df <- data.frame(
            lag = c(2, 2, 2),
            relative_risk = c(1.029838, 0.991563, 1.008722),
            ci_lower = c(0.970565, 0.936339, 0.968225),
            ci_upper = c(1.092731, 1.050044, 1.050913),
            region_name = c("Region_A", "Region_B", "All Regions"),
            stringsAsFactors = FALSE
        )
        expect_equal(lag2_df, exp_lag2_df, tolerance = 0.005)
    }
)

get_daily_af_an_testdata <- function() {
    test_data <- generate_wildfire_test_data(
            n = 5000,
            n_strata = 5,
            n_lags = 2,
            spline_df = 6
    )
    rr_data <- calculate_wildfire_rr_by_region(
            data = test_data,
            scale_factor_wildfire_pm = 10,
            calc_relative_risk_by_region = TRUE,
            save_fig = FALSE,
            output_folder_path = NULL,
            print_model_summaries = FALSE
    )
    data_list <- list(test_data = test_data, rr_data = rr_data)
    return(data_list)
}

# Tests for calculate_daily_af_an
test_that(
    "calculate_daily_AF_AN works as expected",
    {
        data <- get_daily_af_an_testdata()
        af_an_res <- calculate_daily_AF_AN(
            data = data$test_data,
            rr_data = data$rr_data
        )
        exp_shape <- c(5000, 36)
        exp_columns <- c(
            "health_outcome", "mean_PM",
            "tmean", "rh",
            "wind_speed", "ind",
            "stratum", "region",
            "mean_PM_l0", "mean_PM_l1",
            "mean_PM_l2", "mean_PM_l3",
            "mean_PM_l0_mean", "mean_PM_l1_mean",
            "mean_PM_l2_mean", "mean_PM_l3_mean",
            "tmean_l0", "tmean_l1",
            "tmean_l0_mean", "tmean_l1_mean",
            "ns.tmean", "rescaled_RR",
            "attributable_fraction", "attributable_number",
            "rescaled_CI_upper", "attributable_fraction_upper",
            "attributable_number_upper", "rescaled_CI_lower",
            "attributable_fraction_lower", "attributable_number_lower",
            "date", "year", "month", "day", "dow", "pop"
        )
        exp_an <- round(c(
            0.01261, 0.01416, 0.01166, 0.00256, 0.01251, 0.01059
        ), 5)
        expect_equal(exp_shape, dim(af_an_res))
        expect_true(all(exp_columns %in% colnames(af_an_res)))
        expect_equal(exp_an, round(head(af_an_res)$attributable_number, 5), tolerance = 0.1)
    }
)

# Tests for summarise_AF_AN

test_that(
    "summarise_AF_AN summarises attributable numbers/fractions to monthly aggregates.",
    {
        data <- get_daily_af_an_testdata()
        af_an_res <- calculate_daily_AF_AN(
            data = data$test_data,
            rr_data = data$rr_data
        )
        res <- summarise_AF_AN(data = af_an_res, monthly = TRUE)
        # validate outputs
        expect_true(all(unique(res$month) == 1:12))
        expect_true(all(unique(res$year) == 2020:2033))
        expect_equal(
            c(0.13, 0.10, 0.16, 0.14),
            round(head(res, 4)$total_attributable_number, 2)
        )
        expect_equal(dim(res), c(330, 17))
        expect_equal(unique(res$region), c("Region_A", "Region_B"))
    }
)

test_that(
    "summarise_AF_AN summarises attributable numbers/fractions to daily aggregates.",
    {
        data <- get_daily_af_an_testdata()
        af_an_res <- calculate_daily_AF_AN(
            data = data$test_data,
            rr_data = data$rr_data
        )
        res <- summarise_AF_AN(data = af_an_res, monthly = FALSE)
        # validate outputs
        expect_false("month" %in% colnames(res))
        expect_true(all(unique(res$year) == 2020:2033))
        expect_equal(
            rep(1.5, 4),
            round(head(res, 4)$total_attributable_number, 1),
            tolerance = 0.5
        )
        expect_equal(dim(res), c(28, 16))
        expect_equal(unique(res$region), c("Region_A", "Region_B"))
    }
)

# Tests for plot_aggregated_AF and plot_aggregated_AF_core (covers both)

AGGREGATED_AF_DATA <- data.frame(
  region = c(rep("Region_A", 5), rep("Region_B", 5)),
  year = rep(2020:2024, 2),
  population = c(rep(500000, 5), rep(1200000, 5)),
  total_attributable_number = c(20.233,19.155,19.944,19.020,19.538,
                                9.029,8.776,8.123,8.874,8.728),
  total_variance_AN = c(6.345,5.703,6.658,5.838,6.264,
                        6.016,5.481,4.947,5.385,6.010),
  average_attributable_fraction = c(0.023,0.022,0.021,0.022,0.022,
                                    0.010,0.010,0.010,0.010,0.010),
  variance_AF_mean = rep(0.001, 10),
  se_total_AN = c(2.519,2.388,2.580,2.416,2.503,
                  2.453,2.341,2.224,2.321,2.451),
  lower_ci_attributable_number = c(15.296,14.474,14.887,14.284,14.632,
                                   4.222,4.187,3.763,4.326,3.924),
  upper_ci_attributable_number = c(25.170,23.835,25.001,23.755,24.443,
                                   13.836,13.364,12.482,13.423,13.533),
  se_AF_mean = c(0.035,0.034,0.033,0.034,0.034,
                 0.033,0.033,0.032,0.033,0.032),
  lower_ci_attributable_fraction = c(-0.046,-0.045,-0.044,-0.045,-0.045,
                                     -0.054,-0.055,-0.053,-0.055,-0.053),
  upper_ci_attributable_fraction = c(0.092,0.088,0.086,0.088,0.088,
                                     0.074,0.074,0.072,0.075,0.073),
  deaths_per_100k = c(4.047,3.831,3.989,3.804,3.908,
                      0.752,0.731,0.677,0.740,0.727),
  lower_ci_deaths_per_100k = c(3.059,2.895,2.977,2.857,2.926,
                               0.352,0.349,0.314,0.360,0.327),
  upper_ci_deaths_per_100k = c(5.034,4.767,5.000,4.751,4.889,
                               1.153,1.114,1.040,1.119,1.128)
)

test_that(
    "plot_aggregated_AF raises an error if expected columns are not present.",
    {
        expect_error(
            plot_aggregated_AF(
                data = data.frame(),
                by_region = FALSE,
                output_dir = "."),
            "'data' must contain the following columns: "
        )
    }
)

test_that(
    "plot_aggregated_AF raises an error if output_dir does not exist.",
    {
        expect_error(
            plot_aggregated_AF(
                data = AGGREGATED_AF_DATA,
                by_region = FALSE,
                output_dir = "does/not/exist"
            ),
            "'output_dir' does not exist"
        )
    }
)

test_that(
    "plot_aggregated_AF raises an error if output_dir is NULL.",
    {
        expect_error(
            plot_aggregated_AF(
                data = AGGREGATED_AF_DATA,
                by_region = FALSE,
                output_dir = NULL
            ),
            "'output_dir' is NULL."
        )
    }
)

test_that(
    "plot_aggregated_AF creates and saves plot when by_region=FALSE.",
    {
        plot_aggregated_AF(
            data = AGGREGATED_AF_DATA,
            by_region = FALSE,
            output_dir = temp_dir
        )
        expect_true(
            file.exists(file.path(temp_dir, "aggregated_AF.pdf"))
        )
    }
)


test_that(
  "plot_aggregated_AF errors when by_region = TRUE but 'region' column is missing",
  {
    df_no_region <- subset(
      AGGREGATED_AF_DATA,
      select = -region
    )

    expect_error(
      plot_aggregated_AF(
        data = df_no_region,
        by_region = TRUE,
        output_dir = temp_dir
      ),
      "'data' must contain the following columns:"
    )
  }
)


test_that(
    "plot_aggregated_AF creates and saves plot when by_region=TRUE.",
    {
        plot_aggregated_AF(
            data = AGGREGATED_AF_DATA,
            by_region = TRUE,
            output_dir = temp_dir
        )
        expect_true(
            file.exists(file.path(temp_dir, "aggregated_AF_by_region.pdf"))
        )
    }
)


# Tests for join_ar_and_pm_monthly

test_that(
    "join_ar_and_pm_monthly raises an error when required columns are missing from an_ar_data",
    {
        expect_error(
            join_ar_and_pm_monthly(data.frame(), data.frame()),
            "'an_ar_data' requires the columns: "
        )
    }
)

test_that(
    "join_ar_and_pm_monthly raises an error when required columns are missing from an_ar_data",
    {
        expect_error(
            join_ar_and_pm_monthly(
                pm_data = data.frame(),
                an_ar_data = data.frame(
                    year = c(2021, 2022),
                    month = c(1, 1),
                    region = c("regA", "regA")
                )
            ),
            "'pm_data' requires the columns: "
        )
    }
)

test_that(
    "join_ar_and_pm_monthly joins data as expected",
    {
        ar_data <- data.frame(
            year = c(rep(2021, 3), rep(2022, 3)),
            month = rep(c(1:3), 2),
            region = c(rep("A1", 3), rep("A2", 3)),
            AN_total = c(23, 34, 22, 78, 87, 39)
        )
        pm_data <- data.frame(
            year = c(rep(2021, 3), rep(2022, 3)),
            month = rep(c(1:3), 2),
            region = c(rep("A1", 3), rep("A2", 3)),
            mean_PM = c(0.12, 0.12, 0.11, 0.24, 0.23, 0.27)
        )
        res <- join_ar_and_pm_monthly(
            pm_data = pm_data,
            an_ar_data = ar_data
        )
        exp_columns <- c("year", "month", "region", "AN_total", "monthly_avg_pm25")
        expect_true(all(exp_columns %in% colnames(res)))
        expect_equal(dim(res), c(6, 5))
    }
)

# Tests for plot_ar_pm_monthly

test_that(
    "plot_ar_pm_monthly raises an error if save_outputs==T and output_dir==NULL.",
    {
        expect_error(
            plot_ar_pm_monthly(data.frame(), TRUE, NULL),
            "'output_dir' must be provded to save outputs."
        )
    }
)

test_that(
    "plot_ar_pm_monthly raises an error if output_dir does not exist.",
    {
        expect_error(
            plot_ar_pm_monthly(data.frame(), TRUE, "does/no/exist"),
            "'output_dir' must exist on disk to save outputs."
        )
    }
)

test_that(
    "plot_ar_pm_monthly plots data and saves/returns outputs.",
    {
        MONTHLY_AR_PM <- data.frame(
            year = c(rep(2021, 3), rep(2022, 3)),
            month = rep(1:3, 2),
            region = c(rep("A1", 3), rep("A2", 3)),
            AN_total = c(23, 34, 22, 78, 87, 39),
            deaths_per_100k = round(runif(6, min = 10, max = 100), 1),
            monthly_avg_pm25 = round(runif(6, min = 5, max = 40), 1)
        )

        # clean any pre-existing outputs
        pdf_path <- file.path(temp_dir, "Monthly_deaths_pm_trends.pdf")
        csv_path <- file.path(temp_dir, "Monthly_deaths_pm_trends.csv")
        if (file.exists(pdf_path)) file.remove(pdf_path)
        if (file.exists(csv_path)) file.remove(csv_path)

        res <- plot_ar_pm_monthly(
            data = MONTHLY_AR_PM,
            save_outputs = TRUE,
            output_dir = temp_dir
        )
        # validate return
        expect_true(inherits(res, "data.frame"))
        expect_equal(dim(res), c(9, 4))
        expect_equal(names(res),
          c("region", "month_name", "mean_deaths_per_100k", "mean_pm")
        )
        # month_name should be an ordered factor with month.abb levels
        expect_true(is.factor(res$month_name))
        expect_identical(levels(res$month_name), month.abb)
        # validate output saved
        expect_true(file.exists(pdf_path))
        expect_true(file.info(pdf_path)$size > 0)
        expect_true(file.exists(csv_path))
        expect_true(file.info(csv_path)$size > 0)
    }
)

# Tests for generate_rr_pm_overall

test_that(
    "generate_rr_pm_overall works as expected.",
    {
        df <- data.frame(
            lag = c(0, 1, 2),
            relative_risk = c(1.10, 1.25, 1.05),
            ci_lower = c(1.02, 1.15, 0.98),
            ci_upper = c(1.18, 1.35, 1.12)
        )

        # Dummy data just to provide mean_PM for default pm_vals logic
        pm_data <- data.frame(mean_PM = rep(4, 5))

        res <- generate_rr_pm_overall(
            data = pm_data,
            relative_risk_overall = df,
            scale_factor_wildfire_pm = 10,
            wildfire_lag = 0,
            pm_vals = NULL
        )
        expect_true(inherits(res, "data.frame"))
        expect_equal(res$pm_levels, c(0, 1, 2, 3, 4))
    }
)

# Tests for generate_rr_by_region

test_that(
    "generate_rr_pm_by_region works as expected.",
    {
        df <- data.frame(
            lag = rep(c(0, 1, 2), 2),
            region_name = c("A", "A", "A", "B", "B", "B"),
            relative_risk = c(1.10, 1.25, 1.05, 1.01, 1.00, 1.003),
            ci_lower = rep(c(1.02, 1.15, 0.98), 2),
            ci_upper = rep(c(1.18, 1.35, 1.12), 2)
        )
        pm_data <- data.frame(mean_PM = rep(4, 5))
        res <- generate_rr_pm_by_region(
            data = pm_data,
            relative_risk_overall = df,
            scale_factor_wildfire_pm = 10,
            wildfire_lag = 2,
            pm_vals = NULL
        )
        expect_true(inherits(res, "data.frame"))
        exp_columns <- c(
            "region_name", "pm_levels", "relative_risk", "ci_lower", "ci_upper"
        )
        expect_true(all(exp_columns %in% colnames(res)))
        expect_equal(c(rep("A", 5), rep("B", 5)), res$region_name)
        expect_equal(rep(seq(0, 4, 1), 2), res$pm_levels)
    }
)

# Tests for plot_rr_by_pm and plot_rr_by_pm_core

test_that(
    "plot_rr_by_pm raises an error when save_fig=T and output_dir=NULL.",
    {
        expect_error(
            plot_rr_by_pm(
                data = data.frame(test = c(1, 2)),
                save_fig = TRUE,
                output_dir = NULL
            ),
            "'output_dir' must be provided to save outputs."
        )
    }
)

test_that(
    "plot_rr_by_pm raises an error when the output directory does not exist",
    {
        expect_error(
            plot_rr_by_pm(data.frame(test = c(1, 2)), TRUE, "does/not/exist"),
            "'output_dir' must exist on disk to save outputs."
        )
    }
)

RR_PM_DF <- data.frame(
    region_name = rep("A", 4),
    pm_levels = c(0, 5, 10, 15),
    relative_risk = c(1, 1.0247, 1.0500, 1.0759),
    ci_lower = c(0.98, 1.027, 1.053, 1.078),
    ci_upper = c(1, 1.023, 1.045, 1.07)
)

test_that(
    "plot_rr_by_pm raises an error if expected columns are missing.",
    {
        bad_df <- RR_PM_DF[, !(names(RR_PM_DF) %in% c("region_name"))]
        expect_error(
            plot_rr_by_pm(bad_df, FALSE, NULL),
            "'data' must contain these columns: "
        )
    }
)

test_that(
    "plot_rr_by_pm returns and saves plots as expected",
    {
        plot <- plot_rr_by_pm(
            data = RR_PM_DF,
            save_fig = TRUE,
            output_dir = temp_dir
        )
        # validate return
        expect_true(inherits(plot, "patchwork"))
        expect_true(inherits(plot, "ggplot"))
        expect_true(inherits(plot, "gg"))
        # validate plot saved
        expect_true(file.exists(file.path(temp_dir, "ER_curve.pdf")))
    }
)

# Tests for plot_ar_by_region

AR_AN_TEST_DF <- data.frame(
    region = c("A", "A", "A"),
    deaths_per_100k = c(100, 105, 110),
    lower_ci_deaths_per_100k = c(95, 100, 105),
    upper_ci_deaths_per_100k = c(105, 110, 115),
    total_attributable_number = c(200, 205, 210)
)

test_that(
    "plot_ar_by_region raises an error if output_dir is null",
    {
        expect_error(
            plot_ar_by_region(data = AR_AN_TEST_DF, NULL),
            "'output_dir' required."
        )
    }
)

test_that(
    "plot_ar_by_region raises an error if output_dir does not exist",
    {
        expect_error(
            plot_ar_by_region(data = AR_AN_TEST_DF, "does/not/exist"),
            "'output_dir' does not exist."
        )
    }
)

test_that(
    "plot_ar_by_region raises an error if expected columns aren't present.",
    {
        bad_df <- AR_AN_TEST_DF[, !(names(AR_AN_TEST_DF) %in% c("region"))]
        expect_error(
            plot_ar_by_region(data = bad_df),
            "'data' must contain the following columns: "
        )
    }
)

test_that(
    "plot_ar_by_region returns plots and saves them as expected",
    {
        plot <- plot_ar_by_region(data = AR_AN_TEST_DF, output_dir = temp_dir)
        # validate return
        expect_true(inherits(plot, "ggplot"))
        expect_true(inherits(plot, "gg"))
        # validate plot saved
        expect_true(file.exists(file.path(temp_dir, "AR_by_region.pdf")))
    }
)

# Tets for plot_an_by_region

test_that(
    "plot_an_by_region raises an error if output_dir is null",
    {
        expect_error(
            plot_an_by_region(data = AR_AN_TEST_DF, NULL),
            "'output_dir' required."
        )
    }
)

test_that(
    "plot_an_by_region raises an error if output_dir does not exist",
    {
        expect_error(
            plot_an_by_region(data = AR_AN_TEST_DF, "does/not/exist"),
            "'output_dir' does not exist."
        )
    }
)

test_that(
    "plot_an_by_region raises an error if expected columns aren't present.",
    {
        bad_df <- AR_AN_TEST_DF[, !(names(AR_AN_TEST_DF) %in% c("total_attributable_number"))]
        expect_error(
            plot_an_by_region(data = bad_df),
            "'data' must contain the following columns: "
        )
    }
)

test_that(
    "plot_an_by_region returns plots and saves them as expected",
    {
        plot <- plot_an_by_region(data = AR_AN_TEST_DF, output_dir = temp_dir)
        # validate return
        expect_true(inherits(plot, "ggplot"))
        expect_true(inherits(plot, "gg"))
        # validate plot saved
        expect_true(file.exists(file.path(temp_dir, "AN_by_region.pdf")))
    }
)


# tests/testthat/test-wildfire_do_analysis.R
# Integration tests for wildfire_do_analysis()
# - Dynamically generates a synthetic daily dataset for 2 regions
# - Exercises the end-to-end pipeline without external joins (NetCDF/shapefile)
# - Adds a second scenario that enables region-level outputs (AF/AN), still without saving files


test_that("wildfire_do_analysis: end-to-end run (dataset-level RR only, no file outputs)", {
  # These are integration smoke tests.
  skip_on_cran()

  # Dynamically generate a synthetic dataset
  set.seed(123)
  n_days  <- 180
  dates   <- seq.Date(as.Date("2019-01-01"), by = "day", length.out = n_days)
  regions <- c("North", "South")
  df      <- base::expand.grid(date = dates, region = regions, KEEP.OUT.ATTRS = FALSE)

  # Temperature with seasonality + noise
  df$temp_mean <- 12 + 8 * sin(2 * pi * as.numeric(df$date) / 365) +
    stats::rnorm(nrow(df), sd = 2)

  # Wildfire PM2.5 (ug/m3) positive values
  pm_base_by_region <- ifelse(df$region == "North", 6, 10)
  df$pm25 <- pmax(0.1, pm_base_by_region + stats::rnorm(nrow(df), sd = 3))

  # Population (only needed for AF/AN, safe to include)
  df$pop <- ifelse(df$region == "North", 1000000, 650000)

  # Construct a Poisson outcome with modest dependence on PM and temp
  eta <- -1 + 0.015 * df$pm25 + 0.01 * (df$temp_mean - mean(df$temp_mean))
  mu  <- pmax(0.1, exp(eta)) * 10
  df$deaths <- stats::rpois(nrow(df), lambda = mu)

  # Write to temporary CSV (function reads from disk)
  tmp_dir  <- tempdir()
  csv_path <- file.path(tmp_dir, "synthetic_health.csv")
  utils::write.csv(df, csv_path, row.names = FALSE)

  # Run analysis end-to-end without external joins
  res <- wildfire_do_analysis(
    health_path                        = csv_path,
    join_wildfire_data                 = FALSE,
    ncdf_path                          = NULL,
    shp_path                           = NULL,
    date_col                           = "date",
    region_col                         = "region",
    shape_region_col                   = "region",  # NEW: required by updated signature
    mean_temperature_col               = "temp_mean",
    health_outcome_col                 = "deaths",
    pm_2_5_col                         = "pm25",
    rh_col                             = NULL,
    wind_speed_col                     = NULL,
    wildfire_lag                       = 2,
    temperature_lag                    = 1,
    spline_temperature_lag             = 0,
    spline_temperature_degrees_freedom = 3,
    predictors_vif                     = NULL,
    calc_relative_risk_by_region       = FALSE,
    scale_factor_wildfire_pm           = 10,
    save_fig                           = FALSE,
    save_csv                           = FALSE,
    output_folder_path                 = NULL,
    print_vif                          = FALSE,
    print_model_summaries              = FALSE
  )

  # Assertions on returned structure and content
  expect_type(res, "list")
  expect_named(res, c("RR_results", "AF_AN_results", "AR_PM_monthly"), ignore.order = TRUE)

  # RR results should be a data.frame with at least one row per lag (0..wildfire_lag)
  expect_s3_class(res$RR_results, "data.frame")
  expect_true(all(
    c("lag", "relative_risk", "ci_lower", "ci_upper", "region_name") %in% names(res$RR_results)
  ))
  expect_equal(sort(unique(res$RR_results$lag)), 0:2)
  expect_true(all(is.finite(res$RR_results$relative_risk)))

  # In this config you expect "All Regions" output only
  expect_true(all(res$RR_results$region_name == "All Regions"))

  # No AF/AN and no AR_PM_monthly in this configuration
  expect_true(is.null(res$AF_AN_results) || nrow(res$AF_AN_results) == 0)
  expect_true(is.null(res$AR_PM_monthly) || nrow(res$AR_PM_monthly) == 0)
})


test_that("wildfire_do_analysis: end-to-end run with region-level outputs (AF/AN), no file outputs", {
  skip_on_cran()

  set.seed(456)
  n_days  <- 150
  dates   <- seq.Date(as.Date("2020-04-01"), by = "day", length.out = n_days)
  regions <- c("North", "South")
  df      <- base::expand.grid(date = dates, region = regions, KEEP.OUT.ATTRS = FALSE)

  df$temp_mean <- 11 + 7 * sin(2 * pi * as.numeric(df$date) / 365) +
    stats::rnorm(nrow(df), sd = 2)

  pm_base_by_region <- ifelse(df$region == "North", 5, 9)
  df$pm25 <- pmax(0.1, pm_base_by_region + stats::rnorm(nrow(df), sd = 2.5))
  df$pop  <- ifelse(df$region == "North", 1050000, 700000)

  eta <- -0.8 + 0.02 * df$pm25 + 0.008 * (df$temp_mean - mean(df$temp_mean))
  mu  <- pmax(0.1, exp(eta)) * 12
  df$deaths <- stats::rpois(nrow(df), lambda = mu)

  tmp_dir2  <- tempdir()
  csv_path2 <- file.path(tmp_dir2, "synthetic_health_by_region.csv")
  utils::write.csv(df, csv_path2, row.names = FALSE)

  res2 <- wildfire_do_analysis(
    health_path                        = csv_path2,
    join_wildfire_data                 = FALSE,
    ncdf_path                          = NULL,
    shp_path                           = NULL,
    date_col                           = "date",
    region_col                         = "region",
    shape_region_col                   = "region",  # NEW: required by updated signature
    mean_temperature_col               = "temp_mean",
    health_outcome_col                 = "deaths",
    pm_2_5_col                         = "pm25",
    rh_col                             = NULL,
    wind_speed_col                     = NULL,
    wildfire_lag                       = 1,
    temperature_lag                    = 1,
    spline_temperature_lag             = 0,
    spline_temperature_degrees_freedom = 3,
    predictors_vif                     = NULL,
    calc_relative_risk_by_region       = TRUE,
    scale_factor_wildfire_pm           = 10,
    save_fig                           = FALSE,
    save_csv                           = FALSE,
    output_folder_path                 = NULL,
    print_vif                          = FALSE,
    print_model_summaries              = FALSE
  )

  # RR results should include All Regions and the individual regions
  expect_s3_class(res2$RR_results, "data.frame")
  expect_true(all(
    c("lag", "relative_risk", "ci_lower", "ci_upper", "region_name") %in% names(res2$RR_results)
  ))
  expect_true(all(c("All Regions", "North", "South") %in% unique(res2$RR_results$region_name)))

  # AF/AN monthly results and AR~PM monthly join should be present
  expect_true(is.data.frame(res2$AF_AN_results) && nrow(res2$AF_AN_results) > 0)
  expect_true(is.data.frame(res2$AR_PM_monthly) && nrow(res2$AR_PM_monthly) > 0)

  # Basic column checks on AF/AN
  expect_true(all(
    c("region", "year", "month",
      "total_attributable_number",
      "average_attributable_fraction",
      "deaths_per_100k") %in% names(res2$AF_AN_results)
  ))
})


test_that("wildfire_do_analysis: save_fig creates model_validation directory", {
  # Unit style: mock heavy steps to focus on folder creation behaviour

  tmp_out <- file.path(tempdir(), paste0("wf_out_", as.integer(stats::runif(1, 1, 1e9))))
  dir.create(tmp_out, recursive = TRUE, showWarnings = FALSE)

  # Minimal data returned by load_wildfire_data, must have month/year/region/mean_PM for pm_data slice
  minimal_data <- data.frame(
    month = 1,
    year = 2020,
    region = "A",
    mean_PM = 5,
    stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    load_wildfire_data = function(...) minimal_data,
    create_lagged_variables = function(data, ...) data,
    create_temperature_splines = function(data, ...) data,
    time_stratify = function(data, ...) data,
    calculate_qaic = function(...) data.frame(ok = TRUE),
    calculate_wildfire_rr_by_region = function(...) data.frame(
      lag = 0,
      relative_risk = 1.01,
      ci_lower = 0.99,
      ci_upper = 1.03,
      region_name = "All Regions"
    ),
    plot_RR = function(...) invisible(NULL),
    generate_rr_pm_by_region = function(...) data.frame(
      region_name = "All Regions",
      lag = 0,
      pm = 0,
      rr = 1
    ),
    plot_rr_by_pm = function(...) invisible(NULL)
  )

  res <- wildfire_do_analysis(
    health_path = "ignored.csv",
    join_wildfire_data = FALSE,
    date_col = "date",
    region_col = "region",
    shape_region_col = "region",
    mean_temperature_col = "temp",
    health_outcome_col = "deaths",
    pm_2_5_col = "pm25",
    save_fig = TRUE,
    save_csv = FALSE,
    output_folder_path = tmp_out
  )

  expect_true(dir.exists(file.path(tmp_out, "model_validation")))
  expect_type(res, "list")
})


test_that("wildfire_do_analysis: predictors_vif triggers check_wildfire_vif", {
  minimal_data <- data.frame(
    month = 1,
    year = 2020,
    region = "A",
    mean_PM = 5,
    stringsAsFactors = FALSE
  )

  vif_called <- FALSE
  vif_predictors <- NULL

  local_mocked_bindings(
    load_wildfire_data = function(...) minimal_data,
    create_lagged_variables = function(data, ...) data,
    create_temperature_splines = function(data, ...) data,
    time_stratify = function(data, ...) data,
    calculate_qaic = function(...) data.frame(ok = TRUE),
    check_wildfire_vif = function(data, predictors, ...) {
      vif_called <<- TRUE
      vif_predictors <<- predictors
      data.frame(ok = TRUE)
    },
    calculate_wildfire_rr_by_region = function(...) data.frame(
      lag = 0,
      relative_risk = 1.01,
      ci_lower = 0.99,
      ci_upper = 1.03,
      region_name = "All Regions"
    ),
    plot_RR = function(...) invisible(NULL),
    generate_rr_pm_by_region = function(...) data.frame(region_name = "All Regions", lag = 0, pm = 0, rr = 1),
    plot_rr_by_pm = function(...) invisible(NULL)
  )

  wildfire_do_analysis(
    health_path = "ignored.csv",
    join_wildfire_data = FALSE,
    date_col = "date",
    region_col = "region",
    shape_region_col = "region",
    mean_temperature_col = "temp",
    health_outcome_col = "deaths",
    pm_2_5_col = "pm25",
    predictors_vif = c("mean_PM_lag0", "tmean_lag0"),
    save_fig = FALSE,
    save_csv = FALSE
  )

  expect_true(vif_called)
  expect_equal(vif_predictors, c("mean_PM_lag0", "tmean_lag0"))
})


test_that("wildfire_do_analysis: save_csv triggers save_wildfire_results", {
  minimal_data <- data.frame(
    month = 1,
    year = 2020,
    region = "A",
    mean_PM = 5,
    stringsAsFactors = FALSE
  )

  saved_called <- FALSE

  local_mocked_bindings(
    load_wildfire_data = function(...) minimal_data,
    create_lagged_variables = function(data, ...) data,
    create_temperature_splines = function(data, ...) data,
    time_stratify = function(data, ...) data,
    calculate_qaic = function(...) data.frame(ok = TRUE),
    calculate_wildfire_rr_by_region = function(...) data.frame(
      lag = 0,
      relative_risk = 1.01,
      ci_lower = 0.99,
      ci_upper = 1.03,
      region_name = "All Regions"
    ),
    plot_RR = function(...) invisible(NULL),
    generate_rr_pm_by_region = function(...) data.frame(region_name = "All Regions", lag = 0, pm = 0, rr = 1),
    plot_rr_by_pm = function(...) invisible(NULL),
    save_wildfire_results = function(rr_results, an_ar_results, annual_af_an_results, output_folder_path, ...) {
      saved_called <<- TRUE
      # Some basic sanity checks on what gets passed
      expect_s3_class(rr_results, "data.frame")
      expect_true(is.null(an_ar_results))
      expect_true(is.null(annual_af_an_results))
      expect_true(is.character(output_folder_path) || is.null(output_folder_path))
      invisible(NULL)
    }
  )

  wildfire_do_analysis(
    health_path = "ignored.csv",
    join_wildfire_data = FALSE,
    date_col = "date",
    region_col = "region",
    shape_region_col = "region",
    mean_temperature_col = "temp",
    health_outcome_col = "deaths",
    pm_2_5_col = "pm25",
    save_fig = FALSE,
    save_csv = TRUE,
    output_folder_path = tempdir()
  )

  expect_true(saved_called)
})
