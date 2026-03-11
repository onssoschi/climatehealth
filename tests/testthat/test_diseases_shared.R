# Tests for diseases_shared.R

if (!exists("with_parameters_test_that")) {
  source("tests/testthat/helper-libraries.R", local = FALSE)
}

if (!"package:climatehealth" %in% search()) {
  pkgload::load_all(".", export_all = TRUE, helpers = FALSE, quiet = TRUE)
}

# Create temp_dir to be used by all MH tests
temp_dir <- tempdir()
temp_dir <- file.path(temp_dir, "diseases_shared_tests")
if (!file.exists(temp_dir)) dir.create(temp_dir)

test_that("smoke test", {
  expect_true(TRUE)
})

# Tests for validate_case_type
with_parameters_test_that("validate_case_type handles valid and invalid inputs", {
    expect_equal(validate_case_type(input_case), expected_case)
    },
    input_case = c("malaria", "MALARIA", "Diarrhea", "DiARrhea"),
    expected_case = c("malaria", "malaria", "diarrhea", "diarrhea"),
    .test_name = c("Malaria lowercase", "Malaria uppercase", "Diarrhea titalised", "Diarrhea mixed case 2")
)

test_that("validate_case_type handles invalid inputs correctly", {
  # Test invalid input
  expect_error(
    validate_case_type("cholera"),
    "'case_type' must be one of diarrhea, malaria"
  )
})

# Tests for load_and_process_data

LAPD_test_data <- data.frame(
    region = c("A", "A", "A", "B"),
    district = c("DA1", "DA1", "DA2", "DB1"),
    date = c(
        seq(from=as.Date("2020-11-01"), by="month", length.out=3),
        as.Date("2020-11-01")
    ),
    year = c(2020, 2020, 2021, 2020),
    month = c(11, 12, 1, 11),
    malaria_cases = c(5L, 10L, 0L, 3L),
    population = c(1500L, 1600L, 1300L, 1300L)
)

with_parameters_test_that(
    "load_and_process_data works with different input types",
    {
        # Configure test for df/file input
        test_input <- if (is.function(test_data_path)) test_data_path() else test_data_path
        # Load and process data
        out <- load_and_process_data(
            health_data_path = test_input,
            region_col = "region",
            district_col = "district",
            date_col = "date",
            year_col = "year",
            month_col = "month",
            case_col = "malaria_cases",
            case_type = "malaria",
            tot_pop_col = "population"
        )
        # Validate returned data
        expect_s3_class(out, "data.frame")
        expect_equal(nrow(out), 4)
        exp_cols <- c("region", "district", "year", "month", "malaria", "tot_pop")
        expect_true(all(exp_cols %in% names(out)))
    },
    test_data_path = list(
        LAPD_test_data,
        function() {
            tmp_rds <- file.path(tempdir(), "LAPD_test_data.rds")
            saveRDS(LAPD_test_data, tmp_rds)
            return(tmp_rds)
        },
        function() {
            tmp_csv <- file.path(tempdir(), "LAPD_test_data.csv")
            readr::write_csv(LAPD_test_data, tmp_csv)
            return(tmp_csv)
        },
        function() {
            testthat::skip_if_not_installed("openxlsx")
            tmp_xlsx <- file.path(tempdir(), "LAPD_test_data.xlsx")
            openxlsx::write.xlsx(LAPD_test_data, tmp_xlsx)
            return(tmp_xlsx)
        }
    ),
    .test_name = c("dataframe input", "rds input", "csv input", "xlsx input")
)

test_that(
    "load_and_process_data raises an error when all date infromation is missing", {
        expect_error(
            load_and_process_data(
                health_data_path = LAPD_test_data,
                region_col = "region",
                district_col = "district",
                date_col = NULL,
                year_col = NULL,
                month_col = NULL,
                case_col = "malaria_cases",
                case_type = "malaria",
                tot_pop_col = "population"
            ),
            "If no date column is provided, you must provide both the year and the month columns."
        )
    }
)

test_that(
    "load_and_process_data correctly infers date when only date_col is provided", {
        # Obtain output
        out <- load_and_process_data(
            health_data_path = LAPD_test_data,
            region_col = "region",
            district_col = "district",
            date_col = "date",
            year_col = NULL,
            month_col = NULL,
            case_col = "malaria_cases",
            case_type = "malaria",
            tot_pop_col = "population"
        )
        # Validate outputs
        expect_true(all(c("year", "month") %in% names(out)))
        expect_equal(out$year, c(2020, 2020, 2021, 2020))
        expect_equal(out$month, c(11, 12, 1, 11))
    }
)

# Tests for load_and_process_map
write_tmp_shapefile <- function() {
    # Create sample geom data
    poly1 <- sf::st_polygon(list(rbind(c(0,0), c(0,1), c(1,1), c(1,0), c(0,0))))
    poly2 <- sf::st_polygon(list(rbind(c(1,0), c(1,1), c(2,1), c(2,0), c(1,0))))
    geom <- sf::st_sfc(poly1, poly2, crs = 4326)
    # Create map data (for SF)
    map_data <- sf::st_sf(
        region = c("A", "B"),
        district = c("DA1", "DB1"),
        geometry = geom,
        crs = 4326
    )
    # Write out shapefile (tmp)
    tmp_shp <- file.path(tempdir(), "test_map.shp")
    sf::st_write(sf::st_as_sf(map_data), tmp_shp, quiet = TRUE, append = F)
    return(tmp_shp)
}

test_that(
    "load_and_process_map reads in a shapefile as intended",
    {
        skip_if_not_installed("INLA")
        # Get local shapefile
        tmp_shp <- write_tmp_shapefile()
        # Load and process map
        out <- load_and_process_map(
            map_path = tmp_shp,
            region_col = "region",
            district_col = "district",
            geometry_col = "geometry",
            output_dir = NULL
        )
        # Validate output
        expect_equal(typeof(out), "list")
        expect_equal(typeof(out$map), "list")
        expect_true(all(c("region", "district", "geometry") %in% names(out$map)))
    }
)

test_that(
    "load_and_process_map correctly reads and writes nb.map files",
    {
        skip_if_not_installed("INLA")
        # define test pathas
        tmp_shp <- write_tmp_shapefile()
        tmp_dir <- tempdir()
        nb_path <- file.path(tmp_dir, "nbfile")
        # assert nb does not exist
        expect_false(file.exists(nb_path))
        # write nb with load_and_process_map
        load_and_process_map(
            map_path = tmp_shp,
            region_col = "region",
            district_col = "district",
            geometry_col = "geometry",
            output_dir = tmp_dir
        )
        expect_true(file.exists(nb_path))
        # run again to assert that formerly written nb files are read
        load_and_process_map(
            map_path = tmp_shp,
            region_col = "region",
            district_col = "district",
            geometry_col = "geometry",
            output_dir = tmp_dir
        )
        # clean up
        file.remove(nb_path)
    }
)

# Tests for load_and_process_climatedata

LAPCD_test_data <- data.frame(
    district = c("DA1", "DA1", "DA1", "DB1", "DB1"),
    year = c(2020, 2020, 2021, 2020, 2021),
    month = c(11, 12, 1, 11, 12),
    tmin = c(15.0, 16.0, 14.0, 12.5, 13.0),
    tmean = c(20.0, 21.0, 19.0, 17.5, 18.0),
    tmax = c(25.0, 26.0, 24.0, 22.5, 23.0),
    rainfall_t = c(100.0, 80.0, 120.0, 90.0, 95.0),
    relative_humidity_t = c(75.0, 70.0, 80.0, 65.0, 68.0),
    runoff_t = c(30.0, 25.0, 35.0, 20.0, 22.0),
    ndvi = c(0.5, 0.4, 0.6, 0.3, 0.35),
    spi = c(1.2, 0.8, 1.5, 0.9, 1.0)
)

with_parameters_test_that(
    "load_and_process_climatedata works with different input types",
    {
        # Configure test for df/file input
        test_input <- if (is.function(test_data_path)) test_data_path() else test_data_path
        # Load and process data
        out <- load_and_process_climatedata(
            climate_data_path = test_input,
            district_col = "district",
            year_col = "year",
            month_col = "month",
            tmin_col = "tmin",
            tmean_col = "tmean",
            tmax_col = "tmax",
            rainfall_col = "rainfall_t",
            r_humidity_col = "relative_humidity_t",
            runoff_col = "runoff_t",
            ndvi_col = "ndvi",
            spi_col = "spi",
            max_lag = 2
        )
        # Validate returned data
        expect_s3_class(out, "data.frame")
        expect_equal(nrow(out), 5)
        exp_cols <- c("rainfall_lag2", "runoff_lag1", "tmean", "month", "year", "district")
        expect_true(all(exp_cols %in% names(out)))
        # Validate grouped lags work correctly
        expect_equal(out$rainfall_lag1, c(NA, 100, 80, NA, 90))
        expect_equal(out$runoff_lag2, c(NA, NA, 30, NA, NA))
    },
    test_data_path = list(
        LAPCD_test_data,
        function() {
            tmp_rds <- file.path(tempdir(), "LAPD_test_data.rds")
            saveRDS(LAPCD_test_data, tmp_rds)
            return(tmp_rds)
        },
        function() {
            tmp_csv <- file.path(tempdir(), "LAPD_test_data.csv")
            readr::write_csv(LAPCD_test_data, tmp_csv)
            return(tmp_csv)
        },
        function() {
            testthat::skip_if_not_installed("openxlsx")
            tmp_xlsx <- file.path(tempdir(), "LAPD_test_data.xlsx")
            openxlsx::write.xlsx(LAPCD_test_data, tmp_xlsx)
            return(tmp_xlsx)
        }
    ),
    .test_name = c("dataframe input", "rds input", "csv input", "xlsx input")
)

# Tests for combine_health_climate_data
test_that(
    "combine_health_climate_data succesfully joins data from 3 data sources",
    {
        map_path <- write_tmp_shapefile()
        # Get processed data
        output <- combine_health_climate_data(
            health_data_path = LAPD_test_data,
            map_path = map_path,
            climate_data_path = LAPCD_test_data,
            region_col = "region",
            district_col = "district",
            date_col = "date",
            year_col = "year",
            month_col = "month",
            case_col = "malaria_cases",
            case_type = "malaria",
            tot_pop_col = "population",
            geometry_col = "geometry",
            tmin_col = "tmin",
            tmean_col = "tmean",
            tmax_col = "tmax",
            rainfall_col = "rainfall_t",
            r_humidity_col = "relative_humidity_t",
            runoff_col = "runoff_t",
            ndvi_col = "ndvi",
            spi_col = "spi",
            max_lag = 2,
            output_dir = NULL
        )
        # Validate outputs
        expect_equal(length(output), 6)
        exp_list_names <- c("map", "nb.map", "graph_file", "data", "grid_data", "summary")
        expect_true(all(exp_list_names %in% names(output)))
        exp_data_names <- c("region", "district_code", "time", "runoff_lag2", "spi", "malaria")
        expect_true(all(exp_data_names %in% names(output$data)))
        expect_equal(as.numeric(output$summary$tmin["Mean"][1]), 14.5)
        expect_equal(max(output$grid_data$district_code), 21)
    }
)

# Tests for plot_health_climate_timeseries
PHT_test_data <- data.frame(
  region    = c("North", "North", "South", "South"),
  district  = c("N1", "N2", "S1", "S2"),
  year      = c(2020, 2020, 2021, 2021),
  month     = c(1, 2, 1, 2),
  malaria   = c(10, 15, 5, 8),
  diarrhea  = c(20, 25, 10, 12),
  tmin      = c(22, 23, 24, 25),
  tmean     = c(28, 29, 30, 31),
  tmax      = c(35, 36, 37, 38),
  rainfall  = c(100, 110, 120, 130),
  r_humidity = c(60, 62, 65, 67),
  runoff     = c(5, 6, 7, 9)
)

test_that(
  "plot_health_climate_timeseries works at country level",
  {
    p <- plot_health_climate_timeseries(
      data = PHT_test_data,
      param_term = "tmean",
      level = "country",
      case_type = "malaria"
    )
    expect_s3_class(p, "ggplot")
    expect_true(!("group" %in% names(p$data)))
  }
)

test_that(
  "plot_health_climate_timeseries works at region level",
    {
    p <- plot_health_climate_timeseries(
      data = PHT_test_data,
      param_term = "tmax",
      level = "region",
      case_type = "malaria"
    )
    expect_s3_class(p, "ggplot")
    expect_equal(unique(p$data$group), c("North", "South"))
    }
)

test_that(
  "plot_health_climate_timeseries works at district level",
  {
    p <- plot_health_climate_timeseries(
      data = PHT_test_data,
      param_term = "rainfall",
      level = "district",
      case_type = "malaria"
    )
    expect_s3_class(p, "ggplot")
    expect_equal(unique(p$data$group), c("N1", "N2", "S1", "S2"))
  }
)

test_that(
  "plot_health_climate_timeseries handles param_term=all as expected",
  {
    p <- plot_health_climate_timeseries(
      data = PHT_test_data,
      param_term = "all",
      level = "country",
      case_type = "malaria"
    )
    expect_s3_class(p, "ggplot")
    expect_equal(unique(p$data$variable), c("malaria", "tmin", "tmean", "tmax", "rainfall", "r_humidity","runoff"))
  }
)

test_that(
  "plot_health_climate_timeseries filters by year correctly",
  {
    p <- plot_health_climate_timeseries(
      data = PHT_test_data,
      param_term = "tmin",
      level = "country",
      case_type = "malaria",
      filter_year = 2020
    )
    # Validate filter works correctly
    expect_s3_class(p, "ggplot")
    expect_equal(p$data$date, as.Date(c("2020-01-01", "2020-02-01")))
  }
)

test_that(
  "plot_health_climate_timeseries saves as expected",
  {
    tmp_dir <- tempdir()
    p <- plot_health_climate_timeseries(
      data = PHT_test_data,
      param_term = c("tmean", "tmax", "tmin"),
      level = "country",
      case_type = "malaria",
      save_fig = TRUE,
      output_dir = tmp_dir
    )
    expect_s3_class(p, "ggplot")
    expect_true(file.exists(
      file.path(tmp_dir, "timeseries_tmean_tmax_tmin_country.pdf")
    ))
  }
)

test_that(
  "plot_health_climate_timeseries errors on missing columns",
  {
    bad_data <- PHT_test_data[, !(names(PHT_test_data) %in% c("tmin"))]
    expect_error(
      plot_health_climate_timeseries(
        data = bad_data,
        param_term = "tmin",
        level = "country",
        case_type = "malaria"
      ),
      "Missing columns: tmin"
    )
  }
)

test_that(
  "plot_health_climate_timeseries errors on invalid level",
  {
    expect_error(
      plot_health_climate_timeseries(
        data = PHT_test_data,
        param_term = "tmean",
        level = "continent",
        case_type = "malaria"
      ),
      "Invalid level"
    )
  }
)

# Tests for set_cross_basis

# Create test data (2 lags for spline knots)
CB_test_data <- data.frame(
  tmax = c(30, 32),
  tmax_lag1 = c(29, 31),
  tmax_lag2 = c(28, 30),
  tmin = c(20, 21),
  tmin_lag1 = c(19, 20),
  tmin_lag2 = c(18, 19),
  ndvi = c(0.5, 0.6),
  ndvi_lag1 = c(0.4, 0.5),
  ndvi_lag2 = c(0.3, 0.4)
)

test_that("set_cross_basis returns expected variables", {
  cb <- set_cross_basis(CB_test_data, max_lag = 2, nk = 2)

  # Still check type
  expect_type(cb, "list")
  # Variables included when base and lag columns are present
  expect_equal(sort(names(cb)), sort(c("tmax", "tmin", "ndvi")))

  # Column name prefixing
  for (var in names(cb)) {
    expect_true(all(grepl(paste0("^basis_", var, "\\."), colnames(cb[[var]]))))
  }

  # Knot expectations determined by nk and predictor values
  attrs <- attributes(cb[["tmax"]])

  # Build the same matrix that the function uses for tmax
  tmax_mat <- as.matrix(CB_test_data[, c("tmax", "tmax_lag1", "tmax_lag2")])

  # Expected knots come from all entries across base + lags
  expected_knots <- dlnm::equalknots(as.numeric(tmax_mat), nk = 2)

  expect_equal(round(attrs$argvar$knots, 2), round(expected_knots, 2))

  # Same knots when an unrelated outcome column is present
  diarrhea_td <- dplyr::mutate(CB_test_data, diarrhea = c(1, 2))
  diarrhea_cb <- set_cross_basis(diarrhea_td, max_lag = 2, nk = 2)
  expect_equal(
    round(attributes(diarrhea_cb[["tmax"]])$argvar$knots, 6),
    round(attributes(cb[["tmax"]])$argvar$knots, 6)
  )
})

test_that("set_cross_basis returns expected variables with ndvi", {
  cb <- set_cross_basis(CB_test_data, max_lag = 2, nk = 2)
  expect_equal(
    sort(names(cb)),
    c("ndvi", "tmax", "tmin")
  )
})

test_that("set_cross_basis skips variables with missing lags", {
  data_missing_tmin <- CB_test_data[, !grepl("^tmin", names(CB_test_data))]
  cb <- set_cross_basis(data_missing_tmin, max_lag = 2, nk = 2)
  expect_false("tmin" %in% names(cb))
})

# Tests for create_inla_indices

# create_inla_indices test data
INLA_IND <- data.frame(
  district_code = c("D1", "D2", "D1", "D2"),
  region_code = c("R1", "R1", "R2", "R2"),
  year = c(2020, 2020, 2021, 2021),
  time = 1:4,
  tot_pop = seq(1000, 1300, 100),
  malaria = 10:13
)

test_that(
  "create_inla_indices adds expected columns",
  {
    skip_if_not_installed("INLA")
    result <- create_inla_indices(INLA_IND, case_type = "malaria")
    # validate base columns
    expect_true(all(c(
      "E", "SIR", "district_index", "region_index", "year_index"
    ) %in% names(result)))
    # validate geographical columns
    expect_equal(result$district_index, c(1, 2, 1, 2))
    expect_equal(result$region_index, c(1, 1, 2, 2))
    expect_equal(result$year_index, c(1, 1, 2, 2))
  }
)

test_that(
  "create_inla_indices computes E and SIR correctly",
  {
    skip_if_not_installed("INLA")
    result <- create_inla_indices(INLA_IND, case_type = "malaria")
    overall_rate <- sum(INLA_IND$malaria) / sum(INLA_IND$tot_pop)
    expected_E <- overall_rate * INLA_IND$tot_pop
    expected_SIR <- INLA_IND$malaria / expected_E
    expect_equal(result$E, expected_E)
    expect_equal(result$SIR, expected_SIR)
  }
)

test_that("create_inla_indices handles NA values in case column", {
  skip_if_not_installed("INLA")
  INLA_NA <- INLA_IND
  INLA_NA$malaria[2] <- NA
  result <- create_inla_indices(INLA_NA, case_type = "malaria")
  # E is still computed for all rows
  expect_true(all(!is.na(result$E)))
  # Check that SIR is NA where case value is NA
  expect_true(is.na(result$SIR[2]))
  expect_equal(result$SIR[-2], result$malaria[-2] / result$E[-2])
})

# Tests for check_diseases_VIF

# HIGH Colinearity
# HIGH Colinearity (strong but non-singular)
set.seed(101)

n <- 160   # slightly larger reduces spline/aliasing instability
tmax_base <- rnorm(n, 32, 2)
tmin_base <- rnorm(n, 22, 2)
ndvi_base <- runif(n, 0.4, 0.8)

CHECK_VIF_DF <- data.frame(
  district_code = rep(paste0("D", 1:8), length.out = n),
  region_code   = rep(paste0("R", 1:3), length.out = n),
  year          = rep(2020, n),
  time          = 1:n,
  tot_pop       = sample(1000:2000, n, replace = TRUE),
  malaria       = sample(10:30, n, replace = TRUE),

  # Base
  tmax = tmax_base,
  tmin = tmin_base,
  ndvi = ndvi_base,

  # Very strongly correlated lags
  tmax_lag1 = 0.985 * tmax_base + rnorm(n, 0, 0.05),
  tmax_lag2 = 0.970 * tmax_base + rnorm(n, 0, 0.07),

  tmin_lag1 = 0.985 * tmin_base + rnorm(n, 0, 0.05),
  tmin_lag2 = 0.970 * tmin_base + rnorm(n, 0, 0.07),

  ndvi_lag1 = 0.97 * ndvi_base + rnorm(n, 0, 0.01),
  ndvi_lag2 = 0.94 * ndvi_base + rnorm(n, 0, 0.015)
)

test_that("check_diseases_vif detects high collinearity", {
  skip_if_not_installed("INLA")
  res <- suppressWarnings(
    check_diseases_vif(
      data = CHECK_VIF_DF,
      inla_param = c("tmax", "tmin"),
      max_lag = 2,
      nk = 1,    # << USE nk = 1, safe and stable
      basis_matrices_choices = c("tmax", "tmin"),
      case_type = "malaria"
    )
  )
  expect_equal(res$interpretation, "High collinearity")
})

test_that(
  "check_diseases_vif errors on missing basis matrix",
  {
    skip_if_not_installed("INLA")
    bad_data <- CHECK_VIF_DF[, !grepl("^tmin", names(CHECK_VIF_DF))]
    expect_error(
      check_diseases_vif(
        data = bad_data,
        inla_param = c("tmax", "tmin"),
        max_lag = 2,
        nk = 2,
        basis_matrices_choices = c("tmax", "tmin"),
        case_type = "malaria"
      ),
      "Missing in basis: tmin"
    )
  }
)

test_that(
  "check_diseases_vif errors on missing inla_param variable",
  {
    skip_if_not_installed("INLA")
    bad_data <- CHECK_VIF_DF[, !(names(CHECK_VIF_DF) %in% c("tmin"))]
    expect_error(
      check_diseases_vif(
        data = bad_data,
        inla_param = c("tmax", "tmin"),
        max_lag = 2,
        nk = 2,
        basis_matrices_choices = c("tmax"),
        case_type = "malaria"
      ),
      "Missing in data: tmin"
    )
  }
)

# Moderate Colinearity
set.seed(55)

MOD_COL_DF <- data.frame(
  district_code = rep(c("D1", "D2", "D3", "D4", "D5"), 20),
  region_code   = rep(c("R1", "R2", "R3", "R1", "R2"), 20),
  year          = rep(2020, 100),
  time          = 1:100,
  tot_pop       = sample(1000:1500, 100, replace = TRUE),
  malaria       = sample(10:30, 100, replace = TRUE),
  tmax          = rnorm(100, mean = 32, sd = 2),
  tmax_lag1     = 0.85 * rnorm(100, mean = 32, sd = 2) + rnorm(100, 0, 1.2),
  tmax_lag2     = 0.65 * rnorm(100, mean = 32, sd = 2) + rnorm(100, 0, 1.5),
  tmin          = rnorm(100, mean = 22, sd = 2),
  tmin_lag1     = 0.85 * rnorm(100, mean = 22, sd = 2) + rnorm(100, 0, 1.2),
  tmin_lag2     = 0.65 * rnorm(100, mean = 22, sd = 2) + rnorm(100, 0, 1.5),
  ndvi          = runif(100, 0.4, 0.8),
  ndvi_lag1     = runif(100, 0.3, 0.7),
  ndvi_lag2     = runif(100, 0.2, 0.6)
)

test_that(
  "check_diseases_vif detects moderate collinearity",
  {
    skip_if_not_installed("INLA")
    result <- suppressWarnings(
      check_diseases_vif(
        data = MOD_COL_DF,
        inla_param = c("tmax", "tmin"),
        max_lag = 2,
        nk = 1,
        basis_matrices_choices = c("tmax", "tmin"),
        case_type = "malaria"
      )
    )
    expect_equal(result$interpretation, "Moderate collinearity")
  }
)

# Low Colinearity
set.seed(456)
LOW_COL_DF <- data.frame(
  district_code = rep(c("D1", "D2", "D3", "D4", "D5"), 40),
  region_code = rep(c("R1", "R2", "R3", "R1", "R2"), 40),
  year = rep(2020, 200),
  time = 1:200,
  tot_pop = sample(1000:1500, 200, replace = TRUE),
  malaria = sample(10:30, 200, replace = TRUE),
  tmax = rnorm(200, mean = 32, sd = 3),
  tmax_lag1 = rnorm(200, mean = 31, sd = 3),
  tmax_lag2 = rnorm(200, mean = 30, sd = 3),
  tmin = rnorm(200, mean = 22, sd = 3),
  tmin_lag1 = rnorm(200, mean = 21, sd = 3),
  tmin_lag2 = rnorm(200, mean = 20, sd = 3),
  ndvi = runif(200, 0.4, 0.8),
  ndvi_lag1 = runif(200, 0.3, 0.7),
  ndvi_lag2 = runif(200, 0.2, 0.6)
)

test_that(
  "check_diseases_vif detects low collinearity",
  {
    skip_if_not_installed("INLA")
    result <- suppressWarnings(
      check_diseases_vif(
        data = LOW_COL_DF,
        inla_param = c("tmax", "tmin"),
        max_lag = 2,
        nk = 1,
        basis_matrices_choices = c("tmax", "tmin"),
        case_type = "malaria"
      )
    )
    expect_equal(result$interpretation, "Low collinearity")
  }
)

# Test for check_and_write_vif
test_that(
  "check_and_write_vif creates VIF DF and writes to a file",
  {
    skip_if_not_installed("INLA")
    # generate and write results
    temp_dir <- tempdir()
    result <- check_and_write_vif(
      data = MOD_COL_DF,
      inla_param = c("tmax", "tmin"),
      basis_matrices_choices = c("tmax", "tmin"),
      max_lag = 2,
      nk = 1,
      case_type = "malaria",
      output_dir = temp_dir
    )
    # validate returned values
    expect_equal(result$interpretation, "Moderate collinearity")
    # validate file outputs
    output_fpath <- file.path(temp_dir, "VIF_results.csv")
    expect_true(file.exists(output_fpath))
    outputted_df <- read.csv(output_fpath)
    expect_equal(nrow(outputted_df), 14)
    expect_equal(ncol(outputted_df), 2)
  }
)

# Tests for run_inla_models

# Test data
RIM_n <- 10
RIM_dummy_data <- data.frame(
  time = rep(1:5, 2),
  year = rep(2020:2021, each = 5),
  district_code = rep(c("A", "B"), each = 5),
  region_code = rep(c("X", "Y"), each = 5),
  tot_pop = rep(1000, RIM_n),
  malaria = rpois(RIM_n, lambda = 5),
  tmax = rnorm(RIM_n),
  tmax_lag1 = rnorm(RIM_n),
  tmax_lag2 = rnorm(RIM_n),
  humidity = rnorm(RIM_n)
)

RIM_combined_data <- list(data = RIM_dummy_data, graph_file = "dummy.graph")

RIM_make_mock_model <- function(dic_val = 123.45, cpo_val = 0.9) {
  list(
    dic = list(dic = dic_val),
    cpo = list(cpo = rep(cpo_val, RIM_n), failure = rep(0, RIM_n))
  )
}

test_that(
  "run_inla_models raises an error when save_model==T and output_dir==NULL",
  {
    skip_if_not_installed("INLA")
    expect_error(
      run_inla_models(
        data.frame(),
        c(),
        c(),
        2,
        2,
        "malaria",
        NULL,
        TRUE
      ),
      "output_dir must be provided if save_model = TRUE"
    )
  }
)

test_that(
  "run_inla_models raises an error if INLA is not installed",
  {
    with_mocked_bindings(
      requireNamespace = function(pkg, quietly=T) FALSE,
      .package = "base",
      code = expect_error(
        run_inla_models(NULL, NULL, NULL, NULL, NULL, "malaria"),
        "INLA is not installed"
      ),
    )
  }
)

test_that(
  "run_inla_models handles NULL basis_matrices_choices",
  {
    skip_if_not_installed("INLA")
    with_mocked_bindings(
      `inla.rerun` = function(x) RIM_make_mock_model(100, 0.8),
      .package = "INLA",
      code = {
        result <- run_inla_models(
          combined_data = RIM_combined_data,
          basis_matrices_choices = NULL,
          inla_param = character(),
          max_lag = 2,
          nk = 2,
          case_type = "malaria"
        )
        expect_equal(result$dic_table$Model, "")
        expect_equal(result$dic_table$DIC, 100)
      }
    )
  }
)

test_that(
  "run_inla_models saves model to output_dir when save_model is TRUE",
  {
    skip_if_not_installed("INLA")
    RIM_temp_dir <- tempdir()
    with_mocked_bindings(
      `inla.rerun` = function(x) RIM_make_mock_model(200, 0.95),
      .package = "INLA",
      code = {
        run_inla_models(
          combined_data = RIM_combined_data,
          basis_matrices_choices = "tmax",
          inla_param = "tmax",
          max_lag = 2,
          nk = 2,
          case_type = "malaria",
          output_dir = RIM_temp_dir,
          save_model = TRUE
        )
        RIM_saved_files <- list.files(
          RIM_temp_dir, pattern = "^model_with_tmax_tmax.RData$", full.names = TRUE
        )
        expect_true(length(RIM_saved_files) == 1)
      }
    )
  }
)

test_that(
  "run_inla_models includes basis and raw variables in model string",
  {
    skip_if_not_installed("INLA")
    with_mocked_bindings(
      `inla.rerun` = function(x) RIM_make_mock_model(300, 0.85),
      .package = "INLA",
      code = {
        result <- run_inla_models(
          combined_data = RIM_combined_data,
          basis_matrices_choices = "tmax",
          inla_param = "humidity",
          max_lag = 2,
          nk = 2,
          case_type = "malaria"
        )
        expect_equal(result$dic_table$Model, "tmax + humidity")
        expect_equal(result$dic_table$DIC, 300)
      }
    )
  }
)

# Tests for plot_monthly_random_effects

# Test dataset
PMRE_n_regions <- 2

PMRE_model <- list(summary.random = list(
  month = data.frame(
    region = rep(c("Region A", "Region B"), each = 12),
    month.ID = rep(1:12, PMRE_n_regions),
    month.mean = rnorm(12 * PMRE_n_regions),
    month.0.025quant = rnorm(12 * PMRE_n_regions, -1),
    month.0.975quant = rnorm(12 * PMRE_n_regions, 1)
  )
))
names(PMRE_model$summary.random$month) <- sub("^month\\.", "", names(PMRE_model$summary.random$month))

PMRE_data <- data.frame(
  region_code = rep(c("A", "B"), each = 6),
  other = rnorm(12)
)

PMRE_grid_data <- data.frame(
  code_num = c("A", "B"),
  name = c("Region A", "Region B"),
  district = c("D1", "D2"),
  district_code = c("A1", "B1"),
  other_info = rnorm(2)
)

PMRE_map <- data.frame(
  region = c("Region A", "Region B"),
  district = c("D1", "D2"),
  geometry = c("geomA", "geomB")
)

PMRE_combined_data <- list(
  data = PMRE_data,
  grid_data = PMRE_grid_data,
  map = PMRE_map
)

test_that(
  "plot_monthly_random_effects raises an error if save_fig = TRUE and output_dir = NULL",
  {
    expect_error(
      plot_monthly_random_effects(
        combined_data = PMRE_combined_data,
        model = PMRE_model,
        save_fig = TRUE,
        output_dir = NULL
      ),
      "output_dir must be provided if save_fig = TRUE"
    )
  }
)

test_that(
  "plot_monthly_random_effects saves and returns PDF when save_fig = TRUE",
  {
    temp_dir <- tempdir()
    p <- plot_monthly_random_effects(
      combined_data = PMRE_combined_data,
      model = PMRE_model,
      save_fig = TRUE,
      output_dir = temp_dir
    )
    # validate returned object
    expect_s3_class(p, "ggplot")
    # validate plot was saved
    fpath <- file.path(temp_dir, "monthly_random_effects.pdf")
    expect_true(file.exists(fpath))
  }
)

# Tests for plot_yearly_spatial_random_effect

# Test datasets
PYSRE_model <- list(summary.random = list(
  district_index = data.frame(
    ID = 1:12,
    mean = rnorm(12),
    `0.025quant` = rnorm(12, -1),
    `0.975quant` = rnorm(12, 1)
  )
))

PYSRE_data <- data.frame(
  district_code = rep(c("A1", "B1"), each = 6),
  region_code = rep(c("A", "B"), each = 6),
  year = rep(2020:2021, each = 6),
  time = rep(2020:2021, each = 6),
  malaria = rpois(12, lambda = 5),
  tot_pop = sample(100:1000, 12, replace = TRUE),
  other = rnorm(12)
)

PYSRE_grid_data <- PMRE_grid_data

# Create dummy polygons for two districts
dummy_polygons <- sf::st_sfc(
  sf::st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)))),
  sf::st_polygon(list(rbind(c(1,1), c(2,1), c(2,2), c(1,2), c(1,1))))
)

PYSRE_map <- sf::st_sf(
  district_code = c("A1", "B1"),
  geometry = dummy_polygons
)

PYSRE_combined_data <- list(
  data = PYSRE_data,
  grid_data = PYSRE_grid_data,
  map = PYSRE_map
)

test_that(
  "plot_yearly_spatial_random_effect raises an error if save_fig=T and output_dir=NULL",
  {
    skip_if_not_installed("INLA")
    expect_error(
      plot_yearly_spatial_random_effect(
        combined_data = PYSRE_combined_data,
        model = PYSRE_model,
        case_type = "malaria",
        save_fig = TRUE,
        output_dir = NULL
      ),
      "output_dir must be provided if save_fig = TRUE"
    )
  }
)

test_that(
  "plot_yearly_spatial_random_effect saves and returns PDF when save_fig=T",
  {
    skip_if_not_installed("INLA")
    temp_dir <- tempdir()
    p <- plot_yearly_spatial_random_effect(
      combined_data = PYSRE_combined_data,
      model = PYSRE_model,
      case_type = "malaria",
      save_fig = TRUE,
      output_dir = temp_dir
    )
    expect_s3_class(p, "ggplot")
    fpath <- file.path(temp_dir, "spatial_random_effects_per_year.pdf")
    expect_true(file.exists(fpath))
  }
)

# Tests for get_predictions

# Create test dataset
set.seed(42)
gp_data <- data.frame(
  region = rep(c("North", "South"), each = 6),
  district = rep(c("N1", "N2", "S1", "S2"), each = 3),
  district_code = rep(c("A1", "A2", "B1", "B2"), each = 3),
  region_code = rep(c("A", "A", "B", "B"), each = 3),
  year = rep(2020:2021, each = 6),
  time = rep(2020:2021, each = 6),
  malaria = rpois(12, lambda = 5),
  tot_pop = sample(100:1000, 12, replace = TRUE),
  tmax = runif(12, min = 20, max = 35),
  tmax_lag1 = runif(12, min = 20, max = 35),
  tmax_lag2 = runif(12, min = 20, max = 35),
  tmax_lag3 = runif(12, min = 20, max = 35)
)

gp_data$region <- as.character(gp_data$region)
gp_data$district <- as.character(gp_data$district)

# Run create_inla_indices and set_cross_basis to get real basis matrix
gp_indexed_data <- create_inla_indices(gp_data, "malaria")
gp_basis_matrices <- set_cross_basis(gp_indexed_data, max_lag=2, nk=2)

# Extract column names for tmax basis
gp_tmax_basis <- gp_basis_matrices$tmax
gp_basis_names <- colnames(gp_tmax_basis)

# Create mock model
gp_model <- list(
  summary.fixed = list(mean = rnorm(length(gp_basis_names))),
  misc = list(lincomb.derived.covariance.matrix = diag(length(gp_basis_names))),
  names.fixed = gp_basis_names
)

test_that(
  "get_predictions returns country-level crosspred object",
  {
    skip_if_not_installed("INLA")
    result <- get_predictions(
      data = gp_data,
      param_term = "tmax",
      max_lag = 3,
      nk = 2,
      model = gp_model,
      level = "country",
      case_type = "malaria"
    )
    # validate outputs
    expect_s3_class(result, "crosspred")
    expect_equal(result$model.link, "log")
    expect_equal(result$model.class, NA)
    expect_equal(
      round(result$coefficients[1], 2),
      -0.78
    )
    expect_equal(
      round(result$allfit[1], 2),
      c("20.5" = 1.15)
    )
  }
)

test_that(
  "get_predictions returns region-level named list of crosspred objects",
  {
    skip_if_not_installed("INLA")
    result <- get_predictions(
      data = gp_data,
      param_term = "tmax",
      max_lag = 2,
      nk = 2,
      model = gp_model,
      level = "region",
      case_type = "malaria"
    )
    expect_type(result, "list")
    expect_named(result, unique(gp_data$region))
    lapply(result, function(x) expect_s3_class(x, "crosspred"))
  }
)

test_that(
  "get_predictions returns district-level named list of crosspred objects",
  {
    skip_if_not_installed("INLA")
    result <- get_predictions(
      data = gp_data,
      param_term = "tmax",
      max_lag = 2,
      nk = 2,
      model = gp_model,
      level = "district",
      case_type = "malaria"
    )
    expect_type(result, "list")
    expect_named(result, unique(gp_data$district))
    lapply(result, function(x) expect_s3_class(x, "crosspred"))
  }
)

test_that(
  "get_predictions errors if param_term is not found in model",
  {
    skip_if_not_installed("INLA")
    bad_model <- gp_model
    bad_model$names.fixed <- paste0("basis_other.", seq_along(gp_basis_names))
    expect_error(
      get_predictions(
        data = gp_data,
        param_term = "tmax",
        max_lag = 2,
        nk = 2,
        model = bad_model,
        level = "country",
        case_type = "malaria"
      ),
      regexp = "coef/vcov not consistent with basis matrix"
    )
  }
)

# Tests for contour_plot

# Create test dataset
set.seed(123)
CP_data <- data.frame(
  region = rep(c("North", "South"), each = 6),
  district = rep(c("N1", "N2", "S1", "S2"), each = 3),
  district_code = rep(c("A1", "A2", "B1", "B2"), each = 3),
  region_code = rep(c("A", "A", "B", "B"), each = 3),
  year = rep(2020:2021, each = 6),
  time = rep(2020:2021, each = 6),
  malaria = rpois(12, lambda = 5),
  tot_pop = sample(100:1000, 12, replace = TRUE),
  tmax = runif(12, min = 20, max = 35),
  tmax_lag1 = runif(12, min = 20, max = 35),
  tmax_lag2 = runif(12, min = 20, max = 35),
  tmax_lag3 = runif(12, min = 20, max = 35)
)

indexed_cp_data <- create_inla_indices(CP_data, "malaria")
cp_basis_matrices <- set_cross_basis(indexed_cp_data,max_lag=2,nk=2)
cp_tmax_basis <- cp_basis_matrices$tmax
cp_basis_names <- colnames(cp_tmax_basis)

cp_model <- list(
  summary.fixed = list(mean = rnorm(length(cp_basis_names))),
  misc = list(lincomb.derived.covariance.matrix = diag(length(cp_basis_names))),
  names.fixed = cp_basis_names
)

test_that(
  "contour_plot errors if save_fig=T and output_dir=NULL",
  {
    expect_error(
      contour_plot(
        data = CP_data,
        param_term = "tmax",
        max_lag = 3,
        nk = 2,
        model = cp_model,
        level = "country",
        case_type = "malaria",
        save_fig = TRUE
      ),
      "'output_dir' must be provided if save_fig = TRUE"
    )
  }
)

test_that(
  "countour_plot errors if data is missing 'year' column",
  {
    no_year <- CP_data %>% dplyr::select(-"year")
    expect_error(
      contour_plot(
        data = no_year,
        param_term = "tmax",
        max_lag = 3,
        nk = 2,
        model = cp_model,
        level = "country",
        case_type = "malaria",
        filter_year = 2025
      ),
      "'year' column not found in data."
    )
  }
)

test_that(
  "contour_plot saves PDF when save_fig = TRUE",
  {
    tmp_dir <- tempdir()
    suppress_plot(
      contour_plot(
        data = CP_data,
        param_term = "tmax",
        max_lag = 3,
        nk = 2,
        model = cp_model,
        level = "country",
        case_type = "malaria",
        save_fig = TRUE,
        output_dir = tmp_dir
      )
    )
    expected_file <- file.path(tmp_dir, "contour_plot_tmax_country.pdf")
    expect_true(file.exists(expected_file))
  }
)

test_that(
  "contour_plot runs without error for region level", {
    expect_error(
      suppress_plot(
        contour_plot(
          data = CP_data,
          param_term = "tmax",
          max_lag = 3,
          nk = 2,
          model = cp_model,
          level = "region",
          case_type = "malaria"
        )
      ),
      NA
    )
  }
)

test_that(
  "contour_plot runs without error for district level", {
    expect_error(
      suppress_plot(
        contour_plot(
          data = CP_data,
          param_term = "tmax",
          max_lag = 3,
          nk = 2,
          model = cp_model,
          level = "district",
          case_type = "malaria"
        )
      ),
      NA
    )
  }
)

test_that(
  "contour_plot filters to year when instructed", {
    expect_error(
      suppress_plot(
        contour_plot(
          data = CP_data,
          param_term = "tmax",
          max_lag = 3,
          nk = 2,
          model = cp_model,
          level = "district",
          case_type = "malaria",
          filter_year = 2020
        )
      ),
      NA
    )
  }
)

# Test for plot_rr_map

# Additional test data (utilise CP_data also)
RR_tests_map <- sf::st_sf(
  district = unique(CP_data$district),
  geometry = sf::st_sfc(
    lapply(1:4,
      function(i) sf::st_polygon(list(matrix(c(0,0,1,0,1,1,0,1,0,0) + i,
      ncol = 2,
      byrow = TRUE
    ))))
  )
)
RR_tests_map$region <- rep(c("North", "South"), each = 2)

test_that(
  "plot_rr_map errors if save_fig=TRUE and output_dir=NULL",
  {
    expect_error(
      plot_rr_map(
        combined_data = list(data = CP_data, map = RR_tests_map),
        param_term = "tmax",
        max_lag = 2,
        nk = 2,
        model = cp_model,
        level = "district",
        case_type = "malaria",
        save_fig = TRUE
      ),
      "output_dir must be provided if save_fig = TRUE"
    )
  }
)

test_that(
  "plot_rr_map raises an error if 'year' column missing",
  {
    no_year <- CP_data %>% dplyr::select(-"year")
    expect_error(
      plot_rr_map(
        combined_data = list(data = no_year, map = RR_tests_map),
        param_term = "tmax",
        max_lag = 2,
        nk = 2,
        model = cp_model,
        level = "district",
        case_type = "malaria",
        filter_year = 2025
      ),
      "year"
    )
  }
)

test_that(
  "plot_rr_map runs without error for region level",
  {
    expect_error(
      suppressWarnings(
        suppress_plot(
          plot_rr_map(
            combined_data = list(data = CP_data, map = RR_tests_map),
            param_term = "tmax",
            max_lag = 2,
            nk = 2,
            model = cp_model,
            level = "region",
            case_type = "malaria"
          )
        )
      ),
      NA
    )
  }
)

test_that(
  "plot_rr_map saves PDF when save_fig=TRUE",
  {
    tmp_dir <- tempdir()
    suppressWarnings(
      suppress_plot(
        plot_rr_map(
          combined_data = list(data = CP_data, map = RR_tests_map),
          param_term = "tmax",
          max_lag = 2,
          nk = 2,
          model = cp_model,
          level = "district",
          case_type = "malaria",
          save_fig = TRUE,
          output_dir = tmp_dir
        )
      )
    )
    expected_file <- file.path(tmp_dir, "RR_map_tmax_district_all_years.pdf")
    expect_true(file.exists(expected_file))
  }
)

test_that(
  "plot_rr_map filters correctly by year",
  {
    suppressWarnings(
    filtered_plot <- suppress_plot(
      plot_rr_map(
        combined_data = list(data = CP_data, map = RR_tests_map),
        param_term = "tmax",
        max_lag = 2,
        nk = 2,
        model = cp_model,
        level = "district",
        case_type = "malaria",
        filter_year = 2020
      )
    )
    )
    expect_s3_class(filtered_plot, "gg")
  }
)

# Tests for plot_relative_risk

# Test dataset
prr_data <- data.frame(
  year = c(2020, 2020, 2020, 2021, 2021, 2021),
  region = rep(c("North", "South"), each = 3),
  district = rep(c("N1", "N2", "S1"), times = 2),
  tmax = runif(6, 20, 35),
  malaria = rpois(6, lambda = 5)
)

# Minimal mock model
prr_model <- list()

mock_validate_case_type <- function(x) "mock_case"

mock_get_predictions <- function(data, param_term, max_lag, nk, model, level, case_type) {
  if (level == "country") {
    return(list(
      predvar = seq(20, 35, length.out = 5),
      allRRfit = seq(0.8, 1.2, length.out = 5),
      allRRlow = seq(0.6, 1.0, length.out = 5),
      allRRhigh = seq(1.0, 1.4, length.out = 5)
    ))
  }
  if (level %in% c("region", "district")) {
    return(list(
      North = list(
        predvar = seq(20, 35, length.out = 5),
        allRRfit = rep(1, 5),
        allRRlow = rep(0.8, 5),
        allRRhigh = rep(1.2, 5)
      ),
      South = list(
        predvar = seq(20, 35, length.out = 5),
        allRRfit = rep(1.1, 5),
        allRRlow = rep(0.9, 5),
        allRRhigh = rep(1.3, 5)
      )
    ))
  }
}

test_that(
  "plot_relative_risk errors if year column missing",
  {
    bad_data <- prr_data
    bad_data$year <- NULL
    expect_error(
      plot_relative_risk(bad_data, prr_model, "tmax", max_lag = 2, nk = 2, level = "district", case_type = "malaria"),
      "'year' column not found"
    )
  }
)

test_that(
  "plot_relative_risk errors if save_fig=T and output_dir=NULL",
  {
    expect_error(
      plot_relative_risk(prr_data, prr_model, "tmax", max_lag = 2, nk = 2, level = "district", case_type = "malaria", save_fig = TRUE),
      "output_dir must be provided"
    )
  }
)

test_that(
  "plot_relative_risk returns list with plots and RR for country level and can save fig/csv",
  {
    tmpdir <- file.path(tempdir(), "nested/dir")
    with_mocked_bindings(
      {
        # normal run
        res <- climatehealth:::plot_relative_risk(
          prr_data,
          prr_model,
          "tmax",
          case_type = "malaria",
          level = "country",
          filter_year = c(2020, 2021),
          save_fig = TRUE,
          save_csv = TRUE,
          output_dir = tempdir()
        )
        expect_type(res, "list")
        expect_named(res, c("plots", "RR"))

        # run with save_fig + save_csv to hit file.path, pdf(), print(), dev.off(), write.csv
        res2 <- climatehealth:::plot_relative_risk(prr_data, prr_model, "tmax",
          case_type = "malaria", level = "country",
          save_fig = TRUE, save_csv = TRUE, output_dir = tmpdir, max_lag = 2, nk = 2 )
        pdf_path <- file.path(tmpdir, "RR_tmax_country_all_plots.pdf")
        csv_path <- file.path(tmpdir, "RR_tmax_country_all_plots.csv")
        expect_true(file.exists(pdf_path))
        expect_true(file.exists(csv_path))
      },
      get_predictions = mock_get_predictions,
      validate_case_type = mock_validate_case_type,
      .package = "climatehealth"
    )
  }
)

test_that(
  "plot_relative_risk returns list with plots and RR for country level.",
  {
    tmpdir <- file.path(tempdir(), "nested/dir")
    with_mocked_bindings(
      {
        # normal run
        res <- climatehealth:::plot_relative_risk(
          prr_data,
          prr_model,
          "tmax",
          case_type = "malaria",
          level = "country"
        )
        expect_type(res, "list")
        expect_named(res, c("plots", "RR"))
        expect_true("All Years" %in% names(res$RR))

        # run with save_fig + save_csv to hit file.path, pdf(), print(), dev.off(), write.csv
        res2 <- climatehealth:::plot_relative_risk(prr_data, prr_model, "tmax",
          case_type = "malaria", level = "country",
          save_fig = TRUE, save_csv = TRUE, output_dir = tmpdir, max_lag = 2, nk = 2)
        pdf_path <- file.path(tmpdir, "RR_tmax_country_all_plots.pdf")
        csv_path <- file.path(tmpdir, "RR_tmax_country_all_plots.csv")
        expect_true(file.exists(pdf_path))
        expect_true(file.exists(csv_path))
      },
      get_predictions = mock_get_predictions,
      validate_case_type = mock_validate_case_type,
      .package = "climatehealth"
    )
  }
)

test_that(
  "plot_relative_risk returns grouped plots for region level and saves fig/csv",
  {
    tmpdir <- tempdir()
    with_mocked_bindings(
      {
        res <- climatehealth:::plot_relative_risk(prr_data, prr_model, "tmax",
          case_type = "malaria", level = "region")
        expect_type(res, "list")
        expect_named(res, c("plots", "RR"))
        expect_true(all(c("North", "South") %in% names(res$plots)))

        # exercise preds loop, build_plot, wrap_plots, plot_annotation, dev.off, write.csv
        res2 <- climatehealth:::plot_relative_risk(
          prr_data,
          prr_model,
          "tmax",
          max_lag = 2,
          nk = 2,
          case_type = "malaria",
          level = "region",
          save_fig = TRUE,
          save_csv = TRUE,
          output_dir = tmpdir
        )
        pdf_path <- file.path(tmpdir, "RR_tmax_region_all_plots.pdf")
        csv_path <- file.path(tmpdir, "RR_tmax_region_all_plots.csv")
        expect_true(file.exists(pdf_path))
        expect_true(file.exists(csv_path))
      },
      get_predictions = mock_get_predictions,
      validate_case_type = mock_validate_case_type,
      .package = "climatehealth"
    )
  }
)

test_that(
  "plot_relative_risk returns grouped plots for district level with filter_year",
  {
    with_mocked_bindings(
      {
        res <- climatehealth:::plot_relative_risk(
          prr_data,
          prr_model,
          "tmax",
          max_lag = 2,
          nk = 2,
          case_type = "malaria",
          level = "district",
          filter_year = 2020,
          save_fig = TRUE,
          output_dir = tempdir()
        )
        expect_type(res, "list")
        expect_named(res, c("plots", "RR"))
        expect_true("2020" %in% names(res$RR))
      },
      get_predictions = mock_get_predictions,
      validate_case_type = mock_validate_case_type,
      .package = "climatehealth"
    )
  }
)

test_that(
  "plot_relative_risk skips plots when allRRfit contains NA",
  {
    mock_with_na <- function(...) {
      list(
        predvar = seq(20, 35, length.out = 5),
        allRRfit = c(NA, rep(1, 4)),
        allRRlow = rep(0.8, 5),
        allRRhigh = rep(1.2, 5)
      )
    }
    with_mocked_bindings(
      {
        res <- climatehealth:::plot_relative_risk(
          prr_data,
          prr_model,
          "tmax",
          max_lag = 2,
          nk = 2,
          case_type = "malaria",
          level = "country"
        )
        expect_type(res, "list")
      },
      get_predictions = mock_with_na,
      validate_case_type = mock_validate_case_type,
      .package = "climatehealth"
    )
  }
)

test_that(
  "plot_relative_risk unwraps single-element named list for country level",
  {
    mock_single <- function(...) {
      list(only = list(
        predvar = seq(20, 35, length.out = 5),
        allRRfit = seq(0.8, 1.2, length.out = 5),
        allRRlow = seq(0.6, 1.0, length.out = 5),
        allRRhigh = seq(1.0, 1.4, length.out = 5)
      ))
    }
    with_mocked_bindings(
      {
        res <- climatehealth:::plot_relative_risk(
          prr_data,
          prr_model,
          "tmax",
          case_type = "malaria",
          level = "country",
          max_lag = 2,
          nk = 2,
        )
        expect_type(res, "list")
        expect_named(res, c("plots", "RR"))
        # confirm it unwrapped: RR element is not a list-of-list
        expect_true(is.list(res$RR[["All Years"]]))
        expect_true(all(c("predvar","allRRfit") %in% names(res$RR[["All Years"]])))
      },
      get_predictions = mock_single,
      validate_case_type = mock_validate_case_type,
      .package = "climatehealth"
    )
  }
)

# Tests for attribution calculation

# Test dataset
AC_data <- tibble::tibble(
  region = rep("North", 10),
  district = rep("A", 10),
  year = rep(2020, 10),
  month = 1:10,
  temp = seq(10, 50, length.out = 10),
  malaria = rep(10, 10),
  tot_pop = rep(1000, 10),
)

# Use a real crossbasis matrix with variation
real_basis <- dlnm::crossbasis(
  AC_data$temp,
  lag = 0,
  argvar = list(fun = "poly", degree = 2),
  arglag = list(fun = "strata", breaks = 1)
)

# Use real coefficients that match the basis matrix
AC_model <- list(
  summary.fixed = list(mean = c(0.5, 0.3)),
  misc = list(lincomb.derived.covariance.matrix = diag(2)),
  names.fixed = c("basis_temp1", "basis_temp2")
)

mock_validate <- function(x) x
mock_indices <- function(data, case_type) data
mock_basis <- function(data, max_lag, nk) list(temp = real_basis)

test_that(
  "attribution_calculation raises an error if there are no terms for a basis",
  {
    skip_if_not_installed("INLA")
    error_data <- tibble::tibble(
      tmax = seq(10, 50, length.out = 10),
      malaria = rep(10, 10),
    )
    error_model <- list(
      summary.fixed = list(mean = c(1.5, 1.0)),
      misc = list(lincomb.derived.covariance.matrix = diag(2)),
      names.fixed = c("intercept", "basis_other1")
    )
    expect_error(
      attribution_calculation(
        data=error_data,
        param_term="tmax",
        model=error_model,
        level="region",
        case_type="malaria"
      ),
      "No basis terms found for tmax"
    )
  }
)

test_that(
  "attribution_calculation raises an error if filter_year=NULL and year isn't in the data",
  {
    skip_if_not_installed("INLA")
    no_year_data <- AC_data %>% select(-"year")
    expect_error(
      attribution_calculation(
        data=no_year_data,
        param_term="tmax",
        model=AC_model,
        level="region",
        case_type="malaria",
        filter_year=2022
      )
    )
  }
)

test_that(
  "attribution_calculation returns empty tibble for empty data frame",
  {
    skip_if_not_installed("INLA")
    empty_data <- AC_data[0, ]
    with_mocked_bindings(
      {
        result <- suppressWarnings(
          climatehealth:::attribution_calculation(
            data = empty_data,
            param_term = "temp",
            model = AC_model,
            level = "region",
            case_type = "malaria"
          )
        )
        expect_s3_class(result, "tbl_df")
        expect_equal(nrow(result), 0)
      },
      validate_case_type = mock_validate,
      create_inla_indices = mock_indices,
      set_cross_basis = mock_basis,
      .package = "climatehealth"
    )
  }
)

test_that(
  "attribution_calculation returns metrics for zero population",
  {
    skip_if_not_installed("INLA")
    zero_pop_data <- AC_data
    zero_pop_data$tot_pop <- 0

    with_mocked_bindings(
      {
        # Suppress missing cols warning for testing
        result <- expect_warning(
          climatehealth:::attribution_calculation(
            data = zero_pop_data,
            param_term = "temp",
            model = AC_model,
            level = "region",
            case_type = "malaria"
          ),
          "Population denominator is zero or missing"
        )
        expect_s3_class(result, "tbl_df")
        expect_equal(nrow(result), nrow(zero_pop_data))

        # AR_Number, AR_Fraction, AR_per_100k are numeric
        expect_true(is.numeric(result$AR_Number))
        expect_true(is.numeric(result$AR_Fraction))
        expect_true(is.numeric(result$AR_per_100k))

        # AR_Number and AR_Fraction, are non-negative
        expect_true(all(result$AR_Number >= 0, na.rm = TRUE))
        expect_true(all(result$AR_Fraction >= 0, na.rm = TRUE))

        # Point estimate (fit) stored in AR_per_100k must be 0, CI columns may be NA
        expect_true(any(result$AR_per_100k == 0))
        expect_true(all(is.na(result$AR_per_100k) | result$AR_per_100k == 0))

      },
      validate_case_type = mock_validate,
      create_inla_indices = mock_indices,
      set_cross_basis = mock_basis,
      .package = "climatehealth"
    )
  }
)

test_that("MER-empty with valid population returns AR_per_100k = 0", {
  skip_if_not_installed("INLA")

  flat_data <- AC_data
  flat_data$temp <- rep(0, nrow(flat_data))

  mock_crosspred <- function(basis, coef, vcov, model.link, bylag, cen, ...) {
    list(
      predvar    = c(0, 1),
      allRRfit   = c(1, 1),
      allRRlow   = c(1, 1),
      allRRhigh  = c(1, 1)
    )
  }

  with_mocked_bindings(
    {
      with_mocked_bindings(
        {
          result <- climatehealth:::attribution_calculation(
            data = flat_data,
            param_term = "temp",
            model = AC_model,
            level = "region",
            case_type = "malaria"
          )

          expect_true(all(is.na(result$MER_Lower)))
          expect_true(all(is.na(result$MER_Upper)))

          expect_equal(unique(result$AR_per_100k), 0)
        },
        crosspred = mock_crosspred,
        .package = "dlnm"
      )
    },
    validate_case_type = mock_validate,
    create_inla_indices = mock_indices,
    set_cross_basis = mock_basis,
    .package = "climatehealth"
  )
})



test_that("MER-empty with zero population returns AR_per_100k = 0 with no warning", {
  skip_if_not_installed("INLA")

  zero_pop <- AC_data
  zero_pop$tot_pop <- 0
  zero_pop$temp <- rep(0, nrow(zero_pop))

  mock_crosspred <- function(basis, coef, vcov, model.link, bylag, cen, ...) {
    list(
      predvar    = c(0, 1),
      allRRfit   = c(1, 1),
      allRRlow   = c(1, 1),
      allRRhigh  = c(1, 1)
    )
  }

  with_mocked_bindings(
    {
      with_mocked_bindings(
        {
          res <- climatehealth:::attribution_calculation(
            data = zero_pop,
            param_term = "temp",
            model = AC_model,
            level = "region",
            case_type = "malaria"
          )

          # MER empty
          expect_true(all(is.na(res$MER_Lower)))
          expect_true(all(is.na(res$MER_Upper)))

          # AR_per_100k must be 0 (not NA)
          expect_equal(unique(res$AR_per_100k), 0)
        },
        crosspred = mock_crosspred,
        .package = "dlnm"
      )
    },
    validate_case_type = mock_validate,
    create_inla_indices = mock_indices,
    set_cross_basis = mock_basis,
    .package = "climatehealth"
  )
})

test_that(
  "attribution_calculation returns NULL if crosspred fails",
  {
    skip_if_not_installed("INLA")

    mock_crosspred <- function(basis, coef, vcov, model.link, bylag = 1, cen = NULL, ...) {
      stop("crosspred failed")
    }
    # outer mocking: climatehealth
    with_mocked_bindings(
      {
        # inner mocking: dlnm::crosspred
        with_mocked_bindings(
          {
            expect_error(
              climatehealth:::attribution_calculation(
                data       = AC_data,
                param_term = "temp",
                model      = AC_model,
                level      = "region",
                case_type  = "malaria"
              ),
              "crosspred failed"
            )
          },
          crosspred = mock_crosspred,   # no quotes
          .package = "dlnm"
        )
      },
      validate_case_type   = mock_validate,
      create_inla_indices  = mock_indices,
      set_cross_basis      = mock_basis,
      .package = "climatehealth"
    )
  }
)

test_that(
  "attribution_calculation errors if RR vector is invalid",
  {
    skip_if_not_installed("INLA")

    null_basis <- function(data, max_lag, nk) {
      list(temp = real_basis)
    }

    null_model <- AC_model
    null_model$summary.fixed$mean <- rep(NA, length(null_model$summary.fixed$mean))

    expect_error(
      with_mocked_bindings(
        {
          climatehealth:::attribution_calculation(
            data       = AC_data,
            param_term = "temp",
            model      = null_model,
            level      = "region",
            case_type  = "malaria"
          )
        },
        validate_case_type  = mock_validate,
        create_inla_indices = mock_indices,
        set_cross_basis     = null_basis,
        .package = "climatehealth"
      ),
      regexp = "not consistent|coef/vcov"
    )
  }
)

test_that(
  "attribution_calculation returns NULL when group is filtered to zero rows",
  {
    skip_if_not_installed("INLA")
    # Group has rows, but all will be filtered out inside compute_metrics_from_pred()
    tricky_data <- tibble::tibble(
      region = rep("North", 5),
      district = rep("A", 5),
      year = rep(2020, 5),
      month = 1:5,
      temp = c(25, NA, 30, NA, 35),       # some NA
      malaria = c(NA, 10, NA, 15, NA),    # some NA
      tot_pop = c(NA, NA, NA, NA, NA)     # all NA → triggers filter
    )

    # Valid basis matrix to ensure crosspred works
    real_basis <- dlnm::crossbasis(
      x = rep(25, 5),
      lag = 0,
      argvar = list(fun = "poly", degree = 2),
      arglag = list(fun = "strata", breaks = 1)
    )

    valid_model <- list(
      summary.fixed = list(mean = c(0.5, 0.3)),
      misc = list(lincomb.derived.covariance.matrix = diag(2)),
      names.fixed = c("basis_temp1", "basis_temp2")
    )

    mock_validate <- function(x) x
    mock_indices <- function(data, case_type) data
    mock_basis <- function(data, max_lag, nk) list(temp = real_basis)

    with_mocked_bindings(
      {
        suppressWarnings(
          result <- climatehealth:::attribution_calculation(
            data = tricky_data,
            param_term = "temp",
            model = valid_model,
            level = "region",
            case_type = "malaria"
          )
        )
        expect_s3_class(result, "tbl_df")
        expect_equal(nrow(result), 0)  # confirms group was skipped
      },
      validate_case_type = mock_validate,
      create_inla_indices = mock_indices,
      set_cross_basis = mock_basis,
      .package = "climatehealth"
    )
  }
)

test_that("Attribution with valid MER produces numeric AR_per_100k", {
  skip_if_not_installed("INLA")

  # Use AC_data as-is where temp varies enough to produce MER
  valid_res <- with_mocked_bindings(
    climatehealth:::attribution_calculation(
      data = AC_data,
      param_term = "temp",
      model = AC_model,
      level = "region",
      case_type = "malaria",
      param_threshold = 0.5   # ensure RR>threshold for some entries
    ),
    validate_case_type = mock_validate,
    create_inla_indices = mock_indices,
    set_cross_basis = mock_basis,
    .package = "climatehealth"
  )

  # Only test the point estimate column
  ar_point <- valid_res$AR_per_100k

  # Should have some numeric output
  expect_true(any(!is.na(ar_point)))

  # All non-NA values must be finite numeric values
  expect_true(all(is.finite(ar_point[!is.na(ar_point)])))
})

test_that(
  "attribution_calculation runs as expected.",
  {
    skip_if_not_installed("INLA")
    tmp_dir <- file.path(tempdir(), "new_nested_dir")
    if (dir.exists(tmp_dir)) unlink(tmp_dir, recursive = TRUE)
    with_mocked_bindings(
      {
        climatehealth:::attribution_calculation(
          data = AC_data,
          param_term = "temp",
          model = AC_model,
          level = "region",
          case_type = "malaria",
          param_threshold = 0.5,
          save_csv = TRUE,
          output_dir = tmp_dir,
          filter_year = 2020
        )
        expect_true(dir.exists(tmp_dir))
        expect_true(file.exists(file.path(tmp_dir, "attribution_region_temp_monthly.csv")))
      },
      validate_case_type = mock_validate,
      create_inla_indices = mock_indices,
      set_cross_basis = mock_basis,
      .package = "climatehealth"
    )
  }
)

test_that(
  "attribution_calculation runs as expected for country level and AR_Number metric.",
  {
    skip_if_not_installed("INLA")
    tmp_dir <- file.path(tempdir(), "new_nested_dir")
    if (dir.exists(tmp_dir)) unlink(tmp_dir, recursive = TRUE)
    with_mocked_bindings(
      {
        climatehealth:::attribution_calculation(
          data = AC_data,
          param_term = "temp",
          model = AC_model,
          level = "region",
          case_type = "malaria",
          param_threshold = 0.5,
          save_csv = TRUE,
          output_dir = tmp_dir,
          filter_year = 2020
        )
        expect_true(dir.exists(tmp_dir))
        expect_true(file.exists(file.path(tmp_dir, "attribution_region_temp_monthly.csv")))
      },
      validate_case_type = mock_validate,
      create_inla_indices = mock_indices,
      set_cross_basis = mock_basis,
      .package = "climatehealth"
    )
  }
)

# Tests for plot_attribution_metric
test_that(
  "plot_attribution_metric errors if param_term is NULL",
  {
    expect_error(
      plot_attribution_metric(
        attr_data = AC_data,
        param_term = NULL,
        case_type = "malaria"
      ),
      "'param_term' must be provided."
    )
  }
)

test_that(
  "plot_attribution_metric returns NULL and warns if level is 'country' and filter_year is set",
  {
    expect_warning(
      result <- plot_attribution_metric(
        attr_data = AC_data,
        level = "country",
        filter_year = 2020,
        param_term = "temp",
        case_type = "malaria"
      ),
      "If level == 'country', filter_year must be NULL."
    )
    expect_null(result)
  }
)

test_that(
  "plot_attribution_metric errors if 'year' column is missing and filter_year is set",
  {
    no_year_data <- AC_data %>% select(-year)
    expect_error(
      plot_attribution_metric(
        attr_data = no_year_data,
        level = "region",
        filter_year = 2020,
        param_term = "temp",
        case_type = "malaria"
      ),
      "'year' column not found in data."
    )
  }
)

test_that(
  "plot_attribution_metric errors if level column is missing",
  {
    no_region_data <- AC_data %>% select(-region)
    expect_error(
      plot_attribution_metric(
        attr_data = no_region_data,
        level = "region",
        param_term = "temp",
        case_type = "malaria"
      ),
      "'region' column not found in data."
    )
  }
)

test_that(
  "plot_attribution_metric returns ggplot object for valid country-level input",
  {
    valid_data <- AC_data %>%
      dplyr::mutate(
        AR_Number = 20,
        AR_Number_LCI = 18,
        AR_Number_UCI = 23,
        AR_Fraction = 20/10,
        AR_Fraction_LCI = 18/10,
        AR_Fraction_UCI = 23/10
      )
    result <- plot_attribution_metric(
      attr_data = valid_data,
      level = "country",
      param_term = "temp",
      case_type = "malaria",
      metrics = "AR_Number"
    )
    result_frac <- plot_attribution_metric(
      attr_data = valid_data,
      level = "country",
      param_term = "temp",
      case_type = "malaria",
      metrics = "AR_Fraction"
    )
    expect_type(result, "list")
    expect_true(inherits(result[[1]], "ggplot"))
    expect_type(result_frac, "list")
    expect_true(inherits(result_frac[[1]], "ggplot"))
  }
)

test_that(
  "plot_attribution_metric returns list of ggplot objects for region-level input",
  {
    valid_data <- AC_data %>%
      dplyr::mutate(
        AR_Fraction = 0.25,
        AR_Fraction_LCI = 0.2,
        AR_Fraction_UCI = 0.3
      )
    result <- plot_attribution_metric(
      attr_data = valid_data,
      level = "region",
      param_term = "temp",
      case_type = "malaria",
      metrics = "AR_Fraction"
    )
    expect_type(result, "list")
    expect_true(inherits(result[[1]][[1]], "ggplot"))
  }
)

test_that(
  "plot_attribution_metric saves figure when save_fig = TRUE",
  {
    tmp_dir <- file.path(tempdir(), "plot_output_dir")
    if (dir.exists(tmp_dir)) unlink(tmp_dir, recursive = TRUE)

    valid_data <- AC_data %>%
      dplyr::mutate(
        AR_per_100k = 100,
        AR_per_100k_LCI = 90,
        AR_per_100k_UCI = 110
      )

    plot_attribution_metric(
      attr_data = valid_data,
      level = "country",
      param_term = "temp",
      case_type = "malaria",
      metrics = "AR_per_100k",
      save_fig = TRUE,
      output_dir = tmp_dir
    )

    expect_true(dir.exists(tmp_dir))
    expect_true(file.exists(file.path(tmp_dir, "plot_AR_per_100k_temp_country.pdf")))
  }
)

test_that(
  "plot_attribution_metric returns grouped bar plots when filter_year has >2 values and level is region",
  {
    suppressWarnings(library(ggplot2))
    PAM_unique_data <- tibble::tibble(
      region = rep(c("North", "South", "East", "West"), each = 3),
      year = rep(c(2020, 2021, 2022), times = 4),
      AR_Fraction = c(0.2, 0.25, 0.3, 0.1, 0.15, 0.2, 0.05, 0.1, 0.15, 0.3, 0.35, 0.4),
      AR_Fraction_LCI = AR_Fraction - 0.05,
      AR_Fraction_UCI = AR_Fraction + 0.05,
      AR_per_100k = AR_Fraction * 10,
      AR_per_100k_UCI = AR_Fraction_UCI * 100,
      AR_per_100k_LCI = AR_Fraction_LCI * 100
    )

    result <- plot_attribution_metric(
      attr_data = PAM_unique_data,
      level = "region",
      param_term = "temp",
      case_type = "malaria",
      metrics = "AR_per_100k",
      filter_year = c(2020, 2021, 2022)
    )

    result_frac <- plot_attribution_metric(
      attr_data = PAM_unique_data,
      level = "region",
      param_term = "temp",
      case_type = "malaria",
      metrics = "AR_Fraction",
      filter_year = c(2020, 2021, 2022)
    )

    expect_type(result, "list")
    expect_type(result_frac, "list")
    flat_plots <- unlist(result, recursive = FALSE)
    flat_plots_f <- unlist(result_frac, recursive = FALSE)
    expect_true(all(purrr::map_lgl(flat_plots, ~ inherits(.x, "ggplot"))))
    expect_true(all(purrr::map_lgl(flat_plots_f, ~ inherits(.x, "ggplot"))))
  }
)

test_that(
  "plot_attribution_metric saves PDF correctly with district plots",
  {
    library(ggplot2)
    # Data for district plotting
    PAM_unique_data <- tibble::tibble(
      district = paste0("District_", 1:5),
      year = rep(2020, 5),
      AR_Number = c(10, 20, 30, 40, 50),
      AR_Number_LCI = AR_Number - 5,
      AR_Number_UCI = AR_Number + 5
    )

    PAM_unique_data <- PAM_unique_data %>%
      dplyr::mutate(
        AR_Fraction = 0.2,
        AR_Fraction_LCI = 0.15,
        AR_Fraction_UCI = 0.25
      )

    tmp_dir <- file.path(tempdir(), "plot_test_dir")
    if (dir.exists(tmp_dir)) unlink(tmp_dir, recursive = TRUE)

    result <- plot_attribution_metric(
      attr_data = PAM_unique_data,
      level = "district",
      param_term = "temp",
      case_type = "malaria",
      metrics = "AR_Number",
      save_fig = TRUE,
      output_dir = tmp_dir
    )

    # Confirm directory was created
    expect_true(dir.exists(tmp_dir))

    # Confirm PDF file was saved
    expected_file <- file.path(tmp_dir, "plot_AR_Number_temp_district.pdf")
    expect_true(file.exists(expected_file))
    # Confirm result is a list of ggplot objects
    expect_type(result, "list")
    flat_plots <- unlist(result, recursive = FALSE)
    expect_true(all(purrr::map_lgl(flat_plots, ~ inherits(.x, "ggplot"))))
  }
)

test_that(
  "plot_attribution_metric saves multi-year grouped region plots to PDF",
  {
    library(ggplot2)
    # Create 6 regions × 3 years to trigger grouped logic and chunking
    test_data <- expand.grid(
      region = paste0("Region_", 1:6),
      year = c(2020, 2021, 2022)
    ) %>%
      dplyr::mutate(
        AR_per_100k = runif(n(), 100, 500),
        AR_per_100k_LCI = AR_per_100k - 20,
        AR_per_100k_UCI = AR_per_100k + 20,
        AR_Fraction = (AR_per_100k_UCI - AR_per_100k_LCI) / AR_per_100k
      )

    tmp_dir <- file.path(tempdir(), "multi_year_region_pdf")
    if (dir.exists(tmp_dir)) unlink(tmp_dir, recursive = TRUE)

    result <- plot_attribution_metric(
      attr_data = test_data,
      level = "region",
      param_term = "temp",
      case_type = "malaria",
      metrics = "AR_per_100k",
      filter_year = c(2020, 2021, 2022),
      save_fig = TRUE,
      output_dir = tmp_dir
    )

    # Confirm directory and file creation
    expect_true(dir.exists(tmp_dir))
    expected_file <- file.path(tmp_dir, "plot_AR_per_100k_temp_Year_region.pdf")
    expect_true(file.exists(expected_file))

    # Confirm result is a list of ggplot objects
    flat_plots <- unlist(result, recursive = FALSE)
    expect_true(all(purrr::map_lgl(flat_plots, ~ inherits(.x, "ggplot"))))
  }
)

test_that(
  "plot_attribution_metric saves multi-year grouped region plots to PDF for AR_Number",
  {
    library(ggplot2)
    # Create 6 regions × 3 years to trigger grouped logic and chunking
    test_data <- expand.grid(
      region = paste0("Region_", 1:6),
      year = c(2020)
    ) %>%
      dplyr::mutate(
        AR_per_100k = runif(n(), 100, 500),
        AR_per_100k_LCI = AR_per_100k - 20,
        AR_per_100k_UCI = AR_per_100k + 20,
        AR_Fraction = (AR_per_100k_UCI - AR_per_100k_LCI) / AR_per_100k,
        AR_Number = AR_per_100k * 10,
        AR_Number_LCI = AR_Number * 0.91,
        AR_Number_UCI = AR_Number * 1.09
      )

    tmp_dir <- file.path(tempdir(), "multi_year_region_pdf")
    if (dir.exists(tmp_dir)) unlink(tmp_dir, recursive = TRUE)

    result <- plot_attribution_metric(
      attr_data = test_data,
      level = "region",
      param_term = "temp",
      case_type = "malaria",
      metrics = "AR_Number",
      filter_year = c(2020),
      save_fig = TRUE,
      output_dir = tmp_dir
    )

    # Confirm directory and file creation
    expect_true(dir.exists(tmp_dir))
    expected_file <- file.path(tmp_dir, "plot_AR_Number_temp_Year_region.pdf")
    expect_true(file.exists(expected_file))

    # Confirm result is a list of ggplot objects
    flat_plots <- unlist(result, recursive = FALSE)
    expect_true(all(purrr::map_lgl(flat_plots, ~ inherits(.x, "ggplot"))))
  }
)

# Tests for plot_avg_monthly

# Synthetic test fixtures
set.seed(123)

attr_df <- data.frame(
  month   = rep(1:12, 2),
  year    = rep(2020:2021, each = 12),
  region  = rep(c("North", "South"), each = 12),
  district = rep(c("A", "B"), times = 12),
  AR_Number    = runif(24, 10, 50),
  AR_per_100k  = runif(24, 0, 20),
  AR_Fraction  = runif(24, 0, 0.1)
)

attr_list <- list(overall_month = attr_df)

c_df <- data.frame(
  month   = rep(1:12, 2),
  year    = rep(2020:2021, each = 12),
  region  = rep(c("North", "South"), each = 12),
  district = rep(c("A", "B"), times = 12),
  tmax     = runif(24, 20, 35),
  rainfall = runif(24, 0, 100)
)

case_type = "malaria"

# Basic structure tests
test_that("plot_avg_monthly returns a named list per metric", {
  res <- plot_avg_monthly(
    attr_data = attr_list,
    c_data = c_df,
    metrics = c("AR_Number", "AR_Fraction"),
    param_term = "tmax",
    case_type = case_type,
    level = "country",
    save_fig = FALSE
  )

  expect_type(res, "list")
  expect_named(res, c("AR_Number", "AR_Fraction"))
})

test_that("country-level output returns exactly one plot per metric", {
  res <- plot_avg_monthly(
    attr_data = attr_list,
    c_data = c_df,
    metrics = "AR_Number",
    param_term = "tmax",
    case_type = case_type,
    level = "country",
    save_fig = FALSE
  )

  expect_equal(length(res[["AR_Number"]]), 1)
  expect_s3_class(res[["AR_Number"]][["Country"]], "ggplot")
})

# Numeric regression test
test_that("plot_avg_monthly correctly uses mean aggregation for AR_Number", {
  # tiny fixture with known values
  attr_df <- data.frame(
    month = c(1, 1),
    year = c(2020, 2021),
    region = c("North", "North"),
    district = c("A", "A"),
    AR_Number = c(10, 30),
    AR_per_100k = c(5, 15),
    AR_Fraction = c(0.01, 0.02)
  )

  attr_list <- list(overall_month = attr_df)

  c_df <- data.frame(
    month = c(1,1),
    year = c(2020,2021),
    region = c("North","North"),
    district = c("A","A"),
    tmax = c(25, 25)
  )

  res <- plot_avg_monthly(
    attr_data = attr_list,
    c_data = c_df,
    metrics = "AR_Number",
    param_term = "tmax",
    case_type = "malaria",
    level = "region",
    save_fig = FALSE
  )

  pd <- ggplot_build(res$AR_Number$North)$data[[1]]

  # Mean(10, 30) = 20
  expect_equal(unique(pd$y), 20)
})

# Region and district grouping tests
test_that("region-level plots return one entry per region", {
  res <- plot_avg_monthly(
    attr_data = attr_list,
    c_data = c_df,
    metrics = "AR_Number",
    param_term = "tmax",
    case_type = case_type,
    level = "region",
    save_fig = FALSE
  )

  expect_equal(sort(names(res$AR_Number)), sort(unique(attr_df$region)))
  lapply(res$AR_Number, function(p) expect_s3_class(p, "ggplot"))
})

test_that("district-level plots return one entry per district", {
  res <- plot_avg_monthly(
    attr_data = attr_list,
    c_data = c_df,
    metrics = "AR_Number",
    param_term = "tmax",
    case_type = case_type,
    level = "district",
    save_fig = FALSE
  )

  expect_equal(sort(names(res$AR_Number)), sort(unique(attr_df$district)))
})


# 3. Filtering by year
test_that("filter_year restricts data before aggregation", {
  res <- plot_avg_monthly(
    attr_data = attr_list,
    c_data = c_df,
    metrics = "AR_Number",
    filter_year = 2020,
    param_term = "tmax",
    case_type = case_type,
    level = "country",
    save_fig = FALSE
  )

  # Extract the plotted y-values
  p_dat <- ggplot_build(res$AR_Number$Country)$data[[1]]
  expect_equal(nrow(p_dat), 12)     # only one year's worth of months
})

# 4. Validation errors
test_that("invalid level throws an error", {
  expect_error(
    plot_avg_monthly(
      attr_data = attr_list,
      c_data = c_df,
      metrics = "AR_Number",
      param_term = "tmax",
      case_type = case_type,
      level = "banana"
    ),
    "should be one of"
  )
})

test_that("invalid metrics throw an error", {
  expect_error(
    plot_avg_monthly(
      attr_data = attr_list,
      c_data = c_df,
      metrics = "BadMetric",
      param_term = "tmax",
      case_type = case_type
    ),
    "'arg' should be one of \"AR_Number\", \"AR_per_100k\", \"AR_Fraction\""
  )
})

test_that("missing metric column errors", {
  bad_df <- attr_df %>% select(-AR_Number)  # remove needed column

  expect_error(
    plot_avg_monthly(
      attr_data = bad_df,
      c_data = c_df,
      metrics = "AR_Number",
      param_term = "tmax",
      case_type = case_type,
    ),
    regexp = "Metric `AR_Number` is not available in `attr_data`."
  )
})

test_that("missing param_term in climate data errors", {
  expect_error(
    plot_avg_monthly(
      attr_data = attr_list,
      c_data = c_df %>% select(-tmax),
      metrics = "AR_Number",
      param_term = "tmax",
      case_type = case_type
    ),
    regexp = "Column `tmax` not found"
  )
})


# 5. Multiple metrics behave independently
test_that("multiple metrics produce independent plot lists", {
  res <- plot_avg_monthly(
    attr_data = attr_list,
    c_data = c_df,
    metrics = c("AR_Number", "AR_per_100k"),
    param_term = "rainfall",
    case_type = case_type,
    level = "country"
  )

  expect_true("AR_Number" %in% names(res))
  expect_true("AR_per_100k" %in% names(res))
  expect_false(identical(res$AR_Number, res$AR_per_100k))
})


# 6. PDF saving (safe test)
test_that("saving PDF produces a file", {
  tmp <- tempdir()

  plot_avg_monthly(
    attr_data = attr_list,
    c_data = c_df,
    metrics = "AR_Number",
    param_term = "tmax",
    case_type = case_type,
    level = "country",
    save_fig = TRUE,
    output_dir = tmp
  )

  out_path <- file.path(tmp, "monthly_AR_Number_country.pdf")
  expect_true(file.exists(out_path))
  file.remove(out_path)
})
