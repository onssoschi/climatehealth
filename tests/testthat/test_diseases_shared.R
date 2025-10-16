# Tests for diseases_shared.R

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
    cvh = c(0.5, 0.4, 0.6, 0.3, 0.35),
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
            cvh_col = "cvh",
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
            tmp_xlsx <- file.path(tempdir(), "LAPD_test_data.xlsx")
            openxlsx::write.xlsx(LAPCD_test_data, tmp_xlsx)
            return(tmp_xlsx)
        }
    ),
    .test_name = c("dataframe input", "rds input", "csv input", "xlsx input")
)

# Tests for combine_health_climate_data

test_that(
    "comine_health_climate_data succesfully joins data from 3 data sources",
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
            cvh_col = "cvh",
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

# Setup test data
PHT_test_data <- data.frame(
  region = c("North", "North", "South", "South"),
  district = c("N1", "N2", "S1", "S2"),
  year = c(2020, 2020, 2021, 2021),
  month = c(1, 2, 1, 2),
  malaria = c(10, 15, 5, 8),
  diarrhea = c(20, 25, 10, 12),
  tmin = c(22, 23, 24, 25),
  tmean = c(28, 29, 30, 31),
  tmax = c(35, 36, 37, 38),
  rainfall = c(100, 110, 120, 130)
)

# Basic test for country-level aggregation
test_that("plot_health_climate_timeseries works at country level", {
  p <- plot_health_climate_timeseries(
    data = PHT_test_data,
    param_term = "tmean",
    level = "country",
    case_type = "malaria"
  )
  expect_s3_class(p, "ggplot")
  expect_true(!("group" %in% names(p$data)))
})

# Test for region-level aggregation
test_that("plot_health_climate_timeseries works at region level", {
  p <- plot_health_climate_timeseries(
    data = PHT_test_data,
    param_term = "tmax",
    level = "region",
    case_type = "malaria"
  )
  expect_s3_class(p, "ggplot")
  expect_equal(unique(p$data$group), c("North", "South"))
})

# Test for district-level aggregation
test_that("plot_health_climate_timeseries works at district level", {
  p <- plot_health_climate_timeseries(
    data = PHT_test_data,
    param_term = "rainfall",
    level = "district",
    case_type = "malaria"
  )
  expect_s3_class(p, "ggplot")
  expect_equal(unique(p$data$group), c("N1", "N2", "S1", "S2"))
})

# Test for plotting all variables
test_that("plot_health_climate_timeseries handles param_term = 'all'", {
  p <- plot_health_climate_timeseries(
    data = PHT_test_data,
    param_term = "all",
    level = "country",
    case_type = "malaria"
  )
  expect_s3_class(p, "ggplot")
  expect_equal(unique(p$data$variable), c("malaria", "tmin", "tmean", "tmax", "rainfall"))
})

# Test filtering by year
test_that("plot_health_climate_timeseries filters by year", {
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
})

test_that("plot_health_climate_timeseries saves figure when save_fig = TRUE", {
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
})

test_that(
    "plot_health_climate_timeseries sums instead of taking mean when param_term==case_col",
    {
        # Create single date dataset
        PHT_data_adjusted <- PHT_test_data %>% 
            mutate(
                date = as.Date(rep("2020-01-01")),
                year = rep(2020),
                month = rep(01)
            )
        # Creat plot
        p <- plot_health_climate_timeseries(
            data = PHT_data_adjusted,
            param_term = c("malaria", "tmin"),
            level = "country",
            case_type = "malaria"
        )
        expect_s3_class(p, "ggplot")
        # Validate aggregation
        expect_equal(p$data$value[1], 38)
    }
)

test_that("plot_health_climate_timeseries errors on missing columns", {
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
})

test_that("plot_health_climate_timeseries errors on invalid level", {
  expect_error(
    plot_health_climate_timeseries(
      data = PHT_test_data,
      param_term = "tmean",
      level = "continent",
      case_type = "malaria"
    ),
    "Invalid level"
  )
})

# Tests for set_cross_basis

# Create test data (2 lags for spline knots, cvh inc.)
CB_test_data <- data.frame(
  tmax = c(30, 32),
  tmax_lag1 = c(29, 31),
  tmax_lag2 = c(28, 30),
  tmin = c(20, 21),
  tmin_lag1 = c(19, 20),
  tmin_lag2 = c(18, 19),
  cvh = c(0.5, 0.6),
  cvh_lag1 = c(0.4, 0.5),
  cvh_lag2 = c(0.3, 0.4)
)

test_that("set_cross_basis returns expected variables (CVH excluded)", {
  cb <- set_cross_basis(CB_test_data, include_cvh = FALSE)

  # Check main outputs
  expect_type(cb, "list")
  expect_equal(sort(names(cb)), sort(c("tmax", "tmin")))

  # Check column name prefixing
  for (var in names(cb)) {
    expect_true(all(grepl(paste0("^basis_", var, "\\."), colnames(cb[[var]]))))
  }

  # Check additional output information
  attrs <- attributes(cb[["tmax"]])
  expect_equal(attrs$class, c("crossbasis", "matrix"))
  expect_equal(round(attrs$argvar$knots, 2), c(30.67, 31.33))
})

test_that("set_cross_basis includes CVH when requested", {
  cb <- set_cross_basis(CB_test_data, include_cvh = TRUE)
  expect_equal(
    sort(names(cb)),
    c("cvh", "tmax", "tmin")
  )
})

test_that("set_cross_basis skips variables with missing lags", {
  data_missing_tmin <- CB_test_data[, !grepl("^tmin", names(CB_test_data))]
  cb <- set_cross_basis(data_missing_tmin)
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
    result <- create_inla_indices(INLA_IND, case_type = "malaria")
    expect_true(all(c(
      "E", "SIR", "district_index", "region_index", "year_index"
    ) %in% names(result)))
  }
)

# Test for correct expected counts and SIR
test_that("create_inla_indices computes E and SIR correctly", {
  result <- create_inla_indices(INLA_IND, case_type = "malaria")
  overall_rate <- sum(INLA_IND$malaria) / sum(INLA_IND$tot_pop)
  expected_E <- overall_rate * INLA_IND$tot_pop
  expected_SIR <- INLA_IND$malaria / expected_E
  expect_equal(result$E, expected_E)
  expect_equal(result$SIR, expected_SIR)
})

test_that("create_inla_indices assigns district indices correctly", {
  result <- create_inla_indices(INLA_IND, case_type = "malaria")
  expect_equal(result$district_index, c(1, 2, 1, 2))
})

test_that("create_inla_indices assigns region indices correctly", {
  result <- create_inla_indices(INLA_IND, case_type = "malaria")
  expect_equal(result$region_index, c(1, 1, 2, 2))
})

test_that("create_inla_indices assigns year indices correctly", {
  result <- create_inla_indices(INLA_IND, case_type = "malaria")
  expect_equal(result$year_index, c(1, 1, 2, 2))
})

test_that("create_inla_indices handles NA values in case column", {
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
set.seed(123)
CHECK_VIF_DF <- data.frame(
  district_code = rep(c("D1", "D2", "D3", "D4", "D5"), 4),
  region_code = rep(c("R1", "R1", "R2", "R2", "R3"), 4),
  year = rep(2020, 20),
  time = 1:20,
  tot_pop = sample(1000:1500, 20, replace = TRUE),
  malaria = sample(10:30, 20, replace = TRUE),
  tmax = rnorm(20, mean = 32, sd = 2),
  tmax_lag1 = rnorm(20, mean = 31, sd = 2),
  tmax_lag2 = rnorm(20, mean = 30, sd = 2),
  tmin = rnorm(20, mean = 22, sd = 2),
  tmin_lag1 = rnorm(20, mean = 21, sd = 2),
  tmin_lag2 = rnorm(20, mean = 20, sd = 2),
  cvh = runif(20, 0.4, 0.8),
  cvh_lag1 = runif(20, 0.3, 0.7),
  cvh_lag2 = runif(20, 0.2, 0.6)
)

test_that(
  "check_diseases_vif returns expected output",
  {
    # Get VIF with warnings supressed (for test dset)
    result <- suppressWarnings(
      check_diseases_vif(
        data = CHECK_VIF_DF,
        inla_param = c("tmax", "tmin"),
        basis_matrices_choices = c("tmax", "tmin"),
        case_type = "malaria"
      )
    )
    expect_type(result, "list")
    expect_named(result, c("vif", "condition_number", "interpretation"))
    expect_true(is.numeric(result$condition_number))
    expect_true(result$interpretation %in% c(
      "Low collinearity", "Moderate collinearity", "High collinearity"
    ))
  }
)

test_that(
  "check_diseases_vif errors on missing basis matrix",
  {
    bad_data <- CHECK_VIF_DF[, !grepl("^tmin", names(CHECK_VIF_DF))]
    expect_error(
      check_diseases_vif(
        data = bad_data,
        inla_param = c("tmax", "tmin"),
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
    bad_data <- CHECK_VIF_DF[, !(names(CHECK_VIF_DF) %in% c("tmin"))]
    expect_error(
      check_diseases_vif(
        data = bad_data,
        inla_param = c("tmax", "tmin"),
        basis_matrices_choices = c("tmax"),
        case_type = "malaria"
      ),
      "Missing in data: tmin"
    )
  }
)

# Moderate Colinearity
MOD_COL_DF <- data.frame(
  district_code = rep(c("D1", "D2", "D3", "D4", "D5"), 20),
  region_code = rep(c("R1", "R2", "R3", "R1", "R2"), 20),
  year = rep(2020, 100),
  time = 1:100,
  tot_pop = sample(1000:1500, 100, replace = TRUE),
  malaria = sample(10:30, 100, replace = TRUE),
  tmax = rnorm(100, mean = 32, sd = 2),
  tmax_lag1 = rnorm(100, mean = 31, sd = 2),
  tmax_lag2 = rnorm(100, mean = 30, sd = 2),
  tmin = rnorm(100, mean = 22, sd = 2),
  tmin_lag1 = rnorm(100, mean = 21, sd = 2),
  tmin_lag2 = rnorm(100, mean = 20, sd = 2),
  cvh = runif(100, 0.4, 0.8),
  cvh_lag1 = runif(100, 0.3, 0.7),
  cvh_lag2 = runif(100, 0.2, 0.6)
)

test_that(
  "check_diseases_vif detects moderate collinearity",
  {
    result <- suppressWarnings(
      check_diseases_vif(
        data = MOD_COL_DF,
        inla_param = c("tmax", "tmin"),
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
  cvh = runif(200, 0.4, 0.8),
  cvh_lag1 = runif(200, 0.3, 0.7),
  cvh_lag2 = runif(200, 0.2, 0.6)
)

test_that(
  "check_diseases_vif detects low collinearity",
  {
    result <- suppressWarnings(
      check_diseases_vif(
        data = LOW_COL_DF,
        inla_param = c("tmax", "tmin"),
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
    # generate and write results
    temp_dir <- tempdir()
    result <- check_and_write_vif(
      data = MOD_COL_DF,
      inla_param = c("tmax", "tmin"),
      basis_matrices_choices = c("tmax", "tmin"),
      case_type = "malaria",
      output_dir = temp_dir
    )
    result <<- result
    # validate returned values
    expect_equal(round(result$condition_number, 2), 10.07)
    expect_equal(result$interpretation, "Moderate collinearity")
    # validate file outputs
    output_fpath <- file.path(temp_dir, "VIF_results.csv")
    expect_true(file.exists(output_fpath))
    outputted_df <- read.csv(output_fpath)
    expect_equal(nrow(outputted_df), 20)
    expect_equal(ncol(outputted_df), 2)
  }
)

# TODO: Add INLA to CI to accomodate tests
# Tests for run_inla_models

test_that(
  "run_inla_models raises an error when save_model==T and output_dir==NULL",
  {
    expect_error(
      run_inla_models(
        data.frame(),
        c(),
        c(),
        "malaria",
        NULL,
        TRUE
      ),
      "output_dir must be provided if save_csv = TRUE"
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
        run_inla_models(NULL, NULL, NULL, "malaria"),
        "INLA is not installed"
      ),
    )
  }
)

# Shared test fixtures
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

# Test 1: basis_matrices_choices = NULL
test_that(
  "run_inla_models handles NULL basis_matrices_choices",
  {
    with_mocked_bindings(
      `inla.rerun` = function(x) RIM_make_mock_model(100, 0.8),
      .package = "INLA",
      code = {
        result <- run_inla_models(
          combined_data = RIM_combined_data,
          basis_matrices_choices = NULL,
          inla_param = character(),
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
    RIM_temp_dir <- tempdir()
    with_mocked_bindings(
      `inla.rerun` = function(x) RIM_make_mock_model(200, 0.95),
      .package = "INLA",
      code = {
        run_inla_models(
          combined_data = RIM_combined_data,
          basis_matrices_choices = "tmax",
          inla_param = "tmax",
          case_type = "malaria",
          output_dir = RIM_temp_dir,
          save_model = TRUE
        )
        RIM_saved_files <- list.files(
          RIM_temp_dir, pattern = "^model_with_.*\\.csv$", full.names = TRUE
        )
        expect_true(length(RIM_saved_files) == 1)
        expect_true(file.exists(RIM_saved_files[1]))
      }
    )
  }
)

test_that(
  "run_inla_models includes basis and raw variables in model string",
  {
    with_mocked_bindings(
      `inla.rerun` = function(x) RIM_make_mock_model(300, 0.85),
      .package = "INLA",
      code = {
        result <- run_inla_models(
          combined_data = RIM_combined_data,
          basis_matrices_choices = "tmax",
          inla_param = "humidity",
          case_type = "malaria"
        )
        expect_equal(result$dic_table$Model, "tmax + humidity")
        expect_equal(result$dic_table$DIC, 300)
      }
    )
  }
)


