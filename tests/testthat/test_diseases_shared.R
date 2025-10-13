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