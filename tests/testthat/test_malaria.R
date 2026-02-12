# integration test for malaria_do_analysis function

# Create temp_dir to be used by all MH tests
temp_dir <- tempdir()
temp_dir <- file.path(temp_dir, "malaria_tests")
if (!file.exists(temp_dir)) dir.create(temp_dir)

# helpers to produce synthetic data
make_synthetic_map_m <- function() {
  suppressPackageStartupMessages(library(sf))

  regions   <- c("RegionA", "RegionA", "RegionB")
  districts <- c("D1",      "D2",      "D3")

  # Three touching squares
  polys <- list(
    list(matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol = 2, byrow = TRUE)),
    list(matrix(c(1,0, 2,0, 2,1, 1,1, 1,0), ncol = 2, byrow = TRUE)),
    list(matrix(c(2,0, 3,0, 3,1, 2,1, 2,0), ncol = 2, byrow = TRUE))
  )

  sf::st_sf(
    region   = regions,
    district = districts,
    geometry = sf::st_sfc(lapply(polys, sf::st_polygon), crs = 4326)
  )
}

make_health_fixture_m <- function() {
  suppressPackageStartupMessages({ library(dplyr); library(tidyr) })

  rd <- tibble::tibble(
    region   = c("RegionA", "RegionA", "RegionB"),
    district = c("D1", "D2", "D3")
  )

  rd |>
    tidyr::crossing(
      year = 2020L,
      month = 1:12
    ) |>
    arrange(region, district, month) |>
    mutate(
      malaria = round(50 + 10*sin(month/12*2*pi) + as.numeric(factor(district))*3),
      tot_pop = 1000L
    )
}


make_climate_fixture_m <- function() {
  suppressPackageStartupMessages({ library(dplyr); library(tidyr) })

  tibble::tibble(district = c("D1","D2","D3")) |>
    tidyr::crossing(year = 2020, month = 1:12) |>
    mutate(
      # stable but non-collinear seasonal temperature curves
      tmin       = 20 + sin((month+0)*pi/6)  + as.numeric(factor(district))*0.1,
      tmean      = 22 + sin((month+1)*pi/6)  + as.numeric(factor(district))*0.2,
      tmax       = 25 + sin((month+2)*pi/6)  + as.numeric(factor(district))*0.3,

      rainfall   = 50 + 40*sin((month-2)*pi/6) + as.numeric(factor(district)),
      r_humidity = 60 + 5*cos(month*pi/6)      + as.numeric(factor(district))*0.5,
      runoff     = 5 + 2*sin((month+3)*pi/6)   + as.numeric(factor(district))*0.2
    )
}



# tests/testthat/test_malaria.R
test_that("malaria_do_analysis runs end-to-end on synthetic data", {
  skip_if_not_installed("sf")
  skip_if_not_installed("INLA")  # optionally guard if your CI lacks INLA

  health  <- make_health_fixture_m()
  climate <- make_climate_fixture_m()

  # Write a tiny shapefile into a temp folder
  map_path <- file.path(tempdir(), "synthetic_map.shp")
  sf::st_write(make_synthetic_map_m(), map_path, quiet = TRUE)

  res <- malaria_do_analysis(
    health_data_path  = health,        # pass data.frame directly
    climate_data_path = climate,       # pass data.frame directly
    map_path          = map_path,
    region_col        = "region",
    district_col      = "district",
    date_col          = NULL,
    year_col          = "year",
    month_col         = "month",
    case_col          = "malaria",
    case_type         = "malaria",
    tot_pop_col       = "tot_pop",
    tmin_col          = "tmin",
    tmean_col         = "tmean",
    tmax_col          = "tmax",
    rainfall_col      = "rainfall",
    r_humidity_col    = "r_humidity",
    runoff_col        = "runoff",
    geometry_col      = "geometry",
    spi_col           = NULL,
    ndvi_col          = NULL,
    max_lag           = 2,
    nk                = 2,
    basis_matrices_choices = "tmax",
    inla_param        = c("tmax"),
    param_term        = "tmax",
    level             = "district",
    param_threshold   = 1,
    filter_year       = NULL,
    family            = "poisson",
    group_by_year     = FALSE,
    cumulative        = FALSE,
    config            = FALSE,
    save_csv          = FALSE,
    save_model        = FALSE,
    save_fig          = FALSE,
    output_dir        = NULL
  )

  # Basic structural assertions
  expect_type(res, "list")
  expect_true(all(c("inla_result", "attr_frac_num", "rr_df") %in% names(res)))

  # For monthly runs we expect `month` in the attribution table
  expect_true(all(c("region","district","year","month") %in% names(res$attr_frac_num)))
})
