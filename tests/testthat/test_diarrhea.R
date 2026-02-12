# integration test for diarrhea_do_analysis function

# Create temp_dir to be used by all Diarrhea tests (kept even though not saving files)
temp_dir <- tempdir()
temp_dir <- file.path(temp_dir, "diarrhea_tests")
if (!file.exists(temp_dir)) dir.create(temp_dir)

# Helpers

make_synthetic_map_d <- function() {
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

make_health_fixture_d <- function() {
  rd <- tibble::tibble(
    region   = c("RegionA", "RegionA", "RegionB"),
    district = c("D1", "D2", "D3")
  )

  rd |>
    tidyr::crossing(
      year = 2020L,
      month = 1:12
    ) |>
    dplyr::arrange(region, district, month) |>
    dplyr::mutate(
      # synthetic diarrhea counts with mild seasonal pattern
      diarrhea = round(40 + 8*sin(month/12*2*pi) + as.numeric(factor(district))*2),
      tot_pop  = 1000L
    )
}

make_climate_fixture_d <- function() {
  tibble::tibble(district = c("D1","D2","D3")) |>
    tidyr::crossing(year = 2020, month = 1:12) |>
    dplyr::mutate(
      # stable but non-collinear seasonal temperature curves
      tmin       = 20 + sin((month+0)*pi/6)  + as.numeric(factor(district))*0.1,
      tmean      = 22 + sin((month+1)*pi/6)  + as.numeric(factor(district))*0.2,
      tmax       = 25 + sin((month+2)*pi/6)  + as.numeric(factor(district))*0.3,

      # rainfall drives diarrhea risk here
      rainfall   = 60 + 35*sin((month-2)*pi/6) + as.numeric(factor(district))*1,
      r_humidity = 60 +  5*cos(month*pi/6)     + as.numeric(factor(district))*0.5,
      runoff     =  5 +  2*sin((month+3)*pi/6) + as.numeric(factor(district))*0.2
    )
}

# Test

test_that("diarrhea_do_analysis runs end-to-end on synthetic data", {
  skip_if_not_installed("sf")
  skip_if_not_installed("INLA")  # guard in case CI lacks INLA

  health  <- make_health_fixture_d()
  climate <- make_climate_fixture_d()

  # Write a tiny shapefile into a temp folder
  map_path <- file.path(tempdir(), "synthetic_map_diarrhea.shp")
  map_d <- make_synthetic_map_d() |> sf::st_transform(3857)
  sf::st_write(map_d, map_path, quiet = TRUE)

  res <- suppressWarnings(
    diarrhea_do_analysis(
      health_data_path  = health,        # pass data.frame directly
      climate_data_path = climate,       # pass data.frame directly
      map_path          = map_path,
      region_col        = "region",
      district_col      = "district",
      date_col          = NULL,
      year_col          = "year",
      month_col         = "month",
      case_col          = "diarrhea",
      case_type         = "diarrhea",
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
      nk                = 1,
      basis_matrices_choices = "rainfall",
      inla_param        = c("rainfall"),
      param_term        = "rainfall",
      level             = "district",
      param_threshold   = 1,
      filter_year       = NULL,
      family            = "poisson",
      group_by_year     = FALSE,
      config            = FALSE,    # keep simple in CI
      save_csv          = FALSE,    # avoid output_dir requirement
      save_model        = FALSE,
      save_fig          = FALSE,
      cumulative        = FALSE,
      output_dir        = NULL
    )
  )

  rr_df <- res$rr_df
  dput(names(rr_df))

  # Basic structural assertions
  expect_type(res, "list")
  expect_true(all(c("inla_result", "attr_frac_num", "rr_df") %in% names(res)))

  # For monthly runs we expect `month` in the attribution table
  expect_true(all(c("region","district","year","month") %in% names(res$attr_frac_num)))
})
