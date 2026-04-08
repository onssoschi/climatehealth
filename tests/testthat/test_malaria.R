# integration test for malaria_do_analysis function
if (Sys.getenv("RUN_INTEGRATION") != "true") {
  testthat::skip("Skipping malaria integration test")
}

if (!"package:climatehealth" %in% search()) {
  pkgload::load_all(".", export_all = TRUE, helpers = FALSE, quiet = TRUE)
}

if (!exists("suppress_plot")) {
  source("tests/testthat/helper-utils.R", local = FALSE)
}

# Create temp_dir to be used by all MH tests
temp_dir <- tempdir()
temp_dir <- file.path(temp_dir, "malaria_tests")
if (!file.exists(temp_dir)) dir.create(temp_dir)

# helpers to produce synthetic data
make_synthetic_map_m <- function() {
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
      malaria = round(50 + 10*sin(month/12*2*pi) + as.numeric(factor(district))*3),
      tot_pop = 1000L
    )
}


make_climate_fixture_m <- function() {
  tibble::tibble(district = c("D1","D2","D3")) |>
    tidyr::crossing(year = 2020, month = 1:12) |>
    dplyr::mutate(
      # stable but non-collinear seasonal temperature curves
      tmin       = 20 + sin((month+0)*pi/6)  + as.numeric(factor(district))*0.1,
      tmean      = 22 + sin((month+1)*pi/6)  + as.numeric(factor(district))*0.2,
      tmax       = 25 + sin((month+2)*pi/6)  + as.numeric(factor(district))*0.3,

      rainfall   = 50 + 40*sin((month-2)*pi/6) + as.numeric(factor(district)),
      r_humidity = 60 + 5*cos(month*pi/6)      + as.numeric(factor(district))*0.5,
      runoff     = 5 + 2*sin((month+3)*pi/6)   + as.numeric(factor(district))*0.2
    )
}

mock_min_data <- tibble(
  region="A",
  district="D1",
  year=2020,
  month=1,
  time=1,
  district_code=1,
  region_code=1,
  malaria=5,
  tot_pop=100,
  tmin=20,
  tmean=21,
  tmax=22,
  rainfall=10,
  r_humidity=50,
  runoff=5,
  E=5
)

# tests/testthat/test_malaria.R
test_that("malaria_do_analysis runs end-to-end on synthetic data", {

  skip_if_not_installed("sf")
  skip_if_not_installed("INLA")
  skip_on_cran()
  skip_if(Sys.getenv("RUN_INTEGRATION") != "true")

  health  <- make_health_fixture_m()
  climate <- make_climate_fixture_m()

  # Write a tiny shapefile into a unique temp path
  map_stub <- tempfile("synthetic_map_malaria_")
  map_path <- paste0(map_stub, ".shp")
  on.exit(unlink(Sys.glob(paste0(map_stub, ".*"))), add = TRUE)
  map_m <- make_synthetic_map_m() |> sf::st_transform(3857)
  sf::st_write(map_m, map_path, quiet = TRUE, append = FALSE)

  res <- suppress_plot(suppressWarnings(
    malaria_do_analysis(
      health_data_path  = health,        # pass data.frame directly
      climate_data_path = climate,       # pass data.frame directly
      map_path          = map_path,
      region_col        = "region",
      district_col      = "district",
      date_col          = NULL,
      year_col          = "year",
      month_col         = "month",
      case_col          = "malaria",
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
      basis_matrices_choices = "tmax",
      inla_param        = c("tmax"),
      param_term        = "tmax",
      level             = "district",
      param_threshold   = 1,
      filter_year       = NULL,
      family            = "nbinomial",
      group_by_year     = FALSE,
      cumulative        = FALSE,
      config            = FALSE,
      save_csv          = FALSE,
      save_model        = FALSE,
      save_fig          = FALSE,
      output_dir        = NULL
    )
  ))

  extract_rr <- function(rr_entry) {
    tibble::tibble(
      predvar  = rr_entry$predvar,
      RR       = rr_entry$allRRfit,
      RR_low   = rr_entry$allRRlow,
      RR_high  = rr_entry$allRRhigh
    )
  }

  rr_list <- res$rr_df[[1]]

  # Validate high‑level structure of rr_df
  expect_true(is.list(res$rr_df))
  expect_equal(length(res$rr_df), 1)
  expect_true("All Years" %in% names(res$rr_df))

  # Validate that rr_list is a list of crosspred objects keyed by district
  expect_true(is.list(rr_list))
  expect_true(all(names(rr_list) %in% c("D1","D2","D3")))
  expect_true(all(purrr::map_lgl(rr_list, ~ inherits(.x, "crosspred"))))

  # Build RR table for deeper testing
  rr_tables <- purrr::imap_dfr(
    rr_list,
    ~ extract_rr(.x) |> dplyr::mutate(district = .y),
    .id = "district_id"
  )

  # RR table should have expected columns
  expect_true(all(c("predvar","RR","RR_low","RR_high","district") %in% names(rr_tables)))

  # Predvar is numeric
  expect_type(rr_tables$predvar, "double")

  # RR values must be numeric and finite
  expect_type(rr_tables$RR, "double")
  expect_true(all(is.finite(rr_tables$RR)))

  # Confidence interval sanity
  expect_true(all(rr_tables$RR_low <= rr_tables$RR))
  expect_true(all(rr_tables$RR <= rr_tables$RR_high))

  # District identifiers present and correct
  expect_true(all(rr_tables$district %in% c("D1","D2","D3")))

  # RR table should have multiple rows
  expect_gt(nrow(rr_tables), 10)

  # Validate INLA output structure
  expect_true(is.list(res$inla_result))
  expect_true("model" %in% names(res$inla_result))
  expect_true(inherits(res$inla_result$model, "inla"))

  # Model should include fixed effects summary
  expect_true("summary.fixed" %in% names(res$inla_result$model))
  expect_true(is.data.frame(res$inla_result$model$summary.fixed))

  # Random effects should exist
  expect_true(any(grepl("random", names(res$inla_result$model))),
              info = "INLA model should contain some random effect components")

  # Spatial & seasonal components
  # These may be NULL if level != "country", but object names should still exist
  expect_true(all(c("reff_plot_monthly","reff_plot_yearly") %in% names(res)))

  # RR map objects exist
  expect_true("rr_map_plot" %in% names(res))

  # Attribution output
  attr_df <- res$attr_frac_num
  expect_true(is.data.frame(attr_df))

  # Must contain core ID variables
  expect_true(all(c("region","district","year","month") %in% names(attr_df)))

  # Attribution numeric columns (does not assert specific names)
  numeric_cols <- purrr::keep(attr_df, is.numeric)
  expect_true(length(numeric_cols) >= 1)

  # Ensure at least one numeric column has at least one finite value
  expect_true(any(is.finite(unlist(numeric_cols))))

  # Ensure at least one numeric column has non-NA values
  expect_true(any(purrr::map_lgl(numeric_cols, ~ any(!is.na(.x)))))

  # Ensure attribution table is not empty
  expect_gt(nrow(attr_df), 5)

  # Combined-data
  # ensure combined_data workflow didn't silently fail
  expect_true("attr_frac_num" %in% names(res))
  expect_gt(nrow(res$attr_frac_num), 5)

  # A simple check that the model produced more than trivial output
  expect_true(nrow(rr_tables) > 10)
})

test_that("malaria_do_analysis errors when save_fig=TRUE and output_dir=NULL", {
  skip_if_not_installed("INLA")

  expect_error(
    malaria_do_analysis(
      health_data_path = tibble::tibble(),
      climate_data_path = tibble::tibble(),
      map_path = system.file("shape/nc.shp", package = "sf"),
      region_col = "region",
      district_col = "district",
      year_col = "year",
      month_col = "month",
      case_col = "malaria",
      tot_pop_col = "tot_pop",
      tmin_col = "tmin",
      tmean_col = "tmean",
      tmax_col = "tmax",
      rainfall_col = "rainfall",
      r_humidity_col = "r_humidity",
      runoff_col = "runoff",
      geometry_col = "geometry",
      basis_matrices_choices = "tmax",
      inla_param = "tmax",
      param_term = "tmax",
      save_fig = TRUE,
      output_dir = NULL
    ),
    regexp = "output_dir"
  )
})
