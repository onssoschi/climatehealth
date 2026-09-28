# integration test for diarrhea_do_analysis function
if (Sys.getenv("RUN_INTEGRATION") != "true") {
  testthat::skip("Skipping diarrhea integration test")
}

if (!"package:climatehealth" %in% search()) {
  pkgload::load_all(".", export_all = TRUE, helpers = FALSE, quiet = TRUE)
}

if (!exists("suppress_plot")) {
  source("tests/testthat/helper-utils.R", local = FALSE)
}

if (!exists("load_plumber_env")) {
  source("tests/testthat/helper-plumber.R", local = FALSE)
}

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
      year  = 2020L,
      month = 1:12
    ) |>
    dplyr::arrange(region, district, month) |>
    dplyr::mutate(
      diarrhea = round(40 + 8*sin(month/12*2*pi) + as.numeric(factor(district))*2),
      tot_pop  = 1000L
    )
}

make_climate_fixture_d <- function() {
  tibble::tibble(district = c("D1","D2","D3")) |>
    tidyr::crossing(year = 2020, month = 1:12) |>
    dplyr::mutate(
      tmin       = 20 + sin((month+0)*pi/6) + as.numeric(factor(district))*0.1,
      tmean      = 22 + sin((month+1)*pi/6) + as.numeric(factor(district))*0.2,
      tmax       = 25 + sin((month+2)*pi/6) + as.numeric(factor(district))*0.3,
      rainfall   = 60 + 35*sin((month-2)*pi/6) + as.numeric(factor(district))*1,
      r_humidity = 60 + 5*cos(month*pi/6)     + as.numeric(factor(district))*0.5,
      runoff     = 5  + 2*sin((month+3)*pi/6) + as.numeric(factor(district))*0.2
    )
}

# tests/testthat/test_diarrhea.R
test_that("diarrhea_do_analysis runs end-to-end on synthetic data", {

  skip_if_not_installed("sf")
  skip_if_not_installed("INLA")
  skip_if_integration_disabled()

  health  <- make_health_fixture_d()
  climate <- make_climate_fixture_d()

  # Write a tiny shapefile into a unique temp path
  map_stub <- tempfile("synthetic_map_diarrhea_")
  map_path <- paste0(map_stub, ".shp")
  on.exit(unlink(Sys.glob(paste0(map_stub, ".*"))), add = TRUE)
  map_d <- make_synthetic_map_d() |> sf::st_transform(3857)
  sf::st_write(map_d, map_path, quiet = TRUE, append = FALSE)

  res <- suppress_plot(suppressWarnings(
    diarrhea_do_analysis(
      health_data_path  = health,     # pass data.frame directly
      climate_data_path = climate,    # pass data.frame directly
      map_path          = map_path,
      region_col        = "region",
      district_col      = "district",
      date_col          = NULL,
      year_col          = "year",
      month_col         = "month",
      case_col          = "diarrhea",
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
      family            = "nbinomial",
      group_by_year     = FALSE,
      config            = FALSE,
      save_csv          = FALSE,
      save_model        = FALSE,
      save_fig          = FALSE,
      cumulative        = FALSE,
      output_dir        = NULL
    )
  ))

  # Helper to convert crosspred objects
  extract_rr <- function(rr_entry) {
    tibble::tibble(
      predvar = rr_entry$predvar,
      RR      = rr_entry$allRRfit,
      RR_low  = rr_entry$allRRlow,
      RR_high = rr_entry$allRRhigh
    )
  }

  # Validate high‑level structure of rr_df
  expect_true(is.list(res$rr_df))
  expect_equal(length(res$rr_df), 1)
  expect_true("All Years" %in% names(res$rr_df))

  rr_list <- res$rr_df[[1]]

  # Validate district-level RR objects
  expect_true(is.list(rr_list))
  expect_true(all(names(rr_list) %in% c("D1","D2","D3")))
  expect_true(all(purrr::map_lgl(rr_list, ~ inherits(.x, "crosspred"))))

  # Build RR table
  rr_tables <- purrr::imap_dfr(
    rr_list,
    ~ extract_rr(.x) |> dplyr::mutate(district = .y),
    .id = "district_id"
  )

  # Expected columns
  expect_true(all(c("predvar","RR","RR_low","RR_high","district") %in% names(rr_tables)))

  # RR numeric + finite
  expect_type(rr_tables$predvar, "integer")
  expect_type(rr_tables$RR, "double")
  expect_true(all(is.finite(rr_tables$RR)))

  # CIs logically consistent
  expect_true(all(rr_tables$RR_low <= rr_tables$RR))
  expect_true(all(rr_tables$RR <= rr_tables$RR_high))

  # District identifiers correct
  expect_true(all(rr_tables$district %in% c("D1","D2","D3")))

  # RR table should not be trivial
  expect_gt(nrow(rr_tables), 10)

  # Validate INLA output structure
  expect_true(is.list(res$inla_result))
  expect_true("model" %in% names(res$inla_result))
  expect_true(inherits(res$inla_result$model, "inla"))

  # Fixed effects exist
  expect_true("summary.fixed" %in% names(res$inla_result$model))
  expect_true(is.data.frame(res$inla_result$model$summary.fixed))

  # Random effects present
  expect_true(any(grepl("random", names(res$inla_result$model))),
              info = "INLA model should contain some random effect components")

  # Monthly + yearly random effects exist (null ok)
  expect_true(all(c("reff_plot_monthly","reff_plot_yearly") %in% names(res)))

  # RR map returned
  expect_true("rr_map_plot" %in% names(res))

  # Attribution table checks
  attr_df <- res$attr_frac_num
  expect_true(is.data.frame(attr_df))
  expect_true(all(c("region","district","year","month") %in% names(attr_df)))

  numeric_cols <- purrr::keep(attr_df, is.numeric)
  expect_true(length(numeric_cols) >= 1)
  expect_true(any(is.finite(unlist(numeric_cols))))
  expect_true(any(purrr::map_lgl(numeric_cols, ~ any(!is.na(.x)))))

  expect_gt(nrow(attr_df), 5)
  expect_true("attr_frac_num" %in% names(res))

  # Ensure more than trivial output
  expect_gt(nrow(rr_tables), 10)
})

test_that("diarrhea_do_analysis runs descriptive stats and creates outputs", {

  skip_if_not_installed("sf")
  skip_if_not_installed("INLA")
  skip_if_integration_disabled()

  health  <- make_health_fixture_d()
  climate <- make_climate_fixture_d()

  # Create shapefile
  map_stub <- tempfile("synthetic_map_diarrhea_desc_")
  map_path <- paste0(map_stub, ".shp")
  on.exit(unlink(Sys.glob(paste0(map_stub, ".*"))), add = TRUE)

  map_d <- make_synthetic_map_d() |> sf::st_transform(3857)
  sf::st_write(map_d, map_path, quiet = TRUE, append = FALSE)

  # Temp output directory
  base_out <- tempfile("diarrhea_out_")
  dir.create(base_out)

  # Run pipeline with descriptive stats ON
  res <- suppress_plot(suppressWarnings(
    diarrhea_do_analysis(
      health_data_path  = health,
      climate_data_path = climate,
      map_path          = map_path,
      region_col        = "region",
      district_col      = "district",
      date_col          = NULL,
      year_col          = "year",
      month_col         = "month",
      case_col          = "diarrhea",
      tot_pop_col       = "tot_pop",
      tmin_col          = "tmin",
      tmean_col         = "tmean",
      tmax_col          = "tmax",
      rainfall_col      = "rainfall",
      r_humidity_col    = "r_humidity",
      runoff_col        = "runoff",
      geometry_col      = "geometry",
      max_lag           = 2,
      nk                = 1,
      basis_matrices_choices = "rainfall",
      inla_param        = "rainfall",
      param_term        = "rainfall",
      level             = "district",

      # key additions
      run_descriptive   = TRUE,
      output_dir        = base_out,
      save_fig          = FALSE,
      save_csv          = FALSE,
      save_model        = FALSE
    )
  ))

  # Validate output directory structure

  # Find created analysis directory
  created_dirs <- list.dirs(base_out, recursive = FALSE)
  expect_true(length(created_dirs) > 0,
              info = "No analysis output directory created")

  latest_dir <- created_dirs[which.max(file.info(created_dirs)$mtime)]

  # Check descriptive_stats subfolder exists
  desc_dir <- file.path(latest_dir, "descriptive_stats")
  expect_true(dir.exists(desc_dir),
              info = "descriptive_stats folder not created")

  # Check at least some files were created
  files <- list.files(desc_dir, recursive = TRUE)
  expect_true(length(files) > 0,
              info = "No descriptive stats output files found")

  # pipeline still returns core outputs
  expect_true("rr_df" %in% names(res))
  expect_true(is.list(res$rr_df))
})

test_that("diarrhea_do_analysis errors when save_fig=TRUE and output_dir=NULL", {
  skip_if_not_installed("INLA")

  expect_error(
    diarrhea_do_analysis(
      health_data_path = tibble::tibble(),
      climate_data_path = tibble::tibble(),
      map_path = system.file("shape/nc.shp", package = "sf"),
      region_col = "region",
      district_col = "district",
      year_col = "year",
      month_col = "month",
      case_col = "diarrhea",
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


# --- API mode ---------------------------------------------------------------

test_that("diarrhea_do_analysis in API mode draws nothing and writes nothing", {
  skip_if_not_installed("sf")
  skip_if_not_installed("INLA")
  skip_if_integration_disabled()

  health  <- make_health_fixture_d()
  climate <- make_climate_fixture_d()

  map_stub <- tempfile("api_mode_map_diarrhea_")
  map_path <- paste0(map_stub, ".shp")
  on.exit(unlink(Sys.glob(paste0(map_stub, ".*"))), add = TRUE)
  map_obj <- make_synthetic_map_d() |> sf::st_transform(3857)
  sf::st_write(map_obj, map_path, quiet = TRUE, append = FALSE)

  out_dir <- withr::local_tempdir()
  withr::local_options(list(climatehealth.api_mode = TRUE))

  # Every output is requested; API mode must override all of them.
  res <- expect_no_plotting(suppressWarnings(
    diarrhea_do_analysis(
      health_data_path  = health,
      climate_data_path = climate,
      map_path          = map_path,
      region_col        = "region",
      district_col      = "district",
      date_col          = NULL,
      year_col          = "year",
      month_col         = "month",
      case_col          = "diarrhea",
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
      family            = "nbinomial",
      group_by_year     = FALSE,
      config            = FALSE,
      save_csv          = TRUE,
      save_model        = TRUE,
      save_fig          = TRUE,
      cumulative        = FALSE,
      output_dir        = out_dir
    )
  ))

  # API mode returns the numerical payload only -- no plot objects.
  expect_type(res, "list")
  expect_true(all(
    c("rr_df", "an_ar_results", "attr_frac_num", "dic_table") %in% names(res)
  ))
  expect_true(is.list(res$rr_df))
  expect_gt(length(res$rr_df), 0)

  expect_length(list.files(out_dir, recursive = TRUE), 0)
})

# --- API request shapes -----------------------------------------------------

# Regression test for the 500 that /diarrhea returned:
#   Error in /diarrhea: Can't subset columns with `vars`.
#   x `vars` must be logical, numeric, or character, not a list.
#
# data_explorer_js sends the column multi-selects as JSON arrays and the two
# tabular inputs as arrays of records. `.with_map_zip()` in
# inst/plumber/plumber.R parses that body itself, so this runs the real wrapper
# over a real body: the shapes it produces are exercised by the whole analysis
# rather than only asserted on in isolation
# (see test_plumber_disease_endpoints.R for the isolated assertions).
test_that("the /diarrhea wrapper runs the analysis on a Flask-shaped body", {
  skip_if_not_installed("sf")
  skip_if_not_installed("INLA")
  skip_if_not_installed("jsonlite")
  skip_if_integration_disabled()

  env <- load_plumber_env()

  health  <- make_health_fixture_d()
  climate <- make_climate_fixture_d()

  map_stub <- tempfile("api_body_map_diarrhea_")
  map_path <- paste0(map_stub, ".shp")
  on.exit(unlink(Sys.glob(paste0(map_stub, ".*"))), add = TRUE)
  map_obj <- make_synthetic_map_d() |> sf::st_transform(3857)
  sf::st_write(map_obj, map_path, quiet = TRUE, append = FALSE)

  # The multi-selects are wrapped in list() so that they serialise as
  # single-element JSON arrays under auto_unbox = TRUE -- the shape
  # _split_csv_or_none() produces in _disease_payload_from_form(). map_path is
  # sent directly rather than as a base64 zip: the zip branch is covered in
  # test_plumber_disease_endpoints.R and needs an external zip command.
  #
  # nk and the save_* flags are not sent by the Flask form; they are pinned
  # here to keep the fitted model small and the test free of file output.
  body <- as.character(jsonlite::toJSON(
    list(
      health_data_path  = health,
      climate_data_path = climate,
      map_path          = map_path,
      region_col        = "region",
      district_col      = "district",
      date_col          = NULL,
      year_col          = "year",
      month_col         = "month",
      case_col          = "diarrhea",
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
      basis_matrices_choices = list("rainfall"),
      inla_param        = list("rainfall"),
      param_term        = "rainfall",
      level             = "district",
      param_threshold   = 1,
      nk                = 1,
      config            = FALSE,
      save_csv          = FALSE,
      save_model        = FALSE,
      save_fig          = FALSE
    ),
    dataframe = "rows",
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    digits = NA
  ))

  # Fail loudly if the body is not the shape this test exists to cover, rather
  # than passing on accidentally-unboxed scalars.
  expect_match(body, '"inla_param":["rainfall"]', fixed = TRUE)
  expect_match(body, '"basis_matrices_choices":["rainfall"]', fixed = TRUE)
  expect_match(body, '"spi_col":null', fixed = TRUE)
  expect_match(body, '"health_data_path":[{', fixed = TRUE)

  handler <- env$.with_map_zip(climatehealth::diarrhea_do_analysis)
  res <- suppress_plot(suppressWarnings(handler(list(postBody = body))))

  # Sourcing plumber.R set climatehealth.api_mode = TRUE, exactly as the
  # running API does, so this is the API-mode payload.
  expect_type(res, "list")
  expect_true(all(
    c("rr_df", "an_ar_results", "attr_frac_num", "dic_table") %in% names(res)
  ))
  expect_true(is.list(res$rr_df))
  expect_gt(length(res$rr_df), 0)
  expect_true(is.data.frame(res$attr_frac_num))
})
