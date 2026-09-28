# Tests for the /diarrhea and /malaria request wrappers in inst/plumber/plumber.R
#
# These two endpoints are the only ones that parse their own request body, so
# they are the only ones that can hand `*_do_analysis()` argument shapes that
# differ from the ones plumber's default body parser produces. That divergence
# is what these tests pin down.

if (!"package:climatehealth" %in% search()) {
  pkgload::load_all(".", export_all = TRUE, helpers = FALSE, quiet = TRUE)
}

if (!exists("load_plumber_env")) {
  source("tests/testthat/helper-plumber.R", local = FALSE)
}

# The payload fields that data_explorer_js sends to /diarrhea and /malaria.
# Mirrors _disease_payload_from_form() in
# data_explorer/routes/indicator_routes.py. Kept here as a drift canary: if the
# Flask form grows a field that is not a formal of the analysis functions,
# `.with_map_zip()` will silently drop it and this test says so.
DISEASE_PAYLOAD_FIELDS <- c(
  "region_col", "district_col", "date_col", "year_col", "month_col",
  "case_col", "tot_pop_col", "tmin_col", "tmean_col", "tmax_col",
  "rainfall_col", "r_humidity_col", "runoff_col", "geometry_col",
  "spi_col", "ndvi_col", "param_term", "level",
  "basis_matrices_choices", "inla_param", "param_threshold", "max_lag",
  "health_data_path", "climate_data_path",
  "map_zip_b64"
)

# A body shaped exactly like the JSON data_explorer_js posts: scalar strings,
# a JSON null, bare numbers, string arrays, and arrays of records.
disease_request_body <- function(map_field) {
  paste0(
    '{
      "region_col": "region",
      "district_col": "district",
      "level": "district",
      "param_term": "rainfall",
      "spi_col": null,
      "max_lag": 2,
      "param_threshold": 1,
      "inla_param": ["rainfall"],
      "basis_matrices_choices": ["rainfall", "tmax"],
      "health_data_path": [
        {"region": "RegionA", "district": "D1", "year": 2020, "month": 1,
         "diarrhea": 40, "tot_pop": 1000},
        {"region": "RegionA", "district": "D2", "year": 2020, "month": 1,
         "diarrhea": 45, "tot_pop": 1100}
      ],
      "climate_data_path": [
        {"district": "D1", "year": 2020, "month": 1, "rainfall": 60.5},
        {"district": "D2", "year": 2020, "month": 1, "rainfall": 70.25}
      ],
      "dataset_title": "a UI-only field that is not a formal",
      ',
    map_field,
    "\n    }"
  )
}

# A stub standing in for `*_do_analysis()`. Every formal is assigned into
# `captured` explicitly so no argument is left as an unforced promise.
capturing_analysis_stub <- function(store) {
  function(health_data_path, climate_data_path, map_path,
           region_col, district_col, level, param_term, spi_col,
           max_lag, param_threshold, inla_param, basis_matrices_choices) {
    store$captured <- list(
      health_data_path = health_data_path,
      climate_data_path = climate_data_path,
      map_path = map_path,
      region_col = region_col,
      district_col = district_col,
      level = level,
      param_term = param_term,
      spi_col = spi_col,
      max_lag = max_lag,
      param_threshold = param_threshold,
      inla_param = inla_param,
      basis_matrices_choices = basis_matrices_choices
    )
    "analysis-ran"
  }
}

test_that(".with_map_zip passes simplified argument shapes to the analysis function", {
  env <- load_plumber_env()

  store <- new.env(parent = emptyenv())
  handler <- env$.with_map_zip(capturing_analysis_stub(store))

  # Replace the decoder so the shapes under test don't depend on an external
  # zip command. The real decoder is covered by its own test below.
  decoded_path <- file.path(tempdir(), "decoded_map.shp")
  env$.decode_map_zip_to_path <- function(b64) {
    store$seen_b64 <- b64
    decoded_path
  }

  body <- disease_request_body('"map_zip_b64": "cGxhY2Vob2xkZXI="')
  result <- handler(list(postBody = body))

  # do.call() succeeded, so the fields that are not formals of the analysis
  # function ("dataset_title", and "map_zip_b64" after it is consumed) were
  # filtered out rather than forwarded.
  expect_identical(result, "analysis-ran")

  captured <- store$captured

  # Scalar strings stay length-1 character vectors.
  expect_identical(captured$region_col, "region")
  expect_identical(captured$district_col, "district")
  expect_identical(captured$level, "district")
  expect_identical(captured$param_term, "rainfall")

  # A JSON null arrives as NULL, so defaulted arguments keep working.
  expect_null(captured$spi_col)

  # Numbers arrive as length-1 numeric vectors, not lists.
  expect_false(is.list(captured$max_lag))
  expect_true(is.numeric(captured$max_lag))
  expect_equal(captured$max_lag, 2)
  expect_false(is.list(captured$param_threshold))
  expect_true(is.numeric(captured$param_threshold))
  expect_equal(captured$param_threshold, 1)

  # String arrays arrive as character vectors, NOT lists. This is the shape
  # that check_diseases_vif() needs in order to subset `data` by name; a
  # one-element array must simplify just like a longer one.
  expect_type(captured$inla_param, "character")
  expect_identical(captured$inla_param, "rainfall")
  expect_type(captured$basis_matrices_choices, "character")
  expect_identical(captured$basis_matrices_choices, c("rainfall", "tmax"))

  # Arrays of records arrive as data.frames, which is what the
  # `*_data_path` arguments document and what coerce_api_records_df()
  # returns unchanged.
  expect_s3_class(captured$health_data_path, "data.frame")
  expect_equal(nrow(captured$health_data_path), 2)
  expect_equal(ncol(captured$health_data_path), 6)
  expect_true(all(
    c("region", "district", "year", "month", "diarrhea", "tot_pop") %in%
      names(captured$health_data_path)
  ))
  expect_identical(captured$health_data_path$district, c("D1", "D2"))

  expect_s3_class(captured$climate_data_path, "data.frame")
  expect_equal(nrow(captured$climate_data_path), 2)
  expect_true(is.numeric(captured$climate_data_path$rainfall))
  expect_equal(captured$climate_data_path$rainfall, c(60.5, 70.25))

  # map_zip_b64 was handed to the decoder and replaced by map_path.
  expect_identical(store$seen_b64, "cGxhY2Vob2xkZXI=")
  expect_identical(captured$map_path, decoded_path)
})

test_that(".with_map_zip leaves an explicit map_path alone when no zip is sent", {
  env <- load_plumber_env()

  store <- new.env(parent = emptyenv())
  handler <- env$.with_map_zip(capturing_analysis_stub(store))

  env$.decode_map_zip_to_path <- function(b64) {
    testthat::fail("The decoder must not run when no map_zip_b64 is present.")
  }

  body <- disease_request_body('"map_path": "/some/where/map.shp"')
  result <- handler(list(postBody = body))

  expect_identical(result, "analysis-ran")
  expect_identical(store$captured$map_path, "/some/where/map.shp")
})

test_that("every field the Flask disease form sends is consumed by both endpoints", {
  for (fn in list(diarrhea_do_analysis, malaria_do_analysis)) {
    fn_args <- names(formals(fn))
    # map_zip_b64 is not a formal: .with_map_zip() turns it into map_path.
    sent <- setdiff(DISEASE_PAYLOAD_FIELDS, "map_zip_b64")
    expect_equal(setdiff(sent, fn_args), character(0))
    expect_true("map_path" %in% fn_args)
  }
})

test_that(".decode_map_zip_to_path extracts a shapefile from a base64 zip", {
  env <- load_plumber_env()

  zip_cmd <- Sys.getenv("R_ZIPCMD", "zip")
  skip_if(
    !nzchar(Sys.which(zip_cmd)),
    paste0("No external zip command available (looked for '", zip_cmd, "').")
  )

  expect_null(env$.decode_map_zip_to_path(NULL))
  expect_null(env$.decode_map_zip_to_path(""))

  staging <- withr::local_tempdir()
  for (ext in c("shp", "shx", "dbf")) {
    writeBin(as.raw(0:9), file.path(staging, paste0("map.", ext)))
  }

  zip_path <- withr::local_tempfile(fileext = ".zip")
  withr::with_dir(
    staging,
    utils::zip(zip_path, list.files(staging), flags = "-qr9X")
  )
  skip_if(!file.exists(zip_path), "Could not build a test zip archive.")

  b64 <- jsonlite::base64_enc(readBin(zip_path, "raw", file.size(zip_path)))
  shp <- env$.decode_map_zip_to_path(b64)

  expect_type(shp, "character")
  expect_length(shp, 1)
  expect_match(shp, "\\.shp$")
  expect_true(file.exists(shp))
})

test_that(".decode_map_zip_to_path errors when the zip holds no shapefile", {
  env <- load_plumber_env()

  zip_cmd <- Sys.getenv("R_ZIPCMD", "zip")
  skip_if(
    !nzchar(Sys.which(zip_cmd)),
    paste0("No external zip command available (looked for '", zip_cmd, "').")
  )

  staging <- withr::local_tempdir()
  writeLines("not a shapefile", file.path(staging, "readme.txt"))

  zip_path <- withr::local_tempfile(fileext = ".zip")
  withr::with_dir(
    staging,
    utils::zip(zip_path, list.files(staging), flags = "-qr9X")
  )
  skip_if(!file.exists(zip_path), "Could not build a test zip archive.")

  b64 <- jsonlite::base64_enc(readBin(zip_path, "raw", file.size(zip_path)))
  expect_error(
    env$.decode_map_zip_to_path(b64),
    "No .shp file found"
  )
})
