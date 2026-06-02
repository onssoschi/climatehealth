# Unit tests for descriptive-stat argument forwarding in do_analysis functions

if (!"package:climatehealth" %in% search()) {
  pkgload::load_all(".", export_all = TRUE, helpers = FALSE, quiet = TRUE)
}

test_that("diarrhea_do_analysis forwards descriptive correlation_method", {
  captured_method <- NULL
  output_dir <- tempfile("diarrhea_desc_args_")
  dir.create(output_dir)

  local_mocked_bindings(
    combine_health_climate_data = function(...) {
      list(data = data.frame(
        region = "A",
        district = "D1",
        diarrhea = 1,
        tot_pop = 1000,
        tmean = 20,
        rainfall = 1,
        runoff = 1,
        r_humidity = 50
      ))
    },
    run_descriptive_stats = function(..., correlation_method = "not supplied") {
      captured_method <<- correlation_method
      stop("captured correlation method", call. = FALSE)
    }
  )

  expect_error(
    diarrhea_do_analysis(
      health_data_path = data.frame(),
      climate_data_path = data.frame(),
      map_path = data.frame(),
      region_col = "region",
      district_col = "district",
      date_col = NULL,
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
      basis_matrices_choices = "rainfall",
      inla_param = "rainfall",
      param_term = "rainfall",
      level = "district",
      output_dir = output_dir,
      run_descriptive = TRUE,
      correlation_method = "spearman",
      save_fig = FALSE,
      save_csv = FALSE,
      save_model = FALSE
    ),
    "captured correlation method"
  )

  expect_equal(captured_method, "spearman")
})

test_that("malaria_do_analysis forwards descriptive correlation_method", {
  captured_method <- NULL
  output_dir <- tempfile("malaria_desc_args_")
  dir.create(output_dir)

  local_mocked_bindings(
    combine_health_climate_data = function(...) {
      list(data = data.frame(
        region = "A",
        district = "D1",
        malaria = 1,
        tot_pop = 1000,
        tmean = 20,
        rainfall = 1,
        runoff = 1,
        r_humidity = 50
      ))
    },
    run_descriptive_stats = function(..., correlation_method = "not supplied") {
      captured_method <<- correlation_method
      stop("captured correlation method", call. = FALSE)
    }
  )

  expect_error(
    malaria_do_analysis(
      health_data_path = data.frame(),
      climate_data_path = data.frame(),
      map_path = data.frame(),
      region_col = "region",
      district_col = "district",
      date_col = NULL,
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
      basis_matrices_choices = "rainfall",
      inla_param = "rainfall",
      param_term = "rainfall",
      level = "district",
      output_dir = output_dir,
      run_descriptive = TRUE,
      correlation_method = "spearman",
      save_fig = FALSE,
      save_csv = FALSE,
      save_model = FALSE
    ),
    "captured correlation method"
  )

  expect_equal(captured_method, "spearman")
})

test_that("suicides_heat_do_analysis forwards descriptive correlation_method", {
  captured_method <- NULL
  output_dir <- tempfile("suicides_desc_args_")
  dir.create(output_dir)

  local_mocked_bindings(
    mh_read_and_format_data = function(...) {
      list(A = data.frame(
        date = as.Date("2020-01-01"),
        region = "A",
        temp = 20,
        suicides = 1,
        population = 1000,
        humidity = 50
      ))
    },
    run_descriptive_stats = function(..., correlation_method = "not supplied") {
      captured_method <<- correlation_method
      stop("captured correlation method", call. = FALSE)
    }
  )

  expect_error(
    suicides_heat_do_analysis(
      data_path = "ignored.csv",
      date_col = "date",
      region_col = "region",
      temperature_col = "tmean",
      health_outcome_col = "suicides",
      population_col = "population",
      independent_cols = "humidity",
      output_folder_path = output_dir,
      run_descriptive = TRUE,
      correlation_method = "spearman"
    ),
    "captured correlation method"
  )

  expect_equal(captured_method, "spearman")
})

test_that("temp_mortality_do_analysis forwards descriptive correlation_method", {
  captured_method <- NULL
  output_dir <- tempfile("temp_mortality_desc_args_")
  dir.create(output_dir)

  local_mocked_bindings(
    hc_read_data = function(...) {
      list(A = data.frame(
        date = as.Date("2020-01-01"),
        region = "A",
        temp = 20,
        deaths = 1,
        population = 1000,
        humidity = 50
      ))
    },
    run_descriptive_stats = function(..., correlation_method = "not supplied") {
      captured_method <<- correlation_method
      stop("captured correlation method", call. = FALSE)
    }
  )

  expect_error(
    temp_mortality_do_analysis(
      data_path = "ignored.csv",
      date_col = "date",
      region_col = "region",
      temperature_col = "tmean",
      dependent_col = "deaths",
      population_col = "population",
      independent_cols = "humidity",
      output_folder_path = output_dir,
      run_descriptive = TRUE,
      correlation_method = "spearman"
    ),
    "captured correlation method"
  )

  expect_equal(captured_method, "spearman")
})

test_that("wildfire_do_analysis forwards descriptive correlation_method", {
  captured_method <- NULL
  minimal_data <- data.frame(
    date = as.Date("2020-01-01"),
    month = 1,
    year = 2020,
    dow = "Wed",
    region = "A",
    tmean = 10,
    mean_PM = 5,
    health_outcome = 2,
    rh = 70,
    wind_speed = 3,
    pop = 1000,
    stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    load_wildfire_data = function(...) minimal_data,
    create_lagged_variables = function(data, ...) data,
    time_stratify = function(data, ...) data,
    run_descriptive_stats = function(..., correlation_method = "not supplied") {
      captured_method <<- correlation_method
      stop("captured correlation method", call. = FALSE)
    }
  )

  expect_error(
    wildfire_do_analysis(
      health_path = "ignored.csv",
      join_wildfire_data = FALSE,
      ncdf_path = NULL,
      shp_path = NULL,
      date_col = "day",
      region_col = "area",
      shape_region_col = NULL,
      mean_temperature_col = "temp_mean",
      health_outcome_col = "deaths",
      population_col = "population",
      rh_col = "humidity",
      wind_speed_col = "wind",
      pm_2_5_col = "pm25",
      run_descriptive = TRUE,
      output_folder_path = tempdir(),
      correlation_method = "spearman",
      save_fig = FALSE,
      save_csv = FALSE
    ),
    "captured correlation method"
  )

  expect_equal(captured_method, "spearman")
})
