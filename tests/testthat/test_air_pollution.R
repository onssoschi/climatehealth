# Tests for air_pollution.R
source("R/air_pollution.R")
devtools::load_all()
# Test for load_air_pollution_data
# Unit test 1
test_that("load_air_pollution_data works with standard column names", {
  #create a minimal synthetic dataset that already uses the standarad column
  #names expected by the function

  LAPD_TEST_DATA <- data.frame(
    date = as.Date("2020-01-01") + 0:5,
    region = rep(c("RegionA", "RegionB"),each = 3),
    pm25 = c(10, 12, 23, 35, 25, 15),
    deaths = c(5, 6, 0, 2, 9, 3),
    humidity = c(80, 75, 25, 67, 34, 45),
    precipitation = c(0.2, 0.0, 0.1, 2.0, 3,2),
    tmax = c(10, 12, 24, 32, 25, 25),
    population = 100000,
    age = "all",
    sex = "both",
    urbanisation = "mixed"
  )

  result <- load_air_pollution_data(LAPD_TEST_DATA)

  # Check type and row count
  expect_s3_class(result,"data.frame")
  expect_equal(nrow(result),nrow(LAPD_TEST_DATA))

  # Check column names
  expected_cols <- c("date", "region", "pm25", "deaths", "humidity",
                     "precipitation", "tmax", "population", "age", "sex",
                     "urbanisation", "year", "month", "day", "dow", "time")
  expect_equal(colnames(result), expected_cols)

  # Check the date is a Date and derived year is correct
  expect_s3_class(result$date, "Date")
  expect_true(all(result$year == 2020))

  # Check time index is 1:nrow
  expect_equal(result$time, seq_len(nrow(result)))
})

# Unit test 2
test_that("load_air_pollution_data returns expected columns and types", {
  # Create a minimal synthetic dataset
  LAPD_TEST_DATA <- data.frame(
    date = c("2024-01-01", "2024-01-02"),
    region = c("RegionA", "RegionB"),
    pm25 = c(12.5, 15.0),
    deaths = c(5, 6),
    humidity = c(80, 75),
    precipitation = c(0.2, 0.0),
    tmax = c(10, 12)
  )

  result <- load_air_pollution_data(LAPD_TEST_DATA)

  # Check column names
  expected_cols <- c("date", "region", "pm25", "deaths", "humidity",
                     "precipitation", "tmax", "population", "age", "sex",
                     "urbanisation", "year", "month", "day", "dow", "time")
  expect_true(all(expected_cols %in% names(result)))

  # Check types
  expect_type(result$date, "double")
  expect_true(is.character(result$region))
  expect_true(is.numeric(result$pm25))
})

test_that("load_air_pollution_data - Renaming works when custom column names provided", {
  LAPD_TEST_DATA <- data.frame(
    my_date = c("2024-01-01", "2024-01-02"),
    my_region = c("RegionA", "RegionB"),
    my_pm = c(12.5, 15.0),
    my_deaths = c(5, 6),
    humidity = c(80, 75),
    precipitation = c(0.2, 0.0),
    tmax = c(10, 12)
  )

  result <- load_air_pollution_data(LAPD_TEST_DATA,
                                    date_col = "my_date",
                                    region_col = "my_region",
                                    pm25_col = "my_pm",
                                    deaths_col = "my_deaths")

  expect_true(all(c("date", "region", "pm25", "deaths") %in% names(result)))
})


test_that("load_air_pollution_data - Missing optional columns are filled with defaults",
          {
            LAPD_TEST_DATA <- data.frame(
              date = c("2024-01-01"),
              province = c("RegionA"),
              pm25 = c(12.5),
              deaths = c(5),
              humidity = c(80),
              precipitation = c(0.2),
              tmax = c(10)
            )

            result <- load_air_pollution_data(LAPD_TEST_DATA)

            expect_equal(result$population, 1)
            expect_equal(result$age, "all")
            expect_equal(result$sex, "both")
            expect_equal(result$urbanisation, "mixed")
          })

test_that("load_air_pollution_data - Date parsing works for DD/MM/YYYY format", {
  LAPD_TEST_DATA <- data.frame(
    date = c("01/01/2024", "02/01/2024"),
    province = c("RegionA", "RegionB"),
    pm25 = c(12.5, 15.0),
    deaths = c(5, 6),
    humidity = c(80, 75),
    precipitation = c(0.2, 0.0),
    tmax = c(10, 12)
  )

  result <- load_air_pollution_data(LAPD_TEST_DATA)
  expect_equal(result$year[1], 2024)
  expect_equal(result$month[1], 1)
})


# Test for create_air_pollution_lags
# Unit test 1
test_that("create_air_pollution_lags - Lag columns and average lag column are created correctly", {
  # Synthetic dataset
  CAPL_TEST_DATA <- data.frame(
    date = as.Date("2024-01-01") + 0:4,
    region = rep("RegionA", 5),
    pm25 = c(10, 20, 30, 40, 50)
  )

  result <- create_air_pollution_lags(CAPL_TEST_DATA, max_lag = 2)

  # Check new columns exist
  expect_true(all(c("pm25_lag1", "pm25_lag2", "pm25_lag0_2") %in% names(result)))

  # Check number of rows (should drop first 2 rows)
  expect_equal(nrow(result), 3)

  # For Region A the remaining dates should be from 3rd orginal row onwards
  expect_equal(result$date, CAPL_TEST_DATA$date[3:5])

  # Check lag values
  expect_equal(result$pm25_lag1, c(20, 30, 40))
  expect_equal(result$pm25_lag2, c(10, 20, 30))

  # Check average lag column
  expected_avg <- rowMeans(cbind(result$pm25, result$pm25_lag1, result$pm25_lag2))
  expect_equal(result$pm25_lag0_2, expected_avg)
})

# Unit test 2
test_that("create_air_pollution_lags works with multiple regions", {
  CAPL_TEST_DATA <- data.frame(
    date = rep(as.Date("2024-01-01") + 0:3, 2),
    region = rep(c("R1", "R2"), each = 4),
    pm25 = c(1, 2, 3, 4,  10, 20, 30, 40)
  )

  result <- create_air_pollution_lags(CAPL_TEST_DATA, max_lag = 1)

  # One row dropped per region due to lag, so total rows should be 6
  expect_equal(nrow(result), 6)

  r1 <- result[result$region == "R1", ]
  r2 <- result[result$region == "R2", ]

  expect_equal(r1$pm25, c(2, 3, 4))
  expect_equal(r1$pm25_lag1, c(1, 2, 3))


  expect_equal(r2$pm25, c(20, 30, 40))
  expect_equal(r2$pm25_lag1, c(10, 20, 30))

})

# Unit test 3
test_that("create_air_pollution_lags handles max_lag = 1 correctly", {
  CAPL_TEST_DATA <- data.frame(
    date = as.Date("2014-01-01") + 0:2,
    region = rep("RegionA", 3),
    pm25 = c(10, 20, 30)
  )

  result <- create_air_pollution_lags(CAPL_TEST_DATA, max_lag = 1)

  expect_true(all(c("pm25_lag1", "pm25_lag0_1") %in% names(result)))
  expect_equal(nrow(result), 2) # drops first row
})

# Unit test 4
test_that("create_air_pollution_lags returns empty when max_lag > available rows", {
  CAPL_TEST_DATA <- data.frame(
    date = as.Date("2024-01-01") + 0:2,
    region = rep("RegionA", 3),
    pm25 = c(10, 20, 30)
  )

  result <- create_air_pollution_lags(CAPL_TEST_DATA, max_lag = 5)
  expect_equal(nrow(result), 0)
})

# Unit test 5

test_that("create_air_pollution_lags - Non-exposure columns remain unchanged", {
  CAPL_TEST_DATA <- data.frame(
    date = as.Date("2024-01-01") + 0:3,
    region = rep("RegionA", 4),
    pm25 = c(10, 20, 30, 40),
    humidity = c(80, 81, 82, 83),  # extra covariate
    deaths = c(5, 6, 7, 8)         # extra covariate
  )

  result <- create_air_pollution_lags(CAPL_TEST_DATA, max_lag = 1)

  # Compare unchanged columns (after filtering rows)
  expect_equal(result$deaths, CAPL_TEST_DATA$deaths[2:4])
  expect_equal(result$humidity, CAPL_TEST_DATA$humidity[2:4])
})

# Test for air_pollution_descriptive_stats
# Unit test 1
test_that("air_pollution_descriptive_stats returns correct structure and values",
          {
            #minimal synthetic dataset with deaths and couple of numeric variables
            APDS_TEST_DATA <- data.frame(
              date = as.Date("2024-01-01") + 0:4,
              region = factor(c("A", "B", "A", "B", "A")), # factor
              pm25 = c(10, 20, 30, 40, 60),
              humidity = c(80, 81, 82, 83, 85),
              deaths = c(5, 6, 7, 8, 4)
            )

            result = air_pollution_descriptive_stats(data = APDS_TEST_DATA,
                                                     variables = c("pm25","humidity"))


            # 1. Return type should be a list with names matching variables
            expect_type(result,"list")
            expect_equal(sort(names(result)), sort(c("pm25","humidity")))

            # 2. Each element should be the base R summary for that column
            expect_equal(result$pm25, summary(APDS_TEST_DATA$pm25))
            expect_equal(result$humidity, summary(APDS_TEST_DATA$humidity))

            # 3. Summaries for numeric variable should have length 6
            expect_equal(length(result$pm25), 6)
            expect_equal(length(result$humidity), 6)

            result1 <- air_pollution_descriptive_stats(APDS_TEST_DATA, variables = c("region", "pm25"))

            # Factor checks(Handles multiple variable types correctly)
            expect_equal(result1$region, summary(APDS_TEST_DATA$region))
          })

# Unit test 2
test_that(
  "air_pollution_descriptive_stats throws error if save_outputs = TRUE but output_dir is NULL", {
    APDS_TEST_DATA <- data.frame(deaths = c(1, 2, 3))
    expect_error(
      air_pollution_descriptive_stats(APDS_TEST_DATA, variables = "deaths",
                                      save_outputs = TRUE),
      "An output directory must be passed"
    )
  })

# Unit test 3
test_that("air_pollution_descriptive_stats creates output directory and files when save_outputs = TRUE", {
  tmp_dir <- tempdir()
  APDS_TEST_DATA <- data.frame(deaths = c(1, 2, 3), pm25 = c(10, 20, 30))

  air_pollution_descriptive_stats(APDS_TEST_DATA, variables = c("deaths", "pm25"),
                                  output_dir = tmp_dir, save_outputs = TRUE)

  expect_true(file.exists(file.path(tmp_dir, "air_pollution_descriptive_stats",
                                    "mortality_histogram.png")))
  expect_true(file.exists(file.path(tmp_dir, "air_pollution_descriptive_stats",
                                    "descriptive_statistics.txt")))
})

# Test for plot_air_pollution_variables
# Unit test 1
test_that("plot_air_pollution_variables creates a ggplot object and prints it", {
  PAPV_TEST_DATA <- data.frame(pm25 = c(10, 20, 30),
                               deaths = c(1, 2, 3)
  )
  p = plot_air_pollution_variables(PAPV_TEST_DATA, xvar = "pm25", yvar = "deaths", save_plot = FALSE)


  # Capture printed output
  expect_s3_class(p, ggplot)
})

# Unit test 2
test_that("plot_air_pollution_variables throws error if save_plot = TRUE but output_dir is NULL", {
  PAPV_TEST_DATA <- data.frame(pm25 = c(10, 20, 30),
                               deaths = c(1, 2, 3)
  )
  expect_error(
    plot_air_pollution_variables(PAPV_TEST_DATA, xvar = "pm25", yvar = "deaths", save_plot = TRUE),
    "An output directory must be passed"
  )
})

# Unit test 3
test_that("plot_air_pollution_variables saves plot when save_plot = T and output_dir provided", {
  tmp_dir <- tempdir()
  PAPV_TEST_DATA <- data.frame(pm25 = c(10, 20, 30),
                               deaths = c(1, 2, 3)
  )


  # PAPV_TEST_DATA <- data.frame(
  #   deaths = rpois(10000, lambda = 5),
  #   pm25   = round(runif(10000, min = 5, max = 80), 1)
  # )

  plot_air_pollution_variables(PAPV_TEST_DATA, xvar = "pm25", yvar = "deaths",
                               output_dir = tmp_dir, save_plot = TRUE)

  expected_file <- file.path(tmp_dir, "pm25_vs_deaths.png")
  expect_true(file.exists(expected_file))

  # File name matches the expected pattern
  files <- list.files(tmp_dir, pattern = "pm25_vs_deaths.png", full.names = TRUE)
  expect_equal(length(files), 1)

})

# Unit test 4 (not working as the return object is NULL)
test_that("plot_air_pollution_variables handles different x/y combinations", {
  PAPV_TEST_DATA = data.frame(pm25 = c(10, 20, 30),
                              deaths = c(1, 2, 3),
                              tmax = c(5, 6, 7)
  )

  p1 = plot_air_pollution_variables(PAPV_TEST_DATA, xvar = "pm25", yvar = "deaths",
                                    save_plot = F)

  p2 = plot_air_pollution_variables(PAPV_TEST_DATA, xvar = "tmax", yvar = "deaths",
                                    save_plot = F)

  expect_equal(rlang::as_label(p1$mapping$x), "pm25")
  expect_equal(rlang::as_label(p2$mapping$x), "tmax")
  expect_equal(rlang::as_label(p1$mapping$y), "deaths")
  expect_equal(rlang::as_label(p2$mapping$y), "deaths")
})

# Test for fit_air_pollution_gam and extract_air_pollution_coef
# Synthetic big dataset for testing
FAPG_TEST_DATA <- data.frame(
  deaths = rpois(600, lambda = 5),
  pm25 = runif(600, 5, 80),
  time = 1:600,
  tmax = runif(600, -5, 40),
  humidity = runif(600, 40, 90),
  precipitation = runif(600, 0, 20),
  dow = sample(letters[1:7], 600, replace = TRUE),
  population = sample(100000:1000000, 600, replace = TRUE)
)

# Small dataset
FAPG_SMALL_TEST_DATA = FAPG_TEST_DATA[1:100, ]

# Unit test 1
test_that("fit_air_pollution_gam returns NULL when datasset < 500 rows and
          extract_air_pollution_coef returns NA when model is null", {
            model <- fit_air_pollution_gam(FAPG_SMALL_TEST_DATA, var_name = "pm25")

            expect_warning(
              model <- fit_air_pollution_gam(FAPG_SMALL_TEST_DATA, var_name = "pm25"),
              "Insufficient data"
            )

            expect_null(model)

            result <- extract_air_pollution_coef(model)

            expect_equal(result$coef, NA)
            expect_equal(result$se, NA)
          })

#Unit test 2
test_that("fit_air_pollution_gam returns GAM object when data is sufficient and
          extract_air_pollution_coef returns correct coef and SE for valid model", {
            model =  fit_air_pollution_gam(FAPG_TEST_DATA, var_name = "pm25")

            expect_s3_class(model, "gam")

            result <- extract_air_pollution_coef(model)

            expect_true(is.numeric(result$coef))
            expect_true(is.numeric(result$se))
            expect_false(is.na(result$coef))
            expect_false(is.na(result$se))

          })

# Unit test 3
test_that("fit_air_pollution_gam errors when exposure variable is missing and
          returns NULL when essential predictor is missing", {

            #removed pm25 column (exposure variable)
            expect_error(fit_air_pollution_gam(FAPG_TEST_DATA[ , -2], var_name = "pm25"),
                         " not in dataset.")

            #removed tmax column (predictor variable)
            expect_null(fit_air_pollution_gam(FAPG_TEST_DATA[ , -4]))
          })

#Unit test 4
test_that("fit_air_pollution_gam handles different family values correctly and
          extract_air_pollution_coef returns correct coef and SE for valid model", {
            families <- c("quasipoisson", "poisson", "nb", "ziP")
            for (fam in families) {
              model <- fit_air_pollution_gam(FAPG_TEST_DATA, var_name = "pm25", family = fam)
              expect_s3_class(model, "gam")

              result <- extract_air_pollution_coef(model)

              expect_type(result, "list")
              expect_true(all(c("coef", "se") %in% names(result)))

              expect_true(is.numeric(result$coef))
              expect_true(is.numeric(result$se))
              expect_false(is.na(result$coef))
              expect_false(is.na(result$se))

              expect_type(result$coef, "double")
              expect_type(result$se, "double")

            }
          })

# Unit test 5
test_that("extract_air_pollution_coef handles non model input", {
  result = extract_air_pollution_coef("not a model", "pm25")
  expect_true(is.na(result$coef))
  expect_true(is.na(result$se))
})

# Test for air_pollution_meta_analysis
# Helper: Generate synthetic data
make_test_data <- function(n_regions = 3, n_per_region = 600) {
  regions <- paste0("Region", seq_len(n_regions))
  data.frame(
    region = rep(regions, each = n_per_region),
    deaths = rpois(n_regions * n_per_region, lambda = 5),
    pm25 = runif(n_regions * n_per_region, 5, 80),
    time = rep(seq_len(n_per_region), n_regions),
    tmax = runif(n_regions * n_per_region, -5, 40),
    humidity = runif(n_regions * n_per_region, 40, 90),
    precipitation = runif(n_regions * n_per_region, 0, 20),
    dow = factor(sample(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"),
                        n_regions * n_per_region, replace = TRUE)),
    population = sample(100000:1000000, n_regions * n_per_region, replace = TRUE)
  )
}

# Large APMA dataset
APMA_TEST_DATA_LARGE <- make_test_data(n_regions = 3, n_per_region = 600)

#Unit test 1
test_that("air_pollution_meta_analysis returns valid meta-analysis results for
          multiple regions and different distribution family ", {
            families <- c("quasipoisson", "poisson", "nb", "ziP")
            for (fam in families) {
              result <- air_pollution_meta_analysis(APMA_TEST_DATA_LARGE, var_name = "pm25", family = fam)

              #Top level structure
              expect_type(result, "list")
              expect_true(all(c("region_results", "meta_result", "overall_rr",
                                "overall_ci", "overall_af", "overall_an", "heterogeneity") %in% names(result)))

              # Check numeric outputs
              expect_true(is.numeric(result$overall_rr))
              expect_true(length(result$overall_ci) == 2)
              expect_true(is.numeric(result$overall_af))
              expect_true(is.numeric(result$overall_an))

              # Check range for RR and AF
              expect_true(result$overall_rr >= 0) #since it is defined as an exponential fn
              expect_true(result$overall_af <= 1) #AF'll be always < 1 as per the formula

              # Check heterogeneity metrics
              expect_true(is.list(result$heterogeneity))
              expect_true(all(c("tau2", "I2") %in% names(result$heterogeneity)))
              expect_true(is.numeric(result$heterogeneity$tau2))
              expect_true(is.numeric(result$heterogeneity$I2))

              # Region results structure
              region_results = result$region_results
              expect_s3_class(region_results, "data.frame")
              expect_equal(length(unique(region_results$region)), 3)

              expect_true(all(c("region", "data", "n_obs", "total_deaths", "model", "coef_pm25",
                                "se_pm25", "rr_10ug", "ci_lower", "ci_upper", "af_10ug", "an_10ug")
                              %in% names(region_results)))

              # No missing coeffecients or se used in meta analysis
              expect_false(any(is.na(region_results$coef_pm25)))
              expect_false(any(is.na(region_results$se_pm25)))

              # Meta results structure
              meta_results = result$meta_result
              expect_s3_class(meta_results, "rma.uni")

            }
          })

# Unit test 2
test_that("air_pollution_meta_analysis returns NULL and warns when < 2 regions
          have valid models", {
            # Only one region
            APMA_TEST_DATA <- APMA_TEST_DATA_LARGE[APMA_TEST_DATA_LARGE$region == "RegionA", ]

            expect_warning(
              result <- air_pollution_meta_analysis(APMA_TEST_DATA, var_name = "pm25"),
              "At least 2 regions with successful model fits needed for meta-analysis."
            )
            expect_null(result)
          })

# Unit test 3
test_that("air_pollution_meta_analysis handles insufficient rows in some regions", {
  # RegionA has only 100 rows, others have 600
  regionA <- make_test_data(n_regions = 1, n_per_region = 100)
  regionA$region <- "RegionA"
  regionBC <- make_test_data(n_regions = 2, n_per_region = 600)
  regionBC$region <- rep(c("RegionB", "RegionC"), each = 600)


  APMA_TEST_DATA <- rbind(regionA, regionBC)

  result <- air_pollution_meta_analysis(APMA_TEST_DATA, var_name = "pm25")
  # RegionA should likely fail GAM fit and be excluded
  expect_true(nrow(result$region_results) >= 2)
})

# Test for analyze_air_pollution_lags
CAPL_TEST_DATA <- data.frame(
  date = rep(as.Date("2024-01-01") + 0:3, 2),
  region = rep(c("R1", "R2"), each = 4),
  pm25 = c(1, 2, 3, 4,  10, 20, 30, 40)
)
# Build a dataset using the package function
AAPL_DATA <- create_air_pollution_lags(
  data = CAPL_TEST_DATA,
  max_lag = 2
)

# Unit test 1
test_that("analyze_air_pollution_lags returns expected structure", {

  result <- analyze_air_pollution_lags(
    data = AAPL_DATA,
    max_lag = 2
  )

  # type check
  expect_s3_class(result, "data.frame")
})



