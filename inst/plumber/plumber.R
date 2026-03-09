#* @apiTitle Climatehealth API
#* @apiDescription An API that computes statistics using the climatehealth package

API_STARTUP_TIME <- Sys.time()
options(climatehealth.api_mode = TRUE)

#* @filter cors
function(res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "POST, GET, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type")
  plumber::forward()
}

#* @filter throttle
function(req, res) {
  clean_path <- sub("/$", "", req$PATH_INFO)
  clean_path <- sub("\\?.*$", "", clean_path)

  skip_paths <- c("", "/", "/health", "/version")
  if (clean_path %in% skip_paths || req$REQUEST_METHOD == "OPTIONS") {
    return(plumber::forward())
  }

  decision <- throttle_guard(clean_path, req)
  if (!decision$admitted) {
    return(reject_request(decision, res))
  }

  plumber::forward()
}

#* @get /
function() {
  "Hello, World!"
}

#* @get /version
#* @serializer json
function() {
  list(
    package = "climatehealth",
    package_version = as.character(utils::packageVersion("climatehealth")),
    r_version = R.version.string,
    platform = R.version$platform,
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
}

#* @get /health
#* @serializer json
function(res) {
  uptime_seconds <- as.numeric(difftime(Sys.time(), API_STARTUP_TIME, units = "secs"))
  uptime_formatted <- sprintf(
    "%dd %02dh %02dm %02ds",
    floor(uptime_seconds / 86400),
    floor((uptime_seconds %% 86400) / 3600),
    floor((uptime_seconds %% 3600) / 60),
    floor(uptime_seconds %% 60)
  )

  health_response <- list(
    status = "healthy",
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    uptime = uptime_formatted,
    uptime_seconds = round(uptime_seconds),
    r_version = R.version.string,
    checks = list()
  )

  health_response$checks$api <- list(status = "pass", message = "API responding")

  health_response$checks$package <- tryCatch({
    if (requireNamespace("climatehealth", quietly = TRUE)) {
      list(status = "pass", version = as.character(packageVersion("climatehealth")))
    } else {
      list(status = "fail", message = "climatehealth package not available")
    }
  }, error = function(e) {
    list(status = "fail", message = paste("Package check error:", e$message))
  })

  health_response$checks$computation <- tryCatch({
    val <- cor(c(1, 2, 3, 4, 5), c(10, 20, 30, 40, 50))
    if (abs(val - 1.0) < 0.001) {
      list(status = "pass")
    } else {
      list(status = "warn", message = "Unexpected correlation result")
    }
  }, error = function(e) {
    list(status = "fail", message = paste("Computation error:", e$message))
  })

  health_response$checks$memory <- tryCatch({
    mem <- get_system_memory()
    if (isTRUE(mem$success)) {
      list(
        status = "pass",
        info_available = TRUE,
        used_mb = mem$used_mb,
        total_mb = mem$total_mb,
        available_mb = mem$available_mb,
        source = mem$source
      )
    } else {
      list(
        status = "pass",
        info_available = FALSE,
        message = "Memory metrics unavailable"
      )
    }
  }, error = function(e) {
    list(
      status = "pass",
      info_available = FALSE,
      message = "Error reading memory metrics"
    )
  })

  statuses <- vapply(health_response$checks, function(x) x$status, character(1))
  if (any(statuses == "fail")) {
    health_response$status <- "unhealthy"
    res$status <- 503
  } else if (any(statuses == "warn")) {
    health_response$status <- "degraded"
    res$status <- 200
  } else {
    res$status <- 200
  }

  health_response
}

#* @options /temperature
function(req, res) {
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return(list())
  }
  plumber::forward()
}

#* @post /temperature
temperature_func <- climatehealth::temp_mortality_do_analysis

#* @post /wildfires
wildfire_func <- climatehealth::wildfire_do_analysis

#* @post /mental_health
mental_health_func <- climatehealth::suicides_heat_do_analysis

#* @post /descriptive_stats
descriptive_stats_func <- climatehealth::run_descriptive_stats_api

#* @post /diarrhea
diarrhea_func <- climatehealth::diarrhea_do_analysis

#* @post /malaria
malaria_func <- climatehealth::malaria_do_analysis
