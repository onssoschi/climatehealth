# Entrypoint for climatehealth plumber API
options(climatehealth.api_mode = TRUE)

resolve_api_root <- function() {
  env_root <- Sys.getenv("CLIMATEHEALTH_PLUMBER_DIR", unset = "")
  if (nzchar(env_root)) {
    return(normalizePath(env_root, winslash = "/", mustWork = FALSE))
  }

  pkg_root <- system.file("plumber", package = "climatehealth")
  if (nzchar(pkg_root)) {
    return(normalizePath(pkg_root, winslash = "/", mustWork = FALSE))
  }

  stop("Unable to locate plumber assets. Install or load the climatehealth package first.")
}

resolve_config_path <- function(api_root) {
  env_cfg <- Sys.getenv("CLIMATEHEALTH_API_CONFIG", unset = "")
  if (nzchar(env_cfg)) {
    return(normalizePath(env_cfg, winslash = "/", mustWork = FALSE))
  }

  cfg <- system.file("extdata", "config_templates", "api_config.yml", package = "climatehealth")
  if (!nzchar(cfg)) {
    cfg <- file.path(api_root, "api_config.yml")
  }
  normalizePath(cfg, winslash = "/", mustWork = FALSE)
}

api_root <- resolve_api_root()
config_path <- resolve_config_path(api_root)

if (!file.exists(config_path)) {
  stop(sprintf("API config file not found: %s", config_path))
}

library(logger)
library(plumber)
library(config)

log_file <- file.path(tempdir(), "climatehealth_api.log")
log_appender(appender_file(log_file))
log_threshold(INFO)

source(file.path(api_root, "throttling", "throttle_memory.R"))
source(file.path(api_root, "throttling", "throttle_config.R"))
source(file.path(api_root, "throttling", "throttle_estimator.R"))
source(file.path(api_root, "throttling", "throttle_guard.R"))
source(file.path(api_root, "throttling", "throttle_executor.R"))

cfg <- config::get(file = config_path)

throttle_cfg <- load_throttle_config(cfg$throttling)
guard_fn <- create_throttle_guard(
  throttle_config = throttle_cfg,
  memory_fn = get_system_memory,
  logger_fn = function(msg) logger::log_info(msg)
)

if (isTRUE(throttle_cfg$enabled)) {
  logger::log_info(
    sprintf(
      "Throttling enabled: buffer=%dMB, endpoints=%d",
      throttle_cfg$min_system_buffer_mb,
      length(throttle_cfg$endpoints)
    )
  )
} else {
  logger::log_info("Throttling disabled by configuration.")
}

api_env <- new.env(parent = globalenv())
api_env$throttle_guard <- guard_fn
api_env$reject_request <- reject_request
api_env$run_guarded <- run_guarded
api_env$get_system_memory <- get_system_memory
api_env$config <- cfg

router <- plumber::pr(file.path(api_root, "plumber.R"), envir = api_env)

debug_enabled <- isTRUE(as.logical(Sys.getenv("PLUMBER_DEBUG", "FALSE")))
router <- plumber::pr_set_debug(router, debug_enabled)

docs_enabled <- !identical(toupper(Sys.getenv("PLUMBER_DOCS", "TRUE")), "FALSE")
if (docs_enabled) {
  router <- plumber::pr_set_docs(router, "swagger")
}

router <- plumber::pr_set_error(router, function(req, res, err) {
  msg <- sprintf("Error in %s: %s", req$PATH_INFO, conditionMessage(err))
  logger::log_error(msg)

  is_typed_error <- inherits(err, "climate_error")
  is_validation <- inherits(err, "validation_error") || inherits(err, "column_not_found")
  is_model_error <- inherits(err, "model_error")

  if (is_validation) {
    res$status <- 400
  } else if (is_model_error) {
    res$status <- 422
  } else {
    res$status <- 500
  }

  if (is_typed_error) {
    error_type <- if (!is.null(err$type)) err$type else class(err)[1]
    standard_fields <- c("message", "call", "trace", "parent", "rlang", "type")
    custom_fields <- setdiff(names(err), standard_fields)
    details <- list()
    for (field in custom_fields) {
      details[[field]] <- err[[field]]
    }
    return(list(
      error_type = error_type,
      message = conditionMessage(err),
      details = if (length(details) > 0) details else NULL,
      path = req$PATH_INFO
    ))
  }

  list(
    error_type = "internal_error",
    message = conditionMessage(err),
    path = req$PATH_INFO
  )
})

host <- cfg$api$host %||% "0.0.0.0"
port <- cfg$api$port %||% 8080
msg <- sprintf("Running climatehealth plumber API at http://%s:%s", host, port)
logger::log_info(msg)
message(msg)

router %>%
  plumber::pr_run(port = port, host = host)
