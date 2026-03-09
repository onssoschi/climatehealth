# Export an OpenAPI file to disk

resolve_api_root <- function() {
  env_root <- Sys.getenv("CLIMATEHEALTH_PLUMBER_DIR", unset = "")
  if (nzchar(env_root)) {
    return(normalizePath(env_root, winslash = "/", mustWork = FALSE))
  }

  pkg_root <- system.file("plumber", package = "climatehealth")
  if (!nzchar(pkg_root)) {
    stop("Unable to locate plumber assets for climatehealth.")
  }
  normalizePath(pkg_root, winslash = "/", mustWork = FALSE)
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

library(logger)
library(plumber)
library(jsonlite)
library(config)

source(file.path(api_root, "throttling", "throttle_memory.R"))
source(file.path(api_root, "throttling", "throttle_config.R"))
source(file.path(api_root, "throttling", "throttle_estimator.R"))
source(file.path(api_root, "throttling", "throttle_guard.R"))
source(file.path(api_root, "throttling", "throttle_executor.R"))

cfg <- config::get(file = config_path)
throttle_cfg <- load_throttle_config(cfg$throttling)

api_env <- new.env(parent = globalenv())
api_env$throttle_guard <- create_throttle_guard(
  throttle_config = throttle_cfg,
  memory_fn = get_system_memory
)
api_env$reject_request <- reject_request
api_env$run_guarded <- run_guarded
api_env$get_system_memory <- get_system_memory
api_env$config <- cfg

router <- plumber::pr(file.path(api_root, "plumber.R"), envir = api_env)
router <- plumber::pr_set_docs(router, "swagger")
spec <- suppressWarnings(router$getApiSpec())

strip_null_defaults <- function(x) {
  if (is.list(x)) {
    if ("default" %in% names(x) && is.null(x$default)) {
      x$default <- NULL
    }
    for (nm in names(x)) {
      x[[nm]] <- strip_null_defaults(x[[nm]])
    }
  }
  x
}
spec <- strip_null_defaults(spec)

out_file <- Sys.getenv("OPENAPI_OUT_FILE", unset = file.path(tempdir(), "climatehealth_openapi.json"))
out_file <- normalizePath(out_file, winslash = "/", mustWork = FALSE)
dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)

writeLines(jsonlite::toJSON(spec, pretty = TRUE, auto_unbox = TRUE), out_file)
log_info("Wrote OpenAPI spec to: {out_file}")
cat(out_file, "\n")

