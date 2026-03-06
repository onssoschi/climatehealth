# throttle_config.R - Configuration Loading with Environment Overrides

`%||%` <- function(x, y) if (is.null(x)) y else x

get_env_typed <- function(name, default = NULL, type = "character") {
  val <- Sys.getenv(name, unset = NA_character_)

  if (is.na(val) || val == "") {
    return(default)
  }

  result <- tryCatch({
    switch(type,
      logical = tolower(val) %in% c("true", "1", "yes", "on"),
      numeric = as.numeric(val),
      integer = as.integer(val),
      character = val,
      default
    )
  }, error = function(e) default)

  result
}

load_throttle_config <- function(yaml_config = list()) {
  config <- list(
    enabled = TRUE,
    min_system_buffer_mb = 512,
    default_base_mb = 500,
    default_multiplier = 20,
    endpoints = list()
  )

  if (!is.null(yaml_config$enabled)) {
    config$enabled <- yaml_config$enabled
  }
  if (!is.null(yaml_config$min_system_buffer_mb)) {
    config$min_system_buffer_mb <- yaml_config$min_system_buffer_mb
  }
  if (!is.null(yaml_config$default_base_mb)) {
    config$default_base_mb <- yaml_config$default_base_mb
  }
  if (!is.null(yaml_config$default_multiplier)) {
    config$default_multiplier <- yaml_config$default_multiplier
  }
  if (!is.null(yaml_config$endpoints)) {
    config$endpoints <- yaml_config$endpoints
  }

  config$enabled <- get_env_typed(
    "THROTTLE_ENABLED",
    default = config$enabled,
    type = "logical"
  )

  config$min_system_buffer_mb <- get_env_typed(
    "THROTTLE_MIN_BUFFER_MB",
    default = config$min_system_buffer_mb,
    type = "numeric"
  )

  if (!is.null(config$endpoints) && length(config$endpoints) > 0) {
    for (ep_name in names(config$endpoints)) {
      ep_upper <- toupper(gsub("[^a-zA-Z0-9]", "_", ep_name))

      env_base <- get_env_typed(
        paste0("THROTTLE_", ep_upper, "_BASE_MB"),
        default = NULL,
        type = "numeric"
      )
      if (!is.null(env_base)) {
        config$endpoints[[ep_name]]$base_mb <- env_base
      }

      env_mult <- get_env_typed(
        paste0("THROTTLE_", ep_upper, "_MULTIPLIER"),
        default = NULL,
        type = "numeric"
      )
      if (!is.null(env_mult)) {
        config$endpoints[[ep_name]]$multiplier <- env_mult
      }
    }
  }

  config
}

get_endpoint_config <- function(throttle_config, endpoint_name) {
  ep_name <- gsub("^/", "", endpoint_name)

  if (!is.null(throttle_config$endpoints[[ep_name]])) {
    ep_config <- throttle_config$endpoints[[ep_name]]
  } else {
    ep_config <- list(
      base_mb = throttle_config$default_base_mb %||% 500,
      multiplier = throttle_config$default_multiplier %||% 20,
      arg_name = NULL,
      arg_type = "filepath",
      category = "unknown"
    )
  }

  ep_config$base_mb <- ep_config$base_mb %||% 500
  ep_config$multiplier <- ep_config$multiplier %||% 20
  ep_config$arg_type <- ep_config$arg_type %||% "filepath"

  ep_config
}

print_throttle_config <- function(config) {
  message("=== Throttle Configuration ===")
  message(sprintf("  Enabled: %s", config$enabled))
  message(sprintf("  Min Buffer: %d MB", config$min_system_buffer_mb))
  message(sprintf("  Default Base: %d MB", config$default_base_mb %||% 500))
  message(sprintf("  Default Multiplier: %d", config$default_multiplier %||% 20))

  if (length(config$endpoints) > 0) {
    message("  Endpoints:")
    for (name in names(config$endpoints)) {
      ep <- config$endpoints[[name]]
      message(sprintf(
        "    /%s: base=%dMB, mult=%.1f, type=%s",
        name,
        ep$base_mb %||% 500,
        ep$multiplier %||% 1,
        ep$arg_type %||% "filepath"
      ))
    }
  }

  invisible(config)
}

