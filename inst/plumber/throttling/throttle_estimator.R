# throttle_estimator.R - Type-Aware Cost Estimation

create_cost_estimator <- function(config) {
  `%||%` <- function(x, y) if (is.null(x)) y else x

  extract_data_size_mb <- function(endpoint_config, req) {
    arg_name <- endpoint_config$arg_name
    arg_type <- endpoint_config$arg_type %||% "filepath"

    val <- req$args[[arg_name]] %||% req$body[[arg_name]]

    if (is.null(val) && arg_type == "json_field") {
      val <- req$body
    }

    if (is.null(val) && arg_type != "json_field") {
      return(0)
    }

    size_mb <- tryCatch({
      switch(arg_type,
        filepath = {
          if (is.character(val) && length(val) == 1 && file.exists(val)) {
            s <- file.size(val)
            if (is.na(s)) 0 else s / 1024^2
          } else {
            0
          }
        },
        upload = {
          if (is.list(val)) {
            if (!is.null(val$size)) {
              val$size / 1024^2
            } else if (!is.null(val$datapath) && file.exists(val$datapath)) {
              s <- file.size(val$datapath)
              if (is.na(s)) 0 else s / 1024^2
            } else if (!is.null(val[[1]]) && is.raw(val[[1]])) {
              length(val[[1]]) / 1024^2
            } else {
              0
            }
          } else {
            0
          }
        },
        json_field = {
          payload_field <- endpoint_config$payload_field
          if (is.null(payload_field)) {
            return(0)
          }

          container <- val$payload %||% val

          if (is.character(container) && length(container) == 1) {
            container <- tryCatch(
              jsonlite::fromJSON(container, simplifyVector = FALSE),
              error = function(e) NULL
            )
          }

          if (is.list(container)) {
            target_path <- container[[payload_field]]
            if (!is.null(target_path) && is.character(target_path) && file.exists(target_path)) {
              s <- file.size(target_path)
              if (is.na(s)) 0 else s / 1024^2
            } else {
              0
            }
          } else {
            0
          }
        },
        json_records = {
          if (is.character(val) && length(val) == 1) {
            val <- tryCatch(
              jsonlite::fromJSON(val, simplifyVector = FALSE),
              error = function(e) val
            )
          }
          as.numeric(utils::object.size(val)) / 1024^2
        },
        0
      )
    }, error = function(e) {
      0
    })

    size_mb
  }

  function(endpoint_type, req) {
    clean_key <- sub("^/", "", endpoint_type)
    ep_config <- config$endpoints[[clean_key]]

    if (is.null(ep_config)) {
      return(list(
        estimated_mb = config$default_base_mb %||% 500,
        calculation_method = "default_fallback",
        data_size_mb = 0,
        config_used = list(
          base = config$default_base_mb %||% 500,
          mult = config$default_multiplier %||% 0
        )
      ))
    }

    data_size_mb <- extract_data_size_mb(ep_config, req)
    base_mb <- ep_config$base_mb %||% 500
    multiplier <- ep_config$multiplier %||% 1
    estimated_total <- base_mb + (data_size_mb * multiplier)

    list(
      estimated_mb = round(estimated_total, 2),
      calculation_method = if (data_size_mb > 0) "linear_model" else "base_only",
      data_size_mb = round(data_size_mb, 2),
      config_used = list(base = base_mb, mult = multiplier)
    )
  }
}
