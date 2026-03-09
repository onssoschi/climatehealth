# throttle_guard.R - Admission Control Logic

`%||%` <- function(x, y) if (is.null(x)) y else x

create_decision <- function(admitted, reason, details = list()) {
  structure(
    list(
      admitted = admitted,
      reason = reason,
      timestamp = Sys.time(),
      details = details
    ),
    class = "throttle_decision"
  )
}

check_bypass <- function(req, throttle_enabled) {
  if (!throttle_enabled) {
    return(list(bypass = TRUE, reason = "throttling_disabled"))
  }

  bypass_header <- req$HTTP_X_BYPASS_THROTTLE %||%
    req$headers[["x-bypass-throttle"]] %||%
    req$headers[["X-Bypass-Throttle"]]

  if (!is.null(bypass_header) && tolower(bypass_header) %in% c("true", "1", "yes")) {
    return(list(bypass = TRUE, reason = "bypass_header"))
  }

  list(bypass = FALSE, reason = NULL)
}

check_admission <- function(estimated_cost, available_memory, min_buffer_mb) {
  if (!available_memory$success) {
    return(create_decision(
      admitted = TRUE,
      reason = "memory_detection_unavailable",
      details = list(
        memory_source = available_memory$source,
        estimated_mb = estimated_cost$estimated_mb
      )
    ))
  }

  available_mb <- available_memory$available_mb
  total_mb <- available_memory$total_mb %||% 0
  required_mb <- estimated_cost$estimated_mb + min_buffer_mb

  if (available_mb >= required_mb) {
    return(create_decision(
      admitted = TRUE,
      reason = "sufficient_memory",
      details = list(
        available_mb = available_mb,
        total_mb = total_mb,
        required_mb = required_mb,
        estimated_mb = estimated_cost$estimated_mb,
        buffer_mb = min_buffer_mb,
        headroom_mb = available_mb - required_mb
      )
    ))
  }

  create_decision(
    admitted = FALSE,
    reason = "insufficient_memory",
    details = list(
      available_mb = available_mb,
      total_mb = total_mb,
      required_mb = required_mb,
      estimated_mb = estimated_cost$estimated_mb,
      buffer_mb = min_buffer_mb,
      shortfall_mb = required_mb - available_mb
    )
  )
}

reject_request <- function(decision, res) {
  total_mb <- decision$details$total_mb %||% 0
  required_mb <- decision$details$required_mb %||% 0
  available_mb <- decision$details$available_mb %||% 0

  if (total_mb > 0 && required_mb > total_mb) {
    res$status <- 413
    res$setHeader("X-Throttle-Reason", "file_too_large")
    return(list(
      error = "Request too large",
      reason = "file_too_large",
      message = sprintf(
        "This request requires %.0f MB but the server maximum is %.0f MB. Please reduce your data size.",
        required_mb, total_mb
      ),
      retry_after_seconds = NULL,
      details = list(
        available_mb = available_mb,
        required_mb = required_mb,
        total_mb = total_mb,
        excess_mb = required_mb - total_mb
      )
    ))
  }

  res$status <- 503
  res$setHeader("Retry-After", "60")
  res$setHeader("X-Throttle-Reason", "insufficient_memory")

  list(
    error = "Service temporarily unavailable",
    reason = "insufficient_memory",
    message = sprintf(
      "Server is busy. Available: %.0f MB, Required: %.0f MB. Please retry in 60 seconds.",
      available_mb, required_mb
    ),
    retry_after_seconds = 60,
    details = list(
      available_mb = available_mb,
      required_mb = required_mb,
      total_mb = total_mb,
      shortfall_mb = decision$details$shortfall_mb %||% (required_mb - available_mb)
    )
  )
}

create_throttle_guard <- function(
    throttle_config,
    memory_fn = get_system_memory,
    logger_fn = function(msg) invisible(NULL)
) {
  estimate_cost <- create_cost_estimator(throttle_config)
  min_buffer_mb <- throttle_config$min_system_buffer_mb %||% 512
  enabled <- throttle_config$enabled %||% TRUE

  function(endpoint_name, req) {
    bypass <- check_bypass(req, enabled)
    if (bypass$bypass) {
      tryCatch({
        logger_fn(sprintf("Throttle bypass for %s: %s", endpoint_name, bypass$reason))
      }, error = function(e) {})

      return(create_decision(
        admitted = TRUE,
        reason = paste0("bypass:", bypass$reason),
        details = list(endpoint = endpoint_name)
      ))
    }

    cost <- estimate_cost(endpoint_name, req)
    memory <- memory_fn()
    decision <- check_admission(cost, memory, min_buffer_mb)

    tryCatch({
      if (decision$admitted) {
        if (decision$reason == "memory_detection_unavailable") {
          logger_fn(sprintf(
            "Throttle ADMIT %s (fallback): memory detection unavailable (%s), estimated %dMB",
            endpoint_name, memory$source, cost$estimated_mb
          ))
        } else {
          logger_fn(sprintf(
            "Throttle ADMIT %s: %.0fMB available, %.0fMB required (headroom: %.0fMB)",
            endpoint_name,
            decision$details$available_mb,
            decision$details$required_mb,
            decision$details$headroom_mb
          ))
        }
      } else {
        logger_fn(sprintf(
          "Throttle REJECT %s: %.0fMB available, %.0fMB required (shortfall: %.0fMB)",
          endpoint_name,
          decision$details$available_mb,
          decision$details$required_mb,
          decision$details$shortfall_mb
        ))
      }
    }, error = function(e) {})

    decision
  }
}

format_decision <- function(decision) {
  if (decision$admitted) {
    sprintf("ADMITTED (%s)", decision$reason)
  } else {
    sprintf(
      "REJECTED (%s) - shortfall: %.0fMB",
      decision$reason,
      decision$details$shortfall_mb %||% 0
    )
  }
}

