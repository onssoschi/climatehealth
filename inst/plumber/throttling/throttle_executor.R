# throttle_executor.R - Execution Wrapper with Proper GC Placement

`%||%` <- function(x, y) if (is.null(x)) y else x

run_guarded <- function(res, endpoint_type, func, args, guard, logger_fn = NULL) {
  if (is.null(logger_fn)) {
    logger_fn <- function(msg) invisible(NULL)
  }

  mock_req <- list(
    args = args,
    body = args
  )

  decision <- guard(paste0("/", endpoint_type), mock_req)

  if (!decision$admitted) {
    tryCatch({
      logger_fn(sprintf("Request rejected for /%s: %s", endpoint_type, decision$reason))
    }, error = function(e) {})

    return(reject_request(decision, res))
  }

  tryCatch({
    tryCatch({
      logger_fn(sprintf("Executing /%s analysis...", endpoint_type))
    }, error = function(e) {})

    result <- do.call(func, args)

    tryCatch({
      logger_fn(sprintf("/%s analysis completed successfully", endpoint_type))
    }, error = function(e) {})

    result
  }, error = function(e) {
    tryCatch({
      logger_fn(sprintf("/%s analysis failed: %s", endpoint_type, e$message))
    }, error = function(e2) {})

    res$status <- 500
    list(
      status = "error",
      message = e$message,
      endpoint = endpoint_type
    )
  }, finally = {
    gc(verbose = FALSE, full = TRUE)
  })
}

create_endpoint_wrapper <- function(endpoint_type, guard, logger_fn = NULL) {
  function(res, func, args) {
    run_guarded(res, endpoint_type, func, args, guard, logger_fn)
  }
}

run_with_memory_tracking <- function(func, args, get_rss_fn = NULL) {
  if (is.null(get_rss_fn)) {
    get_rss_fn <- function() {
      if (exists("get_process_rss_mb")) get_process_rss_mb() else NA
    }
  }

  gc(verbose = FALSE, full = TRUE)

  rss_before <- get_rss_fn()
  r_heap_before <- sum(gc(verbose = FALSE)[, 2])
  start_time <- Sys.time()

  result <- tryCatch({
    do.call(func, args)
  }, error = function(e) {
    list(error = e$message)
  })

  end_time <- Sys.time()
  rss_after <- get_rss_fn()
  r_heap_after <- sum(gc(verbose = FALSE, full = TRUE)[, 2])

  list(
    result = result,
    memory = list(
      rss_before_mb = rss_before,
      rss_after_mb = rss_after,
      rss_delta_mb = if (!is.na(rss_before) && !is.na(rss_after)) rss_after - rss_before else NA,
      r_heap_before_mb = r_heap_before,
      r_heap_after_mb = r_heap_after,
      r_heap_delta_mb = r_heap_after - r_heap_before
    ),
    duration_seconds = as.numeric(difftime(end_time, start_time, units = "secs")),
    success = is.null(result$error)
  )
}

