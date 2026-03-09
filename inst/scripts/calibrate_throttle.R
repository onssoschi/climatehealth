#' calibrate_throttle.R - VmRSS-Based Memory Calibration Script
#'
#' This script measures actual peak memory usage for each endpoint to determine
#' accurate base_mb and multiplier values for throttle configuration.
#'
#' CRITICAL: Uses VmRSS (OS-level memory) instead of gc() because:
#' - gc() only tracks R's managed heap
#' - INLA allocates via C/GMRFLib - invisible to R's gc()
#' - Real-world: gc() might report 500MB while OS sees 2GB
#' - VmRSS shows what the OS actually sees - the true OOM risk
#'
#' Usage:
#'   source(system.file("scripts", "calibrate_throttle.R", package = "climatehealth"))
#'   results <- calibrate_endpoint("temperature", test_args)
#'   print(results)

library(parallel)

# =============================================================================
# MEMORY MEASUREMENT FUNCTIONS
# =============================================================================

get_process_rss_mb <- function() {
  if (file.exists("/proc/self/status")) {
    tryCatch({
      lines <- readLines("/proc/self/status", warn = FALSE)
      rss_line <- grep("^VmRSS:", lines, value = TRUE)
      if (length(rss_line) > 0) {
        rss_kb <- as.numeric(gsub("[^0-9]", "", rss_line))
        return(round(rss_kb / 1024, 2))
      }
    }, error = function(e) NA)
  }

  if (Sys.info()["sysname"] == "Darwin") {
    tryCatch({
      pid <- Sys.getpid()
      output <- system(paste("ps -o rss= -p", pid), intern = TRUE)
      rss_kb <- as.numeric(trimws(output))
      return(round(rss_kb / 1024, 2))
    }, error = function(e) NA)
  }

  NA
}

measure_memory_simple <- function(func, args) {
  gc(verbose = FALSE, full = TRUE)

  baseline_rss_mb <- get_process_rss_mb()
  baseline_r_mb <- sum(gc(verbose = FALSE)[, 2])

  start_time <- Sys.time()

  result <- tryCatch({
    do.call(func, args)
  }, error = function(e) {
    list(error = e$message)
  })

  end_time <- Sys.time()

  final_rss_mb <- get_process_rss_mb()
  final_r_mb <- sum(gc(verbose = FALSE, full = TRUE)[, 2])

  peak_rss_mb <- max(baseline_rss_mb, final_rss_mb, na.rm = TRUE)
  duration <- as.numeric(difftime(end_time, start_time, units = "secs"))

  list(
    baseline_rss_mb = baseline_rss_mb,
    final_rss_mb = final_rss_mb,
    peak_rss_mb = peak_rss_mb,
    delta_rss_mb = peak_rss_mb - baseline_rss_mb,
    r_heap_baseline_mb = baseline_r_mb,
    r_heap_final_mb = final_r_mb,
    duration_seconds = round(duration, 2),
    success = is.null(result$error),
    error_msg = if (!is.null(result$error)) result$error else NULL,
    note = "Peak may be underestimated. For accurate peak, use measure_memory_continuous."
  )
}

measure_memory_continuous <- function(func, args, poll_ms = 100) {
  gc(verbose = FALSE, full = TRUE)

  baseline_rss <- get_process_rss_mb()
  poll_file <- tempfile(fileext = ".txt")

  poll_pid <- parallel::mcparallel({
    repeat {
      rss <- get_process_rss_mb()
      if (!is.na(rss)) {
        cat(rss, "\n", file = poll_file, append = TRUE)
      }
      Sys.sleep(poll_ms / 1000)
    }
  })

  start_time <- Sys.time()

  result <- tryCatch({
    do.call(func, args)
  }, error = function(e) list(error = e$message))

  end_time <- Sys.time()

  tryCatch({
    tools::pskill(poll_pid$pid, signal = 9)
  }, error = function(e) {})

  samples <- c(baseline_rss)
  if (file.exists(poll_file)) {
    tryCatch({
      raw_samples <- readLines(poll_file, warn = FALSE)
      numeric_samples <- as.numeric(raw_samples)
      numeric_samples <- numeric_samples[!is.na(numeric_samples)]
      if (length(numeric_samples) > 0) {
        samples <- c(samples, numeric_samples)
      }
    }, error = function(e) {})
    unlink(poll_file)
  }

  final_rss <- get_process_rss_mb()
  samples <- c(samples, final_rss)

  list(
    baseline_rss_mb = baseline_rss,
    final_rss_mb = final_rss,
    peak_rss_mb = max(samples, na.rm = TRUE),
    delta_rss_mb = max(samples, na.rm = TRUE) - baseline_rss,
    sample_count = length(samples),
    samples = samples,
    duration_seconds = round(as.numeric(difftime(end_time, start_time, units = "secs")), 2),
    success = is.null(result$error),
    error_msg = if (!is.null(result$error)) result$error else NULL
  )
}

# =============================================================================
# CALIBRATION WORKFLOW
# =============================================================================

calibrate_endpoint <- function(endpoint_name, func, test_files, make_args, use_continuous = TRUE) {
  message(sprintf("\n=== Calibrating endpoint: %s ===", endpoint_name))

  results <- lapply(names(test_files), function(label) {
    file_path <- test_files[[label]]

    if (!file.exists(file_path)) {
      message(sprintf("  [%s] SKIP - file not found: %s", label, file_path))
      return(NULL)
    }

    file_mb <- round(file.info(file_path)$size / 1024^2, 2)
    message(sprintf("  [%s] Testing with %.2f MB file...", label, file_mb))

    args <- make_args(file_path)

    if (use_continuous) {
      mem_result <- measure_memory_continuous(func, args, poll_ms = 100)
    } else {
      mem_result <- measure_memory_simple(func, args)
    }

    if (mem_result$success) {
      message(sprintf(
        "    Peak: %.0f MB, Delta: %.0f MB, Duration: %.1fs",
        mem_result$peak_rss_mb, mem_result$delta_rss_mb, mem_result$duration_seconds
      ))
    } else {
      message(sprintf("    FAILED: %s", mem_result$error_msg))
    }

    data.frame(
      endpoint = endpoint_name,
      label = label,
      file_mb = file_mb,
      baseline_rss_mb = mem_result$baseline_rss_mb,
      peak_rss_mb = mem_result$peak_rss_mb,
      delta_rss_mb = mem_result$delta_rss_mb,
      duration_sec = mem_result$duration_seconds,
      success = mem_result$success,
      stringsAsFactors = FALSE
    )
  })

  results_df <- do.call(rbind, Filter(Negate(is.null), results))

  if (nrow(results_df) > 1 && all(results_df$success)) {
    model <- lm(peak_rss_mb ~ file_mb, data = results_df)
    recommended_base <- round(coef(model)[1], 0)
    recommended_mult <- round(coef(model)[2], 1)

    message(sprintf("\n  RECOMMENDED CONFIG for %s:", endpoint_name))
    message(sprintf("    base_mb: %d", max(100, recommended_base)))
    message(sprintf("    multiplier: %.1f", max(1, recommended_mult)))

    attr(results_df, "recommended_base_mb") <- max(100, recommended_base)
    attr(results_df, "recommended_multiplier") <- max(1, recommended_mult)
  }

  results_df
}

generate_calibration_report <- function(all_results) {
  report <- c(
    "# Throttle Calibration Report",
    sprintf("Generated: %s", Sys.time()),
    sprintf("Platform: %s", Sys.info()["sysname"]),
    "",
    "## Summary",
    ""
  )

  for (name in names(all_results)) {
    df <- all_results[[name]]
    base_mb <- attr(df, "recommended_base_mb")
    mult <- attr(df, "recommended_multiplier")

    report <- c(
      report,
      sprintf("### %s", name),
      sprintf("- Recommended base_mb: %s", ifelse(is.null(base_mb), "N/A", base_mb)),
      sprintf("- Recommended multiplier: %s", ifelse(is.null(mult), "N/A", mult)),
      ""
    )

    if (!is.null(df) && nrow(df) > 0) {
      report <- c(report, "| File Size | Peak RSS | Delta | Duration |", "|-----------|----------|-------|----------|")
      for (i in seq_len(nrow(df))) {
        report <- c(
          report,
          sprintf(
            "| %.1f MB | %.0f MB | %.0f MB | %.1fs |",
            df$file_mb[i], df$peak_rss_mb[i], df$delta_rss_mb[i], df$duration_sec[i]
          )
        )
      }
      report <- c(report, "")
    }
  }

  paste(report, collapse = "\n")
}

# =============================================================================
# EXAMPLE USAGE (Uncomment to run)
# =============================================================================

# # Load the climatehealth package first
# library(climatehealth)
#
# # Define test files for temperature endpoint
# temp_test_files <- c(
#   "small" = "test_data/temp_5mb.csv",
#   "medium" = "test_data/temp_50mb.csv",
#   "large" = "test_data/temp_100mb.csv"
# )
#
# # Function to create args for temperature analysis
# make_temp_args <- function(file_path) {
#   list(
#     data_path = file_path,
#     date_col = "date",
#     region_col = "region",
#     temperature_col = "temperature",
#     dependent_col = "deaths",
#     population_col = "population"
#   )
# }
#
# # Run calibration
# temp_results <- calibrate_endpoint(
#   "temperature",
#   climatehealth::temp_mortality_do_analysis,
#   temp_test_files,
#   make_temp_args
# )
#
# # Print report
# cat(generate_calibration_report(list(temperature = temp_results)))


message("\nCalibration script loaded. Use calibrate_endpoint() to measure memory usage.")
message("Example: calibrate_endpoint('temperature', func, test_files, make_args)")
