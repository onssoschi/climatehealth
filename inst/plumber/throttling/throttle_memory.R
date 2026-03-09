# throttle_memory.R - Cross-Platform Memory Detection

get_system_memory <- function() {
  result <- list(
    available_mb = NULL,
    total_mb = NULL,
    used_mb = NULL,
    source = "unknown",
    success = FALSE
  )

  tryCatch({
    if (file.exists("/proc/meminfo")) {
      meminfo <- readLines("/proc/meminfo", warn = FALSE)

      available_line <- grep("^MemAvailable:", meminfo, value = TRUE)
      if (length(available_line) == 0) {
        available_line <- grep("^MemFree:", meminfo, value = TRUE)
      }

      total_line <- grep("^MemTotal:", meminfo, value = TRUE)

      if (length(available_line) > 0 && length(total_line) > 0) {
        available_kb <- as.numeric(gsub("[^0-9]", "", available_line))
        total_kb <- as.numeric(gsub("[^0-9]", "", total_line))

        result$available_mb <- round(available_kb / 1024, 2)
        result$total_mb <- round(total_kb / 1024, 2)
        result$used_mb <- result$total_mb - result$available_mb
        result$source <- "proc_meminfo"
        result$success <- TRUE
        return(result)
      }
    }

    container_mem <- get_container_memory_limit()
    if (container_mem$success) {
      rss_mb <- get_process_rss_mb()
      if (!is.null(rss_mb) && !is.na(rss_mb)) {
        result$total_mb <- container_mem$limit_mb
        result$used_mb <- rss_mb
        result$available_mb <- max(0, container_mem$limit_mb - rss_mb)
        result$source <- container_mem$source
        result$success <- TRUE
        return(result)
      }
    }

    if (Sys.info()["sysname"] == "Darwin") {
      vm_stat_result <- tryCatch({
        output <- system("vm_stat", intern = TRUE)
        page_size_line <- grep("page size of", output, value = TRUE)
        page_size <- as.numeric(gsub("[^0-9]", "", page_size_line))
        if (is.na(page_size)) page_size <- 4096

        free_line <- grep("Pages free:", output, value = TRUE)
        free_pages <- as.numeric(gsub("[^0-9]", "", free_line))

        inactive_line <- grep("Pages inactive:", output, value = TRUE)
        inactive_pages <- as.numeric(gsub("[^0-9]", "", inactive_line))

        available_bytes <- (free_pages + inactive_pages) * page_size
        list(available_mb = round(available_bytes / 1024 / 1024, 2), success = TRUE)
      }, error = function(e) list(success = FALSE))

      if (vm_stat_result$success) {
        result$available_mb <- vm_stat_result$available_mb
        result$source <- "macos_vm_stat"
        result$success <- TRUE
        return(result)
      }
    }
  }, error = function(e) {})

  result
}

get_container_memory_limit <- function() {
  result <- list(limit_mb = NULL, source = NULL, success = FALSE)

  cgroup_v2_path <- "/sys/fs/cgroup/memory.max"
  if (file.exists(cgroup_v2_path)) {
    tryCatch({
      val <- readLines(cgroup_v2_path, n = 1, warn = FALSE)
      if (val != "max") {
        result$limit_mb <- round(as.numeric(val) / 1024 / 1024, 2)
        result$source <- "cgroup_v2"
        result$success <- TRUE
        return(result)
      }
    }, error = function(e) {})
  }

  cgroup_v1_path <- "/sys/fs/cgroup/memory/memory.limit_in_bytes"
  if (file.exists(cgroup_v1_path)) {
    tryCatch({
      val <- readLines(cgroup_v1_path, n = 1, warn = FALSE)
      num_val <- as.numeric(val)
      if (!is.na(num_val) && num_val < 1e15) {
        result$limit_mb <- round(num_val / 1024 / 1024, 2)
        result$source <- "cgroup_v1"
        result$success <- TRUE
        return(result)
      }
    }, error = function(e) {})
  }

  result
}

get_process_rss_mb <- function() {
  if (file.exists("/proc/self/status")) {
    tryCatch({
      status_lines <- readLines("/proc/self/status", warn = FALSE)
      rss_line <- grep("^VmRSS:", status_lines, value = TRUE)
      if (length(rss_line) > 0) {
        rss_kb <- as.numeric(gsub("[^0-9]", "", rss_line))
        return(round(rss_kb / 1024, 2))
      }
    }, error = function(e) {})
  }

  if (Sys.info()["sysname"] == "Darwin") {
    tryCatch({
      pid <- Sys.getpid()
      output <- system(paste("ps -o rss= -p", pid), intern = TRUE)
      rss_kb <- as.numeric(trimws(output))
      if (!is.na(rss_kb)) {
        return(round(rss_kb / 1024, 2))
      }
    }, error = function(e) {})
  }

  NA
}

get_process_peak_mb <- function() {
  if (file.exists("/proc/self/status")) {
    tryCatch({
      status_lines <- readLines("/proc/self/status", warn = FALSE)
      peak_line <- grep("^VmPeak:", status_lines, value = TRUE)
      if (length(peak_line) > 0) {
        peak_kb <- as.numeric(gsub("[^0-9]", "", peak_line))
        return(round(peak_kb / 1024, 2))
      }
    }, error = function(e) {})
  }
  NA
}

format_memory_info <- function(mem_info) {
  if (!mem_info$success) {
    return(sprintf("Memory detection failed (source: %s)", mem_info$source))
  }

  sprintf(
    "Available: %.0fMB, Total: %.0fMB, Used: %.0fMB (source: %s)",
    mem_info$available_mb %||% 0,
    mem_info$total_mb %||% 0,
    mem_info$used_mb %||% 0,
    mem_info$source
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x

