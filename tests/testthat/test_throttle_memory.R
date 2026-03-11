#' Unit Tests for throttle_memory.R
#'
#' Tests cross-platform memory detection.

if (!exists("load_throttle_modules")) {
  source("tests/testthat/helper-throttle_modules.R", local = FALSE)
}

context("Throttle Memory")

if (!exists("get_system_memory")) {
  load_throttle_modules(include_executor = FALSE)
}

test_that("get_system_memory returns structured result", {
  mem <- get_system_memory()
  expect_true(is.list(mem))
  expect_true("success" %in% names(mem))
  expect_true("source" %in% names(mem))
  expect_true("available_mb" %in% names(mem))
  expect_true("total_mb" %in% names(mem))
  expect_true("used_mb" %in% names(mem))
})

test_that("get_system_memory does not crash on any platform", {
  expect_error(get_system_memory(), NA)
})

test_that("get_system_memory returns valid source identifier", {
  mem <- get_system_memory()
  valid_sources <- c("proc_meminfo", "cgroup_v1", "cgroup_v2", "macos_vm_stat", "unknown")
  expect_true(mem$source %in% valid_sources)
})

test_that("get_system_memory returns positive values when successful", {
  mem <- get_system_memory()
  if (mem$success) {
    expect_true(mem$available_mb > 0 || is.null(mem$available_mb))
    if (!is.null(mem$total_mb)) {
      expect_true(mem$total_mb > 0)
    }
  }
})

test_that("get_process_rss_mb returns numeric or NA", {
  rss <- get_process_rss_mb()
  expect_true(is.numeric(rss) || is.na(rss))
})

test_that("get_process_rss_mb does not crash", {
  expect_error(get_process_rss_mb(), NA)
})

test_that("get_process_rss_mb returns positive value when available", {
  rss <- get_process_rss_mb()
  if (!is.na(rss)) {
    expect_true(rss > 0)
  }
})

test_that("get_container_memory_limit returns structured result", {
  result <- get_container_memory_limit()
  expect_true(is.list(result))
  expect_true("success" %in% names(result))
  expect_true("source" %in% names(result))
  expect_true("limit_mb" %in% names(result))
})

test_that("get_container_memory_limit does not crash", {
  expect_error(get_container_memory_limit(), NA)
})

test_that("format_memory_info handles successful detection", {
  mem_info <- list(
    success = TRUE,
    available_mb = 4096,
    total_mb = 8192,
    used_mb = 4096,
    source = "proc_meminfo"
  )
  formatted <- format_memory_info(mem_info)
  expect_true(is.character(formatted))
  expect_true(grepl("4096", formatted))
  expect_true(grepl("proc_meminfo", formatted))
})

test_that("format_memory_info handles failed detection", {
  mem_info <- list(
    success = FALSE,
    source = "unknown"
  )
  formatted <- format_memory_info(mem_info)
  expect_true(is.character(formatted))
  expect_true(grepl("failed", formatted, ignore.case = TRUE))
})
