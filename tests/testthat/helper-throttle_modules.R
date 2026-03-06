load_throttle_modules <- function(include_executor = FALSE) {
  throttle_root <- system.file("plumber", "throttling", package = "climatehealth")
  if (!nzchar(throttle_root)) {
    throttle_root <- file.path("inst", "plumber", "throttling")
  }

  required <- c(
    "throttle_memory.R",
    "throttle_config.R",
    "throttle_estimator.R",
    "throttle_guard.R"
  )
  if (isTRUE(include_executor)) {
    required <- c(required, "throttle_executor.R")
  }

  missing <- required[!file.exists(file.path(throttle_root, required))]
  if (length(missing) > 0) {
    stop(
      sprintf(
        "Missing throttling module(s): %s (looked in %s)",
        paste(missing, collapse = ", "),
        throttle_root
      )
    )
  }

  for (fname in required) {
    source(file.path(throttle_root, fname), local = FALSE)
  }
}
