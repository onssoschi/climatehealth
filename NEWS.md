# climatehealth development notes

## Unreleased

## 0.9.30

- Added new descriptive statistics public APIs:
  - `run_descriptive_stats()`
  - `run_descriptive_stats_api()`
- Added run-scoped output folders for descriptive stats:
  - `<output_path>/descriptive_stats/<run_id>/All/`
  - `<output_path>/descriptive_stats/<run_id>/<Region>/`
- Kept backward-compatible aliases with runtime deprecation warnings:
  - `common_descriptive_stats_core()`
  - `common_descriptive_stats()`
  - `common_descriptive_stats_api()`
- Improved descriptive stats robustness:
  - correlation now validates and filters numeric columns explicitly
  - centralized preflight column and parameter validation for enabled features
  - typed summary behavior for non-numeric columns
  - safer plot device cleanup with managed PDF helper
- Expanded tests for wrapper behavior, run-folder outputs, API payload validation,
  endpoint-style contract responses, and deprecated alias compatibility.
- Moved API runtime assets previously held in `climatehealth_pipelines` into the package:
  - plumber routes and startup scripts under `inst/plumber/`
  - throttling modules and tests
  - package-scoped API and developer config templates under `inst/extdata/config_templates/`
  - operational helper scripts under `inst/scripts/`
- Improved package/API compatibility for runtime usage:
  - enabled API mode in plumber runtime
  - added direct endpoint bindings for descriptive statistics, malaria, and diarrhea
  - enforced API-safe return payloads for malaria and diarrhea in API mode
  - extended internals to accept API-style record payloads and map records
  - updated throttle estimation to size JSON-record payloads instead of file paths
- Cleaned up the air pollution module:
  - standardized argument and variable naming
  - refreshed documentation
  - resolved R CMD check notes for global variable bindings
  - added backward-compatible support for legacy `air_pollution_do_analysis()` argument names
  - expanded air pollution regression and integration tests
