# climatehealth development notes

## Unreleased

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
  - correlation now validates/filter numeric columns explicitly
  - centralized preflight column/parameter validation for enabled features
  - typed summary behavior for non-numeric columns
  - safer plot device cleanup with managed PDF helper
- Expanded tests for wrapper behavior, run-folder outputs, API payload validation,
  endpoint-style contract responses, and deprecated alias compatibility.
