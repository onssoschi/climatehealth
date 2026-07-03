# climatehealth development notes

## 1.0.3

- Updated wildfire v5 PM2.5 handling:
  - treats joined wildfire PM2.5 values as already in micrograms, without the
    legacy kg-to-microgram scaling step
  - continues to normalise custom exposure columns to `mean_PM` for downstream
    wildfire calculations
  - refreshes wildfire tests and documentation for the v5 data-source workflow
- Improved temperature-related mortality national-output handling:
  - supports configurable country labels for pooled national outputs
  - includes national attribution fixes used by the meta-analysis workflow
  - updates temperature-mortality documentation for supported spline settings
- Improved CRAN release hygiene:
  - removes test warnings from case-insensitive fixed-string matching and
    tidyselect column selection
  - hardens the CI R package cache setup used by package checks

## 1.0.2

- Improved accessibility of saved plot outputs across the package:
  - added shared helpers for accessible base R, ggplot2, and patchwork outputs
  - applies consistent SOSCHI styling, accessible colours, figure titles, logos,
    and wrapped alternative-text captions
  - improves sizing, layout, and pagination for multi-panel indicator reports
- Integrated descriptive statistics more closely with the main analysis
  workflows:
  - added optional descriptive-statistics controls to the indicator
    `do_analysis()` functions where relevant
  - supports summary statistics, correlation outputs, moving-average plots,
    missing-data summaries, and outlier checks alongside indicator outputs
  - preserves run-scoped descriptive output folders and forwards the selected
    correlation method through the indicator workflows
- Updated indicator workflows and outputs:
  - air pollution now supports additional continuous covariates in regional GAMs,
    normalises custom column names, ignores unusable covariates, handles
    no-excess national power outputs, and returns API-safe results
  - wildfire handling was improved for region/date ordering, region-level
    outputs, PM2.5 scaling, VIF/QAIC checks, descriptive-statistics column
    mapping, and missing population checks
  - mental health and temperature-related workflows gained more reproducible
    attribution options, clearer model-validation outputs, and refreshed
    documentation
  - malaria and diarrhea workflows gained optional descriptive statistics,
    API-mode safeguards, and accessible output updates
- Improved API/runtime support:
  - added an air pollution endpoint to the bundled plumber API
  - added support for base64-encoded map zip uploads for malaria and diarrhea
    API requests
  - prevents API-mode calls from writing plot/CSV outputs or returning
    non-JSON-safe fitted model objects
- Refreshed package documentation and presentation:
  - added UNSC endorsement and CRAN availability information to the README
  - added SOSCHI branding/logo assets for README and report outputs
  - added a README notes column to flag the ongoing temperature-related health
    effects review work
  - updated indicator titles and documentation for consistency
- Improved package maintenance and CI reliability:
  - expanded unit and integration coverage for accessibility, descriptive
    statistics, API mode, and indicator workflows
  - keeps heavy integration tests opt-in for coverage reporting and extends the
    Codecov step timeout
  - removed unused plotting helpers/dependencies and added `png` for logo
    rendering

## 1.0.1

- CRAN resubmission focused on Fedora Linux check reliability and test stability.
- Fixed INLA model execution under check-core constraints:
  - limits INLA thread usage when `_R_CHECK_LIMIT_CORES_` is set
  - keeps compatibility across INLA versions by conditionally passing
    thread-related arguments only when supported
- Hardened CRAN behavior for fragile INLA end-to-end disease tests:
  - retained CRAN-safe skips for diarrhea and malaria integration paths
  - avoids Fedora-specific parallel worker failures seen in prior checks
- Improved integration-test governance in CI:
  - heavy integration tests are opt-in via `RUN_INTEGRATION=true`
  - standard PR/push CI remains fast and high-signal by default
- Build hygiene improvement:
  - excludes `.covrignore` from package build input via `.Rbuildignore`

## 1.0.0

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
