# Climatehealth Pipelines Archive Manifest

This package now embeds the required API runtime assets under `inst/plumber/`.
The following `climatehealth_pipelines/` assets are intentionally **not migrated**
and should be archived with that repository when it is retired.

## Deprecated/Archive Set

- `config/*.yml` indicator configs (replaced by package examples and templates in `inst/extdata/config_templates/`)
- `scripts/temp_mortality.R`
- `scripts/wildfire_smoke_mortality_all_cause.R`
- `scripts/suicides_extreme_heat.R`
- `scripts/descriptive_stats.R`
- `scripts/diarrhea.R`
- `scripts/malaria.R`
- `scripts/air_pollution.R`
- `scripts/multiple_years.R`
- `data_template/regEngWales_data_template.csv`
- repository infrastructure and project artifacts (`dependencies.txt`, `install_dependencies.R`, docs images, `.Rproj`, `git_filter/`, data/log placeholders)

## Active Migration Targets in Package

- `inst/plumber/` API entrypoint and runtime
- `inst/plumber/throttling/` throttle guard modules
- `inst/scripts/calibrate_throttle.R` operational helper
- `tests/testthat/test_throttle_*.R` and helper loader for package-native tests
- `inst/extdata/config_templates/*.yml` package-scoped runtime templates
