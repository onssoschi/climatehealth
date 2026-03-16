![Static Badge](https://img.shields.io/badge/Status-In%20Development-darkgreen?logo=git&logoColor=white) ![Static Badge](https://img.shields.io/badge/Release%20Version-1.0.0-pink?logo=github) ![Static Badge](https://img.shields.io/badge/R_Version-4.4.1-pink?logo=R) [![codecov](https://codecov.io/gh/onssoschi/climatehealth/graph/badge.svg)](https://app.codecov.io/gh/onssoschi/climatehealth)
[![Build R Package on Release](https://github.com/onssoschi/climatehealth/actions/workflows/r_release.yml/badge.svg)](https://github.com/onssoschi/climatehealth/actions/workflows/r_release.yml)
[![R Package Test Coverage](https://github.com/onssoschi/climatehealth/actions/workflows/code_coverage.yml/badge.svg)](https://github.com/onssoschi/climatehealth/actions/workflows/code_coverage.yml)

# Climatehealth Package

## Table of Contents
- [Package Overview](#package-overview)
- [SOSCHI Topics and Indicators](#soschi-topics-and-indicators)
- [Package Installation](#package-installation)
- [Running analysis](#running-analysis)
- [Contributing to the package](#contributing-to-the-package)
- [Authors](#authors)
- [Contributors](#contributors)
- [Project Lead](#project-lead)
- [Methodology review](#methodology-review)
- [Acknowledgements](#acknowledgements)
- [Copyright](#copyright)
- [Citation](#citation)
- [Funding](#funding)

## Package Overview

### Description

This package provides functions for calculating climate–health indicators and supporting the production of climate–health statistics. The methods implemented in the package follow the statistical framework for climate–health indicators developed under the Standards for Official Statistics on Climate–Health Interactions (SOSCHI) project. You can find out more and stay up to date with the [SOSCHI project here](https://climate-health.officialstatistics.org/).

### SOSCHI Topics and Indicators

This package contains scientific functions dedicated to the following topics in the SOSCHI framework. Please note the final methods documents are still in development, so the versions linked below are earlier alpha versions.

| **Topic**                                                       | **Topic Owner** | **Methods**                                                      |
|--------------------------------------------------------------------|-----------------|---------------------------------------------------------------------|
| Temperature-related health effects                                   | ONS             | [Methodology Document](https://doi.org/10.5281/zenodo.14865904)     |
| Health effects of wildfires        | ONS             | [Methodology Document](https://doi.org/10.5281/zenodo.14052183)     |
| Mental health                                                      | ONS             | [Methodology Document](https://doi.org/10.5281/zenodo.14050224)     |
| Water-borne diseases                                                       | AIMS            | [Methodology Document](https://doi.org/10.5281/zenodo.14871506)     |
| Health effects of air pollution                                                      | AIMS            | [Methodology Document](https://doi.org/10.5281/zenodo.14871506)     |
| Vector-borne diseases                                                            | RIPS/AIMS       | [Methodology Document](https://doi.org/10.5281/zenodo.14871506)     |

> [!NOTE]
> Please note that there may be some inconsistencies between the indicators and methods included in the Github code/R package, and those described in the corresponding SOSCHI topic documents, and/or related analysis publications. This is because the final methods documents are still in development, so those linked below in the section 'SOSCHI Topics and Indicators' are an earlier alpha version. 

For information on how to use this R package, check [here](https://github.com/onssoschi/climatehealth/blob/main/vignettes/getting-started.Rmd)

## Package Installation

### Requirements

To install and use the package, you need an R environment.
This package has been developed primarily with **R 4.4.1**, and that version is recommended where possible. **R-4.4.1** can be downloaded [here](https://cran.r-project.org/bin/windows/base/old/4.4.1/)

### Installation options

You can use the package in several ways, depending on whether you want a stable release or want to work with the source code.

#### 1. Load from a local GitHub clone for development
Use this option if you want to inspect the source code, make changes locally, test updates, or contribute to the package.

First clone the repository from GitHub to your machine. Then in R:

```r
install.packages("devtools")
devtools::load_all(path = "{path/to/climatehealth}")
```

This loads the package directly from the local source directory into your current R session. This is useful for development, but won't make the `climatehealth` package available as a standard installed package in your library. Once the source code changes, run `devtools::load_all()` again to reload the updated functions. If you are an end user, follow the standard installation procedure outlined below for your operating system.


#### 2. Install directly from GitHub

If you want to install the latest development version of the package directly from GitHub without cloning the repository, you can use `remotes::install_github()`.

First install the `remotes` package in R if needed:
```r
install.packages("remotes")
```
Then install the package with:
```r
remotes::install_github("onssoschi/climatehealth")
```
This installs the package from the GitHub repository into your R library in the usual way. This option is useful if you want the latest development version of the package but do not need to edit the source code locally.


#### 3. Install the latest Windows binary release (pre-compiled)
If you would like to install the latest official release of the package, download the `.zip` file from the latest GitHub release [here](https://github.com/onssoschi/climatehealth/releases/latest). Then install it in R with:
```r
install.packages(path = "{path/to/climatehealth_<version>.zip}", repos = NULL, type = "win.binary")
```
where `<version>` is the version number of the release you downloaded.

#### 4. Install the latest source release for MacOS or other platforms (needs compilation)
If you are using MacOS and would like to install from the source, download the `.tar.gz` file from the latest GitHub release [here](https://github.com/onssoschi/climatehealth/releases/latest). Then install it in R with:
```r
install.packages(path = "{path/to/climatehealth_<version>.tar.gz}", repos = NULL, type = "source")
```
where `<version>` is the version number of the release you downloaded.

## Running analysis
Once the package is installed or loaded, you can run any of the main analysis entry points depending on your use case.

### Indicator workflows
Most indicator modules are run through a top-level `*_do_analysis()` function. For example:

```r
result <- climatehealth::air_pollution_do_analysis(
  data_path = "{path/to/your_data.csv}"
)
```
Other indicators follow the same pattern, for example:
```r
temp_mortality_do_analysis()
wildfire_do_analysis()
malaria_do_analysis()
diarrhea_do_analysis()
suicides_heat_do_analysis()
```
Each analysis function expects specific input data and parameters for that indicator, so refer to the package documentation for the relevant module before running it.

Example usage scripts for the main indicator workflows are provided in [`inst/examples/`](inst/examples), including:
- `air_pollution_usage.R`
- `temp_mortality_usage.R`
- `wildfires_usage.R`
- `malaria_usage.R`
- `diarrhea_usage.R`
- `suicides_heat_usage.R`
- `descriptive_stats_usage.R`

### Descriptive statistics
You can also run the descriptive statistics workflow directly:
```r
result <- climatehealth::run_descriptive_stats(
  data = my_data,
  output_path = "{path/to/output}"
)
```
This is useful for checking data quality, exploring variables, and generating summary outputs before running an indicator-specific workflow.

## Contributing to the package
We welcome contributions from public users, collaborators, and developers who want to improve the package.
If you want to contribute code, documentation, tests, or bug fixes, please start by reading the CONTRIBUTING guide [CONTRIBUTING](https://github.com/onssoschi/climatehealth/blob/main/CONTRIBUTING.md). This includes guidance on setting up a local development workflow, making changes safely, and submitting updates through GitHub.

## Authors
The package authors reflect direct intellectual and technical contributions to the package architecture, module methods, and reproducible analytical workflows.

- Lead author: Charlie Browning
- Lead maintainer and platform developer: Kenechi Omeke
- Authors: Etse Yawo Dzakpa, Gladin Jose, Matt Pearce, Ellie Watkins, Claire Hunt, Beatrice Byukusenge, Cassien Habyarimana, Venuste Nyagahakwa, Felix Scarbrough, Treesa Shaji, Bonnie Lewis, and Vijendra Ingole

## Contributors
The following contributors supported the package through bug fixes, minor features,
testing, documentation, and review work:
- Sean Lovell
- Antony Brown
- Euan Soutter
- Gillian Flower
- David Furley
- Joe Panes
- Charlotte Romaniuk
- Milly Powell

## Project Lead
- Myer Glickman, Bonnie Lewis and Vijendra Ingole

## Methodology review
- Sewe Maquines

## Acknowledgements
We acknowledge the Topic Expert Group members and Expert Advisory Group members for higher-level leadership, methodological input, and subject-area support to the SOSCHI climate-health work.
We also acknowledge the use of external data and code examples that informed testing and early prototyping.

## Copyright
This is an open access R package published under a UK Open Government Licence 3.0 which is compatible with the Creative Commons CC-BY-SA 4.0 licence. You are free to copy, publish, distribute and adapt the information, provided you acknowledge the source and link to this R package (see citation below). Enquiries concerning the application of this R package can be made to the project team at climate.health@ons.gov.uk.

## Citation

If you use `climatehealth` in publications, cite the package as:

Browning, C., Omeke, K., Dzakpa, E. Y., Jose, G., Pearce, M., Watkins, E., Hunt,
C., Byukusenge, B., Habyarimana, C., Nyagahakwa, V., Scarbrough, F., Shaji, T.,
Lewis, B., and Ingole, V. (2026).
`climatehealth R package: Standards for Official Statistics on Climate-Health Interactions (SOSCHI)`.
R package.

An R package citation file is provided at `inst/CITATION`.

## Funding
![Static Badge](https://img.shields.io/badge/Wellcome_Trust-224682%2FZ%2F21%2FZ-%23ff24de)

This work was supported by Wellcome.
