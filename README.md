![Static Badge](https://img.shields.io/badge/Status-In%20Development-darkgreen?logo=git&logoColor=white) ![Static Badge](https://img.shields.io/badge/Release%20Version-0.9.22-pink?logo=github) ![Static Badge](https://img.shields.io/badge/R_Version-4.4.1-pink?logo=R) [![codecov](https://codecov.io/gh/onssoschi/climatehealth/graph/badge.svg?token=YKRAUQ3JAU)](https://codecov.io/gh/onssoschi/climatehealth)  
[![Build R Package on Release](https://github.com/onssoschi/climatehealth/actions/workflows/r_release.yml/badge.svg)](https://github.com/onssoschi/climatehealth/actions/workflows/r_release.yml) [![R CMD Check and Coverage](https://github.com/onssoschi/climatehealth/actions/workflows/r_check_and_coverage.yml/badge.svg)](https://github.com/onssoschi/climatehealth/actions/workflows/r_check_and_coverage.yml)

# Climatehealth Package

> [!NOTE]
> Please note that there may be some inconsistencies between the indicators and methods included in the Github code/R package, and those described in the corresponding SOSCHI topic documents, and/or related analysis publications. This is because the final methods documents are still in development, so those linked below in the section 'SOSCHI Topics and Indicators' are an earlier alpha version. 

## Table of Contents
- [Package Overview](#package-overview)
- [SOSCHI Topics and Indicators](#package-overview)
- [Package Installation](#package-installation)  
- [Open-Source development](#Open-Source-development)  
- [Authors](#authors)  
- [Contributors](#contributors)  
- [Acknowledgements](#acknowledgements)  
- [Citation](#citation)  
- [Funding](#funding)  

## Package Overview

### Description

This package provides functions for calculating climate–health indicators and supporting the production of climate–health statistics. The methods implemented in the package follow the statistical framework for climate–health indicators developed under the Standards for Official Statistics on Climate–Health Interactions (SOSCHI) project.

### SOSCHI Topics and Indicators

This package contains scientific functions dedicated to the following topics in the SOSCHI framework. Please note the final methods documents are still in development, so those linked below in the section are an earlier alpha version. :

| **Topic**                                                       | **Topic Owner** | **Methods**                                                      |
|--------------------------------------------------------------------|-----------------|---------------------------------------------------------------------|
| Temperature-related health effects                                   | ONS             | [Methodology Document](https://doi.org/10.5281/zenodo.14865904)     |
| Health effects of wildfires        | ONS             | [Methodology Document](https://doi.org/10.5281/zenodo.14052183)     |
| Mental health                                                      | ONS             | [Methodology Document](https://doi.org/10.5281/zenodo.14050224)     |
| Water-borne diseases                                                       | AIMS            | [Methodology Document](https://doi.org/10.5281/zenodo.14871506)     |
| Health effects of air pollution                                                      | AIMS            | [Methodology Document](https://doi.org/10.5281/zenodo.14871506)     |
| Vector-borne diseases                                                            | RIPS/AIMS       | [Methodology Document](https://doi.org/10.5281/zenodo.14871506)     |

For information on how to use this R package, see our official website [here](https://climate-health.officialstatistics.org/r_package/)

## Package Installation

### Requirements

To install the package and utilise its functionality, an R build is required.  
This package was largely developed on **R-4.4.1**, therefore it is recommended. **R-4.4.1** can be downloaded [here](https://cran.r-project.org/bin/windows/base/old/4.4.1/)

### Installation Methods

Installing the _climatehealth_ R package in your R environment can be done in various different ways. Your installation method can vary on your requirements and preference.  


#### Devtools
If you would like to make updates to the package code in realtime, `devtools` should be used to load the functions into memory.  

Firstly, you must install the devtools package in your R environment. This can be done in R by using the following:
```r
install.packages("devtools")
```
Once devtools has been installed, the climatehealth package can be loaded in to memory by running
```r
devtools::load_all(path = "{path/to/climatehealth}")
```
This will allow the package functions to be utilised. Once relevant edits are made to the source code (climatehealth package) in your local environment, simply run the above code again to update the functions.


#### Windows Binary (Pre-Compiled)

If you would like to install the latest official release of the package, the pre-compiled binary for windows can be installed [here](https://github.com/onssoschi/climatehealth/releases/latest).  
> [!TIP]
> Press on this file to begin download  
> <img width="203" height="36" alt="image" src="https://github.com/user-attachments/assets/94dd3fa7-aeeb-4a26-b468-b263af122151" />

By using this, you can avoid downloading the source code from the git repository. Using the latest release build also guarantees that the package is stable. This is the most efficient way to install the package on a windows machine, however you will not be able to make modifications to the source code.  

Once the zip has been downloaded, the following code snippet can be used in R to install the package:
```r
install.packages(path = "{path/to/climatehealth/binary/.zip}", repos = NULL, type = "win.binary")
```
The functions from the R package can then be used either by directly referencing the namespace (e.g., `climatehealth::heat_and_cold_do_analysis()`) or by loading all of the package functions at once (e.g., `library(climatehealth)`).


#### MacOS Source Tarball (Needs Compilation)

If you are using MacOS and would like to install the latest official release of the package, you must download the tarball (.tar.gz) containing the package source [here](https://github.com/onssoschi/climatehealth/releases/latest).  
> [!TIP]
> Press on this file to begin download  
> <img width="223" height="41" alt="image" src="https://github.com/user-attachments/assets/4f1ce1c4-95c6-4086-995a-3aa107babab7" />

This tarball contains the compressed package source that can be used to compile and install the package on a MacOS device. When installing the package using this method, the github repository does not need to be downloaded and the code is coming from a stable release.   

Once the .tar.gz has been downloaded, the following code snipped can be used in R to install the package:
```r
install.packages(path = "{path/to/climatehealth/source/.tar.gz}", repos = NULL, type = "source")
```
The functions from the R package can then be used either by directly referencing the namespace (e.g., `climatehealth::heat_and_cold_do_analysis()`) or by loading all of the package functions at once (e.g., `library(climatehealth)`).

## Integrated API and Runtime Assets
The package now includes the essential runtime assets that were previously held in
`climatehealth_pipelines`.

Included package paths:
- `inst/plumber/` for API routes, startup scripts, OpenAPI export, and throttling modules.
- `inst/extdata/config_templates/` for package-scoped runtime config templates.
- `inst/scripts/` for operational helpers.
- `tests/testthat/test_throttle_*.R` for throttle behavior tests.

Configuration best practice:
- Keep machine-specific input/output paths in user-local config copies.
- Use `inst/extdata/config_templates/developer_config.yml` and
  `inst/extdata/config_templates/api_config.yml` as templates.
- Avoid committing environment-specific paths into package code.

## Open-Source development
See [CONTRIBUTING](CONTRIBUTING.md) for guidance on contributing to this repository before making changes to support open-source development of this R package. This includes guidance on branching, merge requests, and code style. 

## Authors

The package authors reflect direct intellectual and technical contributions to the
package architecture, module methods, and reproducible analytical workflows.

- Lead author: Charlie Browning
- Lead maintainer and platform developer: Kenechi Omeke
- Authors: Etse Yawo Dzakpa, Gladin Jose, Matt Pearce, Ellie Watkins, Claire Hunt, Beatrice Byukusenge, Cassien Habyarimana, Venuste Nyagahakwa, Felix Scarbrough, Treesa Shaji, Bonnie Lewis and Ingole Vijendra 

## Contributors
The following contributors supported the package through bug fixes, minor features,
testing, documentation, and review work:

- Joe Panes

## Project and Workstream Lead
- Vijendra Ingole, Bonnie Lewis, and Myer Glickman

## Methodology review
- Sewe Maquines

## Acknowledgements

We acknowledge the Topic Expert Group members and Expert Advisory Group members for higher-level leadership, methodological input, and subject-area support to the SOSCHI climate-health work.
We also acknowledge the use of external data and code examples that informed testing and early prototyping.

## Copyright
This is an open access Rpackage published under a UK Open Government Licence 3.0 which is compatible with the Creative Commons CC-BY-SA 4.0 licence. You are free to copy, publish, distribute and adapt the information, provided you acknowledge the source and link to this R package (see citation below). Enquiries concerning the application of this R package can be made to the project team at climate.health@ons.gov.uk

## Citation

If you use `climatehealth` in publications, cite the package as:

Browning, C., Omeke, K., Dzakpa, E. Y., Jose, G., Pearce, M., Watkins, E., Hunt,
C., Byukusenge, B., Habyarimana, C., Nyagahakwa, V., Scarbrough, F., and
Shaji, T. (2026).
`climatehealth R package: Standards for Official Statistics on Climate-Health Interactions (SOSCHI)`.
R package.

An R package citation file is provided at `inst/CITATION`.

## Funding
![Static Badge](https://img.shields.io/badge/Wellcome_Trust-224682%2FZ%2F21%2FZ-%23ff24de)

This work was supported by Wellcome.
