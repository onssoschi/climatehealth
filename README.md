![Static Badge](https://img.shields.io/badge/Status-In%20Development-darkgreen?logo=git&logoColor=white) ![Static Badge](https://img.shields.io/badge/Release%20Version-0.9.1-pink?logo=github) ![Static Badge](https://img.shields.io/badge/R_Version-4.4.1-pink?logo=R) [![codecov](https://codecov.io/gh/onssoschi/climatehealth/graph/badge.svg?token=YKRAUQ3JAU)](https://codecov.io/gh/onssoschi/climatehealth)  
[![Build R Package on Release](https://github.com/onssoschi/climatehealth/actions/workflows/r_release.yml/badge.svg)](https://github.com/onssoschi/climatehealth/actions/workflows/r_release.yml) [![R CMD Check](https://github.com/onssoschi/climatehealth/actions/workflows/r_cmd_check.yml/badge.svg)](https://github.com/onssoschi/climatehealth/actions/workflows/r_cmd_check.yml)  

# Climatehealth Package

> [!CAUTION]
> This package is still in development and methods are actively changing.

## Table of Contents
- [📦 Package Overview](#package-overview)  
- [🛠️ Package Installation](#package-installation)  
- [📊 Example Datasets](#example-datasets)  
- [🙌 Contributing](#contributing)  
- [👥 Authors](#authors)  
- [🤝 Contributors](#contributors)  
- [💰 Funding](#funding)  

## Package Overview

### Description

This repository contains code for an R package dedicated to calculating climate-health indicators [beta]. The functions in this package support the production of climate-health statistics. The code aligns with the methodologies set out in a statistical framework of climate-health indicators developed as part of our project Standards for Official Statistics on Climate-Health Interactions (SOSCHI).

### Indicators

This package contains scientific functions dedicated to the following topics in the SOSCHI framework:

| **Indicator**                                                       | **Topic Owner** | **Documents**                                                      |
|--------------------------------------------------------------------|-----------------|---------------------------------------------------------------------|
| Heat- and cold-related mortality                                   | ONS             | [Methodology Document](https://doi.org/10.5281/zenodo.14865904)     |
| Injury and mortality from extreme weather events: Wildfires        | ONS             | [Methodology Document](https://doi.org/10.5281/zenodo.14052183)     |
| Mental health                                                      | ONS             | [Methodology Document](https://doi.org/10.5281/zenodo.14050224)     |
| Waterborne                                                         | AIMS            | [Methodology Document](https://doi.org/10.5281/zenodo.14871506)     |
| Air Pollution                                                      | AIMS            | [Methodology Document](https://doi.org/10.5281/zenodo.14871506)     |
| Malaria                                                            | RIPS/AIMS       | [Methodology Document](https://doi.org/10.5281/zenodo.14871506)     |

For additional information on each topic/indicator, see our official website [here](https://climate-health.officialstatistics.org/framework)

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


## Example Datasets

### Heat-related mortality data

The heat- and cold-related mortality indicator was tested using real data for England and Wales (`inst/testdata/temperature_test_data.csv`). This has been modified from https://github.com/gasparrini/2015_gasparrini_Lancet_Rcodedata, with an additional dummy column of population estimates.

This dataset contains daily number of deaths and minimum, maximum, and mean temperature. The dataset is also disaggregated by [International Territorial Level (ITL 1, previously known as NUTS 1)](https://www.ons.gov.uk/methodology/geography/ukgeographies/eurostat) regions. The dataset includes data for nine England regions and Wales (i.e. a total of ten ITL 1 regions).

## Contributing
See [CONTRIBUTING](CONTRIBUTING.md) for guidance on contributing to this repository before making changes to the code. This includes guidance on branching, merge requests, and code style. 

## Authors

Climate and Health Team, Health and International Partnerships, Office for National Statistics  

[Charlie Browning]

## Contributors

Coming soon...

## Funding
![Static Badge](https://img.shields.io/badge/Wellcome_Trust-224682%2FZ%2F21%2FZ-%23ff24de)


