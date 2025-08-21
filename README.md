![Static Badge](https://img.shields.io/badge/Status-In%20Development-darkgreen?logo=git&logoColor=white) ![Static Badge](https://img.shields.io/badge/Release%20Version-0.8.0-pink?logo=github) ![Static Badge](https://img.shields.io/badge/R_Version-4.4.1-pink?logo=R)  
[![Build R Package on Release](https://github.com/onssoschi/climatehealth/actions/workflows/r_release.yml/badge.svg)](https://github.com/onssoschi/climatehealth/actions/workflows/r_release.yml) [![R CMD Check](https://github.com/onssoschi/climatehealth/actions/workflows/r_cmd_check.yml/badge.svg)](https://github.com/onssoschi/climatehealth/actions/workflows/r_cmd_check.yml)  
![Static Badge](https://img.shields.io/badge/Wellcome_Trust-224682%2FZ%2F21%2FZ-%23ff24de)

# Climate and Health Package (Standards for Official Statistics on Climate-Health Interactions): climatehealth 


> [!CAUTION]
> This package is still in development and methods are actively changing.

## Table of Contents

## Package Overview

### Description

This repository contains code for an R package dedicated to calculating climate-health indicators [beta]. The functions in this package support the production of climate-health statistics.. The code aligns with the methodologies set out in a statistical framework of climate-health indicators developed as part of our project Standards for Official Statistics on Climate-Health Interactions (SOSCHI).

### Indicators

This package contains scientific functions dedicated to the following topics in the SOSCHI framework:

| **Indicator**                                                       | **Topic Owner** | **Documents**                                      |
|--------------------------------------------------------------------|-----------------|----------------------------------------------------|
| Heat- and cold-related mortality                                   | ONS             | [Methodology Document](https://www.google.com)     |
| Injury and mortality from extreme weather events: Wildfires        | ONS             | [Methodology Document](https://www.google.com)     |
| Mental health                                                      | ONS             | [Methodology Document](https://www.google.com)     |
| Waterborne                                                         | AIMS            | [Methodology Document](https://www.google.com)     |
| Air Pollution                                                      | AIMS            | [Methodology Document](https://www.google.com)     |
| Malaria                                                            | RIPS/AIMS       | [Methodology Document](https://www.google.com)     |


## Package Installation

### Requirements

To install the package and utilise its functionality, an R build is required.  
This package was largely developed on **R-4.4.1**, therefore it is recommended. **R-4.4.1** can be downloaded [here](https://cran.r-project.org/bin/windows/base/old/4.4.1/)

### Installation Methods

Installing the _climatehealth_ R package in your R environment can be done in various different ways. Your installation method can vary on your requirements and preference.  

#### Devtools

#### Windows Binary (Pre-Compiled)

#### MacOS Source Tarball (Needs Compilation)


## Example Datasets

### Heat-related mortality data

The heat- and cold-related mortality indicator was tested using real data for England and Wales (`tests/testthat/testdata/regEngWalesPop.csv`). This has been modified from https://github.com/gasparrini/2015_gasparrini_Lancet_Rcodedata, with an additional dummy column of population estimates.

This dataset contains daily number of deaths and minimum, maximum, and mean temperature. The dataset is also disaggregated by [International Territorial Level (ITL 1, previously known as NUTS 1)](https://www.ons.gov.uk/methodology/geography/ukgeographies/eurostat) regions. The dataset includes data for nine England regions and Wales (i.e. a total of ten ITL 1 regions).

## Contributing
See [CONTRIBUTING](CONTRIBUTING.md) for guidance on contributing to this repository before making changes to the code. This includes guidance on branching, merge requests, and code style. 

## Authors

Climate and Health Team, Health and International Partnerships, Office for National Statistics  

[Charlie Browning]

## Contributors

[]


