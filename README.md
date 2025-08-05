![Static Badge](https://img.shields.io/badge/Status-In%20Development-darkgreen?logo=git&logoColor=white) ![Static Badge](https://img.shields.io/badge/Release%20Version-0.8.0-pink?logo=github) ![Static Badge](https://img.shields.io/badge/R_Version-4.4.1-pink?logo=R)  
[![Build R Package on Release](https://github.com/onssoschi/climatehealth/actions/workflows/r_release.yml/badge.svg)](https://github.com/onssoschi/climatehealth/actions/workflows/r_release.yml) [![R CMD Check](https://github.com/onssoschi/climatehealth/actions/workflows/r_cmd_check.yml/badge.svg)](https://github.com/onssoschi/climatehealth/actions/workflows/r_cmd_check.yml) 
![Static Badge](https://img.shields.io/badge/Wellcome_Trust-224682%2FZ%2F21%2FZ-%23ff24de)

# Climate and Health Package (Standards for Official Statistics on Climate-Health Interactions): climatehealth 

This project contains an R package for calculating climate-health indicators [beta]. The functions in this package support the production of climate-health statistics in the analysis pipelines in [climatehealth_pipelines](https://gitlab-app-l-01/hapi/climate-and-health-team/climatehealth_pipelines). The function code aligns with the methodologies set out in a statistical framework of climate-health indicators developed as part of our project _Standards for Official Statistics on Climate-Health Interactions (SOSCHI)_.

## Indicators

This repository currently contains analysis pipelines for the following topics in the SOSCHI framework:

- Heat- and cold-related mortality (ONS)
- Injury and mortality from extreme weather events: Wildfires (ONS)
- Mental health (ONS)

Coming soon:
- Malaria (AIMS/RIPS)
- Waterborne (AIMS)
- Malnutrition (RIPS)
- Flooding (AIMS)
- Air Pollution (AIMS)
- CSM (RIPS)

## Requirements

1. RStudio (or an equivalent way of running R scripts).

2. A git installation (**optional, recommended**).

## Layout of the climatehealth repository

### Top-level files
`climatehealth.Rproj`: The RStudio project file for this package.\
`README.md`: The markdown file that produces this documentation.\
`CONTRIBUTING.md`: A markdown document with guidance on contributing to this repository.\
`DESCRIPTION`: Contains package metadata.\
`NAMESPACE`: Specifies which functions are exported from this package. **Do not edit NAMESPACE manually.**\
`LICENSE`: Gives information on licensing.

### Key folders:
`R`: Contains R scripts with package functions\
`man`: Contains R documentation files for package functions.\
`tests`: Contains unit tests for package functions

### Other folders:
`.gitlab`: Contains a template for creating merge requests.\
`git_filter`: Contains [git-filter-repo](https://github.com/newren/git-filter-repo) - a tool for rewriting git history.

## Contributing
See [CONTRIBUTING](CONTRIBUTING.md) for guidance on contributing to this repository before making changes to the code. This includes guidance on branching, merge requests, and code style. 

## Usage

Steps to use package:

1. Clone the repository 

2. Use `devtools::load_all()` to load the package

To run unit tests:

  * Run `devtools::test()` in the console. Tests in `tests/testthat/`

## Data

#### Heat-related mortality data

The heat- and cold-related mortality indicator was tested using real data for England and Wales (`tests/testthat/testdata/regEngWalesPop.csv`). This has been modified from https://github.com/gasparrini/2015_gasparrini_Lancet_Rcodedata, with an additional dummy column of population estimates.

This dataset contains daily number of deaths and minimum, maximum, and mean temperature. The dataset is also disaggregated by [International Territorial Level (ITL 1, previously known as NUTS 1)](https://www.ons.gov.uk/methodology/geography/ukgeographies/eurostat) regions. The dataset includes data for nine England regions and Wales (i.e. a total of ten ITL 1 regions).

## Project status

* Integrating functions into online platform.
* Development of functions for other indicators

## Authors

Climate and Health Team, Health and International Partnerships, Office for National Statistics  

[Antony Brown, Charlie Browning, Layli Semple, Euan Soutter]


