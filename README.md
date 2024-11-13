# Climate and Health Package (Standards for Official Statistics on Climate-Health Interactions): climatehealth 

This project contains an R package for calculating climate-health indicators [beta]. The functions in this package support the production of climate-health statistics in the analysis pipelines in [climatehealth_pipelines](https://gitlab-app-l-01/hapi/climate-and-health-team/climatehealth_pipelines). The function code aligns with the methodologies set out in a statistical framework of climate-health indicators developed as part of our project _Standards for Official Statistics on Climate-Health Interactions (SOSCHI)_.

## Indicators

This repository currently contains R functions for the following topics in the SOSCHI framework:

- Heat- and cold-related mortality (ONS)
- Injury and mortality from extreme weather events: Wildfires (ONS)

Coming soon:
- Mental health (ONS)

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

This dataset contains daily number of deaths and minimum, maximum, and mean temperature, disaggregated by region (nine English regions).

## Project status

* Integrating functions into online platform.
* Development of functions for other indicators

## Authors

Climate and Health Team, Health and International Partnerships, Office for National Statistics  

[Antony Brown, Charlie Browning, Layli Semple, Euan Soutter]


