# climatehealth package (Standards for Official Statistics on Climate-Health Interactions)

This project contains an R package for calculating climate-health indicators [beta]. The functions in this package support the production of climate-health statistics in the analysis pipelines in [climatehealth_pipelines](https://gitlab-app-l-01/hapi/climate-and-health-team/climatehealth_pipelines). The function code aligns with the methodologies set out in a statistical framework of climate-health indicators developed as part of our project Standards for Official Statistics on Climate-Health Interactions (SOSCHI).

## Layout of the climatehealth repository

### Top-level files
`climatehealth.Rproj`: The RStudio project file for this package.
`README.md`: The markdown file that produces this documentation.
`CONTRIBUTING.md`: A markdown document with guidance on contributing to this repository.\
`DESCRIPTION`: Contains package metadata.
`NAMESPACE`: Specifies which functions are exported from this package. **Do not edit NAMESPACE manually.**\
`LICENSE`: Gives information on licensing.

### Key folders:
`R`: Contains R scripts with package functions
`man`: Contains R documentation files for package functions. 
`tests`: Contains unit tests for package functions

### Other folders:
`.gitlab`: Contains a template for creating merge requests.\
`git_filter`: Contains [git-filter-repo](https://github.com/newren/git-filter-repo) - a tool for rewriting git history.


### Folder description

- `temperature_dlnm_main.R`: Runs distributed lag non-linear model (dlnm) to estimate the mortality risk attributable to non-optimal outdoor temperature (modified from Gasparrini et al. 2015)
- `R`:               Contains packaged R code for each the indicators
- `data`:            Folder for storing data
- `archive`:         Archived scripts
- `api`:             Contains scripts to run API
- `man`:             Automatically generated package documentation from roxygen2 comments (do not edit manually)
- `output`:          Stores outputs from scripts
- `tests`:           Unit tests and test data
- `data_template`:   Template of input data structure and column names
- `git_filter`:      Contains git-filter-repo file for filtering git history

---

## Usage

See [CONTRIBUTING](CONTRIBUTING.md) for guidance on contributing to this repository before making changes to the code.

Steps to calculate indicator:

1. Change `input_csv_path` field in `config.yaml` to your input dataset (daily mortality and temperature measurements per region). 

2. Change the column titles (under "Define columns" in `config.yml`) to reflect those in the data template `data_template/regEngWales_data_template.csv` (modified from Gasparrini et al. 2015). [beta solution; can be made generalisable in function & platform]

3. Run `temperature_dlnm_main.R` to run full analysis **OR** run `devtools:load_all()` to load climatehealth package and run individual functions (individual functions located in `R/`)

To run unit tests:

  * Run `devtools::test()` in the console. Tests in `tests/testthat/`. 

## Data

#### Heat-related mortality data

We are currently testing the indicator using real heat-related mortality data for England and Wales (``tests/testthat/testdata/regEngWalesPop.csv`). This has been modified from https://github.com/gasparrini/2015_gasparrini_Lancet_Rcodedata, with an additional column of population estimates.

This dataset contains daily number of deaths and minimum, maximum, and mean temperature, disaggregated by region (nine English regions).


## Project status

* Integrating functions into online platform.
* Development of functions for other indicators

---

## Authors

Climate and Health, Epidemiology and Global Health Analysis, Office for National Statistics  

[Euan Soutter, Antony Brown, Layli Semple, Vijendra Ingole]


