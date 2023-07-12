# climatehealth

---

## Description

A package for calculating climate-health indicators [beta].

First indicator is modified from Gasparrini et al. (2015).

Gasparrini A, Guo Y, Hashizume M, Lavigne E, Zanobetti A, Schwartz J, Tobias A, Tong S, Rocklöv J, Forsberg B, Leone M, De Sario M, Bell ML, Guo YLL, Wu CF, Kan H, Yi SM, de Sousa Zanotti Stagliorio Coelho M, Saldiva PH, Honda Y, Kim H, Armstrong B. Mortality risk attributable to high and low ambient temperature: a multicountry observational study. The Lancet. 2015;386(9991):369-375

https://github.com/gasparrini/2015_gasparrini_Lancet_Rcodedata

### Folder description

- `temp_dlnm_main.R`: Runs distributed lag non-linear model (dlnm) to estimate the excess risk attributable to non-optimal outdoor temperature (modified from Gasparrini et al. 2015)
- `R`:               Contains packaged R code for each the indicators
- `data`:            Folder for storing data
- `archive`:         Archived scripts
- `man`:             Automatically generated package documentation from roxygen2 comments (do not edit manually)
- `output`:          Stores outputs from scripts
- `tests`:           Unit tests and test data
- `data_template`:   Template of input data structure and column names

---

## Usage

Steps to calculate indicator:

1. Change `input_csv_path` field in `config.yaml` to your input dataset (daily mortality and temperature measurements per region). 

2. Change the column titles to reflect those in the data template `data_template/regEngWales_data_template.csv` (modified from Gasparrini et al. 2015). [beta solution; can be made generalisable in function & platform]

3. Change `output_folder_path` field in `config.yaml` to the folder you want to store outputs.

4. Run `temp_dlnm_main.R` to run full analysis **OR** run `devtools:load_all()` to load indicatorfunctions package and run individual functions (individual functions located in `R/`)

To run unit tests:

  * Run `devtools::test()` in the console. Tests in `tests/testthat/`. Test data in `tests/testthat/testdata/regEngWales.csv` (from https://github.com/gasparrini/2015_gasparrini_Lancet_Rcodedata).

## Data

#### Heat-related mortality data

We are currently testing the indicator using real heat-related mortality data for England and Wales (``tests/testthat/testdata/regEngWales.csv`). The raw data is being used by our team to develop an indicator of heat-related mortality and is taken from https://github.com/gasparrini/2015_gasparrini_Lancet_Rcodedata.

This dataset contains daily number of deaths and minimum, maximum, and mean temperature, disaggregated by region (nine English regions).


## Project status

* Refining usability and developing new indicator functions

---

## Authors

Climate and Health, Epidemiology and Global Health Analysis, Office for National Statistics  

[Euan Soutter, Antony Brown, Cerys Hopkins, Paul Slocombe, Vijendra Ingole]


