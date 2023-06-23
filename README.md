# indicatorfunctions

---

## Description

A package for calculating climate-health indicators [beta].

First indicator is modified from Gasparrini et al. (2015).

Gasparrini A, Guo Y, Hashizume M, Lavigne E, Zanobetti A, Schwartz J, Tobias A, Tong S, Rocklöv J, Forsberg B, Leone M, De Sario M, Bell ML, Guo YLL, Wu CF, Kan H, Yi SM, de Sousa Zanotti Stagliorio Coelho M, Saldiva PH, Honda Y, Kim H, Armstrong B. Mortality risk attributable to high and low ambient temperature: a multicountry observational study. The Lancet. 2015;386(9991):369-375

### Folder description

- `gasparrini_main.R`: Runs distributed lag non-linear model (dlnm) to estimate the excess risk attributable to non-optimal outdoor temperature (modified from Gasparrini et al. 2015)
- `R`:               Contains packaged R code for each the indicators
- `data`:            Contains data for calculating indicators
- `archive`:         Archived scripts
- `man`:             Automatically generated package documentation from roxygen2 comments (do not edit manually)
- `output`:          Outputs from scripts
- `tests`:  Unit tests

---

## Usage

Steps to calculate indicator:

1. Change `input_csv_path` field in `config.yaml` to your input dataset (daily mortality and temperature measurements per region).

2. Change `output_folder_path` field in `config.yaml` to the folder you want to store outputs.

3. Run `devtools:load_all()` to load indicatorfunctions package

3. Run `gasparrini_main.R` to run full analysis

## Data

#### Heat-related mortality data

We are currently testing the indicator using real heat-related mortality data for England and Wales (`data/regEngWales.csv`). The raw data is being used by our team to develop an indicator of heat-related mortality.

This dataset contains daily number of deaths and minimum, maximum, and mean temperature, disaggregated by region (nine English regions).


## Project status

* Refining indicator calculation and usability

---

## Authors

Climate and Health, Epidemiology and Global Health Analysis, Office for National Statistics  

[Euan Soutter, Antony Brown, Cerys Hopkins, Paul Slocombe, Vijendra Ingole]


