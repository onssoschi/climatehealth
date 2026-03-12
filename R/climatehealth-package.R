#' @title climatehealth: Measuring the climate-health relationship
#'
#' @description
#'
#' ## Overview
#' This package provides a suite of analysis functions for measuring the relationship
#' between various climate factors (indicators) and health outcomes.
#'
#' ## Included Indicators
#' - Mortality attributable to high and low outdoor temperatures
#' - Mortality attributable to wildfire-related PM2.5
#' - Suicides attributable to extreme heat
#' - Mortality attributable to short-term exposure to outdoor PM2.5 exposure
#' - Diarrhea cases attributable to extreme temperatures and rainfall
#' - Malaria cases attributable to extreme temperatures and  rainfall
#'
#' ## License
#' MIT
#'
#' ## The full range of topics include
#' - Temperature-related health effects
#' - Health effects of wildfires
#' - Mental Health
#' - Health effects of air pollution
#' - Water-borne diseases
#' - Vector-borne diseases
#'
#' @name climatehealth-package
"_PACKAGE"

# Core graphics functions for plotting
#' @importFrom graphics
#'   barplot lines mtext par
#'   abline axis boxplot filled.contour hist layout legend
#'   plot.new polygon rect text

# Device and color utilities
#' @importFrom grDevices
#'   dev.off pdf
#'   adjustcolor col2rgb colorRampPalette grey png rgb

# Statistical modeling and data utilities
#' @importFrom stats
#'   aggregate as.formula coef median update vcov
#'   approx ar complete.cases cor dpois fitted formula
#'   glm lm pchisq predict qqline qqnorm quantile quasipoisson
#'   resid residuals rnorm sd setNames var

# File I/O and utility functions
#' @importFrom utils
#'   install.packages read.csv write.csv
#'   capture.output combn packageVersion

# Non-standard evaluation support
#' @importFrom data.table :=
## usethis namespace: start
## usethis namespace: end
NULL
