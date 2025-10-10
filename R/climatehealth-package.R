#' @title climatehealth: Measuring the climate-health relationship
#'
#' @description
#' 
#' ## Overview
#' This package provides a suite of analysis functions for measuring the relationship
#' between various climate factors (indicators) and health outcomes.
#'
#' ## Included Indicators
#' - Heat and Cold
#' - Wildfires
#' - Mental Health
#' - Air Pollution
#' - Waterborne Diseases
#' - Vectorborne Diseases
#'
#' ## License
#' MIT
#'
#' ## See Also
#' TBC
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


