library(dplyr)
df <- read.csv("tests/testthat/testdata/regEngWalesPop_og.csv")

df_new <- df %>%
  dplyr::mutate(regnames = recode(regnames, "N-East" = 'North East', "N-West" = 'North West', "S-West" = 'South West',
                                  "S-East" = 'South East', "E-Mid" = 'East Midlands', "W-Mid" = 'West Midlands',
                                  "York&Hum" = 'Yorkshire and The Humber', "East" = "East of England"))

write.csv(df_new, "tests/testthat/testdata/regEngWalesPop.csv")
