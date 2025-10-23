# Load required libraries
library(ggplot2)
library(dplyr)
library(readr)
library(gridExtra)
library(grid)

#install.packages("gridExtra")

# Load your data
data <- read_csv("data/outputs/Temp(Original)/Correct version/wildfire_an_ar.csv", show_col_types = FALSE)

# Aggregate data across all regions by year
# Change mean to sum to get attributable numbers

aggregated_data <- data %>%
  #filter(regnames == "London") %>%
  group_by(year) %>%
  summarise(
    sum_total_deaths = mean(average_attributable_fraction, na.rm = TRUE),
    lower_ci = mean(lower_ci_attributable_fraction, na.rm = TRUE),
    upper_ci = mean(upper_ci_attributable_fraction, na.rm = TRUE)
  )


#aggregated_data <- aggregated_data %>%
#  mutate(date = as.Date(paste(year, month, "01", sep = "-")))


plot_agg_an <- ggplot(aggregated_data, aes(x = year, y = sum_total_deaths)) +
    geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.2, fill = "#4d7789") +
    geom_line(color = "#003c57", size = 1) +
    theme_minimal(base_size = 14) +
    labs(title = "Deaths (per 100K) attributable to Wildfire smoke-related PM2.5 in London (2003-2021)",
         x = "Year",
         y = "Attributable Rate (per 100K)") +
    theme(axis.line = element_line(size = 0.5, colour = "black"))

print(plot_agg_an)








# Assuming 'aggregated_data' is your data frame with 'date' and 'sum_total_deaths'
#plot_hist <- ggplot(aggregated_data, aes(x = year, y = sum_total_deaths)) +
#  geom_col(fill = "#4d7789", color = "black") +
#  theme_minimal(base_size = 14) +
#  labs(
#    title = "Distribution of Deaths (per 100K) attributable to Wildfire smoke-related PM2.5 in England (2003–2021)",
#    x = "Year",
#    y = "Attributable Rate"
#  ) +
#  theme(axis.line = element_line(size = 0.5, colour = "black"))

# To display the plot
#print(plot_hist)
