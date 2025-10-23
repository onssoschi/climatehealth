# Load required libraries
library(ggplot2)
library(dplyr)
library(readr)
library(gridExtra)
library(grid)

#install.packages("gridExtra")

# Load your data
data <- read_csv("data/outputs/Temp(Original)/Correct version/an_ar_pm.csv", show_col_types = FALSE)

# Convert numeric month to abbreviated names
data$month_name <- month.abb[data$month]

#names(aggregated_data)
# Removes leading/trailing spaces
#names(data) <- trimws(names(data))

# Aggregate data across all regions by year
aggregated_data <- data %>%
  #filter(regnames == "London") %>%
  group_by(month_name) %>%
  summarise(
    mean_deaths_per_100k = mean(deaths_per_100k, na.rm = TRUE),
    mean_pm = mean(monthly_avg_pm25, na.rm = TRUE)
  )%>%
  mutate(month_name = factor(month_name, levels = month.abb))


# Calculate scaling factor
scale_factor <- max(aggregated_data$mean_deaths_per_100k) / max(aggregated_data$mean_pm)


# Plot
plot_ar_pm <- ggplot(aggregated_data, aes(x = month_name)) +
  geom_bar(aes(y = mean_deaths_per_100k), stat = "identity", fill = "#003c57", alpha = 0.7) +
  geom_line(aes(y = mean_pm * scale_factor, group = 1), color = "red", size = 1) +
  geom_point(aes(y = mean_pm * scale_factor), color = "red", size = 1) +
  scale_y_continuous(
    name = "Deaths per 100,000 population",
    sec.axis = sec_axis(~ . / scale_factor, name = "Mean PM2.5(µg/m³)")
  ) +
  labs(title = "Monthly Deaths and Mean PM2.5 Concentration - England (2003-2021)",
       x = "Month") +
  theme_light() +
  theme(axis.line = element_line(size = 0.5, colour = "black"))

print(plot_ar_pm)
