


library(dplyr)
library(lubridate)

#list your csv files
data <- read_csv("data/inputs/France/user_testing/all_regions.csv")
pred_data <- read.csv("data/outputs/wildfire_an_ar.csv")


data <- data %>%
  mutate(date = as.Date(date, format = "%d/%m/%Y"))


# Aggregate to monthly average PM2.5 with separate year and month columns
monthly_pm25 <- data %>%
  mutate(year = year(date),
         month = month(date)) %>%
  group_by(regnames, year, month) %>%
  summarise(monthly_avg_pm25 = mean(mean_PM_FRP, na.rm = TRUE), .groups = "drop")

# Check result
# head(monthly_pm25)

#Merge on both id and date
merged_data <- left_join(pred_data, monthly_pm25, by = c("year", "month", "regnames"))

# Specify the path where you want to save the CSV file
save_path <- "data/outputs/an_ar_pm.csv"

# Save the data to CSV
write.csv(merged_data, file = save_path, row.names = FALSE)
