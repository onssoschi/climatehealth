


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

# Charlie Functions
join_ar_and_pm_monthly <- function(
  pm_data,
  an_ar_data
) {
  exp_cols_ar <- c("year", "month", "regnames")
  exp_cols_pm <- c("year", "month", "regnames", "mean_PM_FRP")
  if (!all(exp_cols_ar %in% an_ar_data)) {
    stop(paste0("an_ar_data needs the columns: ",
        paste(exp_cols_ar, collapse=", ")))
  }
  if (!all(exp_cols_pm %in% pm_data)) {
    stop(paste0("an_ar_data needs the columns: ",
        paste(exp_cols_pm, collapse=", ")))
  }
  monthly_pm25 <- data %>%
    group_by(.data$regnames, .data$year, .data$month) %>%
    summarise(
      monthly_avg_pm25 = mean(.data$mean_PM_FRP, na.rm = TRUE),
      .groups = "drop"
    )
  joined_data <- left_join(
    pred_data,
    monthly_pm25,
    by = c("year", "month", "regnames")
  )
  return(joined_data)
}

plot_ar_pm_monthly <- function(data, save_outputs = FALSE, output_dir = NULL) {
  # validate inputs
  if (save_outputs==TRUE && is.null(output_dir)) {
    stop("'output_dir' must be provded to save outputs.")
  }
  if (!file.exists(output_dir)) {
    stop("'output_dir' must exist on disk to save outputs.")
  }
  # Aggregate data across all regions by year
  aggregated_data <- data %>%
    group_by(month_name) %>%
    summarise(
      mean_deaths_per_100k = mean(deaths_per_100k, na.rm = TRUE),
      mean_pm = mean(monthly_avg_pm25, na.rm = TRUE)
    ) %>%
    mutate(month_name = factor(month_name, levels = month.abb))
  # Calculate scaling factor
  scale_factor <- max(aggregated_data$mean_deaths_per_100k) / max(aggregated_data$mean_pm)
  # Plot results
  fpath <- file.path(output_dir, "ar_and_pm_monthly_average")
  if (save_fig) {
    png(paste0(fpath, ".png"))
  }
  plot_ar_pm <- ggplot2::ggplot(
      aggregated_data,
      ggplot2::aes(x = month_name)
    ) +
    ggplot2::geom_bar(
      ggplot2::aes(y = mean_deaths_per_100k),
      stat = "identity",
      fill = "#003c57",
      alpha = 0.7
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = mean_pm * scale_factor, group = 1),
      color = "red", 
      size = 1
    ) +
    ggplot2::geom_point(
      ggplot2::aes(y = mean_pm * scale_factor), color = "red", size = 1) +
    ggplot2::scale_y_continuous(
      name = "Deaths per 100,000 population",
      sec.axis = ggplot2::sec_axis(
        ~ . / scale_factor, name = "Mean PM2.5(µg/m³)"
      )
    ) +
    ggplot2::labs(
      title = "Monthly Deaths and Mean PM2.5 Concentration - England (2003-2021)",
      x = "Month"
    ) +
    ggplot2::theme_light() +
    ggplot2::theme(
      axis.line = ggplot2::element_line(size = 0.5, colour = "black")
    )
  print(plot_ar_pm)
  if (save_outputs) {
    dev.off()
    write.csv(aggregated_data, paste0(fpath, ".csv"))
  }
}