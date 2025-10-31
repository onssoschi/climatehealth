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
  group_by("year") %>%
  summarise(
    sum_total_deaths = mean(.data$average_attributable_fraction, na.rm = TRUE),
    lower_ci = mean(.data$lower_ci_attributable_fraction, na.rm = TRUE),
    upper_ci = mean(.data$upper_ci_attributable_fraction, na.rm = TRUE)
  )


#aggregated_data <- aggregated_data %>%
#  mutate(date = as.Date(paste(year, month, "01", sep = "-")))


plot_agg_an <- ggplot(aggregated_data, aes(x = "year", y = "sum_total_deaths")) +
    geom_ribbon(aes(ymin = "lower_ci", ymax = "upper_ci"), alpha = 0.2, fill = "#4d7789") +
    geom_line(color = "#003c57", size = 1) +
    theme_minimal(base_size = 14) +
    labs(title = "Deaths (per 100K) attributable to Wildfire smoke-related PM2.5",
         x = "Year",
         y = "Attributable Rate (per 100K)") +
    theme(axis.line = element_line(size = 0.5, colour = "black"))

print(plot_agg_an)

plot_aggregated_AN <- function(data, by_region = FALSE, output_dir = ".") {
  # input validation
  expected_cols <- c(
    "year",
    "average_attributable_fraction",
    "lower_ci_attributable_fraction",
    "upper_ci_attributable_fraction"
  )
  if (by_region==TRUE) expected_cols <- c(expected_cols, "regnames")
  if (!all(expected_cols %in% colnames(data))) {
    stop(
      "'data' must contain the following columns: ",
      paste(expected_cols, collapse = ", ")
    )
  }
  if (!file.exists(output_dir)) stop("'output_dir' does not exist.")
  # set up plot
  fpath <- file.path(output_dir, "aggregated_AN.pdf")
  pdf(fpath, width = 8, height = 8)
  # plot for full dataset
  p <- plot_aggregated_AN_core(data=data, region_name="All Regions")
  print(p)
  # plot for regions (conditional)
  if (by_region==TRUE) {
    for (region in unique(data$regnames)) {
      region_data <- data[data$regnames==region, ]
      p <- plot_aggregated_AN_core(data=region_data, region_name=region)
      print(p)
    }
  }
  dev.off()
}

plot_aggregated_AN_core <- function(data, region_name = NULL) {
  # aggregate AN/AR data
  agg_data <- data %>%
    group_by(.data$year) %>%
    summarise(
      sum_total_deaths = mean(.data$average_attributable_fraction, na.rm = TRUE),
      lower_ci = mean(.data$lower_ci_attributable_fraction, na.rm = TRUE),
      upper_ci = mean(.data$upper_ci_attributable_fraction, na.rm = TRUE)
    )
  # create output plot
  title <- "Deaths (per 100K) attributable to Wildfire smoke-related PM2.5"
  if (!is.null(region_name)) title <- paste0(title, " (", region_name, ")")
  plot_agg_an <- ggplot2::ggplot(
      agg_data,
      ggplot2::aes(x = year, y = sum_total_deaths)
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = lower_ci, ymax = upper_ci),
      alpha = 0.2,
      fill = "#4d7789"
    ) +
    ggplot2::geom_line(color = "#003c57", size = 1) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::labs(
      title = title,
      x = "Year",
      y = "Attributable Rate (per 100K)"
    ) +
    ggplot2::theme(
      axis.line = ggplot2::element_line(size = 0.5, colour = "black")
    )
  return(plot_agg_an)
}





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
