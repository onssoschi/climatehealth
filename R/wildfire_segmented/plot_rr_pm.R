library(ggplot2)
library(readr)


# Load your data
data <- read_csv("data/outputs/Temp(Original)/Correct version/rr_pm_all.csv", show_col_types = FALSE)

plot_rr_with_ci <- function(data,
                            title = "All-cause mortality",
                            x_label = "Wildfire-specific PM2.5 (μg/m3)",
                            y_label = "Relative Risk"){
  ggplot(data, aes(x = pm_levels, y = relative_risk)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2, fill = "#4d7789") +
    geom_line(color = "#003c57", size = 1) +
    theme_minimal(base_size = 14) +
    labs(title = title, x = x_label, y = y_label) +
    theme(axis.line = element_line(size = 0.5, colour = "black"))
}

p <- plot_rr_with_ci(data,
                     title = "All-cause mortality - England")

print(p)


ggplot(data, aes(x = pm_levels, y = relative_risk)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2, fill = "#4d7789") +
  geom_line(color = "#003c57", size = 1) +
  labs(x = "Wildfire-related PM2.5(ug/m3)", y = "Relative Risk (RR)")
  facet_wrap(~ region_name, ncol = 1, scales = "fixed") +
  scale_y_continuous(
      limits = c(1, 12),
      breaks = seq(1, 12, by = 1),
      labels = scales::number_format(accuracy = 1)
    ) +
    labs(title = title, x = x_label, y = y_label) +
    theme_minimal(base_size = 14) +
    theme(axis.line = element_line(size = 0.5, colour = "black"),
      plot.background = element_rect(color = "#222222", size = 1),  # black border around the whole plot
      panel.border = element_rect(color = "#222222", fill = NA, size = 0.5)  # black border around each panel
      #panel.background = element_rect(fill = NA, color = NA)  # change panel color here
    )


p <- plot_rr_with_ci(
  data,
  title = "Cardiovascular mortality - Nice"
)

############
library(ggplot2)
library(dplyr)
library(patchwork)
install.packages("patchwork")
# Define the plotting function
plot_rr_with_ci <- function(
  data,
  title = "All-cause mortality",
  x_label = "PM2.5 (ug/m3)",
  y_label = "Relative Risk"
) {
  ggplot(data, aes(x = pm_levels, y = relative_risk)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2, fill = "#4d7789") +
    geom_line(color = "#003c57", size = 1) +
    scale_y_continuous(
      limits = c(-1, 12),
      breaks = seq(-1, 12, by = 1),
      labels = scales::number_format(accuracy = 5)
    ) +
    labs(title = title, x = x_label, y = y_label) +
    theme_minimal(base_size = 14) +
    theme(
      axis.line = element_line(size = 0.5, colour = "black"),
      plot.background = element_rect(color = "#222222", size = 1),
      panel.border = element_rect(color = "#222222", fill = NA, size = 0.5)
    )
}

# Create separate plots for each region
plots <- data %>%
  split(.data$region_name) %>%
  lapply(function(df) plot_rr_with_ci(df, title = unique(df$region_name)))

# Combine all plots into one panel
combined_plot <- wrap_plots(plots, ncol = 1)

# Display the combined plot
print(combined_plot)

