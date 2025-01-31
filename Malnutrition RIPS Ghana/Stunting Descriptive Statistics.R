#' Perform Statistical Analysis on Dataset
#'
#' This function takes a dataset and performs various statistical analyses:
#' - Computes summary statistics for numerical variables
#' - Calculates standard deviation for numeric columns
#' - Generates frequency distributions for categorical variables
#' - Computes a correlation matrix for numeric variables
#' - Produces boxplots for numeric variables grouped by "child_Stunted"
#' - Creates bar charts for categorical variables
#'
#' @param data A dataframe containing the dataset
#' @return A list containing summary statistics, standard deviations, and correlation matrix
#' @import dplyr ggplot2
#' @export

perform_stat_analysis <- function(data) {
  # Load necessary libraries
  library(dplyr)   # For data manipulation
  library(ggplot2) # For visualizations
  
  # Check if the data is loaded
  if (is.null(data)) {
    stop("Error: No dataset provided. Please load the dataset first.")
  }
  
  # Replace spaces with underscores in column names to prevent ggplot2 errors
  colnames(data) <- gsub(" ", "_", colnames(data))
  
  # Identify numeric and categorical columns
  numeric_cols <- select_if(data, is.numeric)
  categorical_cols <- select_if(data, is.character)
  
  # Summary statistics for numeric variables
  print("### Summary Statistics for Numeric Variables:")
  print(summary(numeric_cols))
  
  # Standard deviation for numeric variables
  print("### Standard Deviation for Numeric Variables:")
  print(sapply(numeric_cols, sd, na.rm = TRUE))
  
  # Frequency distribution for categorical variables
  print("### Frequency Distribution for Categorical Variables:")
  for (col in colnames(categorical_cols)) {
    print(paste("#### Frequency for:", col))
    print(table(categorical_cols[[col]], useNA = "ifany"))
  }
  
  # Correlation matrix for numeric variables
  if (ncol(numeric_cols) > 1) {
    print("### Correlation Matrix:")
    print(cor(numeric_cols, use = "complete.obs"))
  } else {
    print("Not enough numeric variables for correlation analysis.")
  }
  
  # Visualizations
  print("### Generating Visualizations...")
  
  # Boxplot for numeric variables
  for (col in colnames(numeric_cols)) {
    p <- ggplot(data, aes_string(x = "child_Stunted", y = col)) +
      geom_boxplot(fill = "skyblue", alpha = 0.6) +
      labs(title = paste("Boxplot of", col, "by Stunted Status"), x = "Stunted Status", y = col) +
      theme_minimal()
    print(p)
  }
  
  # Bar plots for categorical variables
  for (col in colnames(categorical_cols)) {
    p <- ggplot(data, aes_string(x = col)) +
      geom_bar(fill = "blue", alpha = 0.7) +
      labs(title = paste("Bar Plot of", col), x = col, y = "Count") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme_minimal()
    print(p)
  }
  
  # Return key results as a list
  return(list(
    summary_stats = summary(numeric_cols),
    std_deviation = sapply(numeric_cols, sd, na.rm = TRUE),
    correlation_matrix = if (ncol(numeric_cols) > 1) cor(numeric_cols, use = "complete.obs") else NULL
  ))
}

df <- read_csv("C:/Users/kobby/Downloads/prof Eric R/Peter-Stunnting/Stunted_FINAL_Imputed_Scaled.csv")
stats <- perform_stat_analysis(df)
