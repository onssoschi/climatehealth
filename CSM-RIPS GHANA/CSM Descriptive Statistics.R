#' Perform Statistical Analysis on Dataset
#'
#' This function takes a dataset and performs various statistical analyses:
#' - Computes summary statistics for numerical variables
#' - Calculates standard deviation for numeric columns
#' - Generates frequency distributions for categorical variables
#' - Computes a correlation matrix for numeric variables
#' - Produces boxplots for numeric variables grouped by "CSM cases"
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
  
  # Clean column names: Remove special characters
  colnames(data) <- gsub("[^A-Za-z0-9_]", "_", colnames(data))  # Replace non-alphanumeric characters with underscores
  
  # Identify numeric and categorical columns
  numeric_cols <- data %>% select_if(is.numeric)
  categorical_cols <- data %>% select_if(is.character)
  
  # Ensure 'CSM_cases' exists in the dataset
  if (!"CSM_cases" %in% colnames(data)) {
    stop("Error: Column 'CSM cases' not found. Ensure the correct column name is used.")
  }
  
  # Summary statistics for numeric variables
  if (ncol(numeric_cols) > 0) {
    print("### Summary Statistics for Numeric Variables:")
    print(summary(numeric_cols))
  } else {
    print("No numeric variables found.")
  }
  
  # Standard deviation for numeric variables
  if (ncol(numeric_cols) > 0) {
    print("### Standard Deviation for Numeric Variables:")
    print(sapply(numeric_cols, sd, na.rm = TRUE))
  }
  
  # Frequency distribution for categorical variables
  if (ncol(categorical_cols) > 0) {
    print("### Frequency Distribution for Categorical Variables:")
    for (col in colnames(categorical_cols)) {
      print(paste("#### Frequency for:", col))
      print(table(categorical_cols[[col]], useNA = "ifany"))
    }
  } else {
    print("No categorical variables found.")
  }
  
  # Correlation matrix for numeric variables excluding the target variable
  if (ncol(numeric_cols) > 1) {
    print("### Correlation Matrix (Excluding 'CSM cases'):")
    numeric_no_target <- numeric_cols %>% select(-CSM_cases)
    print(cor(numeric_no_target, use = "complete.obs"))
  } else {
    print("Not enough numeric variables for correlation analysis.")
  }
  
  # Boxplots for numeric variables grouped by CSM cases
  print("### Generating Boxplots for Numeric Variables by 'CSM cases'...")
  for (col in colnames(numeric_cols)) {
    if (col != "CSM_cases") { # Exclude target variable from itself
      p <- ggplot(data, aes_string(x = "CSM_cases", y = col)) +
        geom_boxplot(fill = "skyblue", alpha = 0.6) +
        labs(title = paste("Boxplot of", col, "by CSM Cases"), x = "CSM Cases", y = col) +
        theme_minimal()
      print(p)
    }
  }
  
  # Bar plots for categorical variables
  if (ncol(categorical_cols) > 0) {
    print("### Generating Bar Plots for Categorical Variables...")
    for (col in colnames(categorical_cols)) {
      p <- ggplot(data, aes_string(x = col)) +
        geom_bar(fill = "blue", alpha = 0.7) +
        labs(title = paste("Bar Plot of", col), x = col, y = "Count") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        theme_minimal()
      print(p)
    }
  }
  
  # Return key results as a list
  return(list(
    summary_stats = if (ncol(numeric_cols) > 0) summary(numeric_cols) else NULL,
    std_deviation = if (ncol(numeric_cols) > 0) sapply(numeric_cols, sd, na.rm = TRUE) else NULL,
    correlation_matrix = if (ncol(numeric_cols) > 1) cor(numeric_no_target, use = "complete.obs") else NULL
  ))
}

# Load dataset and perform analysis
df <- load_stunted_data("C:/Users/kobby/Downloads/prof Eric R/CSM-Peter/CSM-Peter.xlsx") 
stats <- perform_stat_analysis(df)
