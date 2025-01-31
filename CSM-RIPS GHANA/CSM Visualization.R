# ==============================================
# ============ Load Necessary Libraries =========
# ==============================================
library(ggplot2)      # General visualization
library(dplyr)        # Data manipulation
library(ggcorrplot)   # Correlation matrix visualization
library(GGally)       # Scatter plot matrix (multivariate analysis)
library(qcc)          # Pareto Charts
library(readxl)       # For reading Excel files

# =====================================================
# ============== LOAD DATASET ========================
# =====================================================
df <- read_excel("C:/Users/kobby/Downloads/prof Eric R/CSM-Peter/CSM-Peter.xlsx")

# Ensure column names are formatted correctly
colnames(df) <- gsub(" ", "_", colnames(df))

# Convert categorical variables to factors
df$CSM_cases <- as.factor(df$CSM_cases)
df$Region <- as.factor(df$Region)
df$Climatic_Zone <- as.factor(df$Climatic_Zone)

# =====================================================
# ============== UNIVARIATE ANALYSIS ==================
# =====================================================

# 1. Bar Plot for Categorical Variables
univariate_categorical_plot <- function(data, column) {
  ggplot(data, aes_string(x = column)) +
    geom_bar(fill = "blue", alpha = 0.7) +
    labs(title = paste("Distribution of", column), x = column, y = "Count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme_minimal()
}

# 2. Histogram & Boxplot for Numerical Variables
univariate_numerical_plot <- function(data, column) {
  p1 <- ggplot(data, aes_string(x = column)) +
    geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
    labs(title = paste("Histogram of", column), x = column, y = "Count") +
    theme_minimal()
  
  p2 <- ggplot(data, aes_string(y = column)) +
    geom_boxplot(fill = "red", alpha = 0.7) +
    labs(title = paste("Boxplot of", column), y = column) +
    theme_minimal()
  
  print(p1)
  print(p2)
}

# 3. Kernel Density Plot (Smoothed Histogram)
univariate_density_plot <- function(data, column) {
  ggplot(data, aes_string(x = column)) +
    geom_density(fill = "blue", alpha = 0.5) +
    labs(title = paste("Density Plot of", column), x = column, y = "Density") +
    theme_minimal()
}

# =====================================================
# ============== BIVARIATE ANALYSIS ==================
# =====================================================

# 4. Stacked Bar Chart (Categorical vs. CSM cases)
bivariate_categorical_plot <- function(data, column, target = "CSM_cases") {
  ggplot(data, aes_string(x = column, fill = target)) +
    geom_bar(position = "dodge") +
    labs(title = paste("Bivariate Analysis of", column, "vs.", target), x = column, y = "Count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme_minimal()
}

# 5. Boxplot for Numerical vs. CSM cases
bivariate_numerical_plot <- function(data, column, target = "CSM_cases") {
  ggplot(data, aes_string(x = target, y = column, fill = target)) +
    geom_boxplot(alpha = 0.7) +
    labs(title = paste("Boxplot of", column, "by", target), x = target, y = column) +
    theme_minimal()
}

# 6. Scatter Plot with Regression Line (Numeric vs. Numeric)
bivariate_scatter_regression <- function(data, x_col, y_col) {
  ggplot(data, aes_string(x = x_col, y = y_col)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", col = "red") +
    labs(title = paste("Scatter Plot of", x_col, "vs.", y_col), x = x_col, y = y_col) +
    theme_minimal()
}

# =====================================================
# ============== MULTIVARIATE ANALYSIS ================
# =====================================================

# 7. Correlation Matrix Plot (Numeric Variables)
multivariate_correlation_plot <- function(data) {
  numeric_cols <- select_if(data, is.numeric)
  corr_matrix <- cor(numeric_cols, use = "complete.obs")
  ggcorrplot(corr_matrix, method = "circle", lab = TRUE)
}

# 8. Scatter Plot Matrix (Pairwise Variable Relationships)
multivariate_scatter_plot <- function(data, target = "CSM_cases") {
  numeric_cols <- select_if(data, is.numeric)
  data$target_factor <- as.factor(data[[target]])
  ggpairs(data, columns = colnames(numeric_cols), aes(color = target_factor))
}

# =====================================================
# ============== FUNCTION USAGE EXAMPLES =============
# =====================================================

# Example Usage
univariate_categorical_plot(df, "Region")
univariate_numerical_plot(df, "Average_Temperature")
bivariate_categorical_plot(df, "Climatic_Zone", "CSM_cases")
bivariate_numerical_plot(df, "Average_Temperature", "CSM_cases")
bivariate_scatter_regression(df, "Average_Temperature", "CSM_cases")
multivariate_correlation_plot(df)
multivariate_scatter_plot(df, "CSM_cases")
