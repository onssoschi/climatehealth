# ==============================================
# ============ Load Necessary Libraries =========
# ==============================================
library(ggplot2)      # General visualization
library(dplyr)        # Data manipulation
library(ggcorrplot)   # Correlation matrix visualization
library(GGally)       # Scatter plot matrix (multivariate analysis)
library(qcc)          # Pareto Charts

# =====================================================
# ============== UNIVARIATE ANALYSIS ==================
# =====================================================

# 📌 **1. Bar Plot for Categorical Variables**
univariate_categorical_plot <- function(data, column) {
  ggplot(data, aes_string(x = column)) +
    geom_bar(fill = "blue", alpha = 0.7) +
    labs(title = paste("Distribution of", column), x = column, y = "Count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme_minimal()
}

# 📌 **2. Histogram & Boxplot for Numerical Variables**
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

# 📌 **3. Kernel Density Plot (Smoothed Histogram)**
univariate_density_plot <- function(data, column) {
  ggplot(data, aes_string(x = column)) +
    geom_density(fill = "blue", alpha = 0.5) +
    labs(title = paste("Density Plot of", column), x = column, y = "Density") +
    theme_minimal()
}

# 📌 **4. Pareto Chart (Identify Most Frequent Categories)**
univariate_pareto_chart <- function(data, column) {
  freq_table <- as.data.frame(table(data[[column]]))
  names(freq_table) <- c("Category", "Frequency")
  qcc::pareto.chart(freq_table$Frequency, names = freq_table$Category, col = "lightblue", main = paste("Pareto Chart of", column))
}

# =====================================================
# ============== BIVARIATE ANALYSIS ==================
# =====================================================

# 📌 **5. Stacked Bar Chart (Categorical vs. Target)**
bivariate_categorical_plot <- function(data, column, target) {
  ggplot(data, aes_string(x = column, fill = target)) +
    geom_bar(position = "dodge") +
    labs(title = paste("Bivariate Analysis of", column, "vs.", target), x = column, y = "Count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme_minimal()
}

# 📌 **6. Boxplot for Numerical vs. Target**
bivariate_numerical_plot <- function(data, column, target) {
  ggplot(data, aes_string(x = target, y = column, fill = target)) +
    geom_boxplot(alpha = 0.7) +
    labs(title = paste("Boxplot of", column, "by", target), x = target, y = column) +
    theme_minimal()
}

# 📌 **7. Scatter Plot with Regression Line (Numeric vs. Numeric)**
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

# 📌 **8. Correlation Matrix Plot (Numeric Variables)**
multivariate_correlation_plot <- function(data) {
  numeric_cols <- select_if(data, is.numeric)
  corr_matrix <- cor(numeric_cols, use = "complete.obs")
  ggcorrplot(corr_matrix, method = "circle", lab = TRUE)
}

# 📌 **9. Scatter Plot Matrix (Pairwise Variable Relationships)**
multivariate_scatter_plot <- function(data, target) {
  numeric_cols <- select_if(data, is.numeric)
  data$target_factor <- as.factor(data[[target]])  # Convert target to factor for coloring
  ggpairs(data, columns = colnames(numeric_cols), aes(color = target_factor))
}

# =====================================================
# ============== FUNCTION USAGE EXAMPLES =============
# =====================================================

# 📌 **Load dataset**
df <- read_csv("C:/Users/23350/Documents/GitHub/climatehealth/climatehealth/Malnutrition RIPS Ghana/Stunted_FINAL_Imputed.csv")

# Ensure column names do not have spaces
colnames(df) <- gsub(" ", "_", colnames(df))

# ======================================
# ========== Univariate Analysis =======
# ======================================
univariate_categorical_plot(df, "Household_Water")  # Bar Plot
univariate_numerical_plot(df, "Child_age")          # Histogram & Boxplot
univariate_density_plot(df, "Child_age")            # Density Plot
univariate_pareto_chart(df, "Household_Water")      # Pareto Chart

# ======================================
# ========== Bivariate Analysis ========
# ======================================
bivariate_categorical_plot(df, "Household_Water", "child_Stunted") # Categorical vs. Target
bivariate_numerical_plot(df, "Child_age", "child_Stunted")         # Numerical vs. Target
bivariate_scatter_regression(df, "rainfall_2020", "mean_temperature_2020") # Scatter Plot

# ======================================
# ========== Multivariate Analysis =====
# ======================================
multivariate_correlation_plot(df)   # Correlation Matrix
multivariate_scatter_plot(df, "child_Stunted") # Scatter Plot Matrix

