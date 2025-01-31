# =====================================================
# ============ Load Necessary Libraries ==============
# =====================================================
library(readr)        # Read CSV files
library(ggplot2)      # Visualization
library(dplyr)        # Data manipulation
library(caret)        # Data partitioning
library(randomForest) # Random Forest model
library(xgboost)      # XGBoost model
library(e1071)        # Support Vector Machine (SVM)
library(rpart)        # Decision Tree
library(car)          # Multicollinearity Check (VIF)
library(tidymodels)   # Modeling framework
library(glmnet)       # Regularized regression models
library(sjPlot)       # Model visualization
library(ggeffects)    # Prediction visualization
library(ggcorrplot)   # Correlation matrix visualization
library(caret)   # For model evaluation




# ===========================================
# ========== Load Dataset Function ==========
# ===========================================
load_stunted_data <- function(file_path) {
  # Check if the file exists
  if (!file.exists(file_path)) {
    stop("Error: File not found! Please check the path and try again.")
  }
  
  # Try to read the dataset with error handling
  tryCatch({
    data <- read_csv(file_path, show_col_types = FALSE)  # Suppress column type warnings
    
    # Print basic dataset information
    print("Dataset Loaded Successfully")
    print(paste("Number of Rows:", nrow(data)))
    print(paste("Number of Columns:", ncol(data)))
    
    # Display first few rows
    print(head(data))
    
    # Display column names
    print("Column Names:")
    print(names(data))
    
    # Return the dataset
    return(data)
  }, error = function(e) {
    print("Error loading the file. Please check the file format and path.")
    print(e)
    return(NULL)
  })
}

# ===========================================
# ========== Execute Data Loading ===========
# ===========================================

# Define file path
file_path <- "C:/Users/23350/Downloads/prof Eric2 R/prof Eric R/Malnutrition-RIPS GHANA/Stunting_FINAL_DATA.csv"

# Load the dataset
df <- load_stunted_data(file_path)



# ==================================================
# ========== Statistical Analysis Function =========
# ==================================================
perform_stat_analysis <- function(data) {
  
  # Check if the data is loaded
  if (is.null(data)) {
    stop("Error: No dataset provided. Please load the dataset first.")
  }
  
  # Clean column names: Replace spaces with underscores
  colnames(data) <- gsub(" ", "_", colnames(data))
  
  # Identify numeric and categorical columns
  numeric_cols <- select_if(data, is.numeric)
  categorical_cols <- select_if(data, is.character)
  
  # ===========================================
  # ========== Summary Statistics =============
  # ===========================================
  print("Summary Statistics for Numeric Variables:")
  print(summary(numeric_cols))
  
  # ===========================================
  # ========== Standard Deviation =============
  # ===========================================
  print("Standard Deviation for Numeric Variables:")
  print(sapply(numeric_cols, sd, na.rm = TRUE))
  
  # ===========================================
  # ========== Frequency Distribution =========
  # ===========================================
  print("Frequency Distribution for Categorical Variables:")
  for (col in colnames(categorical_cols)) {
    print(paste("Frequency for:", col))
    print(table(categorical_cols[[col]], useNA = "ifany"))
  }
  
  # ===========================================
  # ========== Correlation Matrix =============
  # ===========================================
  if (ncol(numeric_cols) > 1) {
    print("Correlation Matrix for Numeric Variables:")
    print(cor(numeric_cols, use = "complete.obs"))
  } else {
    print("Not enough numeric variables for correlation analysis.")
  }
  
  # Return key results as a list
  return(list(
    summary_stats = summary(numeric_cols),
    std_deviation = sapply(numeric_cols, sd, na.rm = TRUE),
    correlation_matrix = if (ncol(numeric_cols) > 1) cor(numeric_cols, use = "complete.obs") else NULL
  ))
}

# ===========================================
# ========== Execute Statistical Analysis ===
# ===========================================

stats <- perform_stat_analysis(df)



# ==================================================
# ========== Univariate Analysis Functions =========
# ==================================================

# Function to visualize categorical variables (Bar Plot)
univariate_categorical_plot <- function(data, column) {
  ggplot(data, aes_string(x = column)) +
    geom_bar(fill = "blue", alpha = 0.7) +
    labs(title = paste("Distribution of", column), x = column, y = "Count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme_minimal()
}

# Function to visualize numerical variables (Histogram & Boxplot)
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

# ==================================================
# ========== Bivariate Analysis Functions ==========
# ==================================================

# Function to visualize categorical vs. categorical (Stacked Bar Chart)
bivariate_categorical_plot <- function(data, column, target = "child_Stunted") {
  ggplot(data, aes_string(x = column, fill = target)) +
    geom_bar(position = "dodge") +
    labs(title = paste("Bivariate Analysis of", column, "vs.", target), x = column, y = "Count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme_minimal()
}

# Function to visualize numerical vs. categorical (Boxplot)
bivariate_numerical_plot <- function(data, column, target = "child_Stunted") {
  ggplot(data, aes_string(x = target, y = column, fill = target)) +
    geom_boxplot(alpha = 0.7) +
    labs(title = paste("Boxplot of", column, "by", target), x = target, y = column) +
    theme_minimal()
}

# Function to visualize numerical vs. numerical (Scatter Plot with Regression Line)
bivariate_scatter_regression <- function(data, x_col, y_col) {
  ggplot(data, aes_string(x = x_col, y = y_col)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", col = "red") +
    labs(title = paste("Scatter Plot of", x_col, "vs.", y_col), x = x_col, y = y_col) +
    theme_minimal()
}


# ==================================================
# ========== Multivariate Analysis Functions =======
# ==================================================

# Function to visualize correlation matrix
multivariate_correlation_plot <- function(data) {
  numeric_cols <- select_if(data, is.numeric)
  corr_matrix <- cor(numeric_cols, use = "complete.obs")
  ggcorrplot(corr_matrix, method = "circle", lab = TRUE)
}


# ===========================================
# ========== Execute Data Visualization =====
# ===========================================

# Univariate Plots
univariate_categorical_plot(df, "Household_Water")  # Example categorical variable
univariate_numerical_plot(df, "Child_age")         # Example numerical variable

# Bivariate Plots
bivariate_categorical_plot(df, "Household_Water", "child_Stunted")
bivariate_numerical_plot(df, "Child_age", "child_Stunted")
bivariate_scatter_regression(df, "rainfall_2020", "mean_temperature_2020")

# Correlation Matrix
multivariate_correlation_plot(df)




# ==================================================
# ========== Data Splitting Function ===============
# ==================================================
split_data <- function(data, target_col, test_size = 0.2, seed = 42) {
  set.seed(seed)
  
  # Ensure the target column name is valid
  target_col_clean <- make.names(target_col)  
  if (!(target_col_clean %in% colnames(data))) {
    stop(paste("Error: Column", target_col_clean, "not found in dataset! Check column names."))
  }
  
  # Convert categorical columns to factors
  data <- data %>%
    mutate(across(where(is.character), as.factor))
  
  # Apply Label Encoding to categorical variables
  data <- data %>%
    mutate(across(where(is.factor), as.integer))
  
  # Split dataset into training and testing sets
  trainIndex <- createDataPartition(data[[target_col_clean]], p = 1 - test_size, list = FALSE)
  
  train_data <- data[trainIndex, ]
  test_data <- data[-trainIndex, ]
  
  # Print dataset sizes
  cat("\nTraining Set Size:", nrow(train_data), "\n")
  cat("Testing Set Size:", nrow(test_data), "\n")
  
  return(list(train = train_data, test = test_data))
}


# ===========================================
# ========== Execute Data Splitting =========
# ===========================================

# Define target column
target_column <- "child_Stunted"  # Adjust if necessary

# Perform the split
data_splits <- split_data(df, target_col = target_column)

# Extract training and testing sets
train_set <- data_splits$train
test_set <- data_splits$test

# Print first few rows of training and testing sets
cat("\nFirst few rows of Training Set:\n")
print(head(train_set))

cat("\nFirst few rows of Testing Set:\n")
print(head(test_set))


# ==================================================
# ========== Logistic Regression Training ==========
# ==================================================
train_logistic_model <- function(train_data, test_data, target_col = "child_Stunted") {
  
  cat("\nEnsuring target variable is a factor...\n")
  train_data[[target_col]] <- as.factor(train_data[[target_col]])
  test_data[[target_col]] <- factor(test_data[[target_col]], levels = levels(train_data[[target_col]]))
  
  # Train Logistic Regression Model
  cat("\nTraining logistic regression model...\n")
  glm_model <- glm(as.formula(paste(target_col, "~ .")), 
                   data = train_data, 
                   family = binomial, 
                   control = glm.control(maxit = 10000))
  
  # Print Model Summary
  cat("\nModel Summary:\n")
  print(summary(glm_model))
  
  # Multicollinearity Check with VIF
  cat("\nChecking for multicollinearity...\n")
  vif_values <- vif(glm_model)
  print(vif_values)
  
  return(glm_model)
}


# ===========================================
# ========== Execute Model Training =========
# ===========================================

# Train logistic regression model
logistic_model <- train_logistic_model(train_set, test_set, target_col = target_column)



# ==================================================
# ========== Train & Evaluate Logistic Model =======
# ==================================================
train_logistic_model <- function(train_data, test_data, target_col = "child_Stunted", 
                                 include_vars = NULL, exclude_vars = NULL) {
  
  cat("\n[INFO] Ensuring target variable is a factor...\n")
  train_data[[target_col]] <- as.factor(train_data[[target_col]])
  test_data[[target_col]] <- factor(test_data[[target_col]], levels = levels(train_data[[target_col]]))
  
  # **Dynamic Variable Selection**
  if (!is.null(include_vars)) {
    selected_vars <- c(target_col, include_vars)
  } else if (!is.null(exclude_vars)) {
    selected_vars <- setdiff(names(train_data), exclude_vars)
  } else {
    selected_vars <- names(train_data)
  }
  
  # Filter dataset based on selected variables
  train_data <- train_data[selected_vars]
  test_data <- test_data[selected_vars]
  
  # Train Standard Logistic Regression Model
  cat("\n[INFO] Training logistic regression model with selected variables...\n")
  glm_model <- glm(as.formula(paste(target_col, "~ .")), 
                   data = train_data, 
                   family = binomial, 
                   control = glm.control(maxit = 10000))
  
  # Print Model Summary
  cat("\n[INFO] Model Summary:\n")
  print(summary(glm_model))
  
  # Multicollinearity Check with VIF
  cat("\n[INFO] Checking for multicollinearity...\n")
  vif_values <- vif(glm_model)
  print(vif_values)
  
  # Compute Odds Ratios & Confidence Intervals
  cat("\n[INFO] Computing Odds Ratios & Confidence Intervals...\n")
  or_values <- exp(coef(glm_model))
  ci_values <- exp(confint(glm_model))
  
  or_results <- data.frame(
    Variable = names(or_values),
    OR = round(or_values, 3),
    Lower_CI = round(ci_values[, 1], 3),
    Upper_CI = round(ci_values[, 2], 3)
  )
  
  print(or_results)
  
  # Generate Predictions on Test Set
  test_data$Predicted_Prob <- predict(glm_model, newdata = test_data, type = "response")
  test_data$Predicted_Class <- ifelse(test_data$Predicted_Prob > 0.5, 1, 0)
  
  # Ensure Both Classes Exist in Predictions & Actual Values
  actual_levels <- unique(test_data[[target_col]])
  predicted_levels <- unique(test_data$Predicted_Class)
  
  if (length(actual_levels) < 2 || length(predicted_levels) < 2) {
    stop("Error: The dataset does not contain both classes. Check for class imbalance.")
  }
  
  # Compute Confusion Matrix
  cat("\n[INFO] Evaluating Model Performance...\n")
  test_data[[target_col]] <- factor(test_data[[target_col]], levels = c(0, 1))
  test_data$Predicted_Class <- factor(test_data$Predicted_Class, levels = c(0, 1))
  
  conf_matrix <- confusionMatrix(test_data$Predicted_Class, test_data[[target_col]])
  
  cat("\n[INFO] Model Evaluation Metrics:\n")
  cat("Accuracy:", conf_matrix$overall["Accuracy"], "\n")
  cat("Precision:", conf_matrix$byClass["Precision"], "\n")
  cat("Recall:", conf_matrix$byClass["Recall"], "\n")
  cat("F1 Score:", conf_matrix$byClass["F1"], "\n")
  
  return(list(
    glm_model = glm_model,
    vif = vif_values,
    odds_ratios = or_results,
    confusion_matrix = conf_matrix
  ))
}

# ==================================================
# ========== Model Evaluation Function =============
# ==================================================
evaluate_logistic_model <- function(model, test_data, target_col) {
  
  # Predict probabilities and classify based on threshold
  test_data$Predicted_Prob <- predict(model, newdata = test_data, type = "response")
  test_data$Predicted_Class <- ifelse(test_data$Predicted_Prob > 0.5, 1, 0)
  
  # Ensure target variable is a factor and has matching levels
  test_data[[target_col]] <- factor(test_data[[target_col]], levels = c(0, 1))
  test_data$Predicted_Class <- factor(test_data$Predicted_Class, levels = c(0, 1))
  
  # Check for class imbalance issue
  if (length(unique(test_data[[target_col]])) < 2 || length(unique(test_data$Predicted_Class)) < 2) {
    stop("Error: The dataset does not contain both classes. Check for class imbalance.")
  }
  
  # Compute Confusion Matrix
  conf_matrix <- confusionMatrix(test_data$Predicted_Class, test_data[[target_col]])
  
  # Print Evaluation Metrics
  cat("\nModel Evaluation Metrics:\n")
  cat("Accuracy:", conf_matrix$overall["Accuracy"], "\n")
  cat("Precision:", conf_matrix$byClass["Precision"], "\n")
  cat("Recall:", conf_matrix$byClass["Recall"], "\n")
  cat("F1 Score:", conf_matrix$byClass["F1"], "\n")
  
  return(conf_matrix)
}



# ===========================================
# ========== Execute Model Evaluation =======
# ===========================================

evaluate_logistic_model(logistic_model, test_set, target_column)



# ==================================================
# ========== Compute Odds Ratios ===================
# ==================================================
compute_odds_ratios <- function(model) {
  cat("\nComputing Odds Ratios & Confidence Intervals...\n")
  
  or_values <- exp(coef(model))  # Compute odds ratios
  ci_values <- exp(confint(model))  # Compute confidence intervals
  
  # Store results in a dataframe
  or_results <- data.frame(
    Variable = names(or_values),
    Odds_Ratio = round(or_values, 3),
    Lower_CI = round(ci_values[, 1], 3),
    Upper_CI = round(ci_values[, 2], 3)
  )
  
  print(or_results)
  return(or_results)
}


# ===========================================
# ========== Execute Odds Ratios ============
# ===========================================

odds_ratios <- compute_odds_ratios(logistic_model)



# ==================================================
# ========== Feature Importance Visualization ======
# ==================================================
visualize_feature_importance <- function(model) {
  library(ggplot2)
  
  # Extract coefficients
  coef_df <- data.frame(
    Variable = names(coef(model)),
    Coefficient = coef(model)
  )
  
  # Filter out intercept and sort by absolute importance
  coef_df <- coef_df %>% filter(Variable != "(Intercept)") %>% arrange(desc(abs(Coefficient)))
  
  # Plot feature importance
  ggplot(coef_df, aes(x = reorder(Variable, Coefficient), y = Coefficient, fill = Coefficient > 0)) + 
    geom_col() + 
    coord_flip() + 
    labs(title = "Feature Importance (Logistic Regression)", x = "Variables", y = "Coefficient") + 
    theme_minimal()
}

# ===========================================
# ========== Execute Feature Importance =====
# ===========================================

visualize_feature_importance(logistic_model)



# ==================================================
# ========== Model Prediction Function =============
# ==================================================
generate_predictions <- function(model, test_data, target_col) {
  cat("\nGenerating Predictions...\n")
  
  # Predict probabilities and classify based on 0.5 threshold
  test_data$Predicted_Prob <- predict(model, newdata = test_data, type = "response")
  test_data$Predicted_Class <- ifelse(test_data$Predicted_Prob > 0.5, 1, 0)
  
  # Combine actual and predicted values
  results <- test_data %>%
    select(all_of(target_col), Predicted_Prob, Predicted_Class)
  
  print(head(results))  # Display first few predictions
  
  return(results)
}



# ===========================================
# ========== Execute Model Predictions ======
# ===========================================

predictions <- generate_predictions(logistic_model, test_set, target_column)
