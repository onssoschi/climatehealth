# =====================================================
# ============ Load Necessary Libraries ==============
# =====================================================
library(readr)        # Read CSV files
library(ggplot2)      # Visualization
library(dplyr)        # Data manipulation
library(caret)        # Data partitioning & evaluation
library(car)          # Multicollinearity Check (VIF)
library(glmnet)       # Regularized regression models
library(sjPlot)       # Model visualization
library(ggeffects)    # Prediction visualization
library(ggcorrplot)   # Correlation matrix visualization

# ===========================================
# ========== Load & Preprocess Dataset ======
# ===========================================
load_and_preprocess_data <- function(file_path) {
  df <- read_csv(file_path, show_col_types = FALSE)
  df <- df %>% rename_all(~ gsub(" ", "_", .))  # Replace spaces in column names
  df <- df %>% select(-contains("Unnamed"))  # Remove empty columns
  
  # Convert categorical variables to factors
  categorical_cols <- c("Household_Water", "Household_toilet", "Household_location", 
                        "Mother_workstatus", "Sex_child", "Child_fever", "Child_diarr", 
                        "mother_education", "div_cat", "Use_net")
  df[categorical_cols] <- lapply(df[categorical_cols], factor)
  
  # Convert target variable to binary (1 = Stunted, 0 = Normal)
  df$child_Stunted <- ifelse(df$child_Stunted == "Stunted", 1, 0)
  
  return(df)
}

# Load Data
df <- read_csv("C:/Users/23350/Documents/GitHub/climatehealth/climatehealth/Malnutrition RIPS Ghana/Stunted_FINAL_Imputed.csv")


# ==================================================
# ========== Descriptive Statistics ===============
# ==================================================
summary(df)
sapply(df, function(x) if (is.numeric(x)) sd(x, na.rm = TRUE))

# ==================================================
# ========== Data Visualization ====================
# ==================================================
univariate_categorical_plot <- function(data, column) {
  ggplot(data, aes_string(x = column)) +
    geom_bar(fill = "blue", alpha = 0.7) +
    theme_minimal()
}

univariate_numerical_plot <- function(data, column) {
  ggplot(data, aes_string(x = column)) +
    geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
    theme_minimal()
}

multivariate_correlation_plot <- function(data) {
  numeric_cols <- select_if(data, is.numeric)
  corr_matrix <- cor(numeric_cols, use = "complete.obs")
  ggcorrplot(corr_matrix, method = "circle", lab = TRUE)
}

# Visualization Examples
univariate_categorical_plot(df, "Household_Water")
univariate_numerical_plot(df, "Child_age")
multivariate_correlation_plot(df)

# ==================================================
# ========== Data Splitting ========================
# ==================================================
split_data <- function(data, target_col, test_size = 0.2) {
  set.seed(42)
  trainIndex <- createDataPartition(data[[target_col]], p = 1 - test_size, list = FALSE)
  list(train = data[trainIndex, ], test = data[-trainIndex, ])
}

data_splits <- split_data(df, "child_Stunted")
train_set <- data_splits$train
test_set <- data_splits$test

# ==================================================
# ========== Logistic Regression Training ==========
# ==================================================
train_logistic_model <- function(train_data, test_data, target_col = "child_Stunted", 
                                 include_vars = NULL, exclude_vars = NULL) {
  train_data[[target_col]] <- as.factor(train_data[[target_col]])
  test_data[[target_col]] <- factor(test_data[[target_col]], levels = levels(train_data[[target_col]]))
  
  if (!is.null(include_vars)) {
    selected_vars <- c(target_col, include_vars)
  } else if (!is.null(exclude_vars)) {
    selected_vars <- setdiff(names(train_data), exclude_vars)
  } else {
    selected_vars <- names(train_data)
  }
  
  train_data <- train_data[selected_vars]
  test_data <- test_data[selected_vars]
  
  glm_model <- glm(as.formula(paste(target_col, "~ .")), 
                   data = train_data, 
                   family = binomial, 
                   control = glm.control(maxit = 10000))
  
  print(summary(glm_model))
  print(vif(glm_model))
  
  return(glm_model)
}

# Train model with all variables
logistic_results <- train_logistic_model(train_set, test_set)

# Train model with selected variables
logistic_results <- train_logistic_model(train_set, test_set, 
                                         include_vars = c("Child_age", "mean_temperature_2020", "Sex_child", "Mother_workstatus", "Age_mother", "Household_location", "Child_fever", "Child_diarr", "mother_education", "Household_wealth"))

# ==================================================
# ========== Model Evaluation ======================
# ==================================================
evaluate_logistic_model <- function(model, test_data, target_col) {
  test_data$Predicted_Prob <- predict(model, newdata = test_data, type = "response")
  test_data$Predicted_Class <- ifelse(test_data$Predicted_Prob > 0.5, 1, 0)
  
  test_data[[target_col]] <- factor(test_data[[target_col]], levels = c(0, 1))
  test_data$Predicted_Class <- factor(test_data$Predicted_Class, levels = c(0, 1))
  
  conf_matrix <- confusionMatrix(test_data$Predicted_Class, test_data[[target_col]])
  print(conf_matrix)
  return(conf_matrix)
}

evaluate_logistic_model(logistic_results, test_set, "child_Stunted")
