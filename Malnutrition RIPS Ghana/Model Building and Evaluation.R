library(readxl)
library(ggplot2)
library(dplyr)
library(caret)
library(randomForest)
library(xgboost)
library(e1071)    # For SVM
library(rpart)    # For Decision Tree
library(car)
library(tidymodels)
library(glmnet)
library(sjPlot)
library(ggeffects)  # For prediction visualization

# Function to Train, Tune, Evaluate, and Analyze Logistic Regression Model
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
    OR = or_values,
    Lower_CI = ci_values[, 1],
    Upper_CI = ci_values[, 2]
  )
  
  or_results[, 2:4] <- round(or_results[, 2:4], 3)
  print(or_results)
  
  # Visualizing Predictions for `Child_age`
  if ("Child_age" %in% names(train_data)) {
    cat("\n[INFO] Visualizing predictions for 'Child_age'...\n")
    preds <- ggpredict(glm_model, terms = "Child_age [all]")
    
    ggplot(preds, aes(x = x, y = predicted)) +
      geom_line(color = "blue") +
      geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
      labs(
        title = "Predicted Probability of Child Stunting by Age",
        x = "Child Age",
        y = "Predicted Probability"
      ) +
      theme_minimal()
  }
  
  return(list(
    glm_model = glm_model,
    vif = vif_values,
    odds_ratios = or_results
  ))
}

# **Usage Examples**
# 1. Train model with all variables
logistic_results <- train_logistic_model(train_set, test_set)

# 2. Train model with selected variables
logistic_results <- train_logistic_model(train_set, test_set, 
                                         include_vars = c("Child_age", "mean_temperature_2020","Sex_child","Mother_workstatus","Age_mother","Household_location","Child_fever","Child_diarr","mother_education","Household_wealth"))
