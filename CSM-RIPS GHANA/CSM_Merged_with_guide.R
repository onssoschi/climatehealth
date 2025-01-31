# Load necessary libraries
library(readxl)


# ===================================================
# ============== Step 1: Setup Logging & Config ====
# ===================================================

# Load necessary libraries
library(logger)


# ===================================================
# ============== Step 1: Setup Logging & Config ====
# ===================================================

# Load necessary libraries
library(logger)

# Initialize Logger
log_threshold(INFO)
log_appender(appender_console)

# Store pipeline start time
start_time <- Sys.time()  # <-- Add this line to track execution time

log_info("Starting the CSM Analysis pipeline...")

# Function to read configuration settings
read_config <- function() {
  list(
    paths = list(
      data_path = "C:/Users/kobby/Downloads/prof Eric R/CSM-Peter/CSM-Peter.xlsx"
    ),
    output = list(
      save_results = TRUE,
      output_path = "results.csv"
    )
  )
}

# Load Configuration
log_info("Loading pipeline configuration...")
config <- read_config()
log_info("Configuration successfully loaded.")

# Log dataset path
log_debug(paste("Data will be loaded from:", config$paths$data_path))


# ===================================================
# ============== Step 2: Data Loading ===============
# ===================================================

# Load necessary libraries
library(readxl)

# Function to load data with error handling
load_data <- function(file_path) {
  log_info("Attempting to load data...")
  
  # Check if the file exists
  if (!file.exists(file_path)) {
    log_error("File not found! Please check the path and try again.")
    stop("Error: File not found!")
  }
  
  # Try loading the dataset
  tryCatch({
    data <- read_excel(file_path)  # Read Excel file
    log_info(paste("Dataset loaded successfully with", nrow(data), "rows and", ncol(data), "columns."))
    
    # Display first few rows for debugging
    log_debug("Preview of dataset:")
    log_debug(head(data))
    
    return(data)
  }, error = function(e) {
    log_error("Error loading the file. Check the format or path.")
    log_error(e$message)
    return(NULL)
  })
}

# Load dataset using the config path
df <- load_data(config$paths$data_path)


# ===================================================
# ============== Step 3: Data Exploration ===========
# ===================================================

# Load necessary libraries
library(dplyr)

# Function to perform statistical analysis
perform_stat_analysis <- function(data) {
  log_info("Performing descriptive statistics...")
  
  # Check if dataset is valid
  if (is.null(data)) {
    log_error("No dataset found. Please load data first.")
    stop("Error: No dataset provided.")
  }
  
  # Clean column names: Replace non-alphanumeric characters with underscores
  colnames(data) <- gsub("[^A-Za-z0-9_]", "_", colnames(data))
  
  # Identify numeric and categorical columns
  numeric_cols <- data %>% select_if(is.numeric)
  categorical_cols <- data %>% select_if(is.character)
  
  # Summary statistics for numeric variables
  if (ncol(numeric_cols) > 0) {
    log_info("Generating summary statistics for numeric variables...")
    print(summary(numeric_cols))
  } else {
    log_info("No numeric variables found.")
  }
  
  # Standard deviation for numeric variables
  if (ncol(numeric_cols) > 0) {
    log_info("Calculating standard deviations for numeric variables...")
    print(sapply(numeric_cols, sd, na.rm = TRUE))
  }
  
  # Frequency distribution for categorical variables
  if (ncol(categorical_cols) > 0) {
    log_info("Calculating frequency distribution for categorical variables...")
    for (col in colnames(categorical_cols)) {
      log_info(paste("Processing:", col))
      print(table(categorical_cols[[col]], useNA = "ifany"))
    }
  } else {
    log_info("No categorical variables found.")
  }
  
  # Correlation matrix for numeric variables
  if (ncol(numeric_cols) > 1) {
    log_info("Calculating correlation matrix for numeric variables...")
    print(cor(numeric_cols, use = "complete.obs"))
  } else {
    log_info("Not enough numeric variables for correlation analysis.")
  }
  
  log_success("Descriptive statistics completed successfully.")
  
  # Return key results as a list
  return(list(
    summary_stats = if (ncol(numeric_cols) > 0) summary(numeric_cols) else NULL,
    std_deviation = if (ncol(numeric_cols) > 0) sapply(numeric_cols, sd, na.rm = TRUE) else NULL,
    correlation_matrix = if (ncol(numeric_cols) > 1) cor(numeric_cols, use = "complete.obs") else NULL
  ))
}

# Execute statistical analysis
stats <- perform_stat_analysis(df)


# ===================================================
# ============== Step 4: Exploratory Data Analysis ==
# ===================================================

# Load necessary libraries
library(ggplot2)

# Function to visualize categorical variables (Bar Plot)
univariate_categorical_plot <- function(data, column) {
  log_info(paste("Generating bar plot for categorical variable:", column))
  
  # Check if column exists
  if (!(column %in% colnames(data))) {
    log_error(paste("Column", column, "not found in dataset!"))
    return(NULL)
  }
  
  # Plot
  p <- ggplot(data, aes_string(x = column)) +
    geom_bar(fill = "blue", alpha = 0.7) +
    labs(title = paste("Distribution of", column), x = column, y = "Count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme_minimal()
  
  print(p)
  log_success(paste("Plot generated successfully for:", column))
}

# Function to visualize numerical variables (Histogram & Boxplot)
univariate_numerical_plot <- function(data, column) {
  log_info(paste("Generating histogram and boxplot for numerical variable:", column))
  
  # Check if column exists
  if (!(column %in% colnames(data))) {
    log_error(paste("Column", column, "not found in dataset!"))
    return(NULL)
  }
  
  # Histogram
  p1 <- ggplot(data, aes_string(x = column)) +
    geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
    labs(title = paste("Histogram of", column), x = column, y = "Count") +
    theme_minimal()
  
  # Boxplot
  p2 <- ggplot(data, aes_string(y = column)) +
    geom_boxplot(fill = "red", alpha = 0.7) +
    labs(title = paste("Boxplot of", column), y = column) +
    theme_minimal()
  
  print(p1)
  print(p2)
  log_success(paste("Plots generated successfully for:", column))
}

# Example Usage
log_info("Starting EDA visualizations...")
univariate_categorical_plot(df, "Region")  # Adjust column as needed
univariate_numerical_plot(df, "Average_Temperature")  # Adjust column as needed

log_success("Exploratory Data Analysis completed.")


# Bivariate Analysis
# ===================================================

# Function to visualize categorical vs. categorical (Stacked Bar Chart)
bivariate_categorical_plot <- function(data, column, target = "CSM_cases") {
  log_info(paste("Generating stacked bar chart for", column, "vs.", target))
  
  # Check if columns exist
  if (!(column %in% colnames(data)) | !(target %in% colnames(data))) {
    log_error(paste("Column", column, "or", target, "not found in dataset!"))
    return(NULL)
  }
  
  # Plot
  p <- ggplot(data, aes_string(x = column, fill = target)) +
    geom_bar(position = "dodge") +
    labs(title = paste("Bivariate Analysis of", column, "vs.", target), x = column, y = "Count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme_minimal()
  
  print(p)
  log_success(paste("Bivariate categorical plot generated for:", column))
}

# Function to visualize numerical vs. categorical (Boxplot)
bivariate_numerical_plot <- function(data, column, target = "CSM_cases") {
  log_info(paste("Generating boxplot for", column, "by", target))
  
  # Check if columns exist
  if (!(column %in% colnames(data)) | !(target %in% colnames(data))) {
    log_error(paste("Column", column, "or", target, "not found in dataset!"))
    return(NULL)
  }
  
  # Plot
  p <- ggplot(data, aes_string(x = target, y = column, fill = target)) +
    geom_boxplot(alpha = 0.7) +
    labs(title = paste("Boxplot of", column, "by", target), x = target, y = column) +
    theme_minimal()
  
  print(p)
  log_success(paste("Bivariate numerical plot generated for:", column))
}

# Function to visualize numerical vs. numerical (Scatter Plot with Regression Line)
bivariate_scatter_regression <- function(data, x_col, y_col) {
  log_info(paste("Generating scatter plot with regression for", x_col, "vs.", y_col))
  
  # Check if columns exist
  if (!(x_col %in% colnames(data)) | !(y_col %in% colnames(data))) {
    log_error(paste("Column", x_col, "or", y_col, "not found in dataset!"))
    return(NULL)
  }
  
  # Plot
  p <- ggplot(data, aes_string(x = x_col, y = y_col)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", col = "red") +
    labs(title = paste("Scatter Plot of", x_col, "vs.", y_col), x = x_col, y = y_col) +
    theme_minimal()
  
  print(p)
  log_success(paste("Scatter plot with regression generated for:", x_col, "vs.", y_col))
}

# Example Usage
log_info("Starting Bivariate Analysis visualizations...")
bivariate_categorical_plot(df, "Climatic_Zone", "CSM_cases")
bivariate_numerical_plot(df, "Average_Temperature", "CSM_cases")
bivariate_scatter_regression(df, "Average_Temperature", "Average_Rainfall")

log_success("Bivariate Analysis completed.")


# Step 6: Multivariate Analysis 
# ===================================================

# Load necessary libraries
library(ggcorrplot)

# Function to visualize correlation matrix
multivariate_correlation_plot <- function(data) {
  log_info("Generating correlation matrix for numeric variables...")
  
  # Select numeric columns
  numeric_cols <- select_if(data, is.numeric)
  
  # Check if there are enough numeric columns
  if (ncol(numeric_cols) < 2) {
    log_error("Not enough numeric variables for correlation analysis.")
    return(NULL)
  }
  
  # Compute correlation matrix
  corr_matrix <- cor(numeric_cols, use = "complete.obs")
  
  # Plot correlation matrix
  log_info("Plotting correlation matrix...")
  p <- ggcorrplot(corr_matrix, method = "circle", lab = TRUE)
  
  print(p)
  log_success("Correlation matrix plotted successfully.")
}

# Execute Correlation Matrix Plot
multivariate_correlation_plot(df)


# ===================================================
# ============== Step 7: Data Splitting =============
# ===================================================

# Load necessary libraries
library(caret)

# Function to split dataset into train and test sets
split_data <- function(data, target_col, test_size = 0.2, seed = 42) {
  log_info("Splitting dataset into train and test sets...")
  
  # Ensure the target column name is valid
  target_col_clean <- make.names(target_col)  
  if (!(target_col_clean %in% colnames(data))) {
    log_error(paste("[ERROR] Column", target_col_clean, "not found in dataset! Check column names."))
    stop("Error: Target column not found in dataset.")
  }
  
  # Convert categorical columns to factors
  data <- data %>%
    mutate(across(where(is.character), as.factor))
  
  # Apply Label Encoding to categorical variables
  data <- data %>%
    mutate(across(where(is.factor), as.integer))
  
  # Split dataset into training and testing sets
  set.seed(seed)
  trainIndex <- createDataPartition(data[[target_col_clean]], p = 1 - test_size, list = FALSE)
  
  train_data <- data[trainIndex, ]
  test_data <- data[-trainIndex, ]
  
  # Log dataset sizes
  log_info(paste("[INFO] Training Set Size:", nrow(train_data)))
  log_info(paste("[INFO] Testing Set Size:", nrow(test_data)))
  
  return(list(train = train_data, test = test_data))
}

# Define target column
target_column <- "CSM_cases"  # Adjust if necessary

# Perform the split
data_splits <- split_data(df, target_col = target_column)

# Extract training and testing sets
train_set <- data_splits$train
test_set <- data_splits$test

# Print first few rows of training and testing sets
log_info("[INFO] First few rows of Training Set:")
print(head(train_set))

log_info("[INFO] First few rows of Testing Set:")
print(head(test_set))


# ===================================================
# ============== Step 8: Model Building ============
# ===================================================

# Load necessary libraries
library(MASS)

# Function to train Poisson or Quasi-Poisson model
train_model <- function(train_data, response, predictors) {
  log_info("Training model...")
  
  # Construct formula for modeling
  formula <- as.formula(paste(response, "~", paste(predictors, collapse = " + ")))
  
  # Check for overdispersion
  mean_csm <- mean(train_data[[response]])
  var_csm <- var(train_data[[response]])
  log_info(paste("Mean:", mean_csm, "Variance:", var_csm))
  
  # Choose between Poisson and Quasi-Poisson model
  if (var_csm > mean_csm) {
    log_info("Overdispersion detected; Using Quasi-Poisson model.")
    model <- glm(formula, family = quasipoisson, data = train_data)
  } else {
    log_info("No overdispersion detected; Using Poisson model.")
    model <- glm(formula, family = poisson, data = train_data)
  }
  
  # Display model summary
  log_info("Model Summary:")
  print(summary(model))
  
  return(model)
}

# Define predictor variables
predictor_columns <- c("Average_Rainfall", "Average_Temperature")  # Adjust as needed

# Train the model
model <- train_model(train_set, response = target_column, predictors = predictor_columns)

# ===================================================
# ============== Step 9: Model Evaluation ==========
# ===================================================

# Function to evaluate model performance
evaluate_model <- function(model, test_data, response) {
  log_info("Evaluating model performance...")
  
  # Generate predictions
  test_data$Predicted <- predict(model, newdata = test_data, type = "response")
  
  # Calculate Mean Squared Error (MSE)
  mse <- mean((test_data[[response]] - test_data$Predicted)^2)
  log_info(paste("Mean Squared Error (MSE):", round(mse, 4)))
  
  # Scatter plot: Predicted vs. Observed
  p <- ggplot(test_data, aes(x = Predicted, y = .data[[response]])) +
    geom_point(color = "blue") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    theme_minimal() +
    labs(title = "Predicted vs Observed CSM", x = "Predicted CSM", y = "Observed CSM")
  
  print(p)
  log_success("Model evaluation completed.")
}

# Evaluate model on test set
evaluate_model(model, test_set, target_column)


# ===================================================
# ============== Step 10: Model Enhancement =========
# ===================================================

# Function to train a model with interaction terms
train_interaction_model <- function(train_data, response, predictors) {
  log_info("Training model with interaction terms...")
  
  # Construct interaction formula
  interaction_formula <- as.formula(paste(response, "~", predictors[1], "*", predictors[2]))
  
  # Fit the model with interaction
  model_interaction <- glm(interaction_formula, family = quasipoisson, data = train_data)
  
  # Print model summary
  log_info("Interaction Model Summary:")
  print(summary(model_interaction))
  
  return(model_interaction)
}

# Train model with interaction
interaction_model <- train_interaction_model(train_set, response = target_column, predictors = predictor_columns)

# ===================================================
# ============== Step 11: DLNM for Lag Effects ======
# ===================================================

# Load necessary library
library(dlnm)

# Function to apply DLNM for lagged effects of temperature
apply_dlnm <- function(data, response, predictor, lag_max = 30) {
  log_info("Applying Distributed Lag Non-Linear Model (DLNM)...")
  
  # Convert time variable to Date format
  data$Date <- as.Date(data$Date)
  
  # Define cross-basis function for the predictor (e.g., temperature)
  cb <- crossbasis(data[[predictor]], lag = c(0, lag_max),
                   argvar = list(fun = "ns", df = 3),
                   arglag = list(fun = "ns", df = 3))
  
  # Fit the DLNM model
  dlnm_model <- glm(data[[response]] ~ cb, family = quasipoisson(), data = data)
  
  log_info("DLNM Model Summary:")
  print(summary(dlnm_model))
  
  # Predict effects
  pred <- crosspred(cb, dlnm_model, at = seq(min(data[[predictor]]), max(data[[predictor]]), length = 100))
  
  # Plot DLNM predictions
  plot(pred, xlab = predictor, zlab = "Relative Risk", main = "DLNM Prediction")
  
  log_success("DLNM Model applied successfully.")
  return(dlnm_model)
}

# Apply DLNM model
dlnm_model <- apply_dlnm(df, response = "CSM_cases", predictor = "Average_Temperature", lag_max = 30)


# ===================================================
# ============== Step 12: Model Diagnostics =========
# ===================================================

# Function to summarize model performance
summarize_model <- function(model) {
  log_info("Summarizing model performance...")
  
  summary_stats <- summary(model)
  
  cat("\n================ Model Performance Summary ================\n")
  cat("Deviance:", summary_stats$deviance, "\n")
  cat("Residual Degrees of Freedom:", summary_stats$df.residual, "\n")
  cat("Dispersion:", summary_stats$deviance / summary_stats$df.residual, "\n")
  
  log_success("Model performance summary completed.")
  return(summary_stats)
}

# Summarize the main model
main_model_summary <- summarize_model(model)

# Summarize the DLNM model
dlnm_model_summary <- summarize_model(dlnm_model)

# ===================================================
# ============== Step 13: Residual Diagnostics ======
# ===================================================

# Function to plot residual diagnostics
plot_residuals <- function(model) {
  log_info("Generating residual diagnostic plots...")
  
  par(mfrow = c(2, 2))
  plot(model)
  
  log_success("Residual diagnostics completed.")
}

# Execute residual diagnostics
plot_residuals(model)
plot_residuals(dlnm_model)


# ===================================================
# ============== Step 14: Saving Results ============
# ===================================================

# Function to save model results
save_results <- function(data, file_path) {
  log_info("Checking if results need to be saved...")
  
  if (config$output$save_results) {
    log_info(paste("Saving results to:", file_path))
    write.csv(data, file_path, row.names = FALSE)
    log_success("Results saved successfully.")
  } else {
    log_info("Skipping saving results as per configuration settings.")
  }
}

# Save processed data
save_results(df, config$output$output_path)

# ===================================================
# ============== Step 15: Finalizing Pipeline =======
# ===================================================

# Track total execution time
end_time <- Sys.time()
execution_time <- round(difftime(end_time, start_time, units = "secs"), 2)

log_success(paste("CSM Analysis pipeline completed successfully in", execution_time, "seconds."))


