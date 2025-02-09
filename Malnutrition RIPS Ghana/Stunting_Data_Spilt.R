library(dplyr)  # For data manipulation
library(readr)  # For reading CSV files
library(caret)  # For data partitioning

# Function to process data (Encodes categorical variables & Splits into Train/Test)
process_data <- function(file_path, target_col = "child_Stunted", test_size = 0.2, seed = 42) {
  
  # Step 1: Read the dataset
  df <- read_csv(file_path, show_col_types = FALSE)  # Suppress column type warnings
  
  # Step 2: Clean column names to avoid formula errors
  colnames(df) <- make.names(colnames(df), unique = TRUE)
  
  # Step 3: Remove Rows Where `child_Stunted` is Missing
  missing_target <- sum(is.na(df[[target_col]]))  # Count missing values in target column
  
  if (missing_target > 0) {
    cat("\n[INFO] Found", missing_target, "missing values in", target_col, ". Removing these rows...\n")
    df <- df %>% filter(!is.na(.data[[target_col]]))  # Remove rows where `child_Stunted` is NA
    cat("[INFO] Remaining Data Points After Removing Missing `child_Stunted` Rows:", nrow(df), "\n")
  } else {
    cat("\n✅ No missing values found in", target_col, "\n")
  }
  
  # Step 4: Convert character columns to factors (for label encoding)
  df <- df %>%
    mutate(across(where(is.character), as.factor))
  
  # Step 5: Apply Label Encoding (convert categorical to numerical values)
  df <- df %>%
    mutate(across(where(is.factor), as.integer))
  
  # Step 6: Split the dataset into training and testing sets (80% train, 20% test)
  set.seed(seed)  # Ensure reproducibility
  trainIndex <- createDataPartition(df[[target_col]], p = 1 - test_size, list = FALSE)
  
  train_data <- df[trainIndex, ]
  test_data <- df[-trainIndex, ]
  
  # Step 7: Display Training and Testing Set Sizes
  cat("\n[INFO] Training Set Size:", nrow(train_data), "\n")
  cat("[INFO] Testing Set Size:", nrow(test_data), "\n")
  
  # Step 8: Return the processed datasets
  return(list(train = train_data, test = test_data))
}

# ======================= USAGE ==========================
# Run the function with your dataset file path
data_splits <- process_data("C:/Users/23350/Documents/GitHub/climatehealth/climatehealth/Malnutrition RIPS Ghana/Stunted_FINAL_Imputed.csv")

# Extract training and testing sets
train_set <- data_splits$train
test_set <- data_splits$test
