# ===========================================
# ============== Load Libraries =============
# ===========================================
library(dplyr)   # For data manipulation
library(readxl)  # For reading Excel files
library(caret)   # For data partitioning

# ===========================================
# ========== Data Processing Function =======
# ===========================================

process_data <- function(file_path, target_col = "CSM cases", test_size = 0.2, seed = 42) {
  
  # Step 1: Read the dataset (Excel version)
  df <- read_excel(file_path)  
  
  # Step 2: Clean column names to ensure compatibility
  colnames(df) <- make.names(colnames(df), unique = TRUE)
  
  # Step 3: Find the modified target column name
  target_col_clean <- make.names(target_col)  # Convert "CSM cases" to "CSM.cases"
  
  # Step 4: Check if the target column exists
  if (!(target_col_clean %in% colnames(df))) {
    stop(paste("[ERROR] Column", target_col_clean, "not found in dataset! Check column names."))
  }
  
  # Step 5: Drop `Month` column if it exists
  if ("Month" %in% colnames(df)) {
    df <- df %>% select(-Month)
    cat("\n[INFO] Dropped 'Month' column.\n")
  }
  
  # Step 6: Convert categorical columns to factors
  df <- df %>%
    mutate(across(where(is.character), as.factor))
  
  # Step 7: Apply Label Encoding
  df <- df %>%
    mutate(across(where(is.factor), as.integer))
  
  # Step 8: Display Summary Statistics Before Splitting
  cat("\n[INFO] Summary Statistics for", target_col, ":\n")
  print(summary(df[[target_col_clean]]))
  
  # Step 9: Split the dataset into training and testing sets (80% train, 20% test)
  set.seed(seed)
  trainIndex <- createDataPartition(df[[target_col_clean]], p = 1 - test_size, list = FALSE)
  
  train_data <- df[trainIndex, ]
  test_data <- df[-trainIndex, ]
  
  # Step 10: Display Training and Testing Set Sizes
  cat("\n[INFO] Training Set Size:", nrow(train_data), "\n")
  cat("[INFO] Testing Set Size:", nrow(test_data), "\n")
  
  # Step 11: Return the processed datasets
  return(list(train = train_data, test = test_data))
}

# ===========================================
# ============== Model Execution ============
# ===========================================

# Set file path to your dataset
file_path <- "C:/Users/kobby/Downloads/prof Eric R/CSM-Peter/CSM-Peter.xlsx"

# Run the function on the dataset
data_splits <- process_data(file_path, target_col = "CSM cases")

# Extract training and testing sets
train_set <- data_splits$train
test_set <- data_splits$test

# Print first few rows of training and testing sets
cat("\n[INFO] First few rows of Training Set:\n")
print(head(train_set))

cat("\n[INFO] First few rows of Testing Set:\n")
print(head(test_set))

