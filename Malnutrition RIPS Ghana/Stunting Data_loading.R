load_stunted_data <- function(file_path) {
  # Load necessary library
  library(readr)
  
  # Check if the file exists
  if (!file.exists(file_path)) {
    stop("Error: File not found! Please check the path and try again.")
  }
  
  # Try to read the dataset with error handling
  tryCatch({
    data <- read_csv(file_path)
    
    # Print basic information
    print("Dataset Loaded Successfully!")
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

df <- load_stunted_data("C:/Users/23350/Documents/GitHub/climatehealth/climatehealth/Malnutrition RIPS Ghana/Stunted_FINAL_Imputed.csv")
