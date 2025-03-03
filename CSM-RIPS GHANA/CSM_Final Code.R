library(readxl)
library(MASS)
library(mgcv)
library(dlnm)
library(ggplot2)
library(plotly)
library(dplyr)
library(ggcorrplot)
library(tidyr)
library(splines)

# Load dataset
load_stunted_data <- function(file_path) {
  if (!file.exists(file_path)) stop("Error: File not found!")
  data <- read_excel(file_path)
  print("Dataset Loaded Successfully!")
  return(data)
}

df <- load_stunted_data("/Users/e.afful-dadzie/Desktop/CSM.xlsx")

# ===================================================
# ============== Descriptive Statistics =============
# ===================================================

# Summary Key Statistics
summary_statistics <- df %>% summarise(across(where(is.numeric), list(
  mean = mean, median = median, min = min, max = max, variance = var), na.rm = TRUE))
print("### Summary Statistics for Numeric Variables:")
print(summary_statistics)

# Distribution Analysis
numeric_cols <- df %>% select(where(is.numeric))
for (col in names(numeric_cols)) {
  print(ggplot(df, aes_string(x = col)) +
          geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
          labs(title = paste("Histogram of", col), x = col, y = "Count") +
          theme_minimal())
}

# Temporal Analysis (Trends & Seasonality)
df$Date <- as.Date(df$Date)
ggplot(df, aes(x = Date, y = CSM_cases)) +
  geom_line(color = "blue") +
  labs(title = "CSM Cases Over Time", x = "Date", y = "CSM Cases") +
  theme_minimal()

# Correlation & Non-Linearity Checks
correlation_matrix <- cor(numeric_cols, use = "complete.obs")
print("### Correlation Matrix:")
print(correlation_matrix)
ggcorrplot(correlation_matrix, method = "circle", lab = TRUE)

# Spearman & Pearson Correlation
spearman_corr <- cor(df$Maximum_Temperature, df$CSM_cases, method = "spearman", use = "complete.obs")
pearson_corr <- cor(df$Maximum_Temperature, df$CSM_cases, method = "pearson", use = "complete.obs")
print(paste("Spearman Correlation (Temp vs CSM Cases):", spearman_corr))
print(paste("Pearson Correlation (Temp vs CSM Cases):", pearson_corr))

# Generalized Additive Model (GAM) for Non-Linearity Detection
gam_model <- gam(CSM_cases ~ s(Maximum_Temperature, bs = "cs"), family = poisson, data = df)

# Ensure y-axis is numeric
df$CSM_cases <- as.numeric(df$CSM_cases)

# Plot GAM with ggplot
ggplot(df, aes(x = Maximum_Temperature, y = CSM_cases)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), color = "red") +
  labs(title = "GAM Fit: CSM Cases vs Temperature", x = "Maximum Temperature", y = "CSM Cases") +
  theme_minimal()

# Overdispersion & Zero Inflation Check
dispersion_param <- var(df$CSM_cases) / mean(df$CSM_cases)
print(paste("Dispersion Parameter:", dispersion_param))
if (var(df$CSM_cases) > mean(df$CSM_cases)) {
  print("Overdispersion is present.")
}
print(ggplot(df, aes(x = CSM_cases)) +
        geom_histogram(bins = 30, fill = "red", alpha = 0.7) +
        labs(title = "Histogram of CSM Cases (Zero Inflation Check)", x = "CSM Cases", y = "Frequency") +
        theme_minimal())

# Spatial Distribution of CSM Cases (Regional Analysis)
ggplot(df, aes(x = Region, y = CSM_cases)) +
  geom_boxplot(fill = "blue", alpha = 0.7) +
  labs(title = "CSM Cases by Region", x = "Region", y = "CSM Cases") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal()

# ===================================================
# ============== Model Building =====================
# ===================================================

# Fit Quasi-Poisson Model with Average Rainfall
quasi_poisson_model <- glm(CSM_cases ~ Maximum_Temperature + Average_Temperature + Average_Rainfall, 
                           family = quasipoisson, data = df)
print(summary(quasi_poisson_model))

# Function to apply DLNM for lagged effects of temperature
apply_dlnm <- function(data, response, predictor, lag_max = 30) {
  print("Fitting DLNM model...")
  
  # Convert time variable to Date format
  data$Date <- as.Date(data$Date)
  
  # Define cross-basis function for the predictor (e.g., temperature)
  cb <- crossbasis(data[[predictor]], lag = c(0, lag_max),
                   argvar = list(fun = "ns", df = 3),
                   arglag = list(fun = "ns", df = 3))
  
  # Fit the DLNM model
  dlnm_model <- glm(data[[response]] ~ cb, family = quasipoisson(), data = data)
  
  print("DLNM Model Summary:")
  print(summary(dlnm_model))
  
  # Predict effects
  pred <- crosspred(cb, dlnm_model, at = seq(min(data[[predictor]]), max(data[[predictor]]), length = 100))
  
  # Plot DLNM predictions
  plot(pred, xlab = predictor, zlab = "Relative Risk", main = "DLNM Prediction")
  
  return(dlnm_model)
}

# Apply DLNM model
dlnm_model <- apply_dlnm(df, response = "CSM_cases", predictor = "Average_Temperature", lag_max = 30)

# Function to test different lag structures in DLNM
test_lag_effects <- function(data, response, predictor, lag_ranges) {
  for (lags in lag_ranges) {
    cat("\nTesting lag range:", lags, "\n")
    
    cb <- crossbasis(data[[predictor]], lag = lags,
                     argvar = list(fun = "ns", df = 3),
                     arglag = list(fun = "ns", df = 3))
    
    model <- glm(data[[response]] ~ cb, family = quasipoisson(), data = data)
    print(summary(model))
    
    pred <- crosspred(cb, model, at = seq(min(data[[predictor]]),
                                          max(data[[predictor]]), length = 100))
    
    plot(pred, xlab = predictor, zlab = "Relative Risk", main = paste("Lag", lags[1], "-", lags[2]))
  }
}

# Define different lag structures to test
lag_ranges <- list(c(0, 7), c(0, 14), c(0, 21), c(0, 30))

# Execute lag analysis
test_lag_effects(df, response = "CSM_cases", predictor = "Average_Temperature", lag_ranges = lag_ranges)

# Summarize DLNM model
summary(dlnm_model)

