library(readxl)
library(MASS)
library(mgcv)
library(dlnm)
library(ggplot2)
library(plotly)

analyze_csm <- function(data_path, response_variable, predictor_variables, time_variable, lag_max = 30) {
  # Load data
  data <- read_excel(data_path)
  
  # Ensure time variable is in Date format
  data[[time_variable]] <- as.Date(data[[time_variable]])
  
  # Handle missing values (omit rows with missing data)
  data <- na.omit(data)
  
  # Check and print the mean and variance of the response variable
  mean_csm <- mean(data[[response_variable]])
  var_csm <- var(data[[response_variable]])
  print(paste("Mean:", mean_csm, "Variance:", var_csm))
  
  # Check for overdispersion
  if (var_csm > mean_csm) {
    print("Overdispersion detected; Using Quasi-Poisson model.")
  } else {
    print("No overdispersion detected; Poisson model may suffice.")
  }
  
  # Fit the quasi-Poisson model
  formula <- as.formula(paste(response_variable, "~", paste(predictor_variables, collapse = " + ")))
  model <- glm(formula, family = quasipoisson, data = data)
  
  # Display the model summary
  print("Quasi-Poisson Model Summary:")
  print(summary(model))
  
  # Deviance and dispersion
  deviance <- model$deviance
  df_resid <- model$df.residual
  dispersion <- deviance / df_resid
  print(paste("Deviance:", deviance, "Residual Degrees of Freedom:", df_resid, "Dispersion:", dispersion))
  
  # Check residuals
  par(mfrow = c(2, 2))
  plot(model)
  
  # Predicted vs observed values
  data$Predicted_CSM <- predict(model, type = "response")
  
  ggplot(data, aes(x = Predicted_CSM, y = .data[[response_variable]])) +
    geom_point(color = "blue") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    theme_minimal() +
    labs(title = "Predicted vs Observed CSM", x = "Predicted CSM", y = "Observed CSM")
  
  # Visualize the effect of a key predictor
  ggplot(data, aes(x = Average_Temperature, y = Predicted_CSM)) +
    geom_point(color = "blue") +
    geom_smooth(method = "loess", color = "red") +
    theme_minimal() +
    labs(title = "Effect of Average Temperature on Predicted CSM",
         x = "Average Temperature", y = "Predicted CSM")
  
  # Split the data into training and test sets
  set.seed(123)
  train_indices <- sample(1:nrow(data), 0.8 * nrow(data))
  train_data <- data[train_indices, ]
  test_data <- data[-train_indices, ]
  
  # Refit the model on training data
  model_train <- glm(formula, family = quasipoisson, data = train_data)
  
  # Predict on test data
  test_data$Predicted_CSM <- predict(model_train, newdata = test_data, type = "response")
  
  # Calculate mean squared error
  mse <- mean((test_data[[response_variable]] - test_data$Predicted_CSM)^2)
  print(paste("Mean Squared Error:", mse))
  
  # Fit a quasi-Poisson model with interaction terms
  model_interaction <- glm(as.formula(paste(response_variable, "~", predictor_variables[1], "*", predictor_variables[4])),
                           family = quasipoisson, data = data)
  
  # Display the interaction model summary
  print("Interaction Model Summary:")
  print(summary(model_interaction))
  
  # Create a grid of predictor values
  rainfall_seq <- seq(min(data[[predictor_variables[1]]]), max(data[[predictor_variables[1]]]), length = 100)
  temperature_seq <- seq(min(data[[predictor_variables[4]]]), max(data[[predictor_variables[4]]]), length = 100)
  grid <- expand.grid(Average_Rainfall = rainfall_seq, Average_Temperature = temperature_seq)
  
  # Predict CSM based on the interaction model
  grid$Predicted_CSM <- predict(model_interaction, newdata = grid, type = "response")
  
  # Contour plot to visualize interaction
  ggplot(grid, aes(x = Average_Rainfall, y = Average_Temperature, z = Predicted_CSM)) +
    geom_contour_filled() +
    labs(title = "Interaction Effect of Rainfall and Temperature on CSM",
         x = "Average Rainfall", y = "Average Temperature", fill = "Predicted CSM") +
    theme_minimal()
  
  # 3D surface plot
  plot_ly(grid, x = ~Average_Rainfall, y = ~Average_Temperature, z = ~Predicted_CSM,
          type = 'surface', colors = colorRamp(c("blue", "red"))) %>%
    layout(title = "Interaction Effect of Rainfall and Temperature on CSM",
           scene = list(xaxis = list(title = "Average Rainfall"),
                        yaxis = list(title = "Average Temperature"),
                        zaxis = list(title = "Predicted CSM")))
  
  # DLNM Model
  print("Fitting DLNM model...")
  cb <- crossbasis(data[[predictor_variables[4]]], lag = c(0, lag_max),
                   argvar = list(fun = "ns", df = 3),
                   arglag = list(fun = "ns", df = 3))
  
  model_dlnm <- glm(data[[response_variable]] ~ cb, family = quasipoisson(), data = data)
  print("DLNM Model Summary:")
  print(summary(model_dlnm))
  
  # Predict and visualize the effects
  pred <- crosspred(cb, model_dlnm, at = seq(min(data[[predictor_variables[4]]]),
                                             max(data[[predictor_variables[4]]]), length = 100))
  
  # Plot DLNM predictions
  plot(pred, xlab = predictor_variables[4], zlab = "Relative Risk", main = "DLNM Prediction")
  
  # Testing Different Lags
  lag_ranges <- list(c(0, 7), c(0, 14), c(0, 21), c(0, 30))
  for (lags in lag_ranges) {
    cat("\nTesting lag range:", lags, "\n")
    
    cb <- crossbasis(data[[predictor_variables[4]]], lag = lags,
                     argvar = list(fun = "ns", df = 3),
                     arglag = list(fun = "ns", df = 3))
    
    model <- glm(data[[response_variable]] ~ cb, family = quasipoisson(), data = data)
    print(summary(model))
    
    pred <- crosspred(cb, model, at = seq(min(data[[predictor_variables[4]]]),
                                          max(data[[predictor_variables[4]]]), length = 100))
    
    plot(pred, xlab = "Temperature", zlab = "Relative Risk", main = paste("Lag", lags[1], "-", lags[2]))
  }
  
  # Exposure-Response Curve at specific lags
  plot(pred, type = "l", lag = 21, xlab = "Temperature", ylab = "Relative Risk",
       main = "Exposure-Response Curve at Lag = 21")
}

# Run the function
analyze_csm("C:/Users/kobby/Downloads/prof Eric R/CSM-Peter/CSM-Peter.xlsx", 
            response_variable = "CSM_cases", 
            predictor_variables = c("Average_Rainfall", "Maximum_Temperature", "Minimum_Temperature", "Average_Temperature"),
            time_variable = "Date", 
            lag_max = 30)
