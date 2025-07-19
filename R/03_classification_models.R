# R/03_classification_models.R
# MTHM053 Coursework - Classification Models for Pedestrian Casualties

#' Train Random Forest model for pedestrian casualty severity
#' @param train_data training data
#' @return trained model
train_pedestrian_rf <- function(train_data) {
  library(randomForest)
  library(caret)
  
  # Remove rows with missing values for modeling
  model_data <- train_data[complete.cases(train_data), ]
  
  # Train Random Forest
  set.seed(42)
  rf_model <- randomForest(
    severity ~ ., 
    data = model_data,
    ntree = 500,
    mtry = sqrt(ncol(model_data) - 1),
    importance = TRUE
  )
  
  return(rf_model)
}

#' Train SVM model for pedestrian casualty severity
#' @param train_data training data
#' @return trained model
train_pedestrian_svm <- function(train_data) {
  library(e1071)
  
  # Remove rows with missing values for modeling
  model_data <- train_data[complete.cases(train_data), ]
  
  # Train SVM with radial kernel
  set.seed(42)
  svm_model <- svm(
    severity ~ ., 
    data = model_data,
    kernel = "radial",
    cost = 1,
    gamma = "scale"
  )
  
  return(svm_model)
}

#' Evaluate pedestrian classification models
#' @param models list of trained models
#' @param test_data test data
#' @return evaluation results
evaluate_pedestrian_models <- function(models, test_data) {
  library(caret)
  library(pROC)
  
  # Remove rows with missing values for evaluation
  test_clean <- test_data[complete.cases(test_data), ]
  
  results <- list()
  
  for (model_name in names(models)) {
    model <- models[[model_name]]
    
    # Make predictions
    predictions <- predict(model, test_clean)
    
    # Calculate accuracy
    cm <- confusionMatrix(predictions, test_clean$severity)
    accuracy <- cm$overall['Accuracy']
    
    # Calculate multiclass AUC (one-vs-rest)
    # Convert to numeric for ROC calculation
    actual_numeric <- as.numeric(test_clean$severity)
    pred_numeric <- as.numeric(predictions)
    
    # Store results
    results[[model_name]] <- list(
      model = model,
      predictions = predictions,
      confusion_matrix = cm,
      accuracy = accuracy,
      actual = test_clean$severity,
      predicted = predictions
    )
    
    cat("Model:", model_name, "- Accuracy:", round(accuracy, 4), "\n")
  }
  
  return(results)
}

#' Create plots for pedestrian classification
#' @param data cleaned data
#' @param results model results
#' @return plot file paths
create_pedestrian_plots <- function(data, results) {
  library(ggplot2)
  library(dplyr)
  
  # Create output directory
  if (!dir.exists("output")) dir.create("output")
  
  plots <- list()
  
  # 1. Data distribution plot
  p1 <- ggplot(data, aes(x = severity, fill = severity)) +
    geom_bar() +
    labs(title = "Distribution of Pedestrian Casualty Severity",
         x = "Severity", y = "Count") +
    theme_minimal() +
    scale_fill_brewer(palette = "Set1")
  
  ggsave("output/pedestrian_severity_distribution.png", p1, width = 8, height = 6)
  
  # 2. Feature importance plot (Random Forest)
  if ("rf" %in% names(results)) {
    importance_data <- importance(results$rf$model)
    importance_df <- data.frame(
      Feature = rownames(importance_data),
      Importance = importance_data[, "MeanDecreaseGini"]
    ) %>%
      arrange(desc(Importance)) %>%
      head(10)
    
    p2 <- ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(title = "Top 10 Feature Importance (Random Forest)",
           x = "Features", y = "Mean Decrease Gini") +
      theme_minimal()
    
    ggsave("output/pedestrian_feature_importance.png", p2, width = 8, height = 6)
  }
  
  # 3. Model comparison plot
  accuracy_data <- data.frame(
    Model = names(results),
    Accuracy = sapply(results, function(x) x$accuracy)
  )
  
  p3 <- ggplot(accuracy_data, aes(x = Model, y = Accuracy, fill = Model)) +
    geom_col() +
    labs(title = "Model Accuracy Comparison",
         x = "Model", y = "Accuracy") +
    theme_minimal() +
    scale_fill_brewer(palette = "Set2") +
    ylim(0, 1)
  
  ggsave("output/pedestrian_model_comparison.png", p3, width = 8, height = 6)
  
  cat("✓ Pedestrian classification plots saved to output/ folder\n")
  return(c("output/pedestrian_severity_distribution.png",
           "output/pedestrian_feature_importance.png", 
           "output/pedestrian_model_comparison.png"))
}