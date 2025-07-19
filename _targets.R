# _targets.R
library(targets)
library(tarchetypes)

# Source functions
source("R/01_database_exploration.R")

# Set target options
tar_option_set(
  packages = c("DBI", "RPostgres", "dplyr", "ggplot2", "randomForest", "caret", 
               "cluster", "factoextra", "corrplot", "nnet", "pROC", "broom", "MASS"),
  format = "rds"
)

list(
  # === DATA EXTRACTION ===
  tar_target(pedestrian_data, {
    con <- connect_to_database()
    data <- get_pedestrian_casualties_data(con)
    disconnect_database(con)
    data
  }),
  
  tar_target(fire_rescue_data, {
    con <- connect_to_database()
    data <- get_fire_rescue_data(con)
    disconnect_database(con)
    data
  }),
  
  tar_target(olive_oil_data, {
    con <- connect_to_database()
    data <- get_olive_oil_data(con)
    disconnect_database(con)
    data
  }),
  
  # === TASK 1: CLASSIFICATION ===
  tar_target(pedestrian_clean, {
    pedestrian_data %>% 
      filter(!is.na(casualty_severity)) %>%
      mutate(
        severity = factor(casualty_severity, levels = c("Fatal", "Serious", "Slight")),
        sex = factor(sex_of_casualty),
        age = as.numeric(age_of_casualty),
        light = factor(light_conditions),
        weather = factor(weather_conditions),
        location = factor(urban_or_rural_area)
      ) %>%
      select(severity, sex, age, light, weather, location) %>%
      filter(complete.cases(.))
  }),
  
  tar_target(classification_model, {
    set.seed(42)
    train_idx <- sample(nrow(pedestrian_clean), 0.7 * nrow(pedestrian_clean))
    train_data <- pedestrian_clean[train_idx, ]
    test_data <- pedestrian_clean[-train_idx, ]
    
    # Train Random Forest
    rf_model <- randomForest(severity ~ ., data = train_data, ntree = 100)
    rf_predictions <- predict(rf_model, test_data)
    rf_accuracy <- mean(rf_predictions == test_data$severity)
    
    # Train SVM
    svm_model <- e1071::svm(severity ~ ., data = train_data, kernel = "radial")
    svm_predictions <- predict(svm_model, test_data)
    svm_accuracy <- mean(svm_predictions == test_data$severity)
    
    # Confusion Matrix
    rf_cm <- confusionMatrix(rf_predictions, test_data$severity)
    svm_cm <- confusionMatrix(svm_predictions, test_data$severity)
    
    # ROC Analysis (multiclass)
    rf_prob <- predict(rf_model, test_data, type = "prob")
    
    # Calculate multiclass AUC
    rf_auc <- multiclass.roc(test_data$severity, rf_prob)$auc[1]
    
    # Per-class metrics
    rf_precision <- rf_cm$byClass[, "Precision"]
    rf_recall <- rf_cm$byClass[, "Recall"]
    rf_f1 <- rf_cm$byClass[, "F1"]
    
    svm_precision <- svm_cm$byClass[, "Precision"]
    svm_recall <- svm_cm$byClass[, "Recall"]
    svm_f1 <- svm_cm$byClass[, "F1"]
    
    list(
      rf_model = rf_model, svm_model = svm_model,
      rf_accuracy = rf_accuracy, svm_accuracy = svm_accuracy,
      rf_cm = rf_cm, svm_cm = svm_cm,
      rf_auc = rf_auc,
      rf_precision = rf_precision, rf_recall = rf_recall, rf_f1 = rf_f1,
      svm_precision = svm_precision, svm_recall = svm_recall, svm_f1 = svm_f1,
      train_data = train_data, test_data = test_data,
      rf_predictions = rf_predictions, svm_predictions = svm_predictions,
      rf_prob = rf_prob
    )
  }),
  
  # === TASK 2: REGRESSION ===
  tar_target(regression_model, {
    fire_clean <- fire_rescue_data %>%
      filter(extrication != "Unknown", sex != "Unknown") %>%
      mutate(
        extrication_method = factor(extrication),
        sex = factor(sex),
        age_band = factor(age_band)
      )
    
    # Multinomial logistic regression
    model <- nnet::multinom(extrication_method ~ sex + age_band, data = fire_clean, trace = FALSE)
    predictions <- predict(model, fire_clean)
    accuracy <- mean(predictions == fire_clean$extrication_method)
    
    # Model diagnostics
    model_summary <- summary(model)
    aic_value <- AIC(model)
    deviance_value <- deviance(model)
    
    # Residuals
    residuals <- residuals(model)
    fitted_values <- fitted(model)
    
    # Confidence intervals for coefficients
    coef_ci <- confint(model)
    
    # Pseudo R-squared (McFadden's)
    null_model <- nnet::multinom(extrication_method ~ 1, data = fire_clean, trace = FALSE)
    pseudo_r2 <- 1 - (deviance(model) / deviance(null_model))
    
    list(
      model = model, accuracy = accuracy, data = fire_clean, 
      predictions = predictions, model_summary = model_summary,
      aic = aic_value, deviance = deviance_value, pseudo_r2 = pseudo_r2,
      residuals = residuals, fitted_values = fitted_values,
      coef_ci = coef_ci, null_model = null_model
    )
  }),
  
  # === TASK 3: UNSUPERVISED LEARNING ===
  tar_target(clustering_results, {
    # Use the most basic approach - manually select columns we know exist
    fatty_acids <- olive_oil_data[, c("palmitic", "palmitoleic", "stearic", "oleic", "linoleic", "linolenic", "arachidic", "eicosenoic")]
    
    # Remove any rows with missing values
    fatty_acids <- fatty_acids[complete.cases(fatty_acids), ]
    
    # Scale the data
    scaled_data <- scale(fatty_acids)
    
    # PCA
    pca <- prcomp(scaled_data)
    var_explained <- summary(pca)$importance[2, 1:3] * 100
    
    # K-means clustering
    set.seed(42)
    kmeans_result <- kmeans(scaled_data, centers = 3, nstart = 20)
    
    # Silhouette analysis
    sil_kmeans <- silhouette(kmeans_result$cluster, dist(scaled_data))
    avg_sil_kmeans <- mean(sil_kmeans[, 3])
    
    list(
      pca = pca, 
      var_explained = var_explained,
      kmeans = kmeans_result,
      avg_sil_kmeans = avg_sil_kmeans,
      sil_kmeans = sil_kmeans,
      scaled_data = scaled_data, 
      original_data = fatty_acids
    )
  }),
  
  # === ENHANCED PLOTS ===
  tar_target(create_plots, {
    if(!dir.exists("output")) dir.create("output")
    
    # 1. Classification plots
    # Distribution plot
    p1 <- ggplot(pedestrian_clean, aes(x = severity, fill = severity)) +
      geom_bar() +
      labs(title = "Pedestrian Casualty Severity Distribution") +
      theme_minimal()
    ggsave("output/classification_plot.png", p1, width = 8, height = 6)
    
    # Confusion Matrix Heatmap
    cm_data <- as.data.frame(classification_model$rf_cm$table)
    p2 <- ggplot(cm_data, aes(x = Prediction, y = Reference, fill = Freq)) +
      geom_tile() +
      geom_text(aes(label = Freq), color = "white", size = 4) +
      scale_fill_gradient(low = "lightblue", high = "darkblue") +
      labs(title = "Random Forest Confusion Matrix") +
      theme_minimal()
    ggsave("output/confusion_matrix.png", p2, width = 8, height = 6)
    
    # Feature Importance Plot
    importance_data <- data.frame(
      Feature = names(importance(classification_model$rf_model)[, 1]),
      Importance = importance(classification_model$rf_model)[, 1]
    )
    p3 <- ggplot(importance_data, aes(x = reorder(Feature, Importance), y = Importance)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(title = "Random Forest Feature Importance", x = "Features", y = "Mean Decrease Gini") +
      theme_minimal()
    ggsave("output/feature_importance.png", p3, width = 8, height = 6)
    
    # ROC Curve (simplified)
    test_data_binary <- classification_model$test_data
    test_data_binary$severity_binary <- ifelse(test_data_binary$severity == "Fatal", "Fatal", "Non-Fatal")
    rf_prob_binary <- classification_model$rf_prob[, "Fatal"]
    
    roc_obj <- roc(test_data_binary$severity_binary, rf_prob_binary)
    roc_data <- data.frame(
      sensitivity = roc_obj$sensitivities,
      specificity = roc_obj$specificities
    )
    
    p4 <- ggplot(roc_data, aes(x = 1 - specificity, y = sensitivity)) +
      geom_line(color = "red", linewidth = 1) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
      labs(title = paste("ROC Curve (AUC =", round(auc(roc_obj), 3), ")"),
           x = "False Positive Rate", y = "True Positive Rate") +
      theme_minimal()
    ggsave("output/roc_curve.png", p4, width = 8, height = 6)
    
    # 2. Regression plots
    reg_data <- regression_model$data
    p5 <- ggplot(reg_data, aes(x = extrication_method, fill = sex)) +
      geom_bar(position = "dodge") +
      labs(title = "Fire Rescue Extrication Methods by Sex") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggsave("output/regression_plot.png", p5, width = 10, height = 6)
    
    # Simplified residuals plot
    fitted_vals <- as.numeric(fitted(regression_model$model))
    residual_vals <- as.numeric(residuals(regression_model$model))
    
    # Take only the first component if it's a matrix
    if(is.matrix(fitted_vals)) fitted_vals <- fitted_vals[,1]
    if(is.matrix(residual_vals)) residual_vals <- residual_vals[,1]
    
    residual_data <- data.frame(
      fitted = fitted_vals,
      residuals = residual_vals
    )
    
    p6 <- ggplot(residual_data, aes(x = fitted, y = residuals)) +
      geom_point(alpha = 0.6) +
      geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
      labs(title = "Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals") +
      theme_minimal()
    ggsave("output/residuals_plot.png", p6, width = 8, height = 6)
    
    # 3. Clustering plots
    pca_df <- data.frame(
      PC1 = clustering_results$pca$x[, 1],
      PC2 = clustering_results$pca$x[, 2],
      Cluster = factor(clustering_results$kmeans$cluster)
    )
    p7 <- ggplot(pca_df, aes(x = PC1, y = PC2, color = Cluster)) +
      geom_point(alpha = 0.7) +
      labs(title = "Olive Oil Clustering (PCA Space)",
           x = paste0("PC1 (", round(clustering_results$var_explained[1], 1), "%)"),
           y = paste0("PC2 (", round(clustering_results$var_explained[2], 1), "%)")) +
      theme_minimal()
    ggsave("output/clustering_plot.png", p7, width = 8, height = 6)
    
    # Silhouette Plot
    sil_data <- data.frame(
      cluster = clustering_results$sil_kmeans[, 1],
      silhouette = clustering_results$sil_kmeans[, 3]
    )
    p8 <- ggplot(sil_data, aes(x = reorder(1:nrow(sil_data), silhouette), y = silhouette, fill = factor(cluster))) +
      geom_col() +
      labs(title = paste("Silhouette Plot (Average =", round(clustering_results$avg_sil_kmeans, 3), ")"),
           x = "Sample", y = "Silhouette Width") +
      theme_minimal() +
      theme(axis.text.x = element_blank())
    ggsave("output/silhouette_plot.png", p8, width = 10, height = 6)
    
    # PCA Variance Explained
    var_data <- data.frame(
      PC = 1:3,
      variance = clustering_results$var_explained
    )
    p9 <- ggplot(var_data, aes(x = PC, y = variance)) +
      geom_col(fill = "lightblue") +
      labs(title = "PCA Variance Explained", x = "Principal Component", y = "Variance Explained (%)") +
      theme_minimal()
    ggsave("output/pca_variance.png", p9, width = 8, height = 6)
    
    "All plots created successfully"
  }),
  
  # === FINAL RESULTS ===
  tar_target(final_results, {
    results <- list(
      # Classification metrics
      rf_accuracy = classification_model$rf_accuracy,
      svm_accuracy = classification_model$svm_accuracy,
      rf_auc = classification_model$rf_auc,
      rf_precision = classification_model$rf_precision,
      rf_recall = classification_model$rf_recall,
      rf_f1 = classification_model$rf_f1,
      
      # Regression metrics
      regression_accuracy = regression_model$accuracy,
      pseudo_r2 = regression_model$pseudo_r2,
      aic = regression_model$aic,
      deviance = regression_model$deviance,
      
      # Clustering metrics
      optimal_k = 3,  # Fixed since we used k=3 directly
      avg_silhouette = clustering_results$avg_sil_kmeans,
      pca_variance_explained = sum(clustering_results$var_explained),
      
      # Sample sizes
      pedestrian_samples = nrow(classification_model$test_data),
      fire_rescue_samples = nrow(regression_model$data),
      olive_oil_samples = nrow(clustering_results$original_data)
    )
    
    # Save results
    if(!dir.exists("data")) dir.create("data")
    saveRDS(results, "data/final_results.rds")
    readr::write_csv(pedestrian_clean, "data/pedestrian_clean.csv")
    readr::write_csv(regression_model$data, "data/fire_rescue_clean.csv")
    readr::write_csv(clustering_results$original_data, "data/olive_oil_clean.csv")
    
    cat("=== ENHANCED FINAL RESULTS ===\n")
    cat("CLASSIFICATION:\n")
    cat("- Random Forest Accuracy:", round(results$rf_accuracy, 3), "\n")
    cat("- SVM Accuracy:", round(results$svm_accuracy, 3), "\n")
    cat("- Random Forest AUC:", round(results$rf_auc, 3), "\n")
    cat("- RF Precision (avg):", round(mean(results$rf_precision, na.rm = TRUE), 3), "\n")
    cat("- RF Recall (avg):", round(mean(results$rf_recall, na.rm = TRUE), 3), "\n")
    cat("- RF F1 (avg):", round(mean(results$rf_f1, na.rm = TRUE), 3), "\n")
    
    cat("\nREGRESSION:\n")
    cat("- Accuracy:", round(results$regression_accuracy, 3), "\n")
    cat("- Pseudo R²:", round(results$pseudo_r2, 3), "\n")
    cat("- AIC:", round(results$aic, 1), "\n")
    cat("- Deviance:", round(results$deviance, 1), "\n")
    
    cat("\nCLUSTERING:\n")
    cat("- Optimal k:", results$optimal_k, "\n")
    cat("- Average Silhouette:", round(results$avg_silhouette, 3), "\n")
    cat("- PCA Variance (first 3 PCs):", round(results$pca_variance_explained, 1), "%\n")
    
    cat("\n=== PIPELINE COMPLETED SUCCESSFULLY ===\n")
    
    results
  }, packages = "readr")
)