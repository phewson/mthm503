# R/06_final_results.R
# MTHM053 Coursework - Final Results Compilation

#' Compile final results from all three tasks
#' @param pedestrian_results classification results
#' @param fire_rescue_results regression results
#' @param olive_oil_results unsupervised learning results
#' @return compiled final results
compile_final_results <- function(pedestrian_results, fire_rescue_results, olive_oil_results) {
  
  # Extract key metrics from each task
  
  # Task 1: Classification
  classification_summary <- list(
    task = "Pedestrian Casualty Severity Classification",
    models_tested = names(pedestrian_results),
    best_model = names(which.max(sapply(pedestrian_results, function(x) x$accuracy))),
    best_accuracy = max(sapply(pedestrian_results, function(x) x$accuracy)),
    sample_size = length(pedestrian_results[[1]]$actual),
    target_classes = c("Fatal", "Serious", "Slight")
  )
  
  # Task 2: Regression
  regression_summary <- list(
    task = "Fire Rescue Extrication Method Analysis",
    model_type = "Multinomial Logistic Regression",
    accuracy = fire_rescue_results$accuracy,
    sample_size = length(fire_rescue_results$actual),
    predictors = c("sex", "age_band", "n_casualties"),
    target_categories = levels(fire_rescue_results$actual)
  )
  
  # Task 3: Unsupervised Learning
  unsupervised_summary <- list(
    task = "Olive Oil Composition Clustering",
    methods_used = c("K-means", "Hierarchical", "DBSCAN"),
    optimal_clusters = length(unique(olive_oil_results$cluster_data$kmeans_cluster)),
    best_silhouette = olive_oil_results$kmeans_silhouette,
    sample_size = nrow(olive_oil_results$cluster_data),
    features_used = 8  # 8 fatty acids
  )
  
  # Overall project summary
  project_summary <- list(
    total_datasets = 3,
    total_samples = classification_summary$sample_size + 
                   regression_summary$sample_size + 
                   unsupervised_summary$sample_size,
    methods_implemented = c("Random Forest", "SVM", "Multinomial Logistic Regression",
                           "PCA", "K-means", "Hierarchical Clustering", "DBSCAN"),
    key_findings = list(
      classification = paste("Best model:", classification_summary$best_model, 
                            "with", round(classification_summary$best_accuracy, 3), "accuracy"),
      regression = paste("Regression model achieved", 
                        round(regression_summary$accuracy, 3), "accuracy"),
      unsupervised = paste("Optimal clustering found", 
                          unsupervised_summary$optimal_clusters, "clusters with silhouette score",
                          round(unsupervised_summary$best_silhouette, 3))
    )
  )
  
  final_results <- list(
    classification = classification_summary,
    regression = regression_summary,
    unsupervised = unsupervised_summary,
    project_summary = project_summary,
    timestamp = Sys.time()
  )
  
  # Print summary
  cat("\n=== MTHM053 COURSEWORK FINAL RESULTS ===\n")
  cat("1. CLASSIFICATION:", classification_summary$best_model, 
      "achieved", round(classification_summary$best_accuracy, 3), "accuracy\n")
  cat("2. REGRESSION: Model achieved", round(regression_summary$accuracy, 3), "accuracy\n")
  cat("3. UNSUPERVISED:", unsupervised_summary$optimal_clusters, 
      "clusters with silhouette score", round(unsupervised_summary$best_silhouette, 3), "\n")
  cat("Total samples analyzed:", project_summary$total_samples, "\n")
  cat("Methods implemented:", length(project_summary$methods_implemented), "\n")
  cat("===============================================\n")
  
  return(final_results)
}