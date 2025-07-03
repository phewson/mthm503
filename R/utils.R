# R/utils.R
# Utility functions for the analysis

#' Create analysis summary
#' @param classification_results Classification results
#' @param regression_results Regression results
#' @param unsupervised_results Unsupervised learning results
#' @return Summary data frame
create_analysis_summary <- function(classification_results, regression_results, unsupervised_results) {
  summary_df <- data.frame(
    Task = c("Task 1: Classification", "Task 2: Regression", "Task 3: Unsupervised"),
    Best_Model = c(
      ifelse(classification_results$rf_accuracy > classification_results$dt_accuracy, 
             "Random Forest", "Decision Tree"),
      ifelse(regression_results$aic_comparison$AIC[1] < regression_results$aic_comparison$AIC[2], 
             "GLM (Poisson)", "GAM"),
      "K-means Clustering"
    ),
    Performance_Metric = c(
      paste("Accuracy:", round(max(classification_results$rf_accuracy, classification_results$dt_accuracy), 4)),
      paste("AIC:", round(min(regression_results$aic_comparison$AIC), 2)),
      paste("Silhouette Score:", round(unsupervised_results$silhouette_kmeans, 4))
    ),
    Key_Finding = c(
      "Random Forest shows better performance for pedestrian severity prediction",
      "Age and sex interactions significantly affect extrication rates",
      "Natural groupings identified in olive oil composition data"
    )
  )
  
  summary_df
}

#' Print analysis summary
#' @param summary_df Summary data frame
print_analysis_summary <- function(summary_df) {
  cat("=== ANALYSIS SUMMARY ===\n")
  for (i in 1:nrow(summary_df)) {
    cat("\n", summary_df$Task[i], ":\n")
    cat("  Best Model:", summary_df$Best_Model[i], "\n")
    cat("  Performance:", summary_df$Performance_Metric[i], "\n")
    cat("  Key Finding:", summary_df$Key_Finding[i], "\n")
  }
  cat("\n=== END SUMMARY ===\n")
}

#' Validate data quality
#' @param data Data frame to validate
#' @param task_name Name of the task for reporting
validate_data_quality <- function(data, task_name) {
  cat("=== DATA QUALITY CHECK FOR", task_name, "===\n")
  cat("Dimensions:", dim(data), "\n")
  cat("Missing values:", sum(is.na(data)), "\n")
  cat("Complete cases:", sum(complete.cases(data)), "\n")
  
  if (sum(is.na(data)) > 0) {
    cat("Warning: Missing values detected\n")
  }
  
  if (nrow(data) == 0) {
    stop("Error: Empty dataset")
  }
  
  cat("Data quality check passed\n\n")
}

#' Save results to file
#' @param results Analysis results
#' @param filename Output filename
save_results <- function(results, filename) {
  saveRDS(results, file = filename)
  cat("Results saved to:", filename, "\n")
}

#' Load results from file
#' @param filename Input filename
#' @return Loaded results
load_results <- function(filename) {
  if (file.exists(filename)) {
    results <- readRDS(filename)
    cat("Results loaded from:", filename, "\n")
    return(results)
  } else {
    stop("File not found:", filename)
  }
}
