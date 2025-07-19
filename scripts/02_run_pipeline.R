# scripts/02_run_pipeline.R
# MTHM053 Coursework - Run Complete Targets Pipeline

# Load required packages
library(targets)

cat("=== MTHM053 COURSEWORK PIPELINE EXECUTION ===\n")

# Check that all required files exist
required_files <- c(
  "R/01_database_exploration.R",
  "R/02_data_processing.R", 
  "R/03_classification_models.R",
  "R/04_regression_models.R",
  "R/05_unsupervised_models.R",
  "R/06_final_results.R",
  "_targets.R"
)

missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  cat("❌ Missing required files:\n")
  cat(paste(missing_files, collapse = "\n"))
  stop("Please create all required files before running the pipeline")
}

cat("✓ All required files found\n")

# Visualize the pipeline before running
cat("\n=== PIPELINE VISUALIZATION ===\n")
cat("Generating pipeline network diagram...\n")
targets::tar_visnetwork()

# Run the complete pipeline
cat("\n=== RUNNING COMPLETE PIPELINE ===\n")
cat("This will:\n")
cat("1. Connect to database and extract data\n")
cat("2. Clean and preprocess all datasets\n") 
cat("3. Train classification models (Random Forest & SVM)\n")
cat("4. Fit regression model (Multinomial Logistic)\n")
cat("5. Perform unsupervised learning (PCA & Clustering)\n")
cat("6. Generate all plots and save results\n")
cat("7. Compile final results summary\n\n")

tryCatch({
  # Execute the pipeline
  targets::tar_make()
  
  cat("\n=== ✅ PIPELINE COMPLETED SUCCESSFULLY ===\n")
  
  # Show what was created
  cat("\n=== OUTPUTS CREATED ===\n")
  
  if (dir.exists("data")) {
    data_files <- list.files("data", full.names = FALSE)
    cat("Data files saved:\n")
    cat(paste(" -", data_files), sep = "\n")
  }
  
  if (dir.exists("output")) {
    plot_files <- list.files("output", pattern = "*.png", full.names = FALSE)
    cat("\nPlots saved:\n")
    cat(paste(" -", plot_files), sep = "\n")
  }
  
  # Load and display final results
  if (file.exists("data/final_results.rds")) {
    cat("\n=== FINAL RESULTS SUMMARY ===\n")
    final_results <- readRDS("data/final_results.rds")
    
    cat("Classification Task:\n")
    cat("  Best Model:", final_results$classification$best_model, "\n")
    cat("  Accuracy:", round(final_results$classification$best_accuracy, 3), "\n")
    
    cat("Regression Task:\n") 
    cat("  Model Type:", final_results$regression$model_type, "\n")
    cat("  Accuracy:", round(final_results$regression$accuracy, 3), "\n")
    
    cat("Unsupervised Learning Task:\n")
    cat("  Optimal Clusters:", final_results$unsupervised$optimal_clusters, "\n")
    cat("  Silhouette Score:", round(final_results$unsupervised$best_silhouette, 3), "\n")
    
    cat("\nTotal Samples Analyzed:", final_results$project_summary$total_samples, "\n")
  }
  
  cat("\n=== NEXT STEPS ===\n")
  cat("1. Review generated plots in output/ folder\n")
  cat("2. Check saved data in data/ folder\n")
  cat("3. Run tests with: testthat::test_dir('tests')\n")
  cat("4. Generate final report\n")
  
}, error = function(e) {
  cat("❌ Pipeline failed with error:\n")
  cat(e$message, "\n")
  cat("\nTroubleshooting:\n")
  cat("1. Check database connection\n")
  cat("2. Ensure all packages are installed\n")
  cat("3. Check for missing dependencies\n")
  cat("4. Review targets pipeline with: tar_manifest()\n")
})

cat("\n=== PIPELINE EXECUTION COMPLETE ===\n")