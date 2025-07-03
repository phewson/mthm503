# run_analysis.R
# Main script to run the complete analysis

library(targets)
library(here)

# Set working directory to project root
setwd(here())

cat("=== MTHM503 DATA SCIENCE PROJECT ===\n")
cat("Starting complete analysis...\n\n")

# Load the targets pipeline
source("_targets.R")

# Run the complete analysis
cat("1. Running complete targets pipeline...\n")
tar_make()

cat("\n2. Checking target status...\n")
tar_glimpse()

cat("\n3. Loading results...\n")
tar_load(analysis_summary)

cat("\n4. Printing analysis summary...\n")
print(analysis_summary)

cat("\n5. Running tests...\n")
testthat::test_dir("tests/")

cat("\n6. Creating workflow visualization...\n")
source("visualize_workflow.R")

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("All tasks completed successfully!\n")
cat("Check the following outputs:\n")
cat("- _targets/ folder for cached results\n")
cat("- vignettes/ folder for reports\n")
cat("- tests/ folder for test results\n") 