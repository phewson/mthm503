# setup_project.R
# MTHM053 Coursework - One-time Project Setup
# Run this script ONCE to set up your project environment

cat("=== MTHM053 COURSEWORK PROJECT SETUP ===\n")

# Install and initialize renv (required by coursework)
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

library(renv)

# Initialize renv for dependency management
cat("Initializing renv for dependency management...\n")
renv::init()

# Install required packages
required_packages <- c(
  "targets", "tarchetypes",
  "DBI", "RPostgres", 
  "dplyr", "ggplot2", "readr", "tidyr", "lubridate",
  "randomForest", "caret", "pROC", "e1071",
  "cluster", "factoextra", "FactoMineR",
  "corrplot", "VIM", "mice",
  "testthat", "lintr",
  "rmarkdown", "knitr"
)

cat("Installing required packages...\n")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("Installing", pkg, "...\n")
    install.packages(pkg)
  }
}

# Take a snapshot to lock the environment
cat("Creating renv snapshot...\n")
renv::snapshot()

# Create necessary directories
directories <- c("R", "scripts", "tests/testthat", "data", "output", "reports")
for (dir in directories) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    cat("Created directory:", dir, "\n")
  }
}

# Install targets if not available
if (!requireNamespace("targets", quietly = TRUE)) {
  install.packages("targets")
}

cat("\n=== PROJECT SETUP COMPLETE ===\n")
cat("✓ renv initialized and packages installed\n")
cat("✓ Directory structure created\n")
cat("✓ Ready to proceed with coursework\n")

cat("\nNext steps:\n")
cat("1. Create the files as instructed\n")
cat("2. Run scripts/01_run_exploration.R to test database connection\n")
cat("3. Use targets::tar_make() to run the pipeline\n")

# Check git status
if (file.exists(".git")) {
  cat("\n✓ Git repository detected\n")
  cat("Remember to commit your changes regularly!\n")
} else {
  cat("\n⚠ No git repository detected\n")
  cat("Make sure you're working in your forked repository!\n")
}