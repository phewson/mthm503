# scripts/03_install_packages.R
# MTHM053 Coursework - Install Required Packages

cat("=== INSTALLING REQUIRED PACKAGES ===\n")

# List of all required packages for the coursework
required_packages <- c(
  # Core packages
  "targets", "tarchetypes", "renv",
  
  # Database and data manipulation
  "DBI", "RPostgres", "dplyr", "tidyr", "readr", "here",
  
  # Date and time
  "lubridate",
  
  # Visualization
  "ggplot2", "corrplot",
  
  # Machine learning - Classification
  "randomForest", "caret", "pROC", "e1071",
  
  # Machine learning - Regression
  "nnet", "MASS", "broom",
  
  # Machine learning - Unsupervised
  "cluster", "factoextra", "FactoMineR", "dbscan",
  
  # String manipulation
  "stringr",
  
  # Testing
  "testthat", "lintr",
  
  # Reporting
  "rmarkdown", "knitr"
)

# Function to install packages if not already installed
install_if_missing <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      cat("Installing", pkg, "...\n")
      tryCatch({
        install.packages(pkg)
        cat("✓", pkg, "installed successfully\n")
      }, error = function(e) {
        cat("❌ Failed to install", pkg, ":", e$message, "\n")
      })
    } else {
      cat("✓", pkg, "already installed\n")
    }
  }
}

# Install packages
install_if_missing(required_packages)

# Check if all packages can be loaded
cat("\n=== TESTING PACKAGE LOADING ===\n")
failed_packages <- c()

for (pkg in required_packages) {
  result <- tryCatch({
    library(pkg, character.only = TRUE)
    TRUE
  }, error = function(e) {
    failed_packages <<- c(failed_packages, pkg)
    FALSE
  })
  
  if (result) {
    cat("✓", pkg, "loads successfully\n")
  } else {
    cat("❌", pkg, "failed to load\n")
  }
}

# Summary
if (length(failed_packages) == 0) {
  cat("\n✅ ALL PACKAGES INSTALLED AND WORKING\n")
  cat("You can now run the complete pipeline!\n")
} else {
  cat("\n⚠ SOME PACKAGES FAILED:\n")
  cat(paste(failed_packages, collapse = ", "), "\n")
  cat("Please install these manually or check for system dependencies\n")
}

# Update renv snapshot
cat("\n=== UPDATING RENV SNAPSHOT ===\n")
tryCatch({
  renv::snapshot(prompt = FALSE)
  cat("✓ renv snapshot updated\n")
}, error = function(e) {
  cat("❌ Failed to update renv snapshot:", e$message, "\n")
})

cat("\n=== PACKAGE INSTALLATION COMPLETE ===\n")