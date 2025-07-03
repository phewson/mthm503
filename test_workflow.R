# test_workflow.R
# Simple test script to verify workflow setup

cat("=== TESTING WORKFLOW SETUP ===\n")

# Test 1: Check if targets package is available
cat("1. Testing targets package...\n")
if (require(targets, quietly = TRUE)) {
  cat("   ✓ targets package loaded successfully\n")
} else {
  cat("   ✗ targets package not available\n")
  stop("Please install targets package")
}

# Test 2: Check if _targets.R exists
cat("2. Testing _targets.R file...\n")
if (file.exists("_targets.R")) {
  cat("   ✓ _targets.R file found\n")
} else {
  cat("   ✗ _targets.R file not found\n")
  stop("_targets.R file is missing")
}

# Test 3: Check if R functions exist
cat("3. Testing R function files...\n")
required_files <- c(
  "R/task1_classification.R",
  "R/task2_regression.R", 
  "R/task3_unsupervised.R",
  "R/utils.R"
)

for (file in required_files) {
  if (file.exists(file)) {
    cat("   ✓", file, "found\n")
  } else {
    cat("   ✗", file, "missing\n")
  }
}

# Test 4: Check if test files exist
cat("4. Testing test files...\n")
test_files <- c(
  "tests/test_analysis.R",
  "tests/test_load_data.R"
)

for (file in test_files) {
  if (file.exists(file)) {
    cat("   ✓", file, "found\n")
  } else {
    cat("   ✗", file, "missing\n")
  }
}

# Test 5: Try to load targets
cat("5. Testing targets loading...\n")
tryCatch({
  source("_targets.R")
  cat("   ✓ _targets.R loaded successfully\n")
}, error = function(e) {
  cat("   ✗ Error loading _targets.R:", e$message, "\n")
})

# Test 6: Check target structure
cat("6. Testing target structure...\n")
tryCatch({
  tar_glimpse()
  cat("   ✓ Target structure displayed\n")
}, error = function(e) {
  cat("   ✗ Error displaying targets:", e$message, "\n")
})

cat("\n=== WORKFLOW TEST COMPLETE ===\n")
cat("If all tests passed, you can run: source('run_analysis.R')\n") 