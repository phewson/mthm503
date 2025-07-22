# scripts/01_run_exploration.R
# MTHM053 Coursework - Run Database Exploration

# Load required packages quietly
suppressPackageStartupMessages({
  library(DBI)
  library(RPostgres) 
  library(dplyr)
})

cat("=== MTHM053 DATABASE EXPLORATION ===\n")

# Source our functions
source("R/01_database_exploration.R")

# Test database connection and explore data
tryCatch({
  con <- connect_to_database()
  cat("✓ Database connection successful!\n")
  
  # Task-specific data exploration
  cat("\n=== COURSEWORK DATA SUMMARY ===\n")
  
  # Task 1: Classification - Pedestrian casualties
  cat("\n1. CLASSIFICATION TASK: Pedestrian Casualty Severity\n")
  pedestrian_severity <- DBI::dbGetQuery(con, "
    SELECT casualty_severity, COUNT(*) as count
    FROM stats19_casualties 
    WHERE casualty_class = 'Pedestrian'
    GROUP BY casualty_severity
    ORDER BY count DESC
  ")
  print(pedestrian_severity)
  
  # Task 2: Regression - Fire rescue 
  cat("\n2. REGRESSION TASK: Fire Rescue Extrications\n")
  fire_summary <- DBI::dbGetQuery(con, "
    SELECT extrication, COUNT(*) as count
    FROM fire_rescue_extrication_casualties
    GROUP BY extrication
    LIMIT 5
  ")
  print(fire_summary)
  
  # Task 3: Unsupervised Learning - Olive oil
  cat("\n3. UNSUPERVISED LEARNING: Olive Oil Composition\n")
  olive_summary <- DBI::dbGetQuery(con, "
    SELECT COUNT(*) as total_samples,
           ROUND(AVG(oleic)::numeric, 1) as avg_oleic,
           ROUND(AVG(palmitic)::numeric, 1) as avg_palmitic,
           ROUND(AVG(linoleic)::numeric, 1) as avg_linoleic
    FROM olive_oil
  ")
  print(olive_summary)
  
  # Test data extraction functions
  cat("\n=== TESTING DATA EXTRACTION ===\n")
  
  pedestrian_data <- get_pedestrian_casualties_data(con)
  cat("✓ Pedestrian data: ", nrow(pedestrian_data), " rows, ", ncol(pedestrian_data), " features\n")
  
  fire_data <- get_fire_rescue_data(con)
  cat("✓ Fire rescue data: ", nrow(fire_data), " rows, ", ncol(fire_data), " features\n")
  
  olive_data <- get_olive_oil_data(con)
  cat("✓ Olive oil data: ", nrow(olive_data), " rows, ", ncol(olive_data), " features\n")
  
  # Disconnect
  disconnect_database(con)
  
  cat("\n=== ✅ SUCCESS! ALL DATA READY ===\n")
  cat("Classification: Predict pedestrian injury severity (Fatal/Serious/Slight)\n")
  cat("Regression: Analyze fire rescue extrication methods by age/sex\n")
  cat("Unsupervised: Cluster olive oils by fatty acid composition\n")
  
}, error = function(e) {
  cat("❌ Error:", e$message, "\n")
})

cat("\n=== NEXT: Setup Targets Pipeline ===\n")