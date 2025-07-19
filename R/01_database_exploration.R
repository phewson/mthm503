# R/01_database_exploration.R
# MTHM053 Coursework - Database Exploration
# Purpose: Explore the database structure and understand our datasets

#' Connect to the database
#' @return database connection object
connect_to_database <- function() {
  con <- DBI::dbConnect(
    RPostgres::Postgres(),
    dbname = "postgres",
    host = "aws-0-eu-west-2.pooler.supabase.com",
    user = "pgstudent.rvdwflidqvcvffdccwrh",
    password = "0%jkXK^tjMZwuG",
    port = 5432
  )
  return(con)
}

#' Explore database tables and structure
#' @param con database connection
#' @return list of table information
explore_database_structure <- function(con) {
  # Get all table names
  tables <- DBI::dbListTables(con)
  
  # Initialize results list
  table_info <- list()
  
  # Explore each relevant table
  for (table in tables) {
    table_info[[table]] <- list(
      columns = DBI::dbListFields(con, table),
      sample_data = DBI::dbGetQuery(con, paste("SELECT * FROM", table, "LIMIT 3")),
      row_count = DBI::dbGetQuery(con, paste("SELECT COUNT(*) as count FROM", table))$count
    )
  }
  
  return(table_info)
}

#' Get pedestrian casualties data for classification task
#' @param con database connection
#' @return tibble with pedestrian casualties data
get_pedestrian_casualties_data <- function(con) {
  # Get pedestrian data with accident and vehicle information
  query <- "
  SELECT 
    c.casualty_severity,
    c.sex_of_casualty,
    c.age_of_casualty,
    c.age_band_of_casualty,
    c.pedestrian_location,
    c.pedestrian_movement,
    a.obs_date,
    a.light_conditions,
    a.weather_conditions,
    a.urban_or_rural_area,
    a.road_surface_conditions,
    a.special_conditions_at_site,
    v.age_of_driver,
    v.sex_of_driver,
    v.vehicle_type
  FROM stats19_casualties c
  LEFT JOIN stats19_accidents a ON c.accident_index = a.accident_index
  LEFT JOIN stats19_vehicles v ON c.accident_index = v.accident_index
  WHERE c.casualty_class = 'Pedestrian'
  "
  
  data <- DBI::dbGetQuery(con, query)
  return(dplyr::as_tibble(data))
}

#' Get fire rescue extrications data for regression task
#' @param con database connection
#' @return tibble with extrications data
get_fire_rescue_data <- function(con) {
  query <- "SELECT * FROM fire_rescue_extrication_casualties"
  data <- DBI::dbGetQuery(con, query)
  return(dplyr::as_tibble(data))
}

#' Get olive oil data for unsupervised learning task
#' @param con database connection
#' @return tibble with olive oil composition data
get_olive_oil_data <- function(con) {
  query <- "SELECT * FROM olive_oil"
  data <- DBI::dbGetQuery(con, query)
  return(dplyr::as_tibble(data))
}

#' Disconnect from database
#' @param con database connection
disconnect_database <- function(con) {
  DBI::dbDisconnect(con)
}

#' Explore pedestrian data specifically
#' @param con database connection
explore_pedestrian_data <- function(con) {
  # Check unique values in casualty_class to find pedestrians
  casualty_classes <- DBI::dbGetQuery(con, "
    SELECT casualty_class, COUNT(*) as count 
    FROM stats19_casualties 
    GROUP BY casualty_class 
    ORDER BY count DESC
  ")
  
  cat("Available casualty classes:\n")
  print(casualty_classes)
  
  # Check casualty_type codes
  casualty_types <- DBI::dbGetQuery(con, "
    SELECT casualty_type, COUNT(*) as count 
    FROM stats19_casualties 
    GROUP BY casualty_type 
    ORDER BY count DESC
    LIMIT 10
  ")
  
  cat("\nCasualty type codes (top 10):\n")
  print(casualty_types)
  
  # Get pedestrian data using casualty_class
  pedestrian_data <- DBI::dbGetQuery(con, "
    SELECT casualty_severity, COUNT(*) as count
    FROM stats19_casualties 
    WHERE casualty_class = 'Pedestrian'
    GROUP BY casualty_severity
  ")
  
  cat("\nPedestrian casualty severity distribution:\n")
  print(pedestrian_data)
  
  return(pedestrian_data)
}

#' Explore fire rescue data specifically  
#' @param con database connection
explore_fire_rescue_data <- function(con) {
  # Check the structure and unique values
  extrication_types <- DBI::dbGetQuery(con, "
    SELECT extrication, COUNT(*) as count
    FROM fire_rescue_extrication_casualties
    GROUP BY extrication
    ORDER BY count DESC
  ")
  
  cat("Extrication types:\n")
  print(extrication_types)
  
  # Age bands
  age_bands <- DBI::dbGetQuery(con, "
    SELECT age_band, COUNT(*) as count
    FROM fire_rescue_extrication_casualties  
    GROUP BY age_band
    ORDER BY count DESC
  ")
  
  cat("\nAge bands:\n")
  print(age_bands)
  
  # Sex distribution
  sex_dist <- DBI::dbGetQuery(con, "
    SELECT sex, COUNT(*) as count
    FROM fire_rescue_extrication_casualties
    GROUP BY sex
  ")
  
  cat("\nSex distribution:\n")
  print(sex_dist)
  
  return(list(extrication_types = extrication_types, age_bands = age_bands, sex_dist = sex_dist))
}

#' Explore olive oil data specifically
#' @param con database connection  
explore_olive_oil_data <- function(con) {
  # Get basic statistics
  olive_stats <- DBI::dbGetQuery(con, "
    SELECT 
      COUNT(*) as total_samples,
      AVG(palmitic) as avg_palmitic,
      AVG(oleic) as avg_oleic,
      AVG(linoleic) as avg_linoleic
    FROM olive_oil
  ")
  
  cat("Olive oil basic statistics:\n")
  print(olive_stats)
  
  # Check for any grouping variables
  olive_sample <- DBI::dbGetQuery(con, "SELECT * FROM olive_oil LIMIT 10")
  cat("\nFirst 10 olive oil samples:\n")
  print(olive_sample)
  
  return(olive_sample)
}