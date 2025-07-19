# R/02_data_processing.R
# MTHM053 Coursework - Data Processing Functions

#' Clean pedestrian data for classification
#' @param data raw pedestrian data
#' @return cleaned data
clean_pedestrian_data <- function(data) {
  library(dplyr)
  library(lubridate)
  
  cleaned <- data %>%
    # Remove rows with missing target variable
    filter(!is.na(casualty_severity)) %>%
    # Clean and process variables
    mutate(
      # Target variable
      severity = factor(casualty_severity, levels = c("Fatal", "Serious", "Slight")),
      
      # Demographics
      sex = factor(sex_of_casualty),
      age = as.numeric(age_of_casualty),
      age_band = factor(age_band_of_casualty),
      
      # Circumstances
      light_conditions = factor(light_conditions),
      weather = factor(weather_conditions),
      location = factor(urban_or_rural_area),
      road_surface = factor(road_surface_conditions),
      
      # Pedestrian specific
      ped_location = factor(pedestrian_location),
      ped_movement = factor(pedestrian_movement),
      
      # Driver characteristics
      driver_age = as.numeric(age_of_driver),
      driver_sex = factor(sex_of_driver),
      vehicle_type = factor(vehicle_type),
      
      # Time features
      hour = hour(obs_date),
      weekday = wday(obs_date),
      month = month(obs_date)
    ) %>%
    # Remove original columns and select final features
    select(
      severity, sex, age, age_band, light_conditions, weather, location,
      road_surface, ped_location, ped_movement, driver_age, driver_sex,
      vehicle_type, hour, weekday, month
    ) %>%
    # Remove rows with too many missing values
    filter(rowSums(is.na(.)) <= 3)
  
  return(cleaned)
}

#' Split pedestrian data into train/test
#' @param data cleaned pedestrian data
#' @return list with train and test sets
split_pedestrian_data <- function(data) {
  library(caret)
  
  set.seed(42)
  train_index <- createDataPartition(data$severity, p = 0.7, list = FALSE)
  
  train_data <- data[train_index, ]
  test_data <- data[-train_index, ]
  
  return(list(train = train_data, test = test_data))
}

#' Clean fire rescue data for regression
#' @param data raw fire rescue data
#' @return cleaned data
clean_fire_rescue_data <- function(data) {
  library(dplyr)
  
  cleaned <- data %>%
    # Clean variables
    mutate(
      # Target variable (extrication method)
      extrication_method = factor(extrication),
      
      # Predictors
      sex = factor(sex),
      age_band = factor(age_band),
      n_casualties = as.numeric(n_casualties),
      financial_year = factor(financial_year)
    ) %>%
    # Remove missing values and unknown categories
    filter(
      !is.na(extrication_method),
      extrication_method != "Unknown",
      sex != "Unknown",
      age_band != "Unknown"
    ) %>%
    select(extrication_method, sex, age_band, n_casualties, financial_year)
  
  return(cleaned)
}

#' Clean olive oil data for unsupervised learning
#' @param data raw olive oil data
#' @return cleaned data
clean_olive_oil_data <- function(data) {
  library(dplyr)
  
  # Extract region information from ID
  cleaned <- data %>%
    mutate(
      # Extract region from ID (e.g., "North-Apulia-1-1_1" -> "North-Apulia")
      region = stringr::str_extract(id, "^[^-]+-[^-]+"),
      region = factor(region)
    ) %>%
    # Select fatty acid columns for analysis
    select(id, region, palmitic, palmitoleic, stearic, oleic, linoleic, linolenic, arachidic, eicosenoic) %>%
    # Remove any rows with missing values
    filter(complete.cases(.))
  
  return(cleaned)
}

#' Save all important data to files
#' @param ... data objects to save
#' @return file paths
save_important_data <- function(pedestrian_clean, fire_rescue_clean, olive_oil_clean,
                               pedestrian_results, fire_rescue_results, olive_oil_results,
                               final_results) {
  
  # Create data directory if it doesn't exist
  if (!dir.exists("data")) dir.create("data")
  
  # Save cleaned datasets
  readr::write_csv(pedestrian_clean, "data/pedestrian_clean.csv")
  readr::write_csv(fire_rescue_clean, "data/fire_rescue_clean.csv") 
  readr::write_csv(olive_oil_clean, "data/olive_oil_clean.csv")
  
  # Save results
  saveRDS(pedestrian_results, "data/pedestrian_results.rds")
  saveRDS(fire_rescue_results, "data/fire_rescue_results.rds")
  saveRDS(olive_oil_results, "data/olive_oil_results.rds")
  saveRDS(final_results, "data/final_results.rds")
  
  cat("✓ All important data saved to data/ folder\n")
  return("data/data_saved.txt")
}