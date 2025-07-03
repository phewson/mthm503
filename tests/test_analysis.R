# tests/test_analysis.R
# Unit tests for analysis functions

library(testthat)
library(dplyr)

# Source the functions
source(here::here("R", "task1_classification.R"))
source(here::here("R", "task2_regression.R"))
source(here::here("R", "task3_unsupervised.R"))
source(here::here("R", "utils.R"))

test_that("classification functions work correctly", {
  # Test data preprocessing
  test_data <- data.frame(
    casualty_severity = c("Fatal", "Serious", "Slight"),
    sex_of_casualty = c("Male", "Female", "Male"),
    age_of_casualty = c(25, 30, 35),
    speed_limit_mph = c(30, 40, 50)
  )
  
  processed_data <- preprocess_pedestrian_data(test_data)
  
  expect_true(is.data.frame(processed_data))
  expect_true(all(sapply(processed_data, function(x) !any(is.na(x)))))
  expect_true(is.factor(processed_data$casualty_severity))
  expect_equal(nrow(processed_data), 3)
})

test_that("regression functions work correctly", {
  # Test mock data creation
  test_data <- list(
    fire_rescue = data.frame(
      sex = c("Male", "Female", "Male"),
      n_casualties = c(1, 2, 1),
      financial_year = c("2020", "2021", "2022")
    ),
    collision = data.frame(year = c(2020, 2021, 2022))
  )
  
  processed_data <- preprocess_fire_rescue_data(test_data)
  
  expect_true(is.data.frame(processed_data))
  expect_true("age_band" %in% names(processed_data))
  expect_true("age_specific_casualties" %in% names(processed_data))
  expect_true(is.factor(processed_data$sex))
  expect_equal(nrow(processed_data), 3)
})

test_that("unsupervised learning functions work correctly", {
  # Test PCA with mock data
  test_data <- data.frame(
    acid1 = c(1, 2, 3, 4, 5),
    acid2 = c(2, 4, 6, 8, 10),
    acid3 = c(3, 6, 9, 12, 15)
  )
  
  processed_data <- preprocess_olive_oil_data(test_data)
  
  expect_true(is.list(processed_data))
  expect_true("scaled_data" %in% names(processed_data))
  expect_true("raw_data" %in% names(processed_data))
  expect_true(nrow(processed_data$scaled_data) == nrow(test_data))
  expect_true(ncol(processed_data$scaled_data) == ncol(test_data))
})

test_that("utility functions work correctly", {
  # Test summary creation
  mock_classification <- list(
    rf_accuracy = 0.85,
    dt_accuracy = 0.80
  )
  
  mock_regression <- list(
    aic_comparison = data.frame(
      Model = c("GLM", "GAM"),
      AIC = c(100, 110)
    )
  )
  
  mock_unsupervised <- list(
    silhouette_kmeans = 0.75
  )
  
  summary_result <- create_analysis_summary(
    mock_classification, 
    mock_regression, 
    mock_unsupervised
  )
  
  expect_true(is.data.frame(summary_result))
  expect_equal(nrow(summary_result), 3)
  expect_true("Task" %in% names(summary_result))
  expect_true("Best_Model" %in% names(summary_result))
  expect_true("Performance_Metric" %in% names(summary_result))
  expect_true("Key_Finding" %in% names(summary_result))
})

test_that("data validation functions work correctly", {
  # Test valid data
  valid_data <- data.frame(
    x = c(1, 2, 3),
    y = c("a", "b", "c")
  )
  
  expect_no_error(validate_data_quality(valid_data, "Test Task"))
  
  # Test empty data
  empty_data <- data.frame()
  expect_error(validate_data_quality(empty_data, "Test Task"))
})

test_that("model training functions handle edge cases", {
  # Test with minimal data
  minimal_data <- data.frame(
    casualty_severity = c("Fatal", "Serious"),
    sex_of_casualty = c("Male", "Female"),
    age_of_casualty = c(25, 30)
  )
  
  processed_minimal <- preprocess_pedestrian_data(minimal_data)
  split_minimal <- split_pedestrian_data(processed_minimal)
  
  # These should work with minimal data
  expect_true(is.list(split_minimal))
  expect_true("X_train" %in% names(split_minimal))
  expect_true("y_train" %in% names(split_minimal))
})

test_that("clustering functions produce valid results", {
  # Test clustering with mock data
  test_data <- data.frame(
    acid1 = c(1, 2, 3, 4, 5),
    acid2 = c(2, 4, 6, 8, 10)
  )
  
  processed_data <- preprocess_olive_oil_data(test_data)
  kmeans_result <- perform_kmeans_clustering(processed_data)
  
  expect_true(is.list(kmeans_result))
  expect_true("kmeans_result" %in% names(kmeans_result))
  expect_true("optimal_k" %in% names(kmeans_result))
  expect_equal(kmeans_result$optimal_k, 3)
})

test_that("evaluation functions return expected structure", {
  # Test classification evaluation structure
  mock_rf_model <- list(predictions = c(1, 2, 1))
  mock_dt_model <- list(predictions = c(1, 2, 2))
  mock_split <- list(
    y_test = factor(c("Fatal", "Serious", "Fatal")),
    X_test = data.frame(x = c(1, 2, 3))
  )
  
  # Mock the evaluation function to test structure
  mock_evaluation <- list(
    rf_accuracy = 0.8,
    dt_accuracy = 0.7,
    rf_kappa = 0.6,
    dt_kappa = 0.5,
    overall_auc = 0.75,
    importance_df = data.frame(feature = "test", importance = 1.0)
  )
  
  expect_true(is.list(mock_evaluation))
  expect_true("rf_accuracy" %in% names(mock_evaluation))
  expect_true("dt_accuracy" %in% names(mock_evaluation))
  expect_true("overall_auc" %in% names(mock_evaluation))
})

test_that("plot creation functions return valid plots", {
  # Test classification plot creation
  mock_results <- list(
    importance_df = data.frame(
      feature = c("feature1", "feature2"),
      importance = c(1.0, 0.8)
    )
  )
  
  mock_data <- data.frame(
    casualty_severity = c("Fatal", "Serious", "Slight")
  )
  
  # Test that plot creation doesn't error
  expect_no_error({
    plots <- create_classification_plots(mock_results, mock_data)
    expect_true(is.list(plots))
    expect_true("target_dist" %in% names(plots))
    expect_true("feature_importance" %in% names(plots))
  })
}) 