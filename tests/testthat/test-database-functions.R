# tests/testthat/test-database-functions.R
# Test database functions

library(testthat)

# Source the functions
source(here::here("R", "01_database_exploration.R"))

test_that("Database functions exist and are callable", {
  # Test that functions exist
  expect_true(exists("connect_to_database"))
  expect_true(exists("get_pedestrian_casualties_data"))
  expect_true(exists("get_fire_rescue_data"))
  expect_true(exists("get_olive_oil_data"))
  expect_true(exists("disconnect_database"))
  
  # Test that functions are functions
  expect_true(is.function(connect_to_database))
  expect_true(is.function(get_pedestrian_casualties_data))
  expect_true(is.function(get_fire_rescue_data))
  expect_true(is.function(get_olive_oil_data))
  expect_true(is.function(disconnect_database))
})

test_that("Data extraction produces expected structure", {
  # Load the data from targets (which we know works)
  if (file.exists("_targets/objects/pedestrian_data")) {
    pedestrian_data <- targets::tar_read(pedestrian_data)
    expect_s3_class(pedestrian_data, "data.frame")
    expect_true(nrow(pedestrian_data) > 0)
    expect_true("casualty_severity" %in% colnames(pedestrian_data))
  }
  
  if (file.exists("_targets/objects/fire_rescue_data")) {
    fire_rescue_data <- targets::tar_read(fire_rescue_data)
    expect_s3_class(fire_rescue_data, "data.frame")
    expect_true(nrow(fire_rescue_data) > 0)
    expect_true("extrication" %in% colnames(fire_rescue_data))
  }
  
  if (file.exists("_targets/objects/olive_oil_data")) {
    olive_oil_data <- targets::tar_read(olive_oil_data)
    expect_s3_class(olive_oil_data, "data.frame")
    expect_true(nrow(olive_oil_data) > 0)
    expect_true("palmitic" %in% colnames(olive_oil_data))
  }
})