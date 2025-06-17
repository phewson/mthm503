library(testthat)
library(dplyr)
library(rsample)
source(here::here("R", "supervised_classification.R"))

# Sample test dataset
test_df <- tibble(
  type = c("A", "B", "A", "B", "A", "B"),
  wind = c(10, 20, 30, 40, 50, 60),
  pressure = c(1000, 990, 995, 985, 1005, 980)
)

test_df <- test_df %>%
  mutate(type = as.factor(type))

test_that("prepare_split returns an rsplit object", {
  df_split <- prepare_split(test_df)

  # Check if the result is an rsplit object
  expect_s3_class(df_split, "rsplit")

  # Ensure 'type' is a factor
  expect_true(is.factor(test_df$type))
})

test_that("get_train returns correct training set", {
  df_split <- prepare_split(test_df)
  train_data <- get_train(df_split)

  # Check if it's a tibble
  expect_s3_class(train_data, "tbl_df")

  # Ensure train data size is correct (80% of 6 rows)
  expect_equal(nrow(train_data), 4)
})

test_that("get_test returns correct test set", {
  df_split <- prepare_split(test_df)
  test_data <- get_test(df_split)

  # Check if it's a tibble
  expect_s3_class(test_data, "tbl_df")

  # Ensure test data size is correct (20% of 6 rows)
  expect_equal(nrow(test_data), 2)
})

# Sample test dataset
test_predictions <- tibble(
  type = factor(c("A", "B", "A", "B", "A", "B")),
  .pred_class = factor(c("A", "B", "B", "B", "A", "A"))  # Some misclass
)

test_that("confusion_matrix returns correct structure", {
  conf_mat_result <- confusion_matrix(test_predictions)

  # Check if it returns a confusion matrix object
  expect_s3_class(conf_mat_result, "conf_mat")

  # Ensure correct dimensions (should match number of classes)
  expect_equal(nrow(conf_mat_result$table),
               length(levels(test_predictions$type)))
  expect_equal(ncol(conf_mat_result$table),
               length(levels(test_predictions$type)))
})

test_that("accuracy function returns correct accuracy value", {
  acc_result <- accuracy(test_predictions)

  # Check if accuracy value is a numeric metric tibble
  expect_s3_class(acc_result, "tbl_df")

  # Verify correct accuracy computation
  expected_acc <- mean(test_predictions$type == test_predictions$.pred_class)
  expect_equal(acc_result$.estimate, expected_acc, tolerance = 1e-6)
})
