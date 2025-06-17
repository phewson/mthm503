library(ggplot2)
library(dplyr)
library(mdsr)
library(nasaweather)
library(partykit)
library(tidymodels)
library(yardstick)
library(kknn)

load_storms_data <- function() {
  nasaweather::storms
}

simple_count <- function(df) {
  df %>%
    group_by(.data$type) %>%
    summarise(count = n())
}

create_indicators <- function(df, target_class) {
  df |>
    mutate(binary_class = ifelse(.data$type == target_class,
                                 target_class, "other")) |>
    mutate(binary_class = as.factor(.data$binary_class))
}

plot_storm_data <- function(df) {
  ggplot(data = df,
         aes(x = .data$pressure, y = .data$wind, color = .data$type)) +
    geom_point(alpha = 0.5) +
    xlab("Recorded Air Pressure") +
    ylab("Recorded Wind Speed") +
    labs(title = "Plot of wind speed against pressure showing storm status",
         caption = "Jon Hobbs extracted from https://www.nhc.noaa.gov/)")
}

prepare_split <- function(df) {
  df |>
    mutate(type = as.factor(.data$type)) |>
    rsample::initial_split(prop = 0.8)
}

get_train <- function(df_part) {
  df_part |>
    rsample::training()
}

get_test <- function(df_part) {
  df_part |>
    rsample::testing()
}

fit_rpart <- function(train_df, cp) {
  parsnip::decision_tree(mode = "classification", cost_complexity = cp) |>
    parsnip::set_engine("rpart") |>
    parsnip::fit(type ~ pressure + wind + month, data = train_df)
}

fit_knn <- function(train_df, neighbours) {
  parsnip::nearest_neighbor(neighbors = neighbours, mode = "classification") |>
    parsnip::set_engine("kknn", scale = TRUE) |>
    parsnip::fit(type ~ wind + pressure, data = train_df)
}

plot_rpart <- function(classifier) {
  plot(as.party(classifier$fit))
}

make_preds <- function(test_df, classifier) {
  preds <- predict(classifier, test_df, type = "prob")
  test_df |>
    select(.data$type) |>
    bind_cols(preds)
}

make_class_preds <- function(test_df, classifier) {
  preds <- predict(classifier, test_df, type = "class")
  test_df |>
    select(.data$type) |>
    bind_cols(preds)
}

make_roc_curve <- function(preds, type) {
  test_results <- create_indicators(preds, type)
  mytitle <- paste(type, "ROC curve")
  target <- paste(".pred_", type, sep = "")
  roc_curve_data <- roc_curve(test_results, truth = .data$binary_class, target)
  ggplot(roc_curve_data, aes(x = 1 - specificity, y = sensitivity)) +
    geom_line() +
    geom_abline(linetype = "dashed") +
    labs(title = mytitle)
}

confusion_matrix <- function(class_predictions) {
  class_predictions |>
    conf_mat(.data$type, .data$.pred_class)
}

accuracy <- function(class_predictions) {
  class_predictions |>
    yardstick::accuracy(type, .pred_class) # nolint
}
