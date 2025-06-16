library(ggplot2)
library(dplyr)
library(mdsr)
library(nasaweather)
library(partykit)
library(tidymodels)
library(yardstick)


simple_count <- function(df) {
  df %>%
    group_by(.data$type) %>%
    summarise(n())
}

create_indicators <- function(df, target_class) {
  df <- df |> 
    mutate(binary_class = ifelse(.data$type == target_class, target_class, "other")) |> 
    mutate(binary_class = as.factor(.data$binary_class))
  return(df)
}

plot_storm_data <- function(df) {
  ggplot(data = df, aes(x = pressure, y = wind, color = type)) +
    geom_point(alpha = 0.5) + 
    xlab("Recorded Air Pressure") + 
    ylab("Recorded Wind Speed") +
    labs(title = "Plot of wind speed against pressure showing storm status", 
         caption = "Jon Hobbs extracted from https://www.nhc.noaa.gov/)")
}

prepare_split <- function(df){
  df_parts <- df |>
    mutate(type = as.factor(.data$type)) |>
    initial_split(prop = 0.8)  
}

get_train <- function(df_part) {
  df_part |>
    training()
}

get_test <- function(df_part) {
  df_part |>
    testing()
}

fit_rpart <- function(train_df, cp) {
  fitted_tree <- decision_tree(mode = "classification", cost_complexity = cp) |>
    set_engine("rpart") |>
    fit(type ~ pressure + wind + month, data = train_df)
}

plot_rpart <- function(classifier) {
  plot(as.party(classifier$fit))
}

make_preds <- function(test_df, classifier) {
  preds <- predict(classifier, test_df, type = "prob")
  test_results <- test_df |>
    select(.data$type) |>
    bind_cols(preds)
  return(test_results)
}


make_class_preds <- function(test_df, classifier) {
  preds <- predict(classifier, test_df, type = "class")
  test_results <- test_df |>
    select(.data$type) |>
    bind_cols(preds)
  return(test_results)
}

make_roc_curve <- function(preds, type){
  test_results <- create_indicators(preds, type)
  mytitle = paste(type, "ROC curve")
  target = paste(".pred_", type, sep = "")
  roc_curve_data <- roc_curve(test_results, truth = binary_class, target)  
  ggplot(roc_curve_data, aes(x = 1 - specificity, y = sensitivity)) +
    geom_line() +
    geom_abline(linetype = "dashed") +
    labs(title = mytitle)
}
