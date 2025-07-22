# R/04_regression_models.R
# MTHM053 Coursework - Regression Models for Fire Rescue Extrications

#' Fit regression model for fire rescue extrications
#' @param data cleaned fire rescue data
#' @return fitted model
fit_fire_rescue_regression <- function(data) {
  library(nnet)
  library(MASS)
  
  # Since extrication_method is categorical with multiple levels,
  # we'll use multinomial logistic regression
  
  # Fit multinomial logistic regression
  model <- nnet::multinom(
    extrication_method ~ sex + age_band + n_casualties,
    data = data,
    trace = FALSE
  )
  
  return(model)
}

#' Evaluate fire rescue regression model
#' @param model fitted model
#' @param data the data used for modeling
#' @return evaluation results
evaluate_fire_rescue_model <- function(model, data) {
  library(broom)
  library(dplyr)
  
  # Get model summary
  model_summary <- summary(model)
  
  # Make predictions
  predictions <- predict(model, data, type = "class")
  actual <- data$extrication_method
  
  # Calculate accuracy
  accuracy <- mean(predictions == actual)
  
  # Create confusion matrix
  confusion <- table(Predicted = predictions, Actual = actual)
  
  # Get coefficients with significance tests
  # For multinomial regression, we'll extract and format coefficients
  coef_summary <- broom::tidy(model, conf.int = TRUE)
  
  # Analyze effects of age and sex
  age_effect <- data %>%
    group_by(age_band, extrication_method) %>%
    summarise(count = n(), .groups = 'drop') %>%
    mutate(prop = count / sum(count))
  
  sex_effect <- data %>%
    group_by(sex, extrication_method) %>%
    summarise(count = n(), .groups = 'drop') %>%
    mutate(prop = count / sum(count))
  
  results <- list(
    model = model,
    model_summary = model_summary,
    predictions = predictions,
    actual = actual,
    accuracy = accuracy,
    confusion_matrix = confusion,
    coefficients = coef_summary,
    age_effect = age_effect,
    sex_effect = sex_effect
  )
  
  cat("Fire Rescue Model Accuracy:", round(accuracy, 4), "\n")
  
  return(results)
}

#' Create plots for fire rescue regression
#' @param data cleaned data
#' @param model fitted model
#' @return plot file paths
create_fire_rescue_plots <- function(data, model) {
  library(ggplot2)
  library(dplyr)
  
  # Create output directory
  if (!dir.exists("output")) dir.create("output")
  
  # 1. Distribution of extrication methods by sex
  p1 <- ggplot(data, aes(x = extrication_method, fill = sex)) +
    geom_bar(position = "dodge") +
    labs(title = "Fire Rescue Extrication Methods by Sex",
         x = "Extrication Method", y = "Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_brewer(palette = "Set1")
  
  ggsave("output/fire_rescue_by_sex.png", p1, width = 12, height = 6)
  
  # 2. Distribution of extrication methods by age band
  p2 <- ggplot(data, aes(x = extrication_method, fill = age_band)) +
    geom_bar(position = "dodge") +
    labs(title = "Fire Rescue Extrication Methods by Age Band",
         x = "Extrication Method", y = "Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_brewer(palette = "Set2")
  
  ggsave("output/fire_rescue_by_age.png", p2, width = 12, height = 6)
  
  # 3. Proportion analysis
  prop_data <- data %>%
    group_by(sex, age_band, extrication_method) %>%
    summarise(count = n(), .groups = 'drop') %>%
    group_by(sex, age_band) %>%
    mutate(prop = count / sum(count))
  
  p3 <- ggplot(prop_data, aes(x = age_band, y = prop, fill = extrication_method)) +
    geom_col(position = "stack") +
    facet_wrap(~sex) +
    labs(title = "Proportion of Extrication Methods by Sex and Age",
         x = "Age Band", y = "Proportion") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_brewer(palette = "Set3")
  
  ggsave("output/fire_rescue_proportions.png", p3, width = 12, height = 8)
  
  # 4. Number of casualties analysis
  p4 <- ggplot(data, aes(x = n_casualties, fill = extrication_method)) +
    geom_histogram(alpha = 0.7, position = "identity", bins = 20) +
    labs(title = "Number of Casualties by Extrication Method",
         x = "Number of Casualties", y = "Count") +
    theme_minimal() +
    scale_fill_brewer(palette = "Set1")
  
  ggsave("output/fire_rescue_casualties.png", p4, width = 10, height = 6)
  
  cat("✓ Fire rescue regression plots saved to output/ folder\n")
  return(c("output/fire_rescue_by_sex.png",
           "output/fire_rescue_by_age.png",
           "output/fire_rescue_proportions.png",
           "output/fire_rescue_casualties.png"))
}