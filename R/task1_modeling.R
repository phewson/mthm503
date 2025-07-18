# R/task1_modeling.R

# Task 1: Classification Modeling & Feature Selection Utilities
# This script defines model-fitting, evaluation, and feature-selection functions
# for pedestrian crash severity classification.

# Load required libraries
library(nnet)       # multinomial logistic regression
library(ranger)     # random forest
library(caret)      # confusionMatrix, rfe
library(yardstick)  # AUC metrics
dplyr <- NULL     # avoid partial matches
# R/task1_modeling.R

# Task 1: Classification Modeling & Feature Selection Utilities
# This script defines model-fitting, evaluation, and feature-selection functions
# for pedestrian crash severity classification.


# 1) Fit multinomial logistic regression
task1_fit_multinom <- function(df, features = NULL) {
  # Build formula: use supplied features or default set
  default_feats <- c("weather_conditions", "light_conditions", "age_group",
                     "sex_of_casualty", "urban_or_rural_area")
  feats <- if (is.null(features)) default_feats else features
  f <- as.formula(paste("casualty_severity ~", paste(feats, collapse = " + ")))
  nnet::multinom(f, data = df, trace = FALSE)
}

# 2) Fit random forest classifier with variable importance
task1_fit_rf <- function(df, features = NULL, num.trees = 500) {
  # Build formula
  default_feats <- c("weather_conditions", "light_conditions", "age_group",
                     "sex_of_casualty", "urban_or_rural_area")
  feats <- if (is.null(features)) default_feats else features
  f <- as.formula(paste("casualty_severity ~", paste(feats, collapse = " + ")))
  
  ranger::ranger(
    formula = f,
    data = df,
    probability = TRUE,
    num.trees = num.trees,
    respect.unordered.factors = TRUE,
    importance = "impurity",
    verbose = FALSE
  )
}

# 3) Extract variable importance from a random forest model
task1_rf_varimp <- function(rf_model) {
  imp <- ranger::importance(rf_model)
  # return sorted named vector
  sort(imp, decreasing = TRUE)
}

# 4) Select top-N features by importance
task1_select_top_features <- function(rf_model, n = 20) {
  imp <- task1_rf_varimp(rf_model)
  names(imp)[seq_len(min(n, length(imp)))]
}

# 5) Evaluate a model via confusion matrix
task1_eval_confusion <- function(model, df) {
  if (inherits(model, "ranger")) {
    prob_mat <- predict(model, data = df, type = "response")$predictions
    # choose class with highest prob
    class_names <- colnames(prob_mat)
    preds <- factor(class_names[max.col(prob_mat, ties.method = "first")],
                    levels = levels(df$casualty_severity))
  } else {
    preds <- predict(model, df, type = "class")
  }
  caret::confusionMatrix(preds, df$casualty_severity)
}

# 6) Compute multiclass AUC
task1_eval_auc <- function(model, df) {
  if (inherits(model, "ranger")) {
    prob_mat <- predict(model, data = df, type = "response")$predictions
  } else {
    prob_mat <- predict(model, df, type = "prob")
  }
  classes <- levels(df$casualty_severity)
  prob_df <- as.data.frame(prob_mat)
  colnames(prob_df) <- paste0(".pred_", classes)
  df_probs <- tibble::tibble(truth = df$casualty_severity) %>%
    cbind(prob_df)
  yardstick::roc_auc(
    df_probs,
    truth = truth,
    dplyr::starts_with(".pred_"),
    event_level = "first",
    estimator = "macro_weighted"
  )
}

# 7) End-to-end top-N feature pipeline
task1_feature_elim_pipeline <- function(df, top_n = 20) {
  # initial RF
  rf_init <- task1_fit_rf(df)
  # select top features present in df
  top_feats <- intersect(task1_select_top_features(rf_init, n = top_n), names(df))
  # retrain models on those features
  rf_top      <- task1_fit_rf(df, features = top_feats)
  multinom_top<- task1_fit_multinom(df, features = top_feats)
  # evaluate
  list(
    top_features   = top_feats,
    rf_model       = rf_top,
    multinom_model = multinom_top,
    rf_cm          = task1_eval_confusion(rf_top, df),
    rf_auc         = task1_eval_auc(rf_top, df),
    multinom_cm    = task1_eval_confusion(multinom_top, df),
    multinom_auc   = task1_eval_auc(multinom_top, df)
  )
}
