# R/task1_classification.R

# Task 1: Classification EDA Utilities
# This script defines functions for exploratory data analysis
# on the pedestrian crash dataset.

# Load necessary libraries
library(dplyr)
library(purrr)
library(tibble)
library(magrittr)

# 1) Column overview: names, types, and missingness
eda_overview <- function(df) {
  tibble(
    column      = names(df),
    type        = map_chr(df, ~ class(.x)[1]),
    missing     = map_int(df, ~ sum(is.na(.x))),
    missing_pct = missing / nrow(df)
  )
}

# 2) Severity balance: counts and proportions of each injury severity
eda_severity <- function(df) {
  df %>%
    count(casualty_severity) %>%
    mutate(
      pct = n / sum(n)
    ) %>%
    arrange(desc(n))
}
