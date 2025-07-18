# _targets.R
library(targets)
library(tarchetypes)
library(here)

# Load all helper functions from the R/ folder
invisible(lapply(dir("R", full.names = TRUE), source))

# Set required packages for all targets
tar_option_set(
  packages = c(
    "dplyr", "lubridate", "tidyr",
    "nnet", "ranger", "caret", "yardstick",
    "tibble", "magrittr", "purrr", "DBI"
  )
)

# Define the pipeline
list(
  
  # ─────────────────────────────
  # TASK 1: CLASSIFICATION MODEL
  # ─────────────────────────────
  
  # 1. Load raw pedestrian data
  tar_target(
    raw_pedestrian,
    load_stats19_data()
  ),
  
  # 2. Clean pedestrian data
  tar_target(
    df_clean,
    task1_clean_data(raw_pedestrian)
  ),
  
  # 3. Run feature selection & modeling pipeline
  tar_target(
    model_results,
    task1_feature_elim_pipeline(df_clean, top_n = 20)
  ),
  
  # 4. Extract evaluation results
  tar_target(rf_auc,         model_results$rf_auc),
  tar_target(rf_conf,        model_results$rf_cm),
  tar_target(multinom_auc,   model_results$multinom_auc),
  tar_target(multinom_conf,  model_results$multinom_cm),
  tar_target(top_features,   model_results$top_features),
  
  # ─────────────────────────────
  # TASK 2: REGRESSION MODEL
  # ─────────────────────────────
  
  # 5. Load extrication data from DB
  tar_target(
    fire_rescue_extrication_casualties,
    load_extrication_data_from_db()
  ),
  
  # 6. Load annual STATS19 summary
  tar_target(
    stats19_by_financial_year,
    load_stats19_by_year()
  ),
  
  # 7. Merge and engineer extrication dataset
  tar_target(
    raw_extrication,
    load_extrication_data(fire_rescue_extrication_casualties, stats19_by_financial_year)
  ),
  
  # 8. Clean for modeling
  tar_target(
    df2_clean,
    task2_clean_data(raw_extrication)
  ),
  
  # 9. Fit Poisson regression model
  tar_target(
    mod2_poisson,
    fit_poisson_model(df2_clean)
  ),
  
  # 10. Summarise Poisson model
  tar_target(
    mod2_poisson_summary,
    summarize_poisson_model(mod2_poisson)
  ),
  
  # 11. Load raw olive_oil table from DB
  tar_target(
    raw_olive_oil,
    load_olive_oil_from_db()
  ),
  
  # 12. Clean and EDA
  tar_target(
    olive_data_clean,
    explore_olive_oil_data(raw_olive_oil)
  ),
  
  tar_target(
    olive_data_scaled,
    normalize_olive_data(raw_olive_oil)
  ),
  tar_target(
    elbow_plot,
    run_elbow_method(olive_data_scaled)
  ),
  tar_target(
    olive_clusters,
    run_kmeans(olive_data_scaled, k = 5)  # ✅ Now it matches
  ),
  
  tar_target(
    olive_pca_cluster_plot,
    plot_pca_clusters(olive_data_scaled, olive_clusters)
  ),
  tar_target(
    kmeans_summary,
    summarize_kmeans_results(olive_clusters)
  )
  
)
