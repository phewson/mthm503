library(targets)
library(tarchetypes)
library(here)

source(here("R", "functions.R"))
source(here("R", "load_data.R"))
source(here("R", "utils.R"))
source(here("R", "clustering.R"))

tar_option_set(packages = c("dplyr", "DBI", "RPostgres"))

list(
  tar_target(
    raw_data,
    load_data()
  ),
  tar_target(
    summary_data,
    summarise_data(raw_data)
  ),
  tar_target(
    summary_casualty_sex,
    summarise_casualty_sex(raw_data)
  ),
  tar_render(
    report,
    "vignettes/Report.Rmd"
  ),
  tar_target(
    car_df,
    get_car_data()
  ),
  tar_target(
    pairwise_cars,
    pairwise_scatterplot(car_df[, 4:8])
  ),
  tar_target(
    car_dist,
    get_distance_matrix(car_df[, 4:8], car_df$model)
  ),
  tar_target(
    car_hclust,
    get_hclust(car_dist)
  ),
  tar_target(
    car_hclust_groups,
    cut_hclust(car_hclust, 2)
  ),
  tar_target(
    pairwise_cars_clust2,
    pairwise_scatterplot_clusters(car_df[,4:8], as.factor(car_hclust_groups))
  ),
  tar_target(
    silhouettes,
    sil_plot(car_hclust, car_dist, 10)
  ),
  tar_target(
    cars_kmeans_2,
    apply_kmeans(car_df[, 4:8], 2)
  ),
  tar_target(
    cars_elbows,
    elbow_plot(car_df[, 4:8], 10)
  ),
  tar_target(
    pairwise_cars_k2,
    pairwise_scatterplot_clusters(car_df[,4:8], as.factor(cars_kmeans_2$cluster))
  ),
  tar_target(
    cars_pca,
    fit_pca(car_df[, 4:8], car_df$model)
  ),
  tar_target(
    cars_scores,
    pca_scores(cars_pca)
  ),
  tar_target(
    pairwise_cars_k2_pca,
    pairwise_scatterplot_clusters(cars_scores[, 1:2], as.factor(cars_kmeans_2$cluster))
  ),
  tar_target(
    pairwise_cars_h2_pca,
    pairwise_scatterplot_clusters(cars_scores[, 1:2], as.factor(car_hclust_groups))
  ),
  tar_render(
    unsupervised_report,
    "vignettes/unsupervised.Rmd"
  )
)
