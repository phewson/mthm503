library(RSQLite)

get_car_data <- function() {
  dbfile <- here::here("mock_data", "cars.sqlite")
  con <- dbConnect(RSQLite::SQLite(), dbname = dbfile)
  car_df <- dbReadTable(con, "audi")
  dbDisconnect(con)
  return(car_df) # nolint
}

get_distance_matrix <- function(df, carnames) {
  ensure_data_frame_or_matrix(df)
  rownames(df) <- carnames
  dist(df)
}

get_hclust <- function(dist_mat) {
  hclust(dist_mat)
}

cut_hclust <- function(tree, k) {
  cutree(tree, k)
}

pairwise_scatterplot <- function(df) {
  ensure_data_frame_or_matrix(df)
  df |>
    GGally::ggpairs()
}

pairwise_scatterplot_clusters <- function(df, clusters) {
  ensure_data_frame_or_matrix(df)
  GGally::ggpairs(df,
                  mapping = ggplot2::aes(color = clusters))
}

apply_kmeans <- function(df, k) {
  ensure_data_frame_or_matrix(df)
  kmeans(df, k)
}

fit_pca <- function(df, names) {
  ensure_data_frame_or_matrix(df)
  rownames(df) <- names
  prcomp(df, scale = TRUE)
}

scree_plot <- function(fitted_pca) {
  plot(fitted_pca)
}

pca_scores <- function(fitted_pca) {
  predict(fitted_pca)
}

sil_plot <- function(tree, dist_mat, kmax) {
  sil_widths <- numeric()
  for (k in 2:kmax) {
    groups <- cutree(tree, k = k)
    sil <- cluster::silhouette(groups, dist_mat)
    sil_widths[k] <- mean(sil[, 3])
  }
  return(sil_widths[2:kmax]) # nolint
}

elbow_plot <- function(df, kmax) {
  wss <- numeric()
  for (k in 1:kmax) {
    km <- kmeans(df, centers = k, nstart = 25)
    wss[k] <- km$tot.withinss
  }
  return(wss) # nolint
}

ensure_data_frame_or_matrix <- function(df) {
  if (!is.data.frame(df) && !is.matrix(df)) {
    stop("Input must be a data frame or matrix, got: ",
         paste(class(df), collapse = ", "))
  }
  if (!all(sapply(df, is.numeric))) {
    stop("All columns must be numeric")
  }
  invisible(TRUE)
}
