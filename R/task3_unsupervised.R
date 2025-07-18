library(dplyr)
library(tidyr)
library(ggplot2)

# STEP 1: Load + basic debugging (no filtering or cleaning)
load_olive_oil_data <- function(olive_data) {
  cat("🟢 Step 1: Raw olive oil data preview:\n")
  print(head(olive_data))
  print(str(olive_data))
  
  cat("🟡 Step 2: Missing values by column:\n")
  print(colSums(is.na(olive_data)))
  
  return(olive_data)  # Just return raw
}

# STEP 2: Optional EDA — but no cleaning
explore_olive_oil_data <- function(df) {
  cat("📊 Summary statistics for numeric variables:\n")
  print(summary(df))
  
  cat("📌 Structure:\n")
  print(str(df))
  
  # Only numeric columns
  acid_cols <- df %>%
    select(where(is.numeric)) %>%
    names()
  
  # Plot histograms for numeric variables
  for (col in acid_cols) {
    print(
      ggplot(df, aes_string(x = col)) +
        geom_histogram(bins = 30, fill = "skyblue", color = "white") +
        theme_minimal() +
        ggtitle(paste("Distribution of", col))
    )
  }
}

library(dplyr)
library(ggplot2)
library(cluster)


normalize_olive_data <- function(df) {
  fatty_acid_cols <- df %>%
    select(where(is.numeric)) %>%
    names()
  
  df_scaled <- df %>%
    select(all_of(fatty_acid_cols)) %>%
    scale() %>%
    as.data.frame()
  
  return(df_scaled)
}
run_elbow_method <- function(scaled_df, max_k = 10) {
  wss <- sapply(1:max_k, function(k) {
    kmeans(scaled_df, centers = k, nstart = 25)$tot.withinss
  })
  
  elbow_plot <- ggplot(data.frame(k = 1:max_k, wss = wss), aes(x = k, y = wss)) +
    geom_line() +
    geom_point() +
    theme_minimal() +
    labs(
      title = "Elbow Method for Optimal k",
      x = "Number of clusters (k)",
      y = "Total within-cluster sum of squares"
    )
  
  return(elbow_plot)
}
# STEP 3: Apply K-Means clustering
run_kmeans <- function(scaled_df, k = 3) {
  set.seed(42)
  kmeans(scaled_df, centers = k, nstart = 25)
}

# STEP 4: PCA & Cluster Plotting
plot_pca_clusters <- function(scaled_df, cluster_obj) {
  pca <- prcomp(scaled_df)
  df_pca <- as.data.frame(pca$x[, 1:2])
  df_pca$cluster <- factor(cluster_obj$cluster)
  
  ggplot(df_pca, aes(x = PC1, y = PC2, color = cluster)) +
    geom_point(size = 2, alpha = 0.7) +
    theme_minimal() +
    labs(title = "PCA of Olive Oil Data with K-Means Clusters")
}
summarize_kmeans_results <- function(kmeans_result) {
  tibble::tibble(
    total_withinss = kmeans_result$tot.withinss,
    betweenss = kmeans_result$betweenss,
    totss = kmeans_result$totss,
    ratio = round(kmeans_result$betweenss / kmeans_result$totss, 3),
    cluster_sizes = paste(kmeans_result$size, collapse = ", ")
  )
}

