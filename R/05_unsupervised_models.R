# R/05_unsupervised_models.R
# MTHM053 Coursework - Unsupervised Learning for Olive Oil Composition

#' Perform PCA on olive oil data
#' @param data cleaned olive oil data
#' @return PCA results
perform_olive_oil_pca <- function(data) {
  library(FactoMineR)
  library(factoextra)
  
  # Select only fatty acid columns for PCA
  fatty_acids <- data %>%
    select(palmitic, palmitoleic, stearic, oleic, linoleic, linolenic, arachidic, eicosenoic)
  
  # Scale the data (important for PCA)
  scaled_data <- scale(fatty_acids)
  
  # Perform PCA
  pca_result <- PCA(scaled_data, graph = FALSE)
  
  # Extract important information
  results <- list(
    pca = pca_result,
    scaled_data = scaled_data,
    eigenvalues = pca_result$eig,
    var_contrib = pca_result$var$contrib,
    individuals = pca_result$ind$coord,
    original_data = data
  )
  
  # Print summary
  cat("PCA Summary:\n")
  cat("Variance explained by first 3 PCs:", 
      round(sum(pca_result$eig[1:3, 2]), 2), "%\n")
  
  return(results)
}

#' Perform clustering on olive oil data
#' @param data cleaned olive oil data
#' @param pca_results PCA results
#' @return clustering results
perform_olive_oil_clustering <- function(data, pca_results) {
  library(cluster)
  library(factoextra)
  library(stats)
  
  # Use PCA coordinates for clustering
  pca_coords <- pca_results$individuals[, 1:5]  # First 5 PCs
  
  # Determine optimal number of clusters using elbow method
  wss <- sapply(2:10, function(k) {
    kmeans(pca_coords, k, nstart = 20)$tot.withinss
  })
  
  # Use silhouette analysis to determine optimal k
  sil_width <- sapply(2:10, function(k) {
    km <- kmeans(pca_coords, k, nstart = 20)
    sil <- silhouette(km$cluster, dist(pca_coords))
    mean(sil[, 3])
  })
  
  optimal_k <- which.max(sil_width) + 1
  
  # Perform k-means clustering with optimal k
  set.seed(42)
  kmeans_result <- kmeans(pca_coords, optimal_k, nstart = 20)
  
  # Perform hierarchical clustering
  dist_matrix <- dist(pca_coords)
  hclust_result <- hclust(dist_matrix, method = "ward.D2")
  hclust_clusters <- cutree(hclust_result, k = optimal_k)
  
  # DBSCAN clustering
  library(dbscan)
  eps_value <- kNNdistplot(pca_coords, k = 4)  # This will help determine eps
  dbscan_result <- dbscan(pca_coords, eps = 0.5, minPts = 5)
  
  results <- list(
    optimal_k = optimal_k,
    wss = wss,
    silhouette_widths = sil_width,
    kmeans = kmeans_result,
    kmeans_clusters = kmeans_result$cluster,
    hclust = hclust_result,
    hclust_clusters = hclust_clusters,
    dbscan = dbscan_result,
    dbscan_clusters = dbscan_result$cluster,
    pca_coords = pca_coords
  )
  
  cat("Optimal number of clusters (k-means):", optimal_k, "\n")
  cat("K-means silhouette score:", round(max(sil_width), 3), "\n")
  cat("DBSCAN found", length(unique(dbscan_result$cluster[dbscan_result$cluster > 0])), "clusters\n")
  
  return(results)
}

#' Evaluate olive oil clustering
#' @param data cleaned data
#' @param clusters clustering results
#' @param pca_results PCA results
#' @return evaluation results
evaluate_olive_oil_clustering <- function(data, clusters, pca_results) {
  library(cluster)
  library(dplyr)
  
  # Calculate silhouette scores for each method
  pca_coords <- clusters$pca_coords
  
  # K-means silhouette
  kmeans_sil <- silhouette(clusters$kmeans_clusters, dist(pca_coords))
  kmeans_avg_sil <- mean(kmeans_sil[, 3])
  
  # Hierarchical clustering silhouette
  hclust_sil <- silhouette(clusters$hclust_clusters, dist(pca_coords))
  hclust_avg_sil <- mean(hclust_sil[, 3])
  
  # DBSCAN silhouette (only for non-noise points)
  dbscan_clusters_clean <- clusters$dbscan_clusters[clusters$dbscan_clusters > 0]
  if (length(unique(dbscan_clusters_clean)) > 1) {
    dbscan_coords_clean <- pca_coords[clusters$dbscan_clusters > 0, ]
    dbscan_sil <- silhouette(dbscan_clusters_clean, dist(dbscan_coords_clean))
    dbscan_avg_sil <- mean(dbscan_sil[, 3])
  } else {
    dbscan_avg_sil <- NA
  }
  
  # Analyze clusters by region (if available)
  cluster_data <- data %>%
    mutate(
      kmeans_cluster = clusters$kmeans_clusters,
      hclust_cluster = clusters$hclust_clusters,
      dbscan_cluster = clusters$dbscan_clusters
    )
  
  # Regional analysis (if region information available)
  if ("region" %in% colnames(data)) {
    region_analysis <- cluster_data %>%
      group_by(region, kmeans_cluster) %>%
      summarise(count = n(), .groups = 'drop') %>%
      group_by(region) %>%
      mutate(prop = count / sum(count))
  } else {
    region_analysis <- NULL
  }
  
  # Fatty acid composition by cluster
  fatty_acid_summary <- cluster_data %>%
    group_by(kmeans_cluster) %>%
    summarise(
      across(c(palmitic, palmitoleic, stearic, oleic, linoleic, linolenic, arachidic, eicosenoic),
             list(mean = mean, sd = sd)),
      count = n(),
      .groups = 'drop'
    )
  
  results <- list(
    kmeans_silhouette = kmeans_avg_sil,
    hclust_silhouette = hclust_avg_sil,
    dbscan_silhouette = dbscan_avg_sil,
    cluster_data = cluster_data,
    region_analysis = region_analysis,
    fatty_acid_summary = fatty_acid_summary
  )
  
  cat("Clustering Evaluation:\n")
  cat("K-means average silhouette:", round(kmeans_avg_sil, 3), "\n")
  cat("Hierarchical clustering average silhouette:", round(hclust_avg_sil, 3), "\n")
  if (!is.na(dbscan_avg_sil)) {
    cat("DBSCAN average silhouette:", round(dbscan_avg_sil, 3), "\n")
  }
  
  return(results)
}

#' Create plots for olive oil unsupervised learning
#' @param data cleaned data
#' @param pca_results PCA results
#' @param clusters clustering results
#' @return plot file paths
create_olive_oil_plots <- function(data, pca_results, clusters) {
  library(ggplot2)
  library(factoextra)
  library(corrplot)
  library(dplyr)
  
  # Create output directory
  if (!dir.exists("output")) dir.create("output")
  
  # 1. PCA biplot
  p1 <- fviz_pca_biplot(pca_results$pca, 
                        title = "PCA Biplot - Olive Oil Fatty Acid Composition",
                        addEllipses = TRUE) +
    theme_minimal()
  
  ggsave("output/olive_oil_pca_biplot.png", p1, width = 10, height = 8)
  
  # 2. Scree plot
  p2 <- fviz_eig(pca_results$pca, 
                 title = "Scree Plot - Variance Explained",
                 addlabels = TRUE) +
    theme_minimal()
  
  ggsave("output/olive_oil_scree_plot.png", p2, width = 8, height = 6)
  
  # 3. K-means clustering plot
  pca_df <- data.frame(
    PC1 = pca_results$individuals[, 1],
    PC2 = pca_results$individuals[, 2],
    Cluster = factor(clusters$kmeans_clusters),
    Region = data$region
  )
  
  p3 <- ggplot(pca_df, aes(x = PC1, y = PC2, color = Cluster)) +
    geom_point(alpha = 0.7, size = 2) +
    labs(title = "K-means Clustering (PCA space)",
         x = paste0("PC1 (", round(pca_results$eigenvalues[1, 2], 1), "%)"),
         y = paste0("PC2 (", round(pca_results$eigenvalues[2, 2], 1), "%)")) +
    theme_minimal() +
    scale_color_brewer(palette = "Set1")
  
  ggsave("output/olive_oil_kmeans_clusters.png", p3, width = 10, height = 8)
  
  # 4. Hierarchical clustering dendrogram
  png("output/olive_oil_dendrogram.png", width = 12, height = 8, units = "in", res = 300)
  plot(clusters$hclust, main = "Hierarchical Clustering Dendrogram", 
       xlab = "Samples", ylab = "Distance")
  rect.hclust(clusters$hclust, k = clusters$optimal_k, border = 2:4)
  dev.off()
  
  # 5. Correlation matrix of fatty acids
  fatty_acids <- data %>%
    select(palmitic, palmitoleic, stearic, oleic, linoleic, linolenic, arachidic, eicosenoic)
  
  png("output/olive_oil_correlation_matrix.png", width = 10, height = 8, units = "in", res = 300)
  corrplot(cor(fatty_acids), method = "color", type = "upper", 
           title = "Fatty Acid Correlation Matrix", mar = c(0,0,1,0))
  dev.off()
  
  # 6. Cluster comparison by region (if available)
  if ("region" %in% colnames(data)) {
    region_cluster_data <- pca_df %>%
      group_by(Region, Cluster) %>%
      summarise(count = n(), .groups = 'drop')
    
    p6 <- ggplot(region_cluster_data, aes(x = Region, y = count, fill = Cluster)) +
      geom_col(position = "dodge") +
      labs(title = "Cluster Distribution by Region",
           x = "Region", y = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_brewer(palette = "Set2")
    
    ggsave("output/olive_oil_clusters_by_region.png", p6, width = 12, height = 6)
  }
  
  cat("✓ Olive oil unsupervised learning plots saved to output/ folder\n")
  return(c("output/olive_oil_pca_biplot.png",
           "output/olive_oil_scree_plot.png",
           "output/olive_oil_kmeans_clusters.png",
           "output/olive_oil_dendrogram.png",
           "output/olive_oil_correlation_matrix.png"))
}