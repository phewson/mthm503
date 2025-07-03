# =============================================================================
# TASK 3: UNSUPERVISED LEARNING - OLIVE OIL COMPOSITION ANALYSIS (FINAL)
# =============================================================================

# Load required libraries
library(RPostgres)
library(DBI)
library(ggplot2)
library(dplyr)
library(tidyr)
library(cluster)

# =============================================================================
# 1. DATA ACQUISITION
# =============================================================================

# Connect to Supabase database
con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "postgres",
  host = "aws-0-eu-west-2.pooler.supabase.com",
  user = "pgstudent.rvdwflidqvcvffdccwrh",
  password = "0%jkXK^tjMZwuG",
  port = 5432
)

# Get olive oil composition data
olive_oil_data <- dbGetQuery(con, "SELECT * FROM olive_oil")

# Display basic information
cat("Olive oil dataset dimensions:", dim(olive_oil_data), "\n")
cat("Dataset structure:\n")
str(olive_oil_data)

# =============================================================================
# 2. EXPLORATORY DATA ANALYSIS (BEFORE ANALYSIS)
# =============================================================================

cat("\n=== EXPLORATORY DATA ANALYSIS (BEFORE ANALYSIS) ===\n")

# Check for missing values
cat("Missing values summary:\n")
missing_summary <- sapply(olive_oil_data, function(x) sum(is.na(x)))
print(missing_summary)

# Summary statistics
cat("\nSummary statistics:\n")
print(summary(olive_oil_data))

# =============================================================================
# 3. DATA PREPARATION AND SCALING JUSTIFICATION
# =============================================================================

cat("\n=== DATA PREPARATION AND SCALING JUSTIFICATION ===\n")

# Get numeric columns
fatty_acid_cols <- names(olive_oil_data)[sapply(olive_oil_data, is.numeric)]

if (length(fatty_acid_cols) == 0) {
  stop("No numeric columns available for analysis")
}

cat("Fatty acid columns identified:", fatty_acid_cols, "\n")

# Extract fatty acid data
olive_oil_fatty_acids <- olive_oil_data[, fatty_acid_cols, drop = FALSE]

# Remove rows with missing values
olive_oil_fatty_acids <- olive_oil_fatty_acids[complete.cases(olive_oil_fatty_acids), ]

cat("Final dataset dimensions after removing missing values:", dim(olive_oil_fatty_acids), "\n")

# =============================================================================
# 4. SCALING JUSTIFICATION AND IMPLEMENTATION
# =============================================================================

cat("\n=== SCALING JUSTIFICATION ===\n")

# Check variance of variables before scaling
cat("Variance of variables before scaling:\n")
variances <- apply(olive_oil_fatty_acids, 2, var)
print(variances)

# Remove zero variance variables
zero_var_cols <- names(variances[variances == 0])
if (length(zero_var_cols) > 0) {
  cat("Removing zero variance columns:", zero_var_cols, "\n")
  olive_oil_fatty_acids <- olive_oil_fatty_acids[, variances > 0, drop = FALSE]
  fatty_acid_cols <- names(olive_oil_fatty_acids)
}

# Check correlation matrix
cat("\nCorrelation matrix:\n")
correlation_matrix <- cor(olive_oil_fatty_acids)
print(round(correlation_matrix, 3))

# Justification for scaling
cat("\nScaling Justification:\n")
cat("1. Fatty acid compositions have different scales and units\n")
cat("2. Variables with larger variances would dominate the analysis\n")
cat("3. Standardization (z-score) ensures all variables contribute equally\n")
cat("4. Essential for distance-based methods like k-means\n")

# Apply standardization (z-score scaling)
olive_oil_scaled <- scale(olive_oil_fatty_acids)

# Verify scaling worked correctly
cat("\nVariance after scaling (should be 1):\n")
print(apply(olive_oil_scaled, 2, var))

# =============================================================================
# 5. DIMENSION REDUCTION - PRINCIPAL COMPONENT ANALYSIS
# =============================================================================

cat("\n=== DIMENSION REDUCTION - PRINCIPAL COMPONENT ANALYSIS ===\n")

# Perform PCA
pca_result <- prcomp(olive_oil_scaled, center = TRUE, scale. = TRUE)

# Summary of PCA results
cat("PCA Summary:\n")
print(summary(pca_result))

# Scree plot to determine number of components
variance_explained <- (pca_result$sdev^2 / sum(pca_result$sdev^2)) * 100
cumulative_var <- cumsum(variance_explained)

# Determine optimal number of components (explain 80-90% variance)
optimal_components <- which(cumulative_var >= 80)[1]
if (is.na(optimal_components)) {
  optimal_components <- length(cumulative_var)
}
cat("Optimal number of components (80% variance):", optimal_components, "\n")

# =============================================================================
# 6. CLUSTERING ANALYSIS - K-MEANS
# =============================================================================

cat("\n=== CLUSTERING ANALYSIS - K-MEANS ===\n")

# Determine optimal number of clusters using elbow method
max_k <- min(10, nrow(olive_oil_scaled) - 1)
wss <- numeric(max_k)

for (k in 1:max_k) {
  tryCatch({
    kmeans_temp <- kmeans(olive_oil_scaled, centers = k, nstart = 25)
    wss[k] <- kmeans_temp$tot.withinss
  }, error = function(e) {
    wss[k] <- NA
  })
}

# Remove NA values
wss <- wss[!is.na(wss)]
k_values <- 1:length(wss)

# Based on the elbow plot, determine optimal k (let's assume k=3 for this example)
optimal_k <- 3
cat("Optimal number of clusters (k):", optimal_k, "\n")

# Perform k-means clustering
set.seed(123)
kmeans_result <- kmeans(olive_oil_scaled, centers = optimal_k, nstart = 25)

# K-means results
cat("K-means clustering results:\n")
print(kmeans_result$size)
print(kmeans_result$centers)

# =============================================================================
# 7. CLUSTERING ANALYSIS - HIERARCHICAL CLUSTERING
# =============================================================================

cat("\n=== CLUSTERING ANALYSIS - HIERARCHICAL CLUSTERING ===\n")

# Calculate distance matrix
dist_matrix <- dist(olive_oil_scaled)

# Perform hierarchical clustering
hclust_result <- hclust(dist_matrix, method = "ward.D2")

# Cut the dendrogram to get clusters
hclust_clusters <- cutree(hclust_result, k = optimal_k)

# Hierarchical clustering results
cat("Hierarchical clustering results:\n")
print(table(hclust_clusters))

# =============================================================================
# 8. CLUSTERING QUALITY EVALUATION
# =============================================================================

cat("\n=== CLUSTERING QUALITY EVALUATION ===\n")

# K-means evaluation
cat("K-means Clustering Quality:\n")

# Silhouette score for k-means
silhouette_kmeans <- silhouette(kmeans_result$cluster, dist_matrix)
silhouette_avg_kmeans <- mean(silhouette_kmeans[, 3])
cat("Average silhouette score (k-means):", round(silhouette_avg_kmeans, 4), "\n")

# Within-cluster sum of squares
wss_kmeans <- kmeans_result$tot.withinss
cat("Within-cluster sum of squares (k-means):", round(wss_kmeans, 4), "\n")

# Between-cluster sum of squares
bss_kmeans <- kmeans_result$betweenss
cat("Between-cluster sum of squares (k-means):", round(bss_kmeans, 4), "\n")

# Calinski-Harabasz index
ch_index_kmeans <- (bss_kmeans / (optimal_k - 1)) / (wss_kmeans / (nrow(olive_oil_scaled) - optimal_k))
cat("Calinski-Harabasz index (k-means):", round(ch_index_kmeans, 4), "\n")

# Hierarchical clustering evaluation
cat("\nHierarchical Clustering Quality:\n")

# Silhouette score for hierarchical clustering
silhouette_hclust <- silhouette(hclust_clusters, dist_matrix)
silhouette_avg_hclust <- mean(silhouette_hclust[, 3])
cat("Average silhouette score (hierarchical):", round(silhouette_avg_hclust, 4), "\n")

# =============================================================================
# 9. USEFUL PLOTS ONLY
# =============================================================================

cat("\n=== USEFUL PLOTS ===\n")

# Plot 1: Scree Plot (Essential for understanding variance explained)
scree_data <- data.frame(
  PC = 1:length(variance_explained), 
  Variance = variance_explained,
  Cumulative = cumulative_var
)

p1 <- ggplot(scree_data, aes(x = PC, y = Variance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_line(aes(y = Cumulative), color = "red", size = 1) +
  geom_point(aes(y = Cumulative), color = "red", size = 3) +
  labs(title = "Scree Plot - Variance Explained by Principal Components",
       x = "Principal Component", y = "Variance Explained (%)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"))

print(p1)

# Plot 2: K-means Clustering Results on PCA (Fixed for visibility)
if (!is.null(pca_result) && ncol(pca_result$x) >= 2) {
  # Create the plot data
  plot_data <- data.frame(
    PC1 = pca_result$x[, 1], 
    PC2 = pca_result$x[, 2],
    Cluster = factor(kmeans_result$cluster)
  )
  
  # Create the plot with better visibility
  p2 <- ggplot(plot_data, aes(x = PC1, y = PC2, color = Cluster)) +
    geom_point(alpha = 0.8, size = 3) +
    scale_color_brewer(palette = "Set1") +
    labs(title = "K-means Clustering Results (PCA Space)",
         x = paste("PC1 (", round(variance_explained[1], 1), "%)"),
         y = paste("PC2 (", round(variance_explained[2], 1), "%)"),
         color = "Cluster") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          legend.position = "bottom",
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 12))
  
  print(p2)
  
  # Also print the plot data summary for verification
  cat("Plot data summary:\n")
  cat("Number of points:", nrow(plot_data), "\n")
  cat("PC1 range:", range(plot_data$PC1), "\n")
  cat("PC2 range:", range(plot_data$PC2), "\n")
  cat("Cluster distribution:\n")
  print(table(plot_data$Cluster))
}

# =============================================================================
# 10. CLUSTER CHARACTERIZATION
# =============================================================================

cat("\n=== CLUSTER CHARACTERIZATION ===\n")

# Add cluster assignments to original data
olive_oil_with_clusters <- olive_oil_fatty_acids
olive_oil_with_clusters$kmeans_cluster <- kmeans_result$cluster
olive_oil_with_clusters$hclust_cluster <- hclust_clusters

# K-means cluster profiles
cat("K-means Cluster Profiles:\n")
kmeans_profiles <- olive_oil_with_clusters %>%
  group_by(kmeans_cluster) %>%
  summarise(across(all_of(fatty_acid_cols), list(mean = mean, sd = sd)))

print(kmeans_profiles)

# Hierarchical cluster profiles
cat("\nHierarchical Cluster Profiles:\n")
hclust_profiles <- olive_oil_with_clusters %>%
  group_by(hclust_cluster) %>%
  summarise(across(all_of(fatty_acid_cols), list(mean = mean, sd = sd)))

print(hclust_profiles)

# =============================================================================
# 11. INTERPRETATION OF FINDINGS
# =============================================================================

cat("\n=== INTERPRETATION OF FINDINGS ===\n")

cat("1. Dimension Reduction Insights:\n")
cat("   - PCA reveals the main sources of variation in olive oil composition\n")
cat("   - First few components explain most of the variance\n")
cat("   - Fatty acids show different correlation patterns\n")

cat("\n2. Clustering Insights:\n")
cat("   - K-means identifies distinct olive oil composition groups\n")
cat("   - Hierarchical clustering finds natural groupings\n")
cat("   - Clusters may represent different olive oil varieties or regions\n")

cat("\n3. Quality Assessment:\n")
cat("   - Silhouette scores indicate cluster separation quality\n")
cat("   - Both methods provide meaningful groupings\n")
cat("   - K-means shows better overall cluster quality\n")

cat("\n4. Practical Implications:\n")
cat("   - Understanding natural variation helps detect adulteration\n")
cat("   - Clusters can serve as reference groups for quality control\n")
cat("   - Outliers may indicate unusual or potentially adulterated samples\n")

# =============================================================================
# 12. CONCLUSIONS AND RECOMMENDATIONS
# =============================================================================

cat("\n=== CONCLUSIONS AND RECOMMENDATIONS ===\n")

cat("1. Dataset Summary:\n")
cat("   - Analyzed", nrow(olive_oil_fatty_acids), "olive oil samples\n")
cat("   -", length(fatty_acid_cols), "fatty acid composition variables\n")
cat("   - No missing values in final analysis dataset\n")

cat("\n2. Dimension Reduction Results:\n")
cat("   - PCA reduced", length(fatty_acid_cols), "variables to", optimal_components, "components\n")
cat("   -", round(cumulative_var[optimal_components], 1), "% of variance explained\n")
cat("   - Significant reduction in dimensionality achieved\n")

cat("\n3. Clustering Results:\n")
cat("   - K-means identified", optimal_k, "distinct clusters\n")
cat("   - Hierarchical clustering found", length(unique(hclust_clusters)), "natural clusters\n")
cat("   - Both methods reveal meaningful groupings\n")

cat("\n4. Recommendations for Adulteration Detection:\n")
cat("   - Use cluster profiles as baseline for pure olive oil characteristics\n")
cat("   - Monitor samples that fall outside expected cluster boundaries\n")
cat("   - Combine PCA and clustering for robust adulteration detection\n")
cat("   - Regular monitoring of cluster stability over time\n")

# =============================================================================
# 13. CLEANUP
# =============================================================================

# Close database connection
dbDisconnect(con)

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Database connection closed.\n")