#  Unsupervised Learning Task
required_packages <- c("dplyr", "tidyr", "ggplot2", "tidyverse", "corrplot", "GGally",
                       "cluster", "factoextra", "dbscan", "caret", "gridExtra")

new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load libraries 
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidyverse)  # includes ggplot2, dplyr, tidyr, readr etc.
library(corrplot)
library(GGally)
library(cluster)
library(factoextra)
library(dbscan)
library(caret)
library(gridExtra)

# --- Load data from database ---
source("connection.R")  # Assumes 'con' is an active DB connection
olive_data <- dbReadTable(con, "olive_oil")

# --- Exploratory Data Analysis (EDA) 
str(olive_data)
summary(olive_data)
colSums(is.na(olive_data))  # Check for missing values

# Define fatty acid columns to analyze (numeric only)
fatty_acids <- c("palmitic", "palmitoleic", "stearic", "oleic", "linoleic",
                 "linolenic", "arachidic", "eicosenoic")

# Subset numeric fatty acid data
numeric_olive <- olive_data %>% dplyr::select(dplyr::all_of(fatty_acids))

# Correlation matrix and heatmap visualization
cor_matrix <- cor(numeric_olive)
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.8, addCoef.col = "black")

# Pairwise scatterplots for fatty acids
GGally::ggpairs(numeric_olive)

# --- Data scaling ---
# Standardize variables to have mean=0 and sd=1 for PCA and clustering
scaled_data <- scale(numeric_olive)

# --- Principal Component Analysis (PCA) ---
pca_result <- prcomp(scaled_data, center = TRUE, scale. = TRUE)

# Scree plot to determine number of components
factoextra::fviz_eig(pca_result, addlabels = TRUE)

# Biplot of PCA (showing variables and observations)
factoextra::fviz_pca_biplot(pca_result, repel = TRUE, col.var = "blue")

# Print PCA loadings for first two components
loadings <- pca_result$rotation[, 1:2]
print("PCA Loadings (First 2 PCs):")
print(loadings)

# Extract PCA scores for first two components
pca_data <- as.data.frame(pca_result$x[, 1:2])

# --- K-Means Clustering ---
set.seed(123)  # For reproducibility

# Elbow method to decide optimal number of clusters
factoextra::fviz_nbclust(scaled_data, kmeans, method = "wss")

# Run k-means with chosen k=3 (based on elbow plot)
kmeans_result <- kmeans(scaled_data, centers = 3, nstart = 25)
pca_data$cluster_kmeans <- as.factor(kmeans_result$cluster)

# Visualize k-means clusters on PCA components
factoextra::fviz_cluster(kmeans_result, data = scaled_data, geom = "point", ellipse.type = "norm")

# --- DBSCAN Clustering ---
# kNN distance plot to help choose eps parameter
dbscan::kNNdistplot(scaled_data, k = 5)
abline(h = 1.5, col = "red", lty = 2)  # Suggested epsilon threshold

# Run DBSCAN clustering
dbscan_result <- dbscan::dbscan(scaled_data, eps = 1.5, minPts = 5)
pca_data$cluster_dbscan <- as.factor(dbscan_result$cluster)

# Count noise points (cluster 0 = noise)
num_noise <- sum(dbscan_result$cluster == 0)
cat("Number of noise points detected by DBSCAN:", num_noise, "\n")

# Plot DBSCAN clusters on PCA components
ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster_dbscan)) +
  geom_point(size = 2) +
  labs(title = "DBSCAN Clusters on PCA Components") +
  theme_minimal()

# --- Clustering Quality Assessment ---
# Silhouette width for k-means clusters
sil_kmeans <- cluster::silhouette(kmeans_result$cluster, dist(scaled_data))
factoextra::fviz_silhouette(sil_kmeans)
cat("Average silhouette width (K-Means): ", mean(sil_kmeans[, 3]), "\n")

# Silhouette width for DBSCAN (excluding noise points)
sil_dbscan <- cluster::silhouette(
  dbscan_result$cluster[dbscan_result$cluster != 0],
  dist(scaled_data[dbscan_result$cluster != 0, ])
)
cat("Average silhouette width (DBSCAN, excluding noise): ", mean(sil_dbscan[, 3]), "\n")

# --- Add cluster labels back to original data ---
olive_clustered <- olive_data %>%
  mutate(cluster_kmeans = as.factor(kmeans_result$cluster))

# --- Summary statistics for clusters ---
cluster_summary <- olive_clustered %>%
  group_by(cluster_kmeans) %>%
  summarise(across(dplyr::all_of(fatty_acids), mean, .names = "mean_{col}"))

cat("Cluster-wise mean fatty acid composition:\n")
print(cluster_summary)

# --- Boxplots of fatty acid composition by K-Means cluster ---
olive_long <- olive_clustered %>%
  dplyr::select(dplyr::all_of(fatty_acids), cluster_kmeans) %>%
  tidyr::pivot_longer(cols = -cluster_kmeans, names_to = "fatty_acid", values_to = "value")

ggplot(olive_long, aes(x = cluster_kmeans, y = value)) +
  geom_boxplot() +
  facet_wrap(~ fatty_acid, scales = "free_y") +
  labs(
    title = "Fatty Acid Composition by K-Means Cluster",
    x = "Cluster",
    y = "Fatty Acid Composition (%)"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
