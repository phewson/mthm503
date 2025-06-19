library(testthat)
source(here::here("R", "clustering.R"))
# Example data
df <- scale(mtcars[, 1:3])  # scaled numeric data
carnames <- rownames(mtcars)
dist_mat <- dist(df)
tree <- hclust(dist_mat)

test_that("get_distance_matrix returns dist obj with rownames", {
  dist_mat <- get_distance_matrix(df, carnames)

  # Check class
  expect_s3_class(dist_mat, "dist")

  # Check attributes
  expect_equal(attr(dist_mat, "Labels"), carnames)

  # Check length (dist object has n * (n-1) / 2 elements)
  n <- nrow(df)
  expect_length(dist_mat, n * (n - 1) / 2)
})

test_that("get_hclust returns hclust object", {
  dist_mat <- get_distance_matrix(df, carnames)
  tree <- get_hclust(dist_mat)

  expect_s3_class(tree, "hclust")
  expect_equal(length(tree$order), nrow(df))  # order length matches data
})

test_that("cut_hclust returns cluster assignments of correct length", {
  dist_mat <- get_distance_matrix(df, carnames)
  tree <- get_hclust(dist_mat)
  clusters <- cut_hclust(tree, k = 3)

  # Check it returns a named integer vector
  expect_type(clusters, "integer")
  expect_length(clusters, nrow(df))
  expect_named(clusters)

  # Should have exactly k unique clusters
  expect_equal(length(unique(clusters)), 3)
})

test_that("apply_kmeans returns kmeans object with correct n clusters", {
  km <- apply_kmeans(df, k = 3)

  expect_s3_class(km, "kmeans")
  expect_equal(length(unique(km$cluster)), 3)
  expect_equal(length(km$cluster), nrow(df))
})

test_that("fit_pca returns prcomp object with correct rownames", {
  pca <- fit_pca(df, carnames)

  expect_s3_class(pca, "prcomp")
  expect_equal(rownames(pca$x), carnames)
})

test_that("pca_scores returns matrix of correct dimensions", {
  pca <- fit_pca(df, carnames)
  scores <- pca_scores(pca)

  expect_true(is.matrix(scores))
  expect_equal(rownames(scores), carnames)
  expect_equal(nrow(scores), nrow(df))
  expect_equal(ncol(scores), ncol(df))
})

test_that("scree_plot runs without error", {
  pca <- fit_pca(df, carnames)

  # Ensure plot function doesn't error
  expect_silent(scree_plot(pca))
})

test_that("sil_plot returns numeric vector of correct length", {
  kmax <- 5
  sil_widths <- sil_plot(tree, dist_mat, kmax)

  expect_type(sil_widths, "double")
  expect_equal(length(sil_widths), kmax - 1)
})

test_that("elbow_plot returns numeric vector of correct length", {
  kmax <- 5
  wss <- elbow_plot(df, kmax)

  expect_type(wss, "double")
  expect_equal(length(wss), kmax)
})

test_that("accepts numeric data frame", {
  df <- data.frame(a = 1:3, b = c(1.5, 2.5, 3.5))
  expect_invisible(ensure_data_frame_or_matrix(df))
})

test_that("accepts numeric matrix", {
  mat <- matrix(1:9, nrow = 3)
  expect_invisible(ensure_data_frame_or_matrix(mat))
})

test_that("rejects non-numeric data frame", {
  df <- data.frame(a = 1:3, b = c("x", "y", "z"))
  expect_error(ensure_data_frame_or_matrix(df),
               "All columns must be numeric")
})

test_that("rejects vector input", {
  v <- c(1, 2, 3)
  expect_error(ensure_data_frame_or_matrix(v),
               "Input must be a data frame or matrix")
})

test_that("rejects list input", {
  l <- list(a = 1:3, b = 4:6)
  expect_error(ensure_data_frame_or_matrix(l),
               "Input must be a data frame or matrix")
})
