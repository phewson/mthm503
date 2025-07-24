test_that("normalize_olive_data returns a scaled dataframe", {
  df <- data.frame(a = 1:10, b = 11:20)
  scaled <- normalize_olive_data(df)
  
  expect_s3_class(scaled, "data.frame")
  expect_equal(ncol(scaled), 2)
  expect_true(all(abs(colMeans(scaled)) < 1e-10))  # centered
})

test_that("run_kmeans returns valid kmeans object", {
  df <- data.frame(
    FA1 = rnorm(100),
    FA2 = rnorm(100),
    FA3 = rnorm(100),
    FA4 = rnorm(100)
  )
  scaled_df <- normalize_olive_data(df)
  km <- run_kmeans(scaled_df, k = 3)
  
  expect_s3_class(km, "kmeans")
  expect_equal(length(km$cluster), nrow(scaled_df))
  expect_equal(ncol(km$centers), ncol(scaled_df))  # adapted to olive structure
})

test_that("run_elbow_method returns a ggplot object", {
  df <- data.frame(
    FA1 = rnorm(100),
    FA2 = rnorm(100),
    FA3 = rnorm(100),
    FA4 = rnorm(100)
  )
  scaled_df <- normalize_olive_data(df)
  elbow_plot <- run_elbow_method(scaled_df, max_k = 5)
  
  expect_s3_class(elbow_plot, "gg")
})

test_that("summarize_kmeans_results returns summary tibble", {
  df <- data.frame(
    FA1 = rnorm(100),
    FA2 = rnorm(100),
    FA3 = rnorm(100),
    FA4 = rnorm(100)
  )
  scaled_df <- normalize_olive_data(df)
  km <- run_kmeans(scaled_df, k = 3)
  summary_tbl <- summarize_kmeans_results(km)
  
  expect_s3_class(summary_tbl, "tbl_df")
  expect_named(summary_tbl, c("total_withinss", "betweenss", "totss", "ratio", "cluster_sizes"))
  expect_equal(nrow(summary_tbl), 1)
})

test_that("plot_pca_clusters returns a ggplot object", {
  df <- data.frame(
    FA1 = rnorm(100),
    FA2 = rnorm(100),
    FA3 = rnorm(100),
    FA4 = rnorm(100)
  )
  scaled_df <- normalize_olive_data(df)
  km <- run_kmeans(scaled_df, k = 3)
  p <- plot_pca_clusters(scaled_df, km)
  
  expect_s3_class(p, "gg")
})
