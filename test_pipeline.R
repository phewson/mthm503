# test_pipeline.R

# Set working directory (optional, if not using RStudio project)
setwd(here::here())

# Load required packages
library(targets)
library(tarchetypes)
library(visNetwork)  # for tar_visnetwork()

# Make sure environment is synced
renv::restore()

# Show pipeline structure
tar_visnetwork()

# Run entire pipeline
tar_make()

# See sample outputs
cat("\n=== Top 5 rows of cleaned data ===\n")
print(head(tar_read(clean_classif_data)))

cat("\n=== Multinomial Confusion Matrix ===\n")
print(tar_read(eval_multinom_conf))

cat("\n=== RF AUC ===\n")
print(tar_read(eval_rf_auc))
