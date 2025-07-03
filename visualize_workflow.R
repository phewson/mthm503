# visualize_workflow.R
# Script to visualize the targets workflow

library(targets)
library(here)

# Load the targets
source(here("_targets.R"))

# Visualize the workflow
cat("=== TARGETS WORKFLOW VISUALIZATION ===\n")

# Show target status
cat("\n1. Target Status:\n")
tar_glimpse()

# Show target progress
cat("\n2. Target Progress:\n")
tar_progress()

# Show target dependencies
cat("\n3. Target Dependencies:\n")
tar_network()

# Show target metadata
cat("\n4. Target Metadata:\n")
tar_meta()

# Show target manifest
cat("\n5. Target Manifest:\n")
tar_manifest()

# Show target outdated
cat("\n6. Outdated Targets:\n")
tar_outdated()

# Show target objects
cat("\n7. Target Objects:\n")
tar_objects()

cat("\n=== WORKFLOW VISUALIZATION COMPLETE ===\n")
cat("Use tar_visnetwork() to see the interactive network diagram\n")
cat("Use tar_glimpse() to see target status\n")
cat("Use tar_progress() to see target progress\n")
cat("Use tar_outdated() to see what needs updating\n") 