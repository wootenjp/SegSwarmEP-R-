#!/usr/bin/env Rscript
# Manual test script for SegSwarmEP package
# This script demonstrates how to run the ACO algorithm with your specified parameters
#
# Parameters:
# - num_districts: 7 (number of clusters)
# - num_ants: 80
# - num_iterations: 120
# - max_district_size: 124,000
#
# To run this script:
# 1. Install the package: devtools::install() or R CMD INSTALL .
# 2. Run: Rscript tests/manual_test.R
# Or source it in an R session: source("tests/manual_test.R")

library(SegSwarmEP)

cat("SegSwarmEP Manual Test\n")
cat("======================\n\n")

cat("This script would run ACO with the following parameters:\n")
cat("  - Number of districts (clusters): 7\n")
cat("  - Number of ants: 80\n")
cat("  - Number of iterations: 120\n")
cat("  - Max district size: 124,000\n\n")

# Load your data here
# Example: sf_data <- readRDS("path/to/your/data.rds")
# Or use one of the workspace files in the repository

# Uncomment and modify this section when you have your data ready:
# 
# cat("Loading data...\n")
# # Option 1: Load from workspace file
# # load("ACO_7named_workspace.rda")  # Adjust based on actual workspace contents
# #
# # Option 2: Load from your own spatial data file
# # sf_data <- sf::st_read("your_shapefile.shp")
# # # Ensure it has the required columns: white, nonwhite, total, geometry
# 
# cat("Preparing districts...\n")
# prep <- prepare_districts(sf_data, crs = 32618)  # Adjust CRS as needed
# 
# cat("Running ACO algorithm...\n")
# cat("This may take several minutes depending on data size...\n\n")
# 
# result <- aco_districting(
#   sf_data           = prep$sf_data,
#   adj_matrix        = prep$adj_matrix,
#   graph             = prep$graph,
#   num_districts     = 7,
#   group_a           = "white",
#   group_b           = "nonwhite",
#   total_col         = "total",
#   max_district_size = 124000,
#   num_ants          = 80,
#   num_iterations    = 120,
#   verbose           = TRUE
# )
# 
# cat("\nTest completed!\n")
# cat("Best score:", max(result$score_history), "\n")
# cat("Final Theil's H:", tail(result$h_values, 1), "\n")
# 
# # Add assignment to spatial data
# prep$sf_data$cluster <- result$best_assignment
# 
# # Save results
# saveRDS(result, "test_results.rds")
# saveRDS(prep$sf_data, "test_sf_data_with_clusters.rds")
# cat("\nResults saved to test_results.rds and test_sf_data_with_clusters.rds\n")
# 
# # Optional: Create a simple plot if ggplot2 is available
# if (requireNamespace("ggplot2", quietly = TRUE)) {
#   library(ggplot2)
#   p <- plot_districts(prep$sf_data, cluster_col = "cluster")
#   ggsave("test_districts_map.png", p, width = 10, height = 8)
#   cat("Map saved to test_districts_map.png\n")
# }

cat("\nTo run this test:\n")
cat("1. Uncomment the code section above\n")
cat("2. Load or prepare your spatial data\n")
cat("3. Adjust CRS and column names as needed\n")
cat("4. Run: source('tests/manual_test.R')\n")
