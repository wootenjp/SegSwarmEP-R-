# SegSwarmEP Example Script
# Demonstrates how to run the ACO redistricting algorithm

# Install and load the package
# remotes::install_github("wootenjp/SegSwarmEP-R-")
library(SegSwarmEP)

# Download example data (only need to do this once)
if (!file.exists("ACO_7_workspace.rda")) {
  download_example_data("ACO_7")  # Or "ACO_21" for larger example
}

# Load the workspace data
load("ACO_7_workspace.rda")

# The workspace contains:
#   sf.dist    - sf spatial data frame of school districts
#   adj_matrix - adjacency matrix
#   graph      - igraph object
#   df.dist    - data frame (non-spatial)

# Parameters for a quick test run
max_district_size <- 124000 
num_ants <- 80               # Use 400+ for production
num_districts <- 7 
num_iterations <- 120        # Use 320+ for production
demographic_scale <- 45
compactness_scale <- 120
penalty_contiguity <- -30
bonus_contiguity <- 60
size_scale_factor <- log(10)

# Run the ACO algorithm
result <- aco_districting(
  sf_data            = sf.dist,
  adj_matrix         = adj_matrix,
  graph              = graph,
  num_districts      = num_districts,
  group_a            = "wa",                    # white enrollment
  group_b            = "hb",                    # Hispanic + Black enrollment
  total_col          = "total",
  id_col             = "district",
  county_col         = "county",
  max_district_size  = max_district_size,
  num_ants           = num_ants,
  num_iterations     = num_iterations,
  demographic_scale  = demographic_scale,
  compactness_scale  = compactness_scale,
  penalty_contiguity = penalty_contiguity,
  bonus_contiguity   = bonus_contiguity,
  size_scale_factor  = size_scale_factor,
  verbose            = TRUE
)

# Attach the results to your sf object
sf.dist$cluster <- result$best_assignment

# Summarize the results
summary_table <- summarise_clusters(
  sf.dist, 
  group_a = "wa", 
  group_b = "hb", 
  total_col = "total"
)
print(summary_table)

# Visualize the results (requires ggplot2)
if (require(ggplot2, quietly = TRUE)) {
  plot_aco_map(sf.dist)
  plot_score_history(result)
  plot_h_history(result)
}

# Check contiguity of all clusters
contiguous <- check_contiguity(result$best_assignment, adj_matrix)
cat("All districts contiguous:", contiguous, "\n")
