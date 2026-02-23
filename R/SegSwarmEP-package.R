#' @section Quick Start:
#' 
#' **Step 1: Download example data**
#' 
#' ```r
#' library(SegSwarmEP)
#' download_example_data("ACO_7")  # ~2MB, recommended for testing
#' load("ACO_7_workspace.rda")     # loads: sf.dist, adj_matrix, graph, df.dist
#' ```
#' 
#' **Step 2: Run the optimization**
#' 
#' ```r
#' result <- aco_districting(
#'   sf_data        = sf.dist,
#'   adj_matrix     = adj_matrix,
#'   graph          = graph,
#'   num_districts  = 7,
#'   group_a        = "wa",        # white enrollment
#'   group_b        = "hb",        # Hispanic + Black enrollment
#'   total_col      = "total",
#'   num_ants       = 80,          # use 400+ for production
#'   num_iterations = 120          # use 320+ for production
#' )
#' ```
#' 
#' **Step 3: Analyze results**
#' 
#' ```r
#' # Attach cluster assignments
#' sf.dist$cluster <- result$best_assignment
#' 
#' # Get demographic summary
#' summary_table <- summarise_clusters(sf.dist, group_a = "wa", group_b = "hb")
#' print(summary_table)
#' 
#' # Visualize (requires ggplot2)
#' plot_aco_map(sf.dist)
#' plot_score_history(result)
#' plot_h_history(result)
#' 
#' # Check contiguity
#' check_contiguity(result$best_assignment, adj_matrix)
#' ```
#' 
#' @section Using Your Own Data:
#' 
#' To use your own school district data:
#' 
#' ```r
#' library(sf)
#' library(SegSwarmEP)
#' 
#' # Load your spatial data (shapefile, geopackage, etc.)
#' my_districts <- st_read("path/to/your/data.shp")
#' 
#' # Prepare the data (computes centroids, adjacency, graph)
#' prep <- prepare_districts(
#'   my_districts,
#'   crs = 32618  # Use appropriate UTM zone for your region
#' )
#' 
#' # Run the optimization
#' result <- aco_districting(
#'   sf_data        = prep$sf_data,
#'   adj_matrix     = prep$adj_matrix,
#'   graph          = prep$graph,
#'   num_districts  = 10,           # your target number
#'   group_a        = "white",      # your column names
#'   group_b        = "nonwhite",
#'   total_col      = "total"
#' )
#' ```
#' 
#' @section Main Functions:
#' 
#' **Data:**
#' - [download_example_data()] - Download example NJ datasets from GitHub
#' 
#' **Workflow:**
#' - [prepare_districts()] - Prepare sf data (centroids, adjacency, graph)
#' - [aco_districting()] - Run the ACO optimization algorithm
#' 
#' **Analysis:**
#' - [calculate_score()] - Score any district assignment
#' - [summarise_clusters()] - Demographic summary per cluster
#' - [check_contiguity()] - Verify geographic contiguity
#' 
#' **Visualization:**
#' - [plot_aco_map()] - Map of cluster assignments
#' - [plot_score_history()] - Convergence of objective score
#' - [plot_h_history()] - Convergence of Theil's H segregation index
#' 
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom igraph graph_from_adjacency_matrix components neighborhood induced_subgraph degree vcount
#' @importFrom sf st_transform st_centroid st_coordinates st_touches st_area st_length st_boundary st_union st_drop_geometry st_distance
#' @importFrom segregation mutual_total dissimilarity
#' @importFrom data.table data.table rbindlist setDT
#' @importFrom parallel makeCluster stopCluster clusterExport clusterEvalQ parLapply detectCores
#' @importFrom reshape2 melt
#' @importFrom utils combn
#' @importFrom stats median
## usethis namespace: end
NULL

# Suppress R CMD CHECK notes for ggplot2 aes() column name references
utils::globalVariables(c("iteration", "score", "h", "cluster", ".data"))

