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

