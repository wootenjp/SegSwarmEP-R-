# Internal utility functions used across the SegSwarmEP package.
# These are not exported.

# Check whether a set of nodes forms a single contiguous component.
.is_contiguous <- function(nodes, adj_matrix) {
  if (length(nodes) <= 1L) return(TRUE)
  subgraph <- adj_matrix[nodes, nodes, drop = FALSE]
  g <- igraph::graph_from_adjacency_matrix(subgraph, mode = "undirected")
  igraph::components(g)$no == 1L
}

# Collect per-district enrollment/demographic data into a data.table.
# Returns an empty data.table if the assignment is entirely NA.
.create_districts_result <- function(best_assignment, sf_data,
                                      group_a, group_b, total_col,
                                      id_col = NULL, county_col = NULL) {
  if (is.null(best_assignment) || all(is.na(best_assignment))) {
    return(data.table::data.table())
  }
  valid <- best_assignment[!is.na(best_assignment)]
  if (length(valid) == 0L) return(data.table::data.table())

  num_dist <- max(valid)

  # Build column selection vector
  keep_cols <- unique(c(group_a, group_b, total_col))
  if (!is.null(id_col)     && id_col     %in% colnames(sf_data)) keep_cols <- c(id_col, keep_cols)
  if (!is.null(county_col) && county_col %in% colnames(sf_data)) keep_cols <- c(county_col, keep_cols)
  keep_cols <- unique(keep_cols)

  missing <- setdiff(keep_cols, colnames(sf_data))
  if (length(missing) > 0L) {
    stop("Missing columns in sf_data: ", paste(missing, collapse = ", "))
  }

  rows <- lapply(seq_len(num_dist), function(i) {
    nodes <- which(best_assignment == i)
    if (length(nodes) == 0L) return(NULL)
    d <- sf::st_drop_geometry(sf_data[nodes, keep_cols, drop = FALSE])
    d$cluster <- i
    d
  })
  rows <- rows[!vapply(rows, is.null, logical(1L))]
  if (length(rows) == 0L) return(data.table::data.table())
  data.table::rbindlist(rows, fill = TRUE)
}
