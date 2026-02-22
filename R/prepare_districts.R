#' Prepare school district spatial data for ACO redistricting
#'
#' Transforms an `sf` spatial data frame of school districts into the format
#' required by [aco_districting()]. This function reprojects the data, computes
#' district centroids, builds the geographic adjacency matrix, and constructs
#' the corresponding `igraph` graph.
#'
#' @param sf_data An `sf` object where each row is a school district (or local
#'   education agency). Must contain a `geometry` column with polygon or
#'   multipolygon features.
#' @param crs Integer EPSG code for the projected coordinate reference system
#'   to use for distance and area calculations. Defaults to `32618` (UTM
#'   Zone 18N, appropriate for New Jersey and the mid-Atlantic US). Choose a
#'   UTM zone appropriate for your state/region.
#' @param adjacency_method Character. Either `"touches"` (default) to mark
#'   districts as adjacent when they share a boundary, or `"distance"` to use
#'   a spatial distance threshold.
#' @param distance_threshold Numeric. When `adjacency_method = "distance"`,
#'   the maximum centroid-to-centroid distance (in the units of `crs`, usually
#'   metres) for two districts to be considered adjacent. Ignored when
#'   `adjacency_method = "touches"`.
#'
#' @return A named list with four elements:
#' \describe{
#'   \item{`sf_data`}{The input `sf` object reprojected to `crs` and augmented
#'     with two new columns: `center_x` and `center_y` (centroid coordinates).}
#'   \item{`adj_matrix`}{A square integer matrix of size `n x n` (where `n`
#'     is the number of districts). Entry `[i, j]` equals 1 if districts `i`
#'     and `j` are adjacent, 0 otherwise. The diagonal is 0.}
#'   \item{`graph`}{An undirected `igraph` graph built from `adj_matrix`.}
#'   \item{`search_radius`}{A numeric scalar giving a sensible default initial
#'     search radius (in CRS units) for the ACO ants. Computed as one-third of
#'     the larger spatial extent of the study area.}
#' }
#'
#' @examples
#' \dontrun{
#' library(sf)
#' # sf_nj is your sf object of NJ school districts
#' prep <- prepare_districts(sf_nj, crs = 32618)
#' str(prep)
#' }
#'
#' @export
prepare_districts <- function(sf_data,
                               crs = 32618,
                               adjacency_method = c("touches", "distance"),
                               distance_threshold = NULL) {
  adjacency_method <- match.arg(adjacency_method)

  if (!inherits(sf_data, "sf")) {
    stop("`sf_data` must be an sf object.")
  }

  # Reproject to a metric CRS for area/distance calculations
  sf_data <- sf::st_transform(sf_data, crs = crs)

  # Compute centroids and extract coordinates
  centroids <- sf::st_centroid(sf::st_geometry(sf_data))
  coords    <- sf::st_coordinates(centroids)
  sf_data$center_x <- coords[, 1]
  sf_data$center_y <- coords[, 2]

  n <- nrow(sf_data)

  # Build adjacency matrix
  if (adjacency_method == "touches") {
    touches    <- sf::st_touches(sf_data, sparse = FALSE)
    adj_matrix <- matrix(as.integer(touches), nrow = n, ncol = n)
  } else {
    # Distance-based adjacency
    if (is.null(distance_threshold)) {
      stop("`distance_threshold` must be specified when adjacency_method = 'distance'.")
    }
    dist_mat   <- as.matrix(sf::st_distance(centroids))
    adj_matrix <- matrix(
      as.integer(dist_mat <= distance_threshold & dist_mat > 0),
      nrow = n, ncol = n
    )
  }

  diag(adj_matrix) <- 0
  dimnames(adj_matrix) <- list(seq_len(n), seq_len(n))

  # Build igraph
  graph <- igraph::graph_from_adjacency_matrix(adj_matrix, mode = "undirected")

  # Default search radius: 1/3 of the larger spatial extent
  x_range       <- diff(range(coords[, 1]))
  y_range       <- diff(range(coords[, 2]))
  search_radius <- max(x_range, y_range) / 3

  list(
    sf_data       = sf_data,
    adj_matrix    = adj_matrix,
    graph         = graph,
    search_radius = search_radius
  )
}
