#' Create an Initial Region Assignment
#'
#' Assigns school districts to regions using a simple greedy algorithm that
#' respects geographic adjacency. Useful for generating a starting solution for
#' [seg_swarm()].
#'
#' @param enrollment A data frame with one row per district.
#' @param n_regions Integer; desired number of regions.
#' @param id_col Character string; name of the column containing unique
#'   district identifiers (default `"district_id"`). If `NULL`, row indices
#'   are used.
#' @param adjacency Optional square 0/1 adjacency matrix. When supplied,
#'   regions are grown by greedily merging adjacent districts. When `NULL` a
#'   simple sequential partition is used.
#' @param seed Integer; random seed for reproducibility (default `NULL`).
#'
#' @return An integer vector of length `nrow(enrollment)` with values in
#'   `1:n_regions`.
#'
#' @examples
#' enroll <- data.frame(
#'   district_id = paste0("D", 1:9),
#'   white    = c(200, 180, 50, 40, 100, 90, 30, 20, 150),
#'   nonwhite = c(50,  60, 200, 190, 100, 110, 170, 180, 50)
#' )
#' create_regions(enroll, n_regions = 3, id_col = "district_id")
#'
#' @export
create_regions <- function(enrollment,
                           n_regions,
                           id_col    = "district_id",
                           adjacency = NULL,
                           seed      = NULL) {

  enrollment <- as.data.frame(enrollment)
  n          <- nrow(enrollment)

  if (n_regions > n) {
    stop("`n_regions` cannot exceed the number of districts.", call. = FALSE)
  }
  if (n_regions < 1L) {
    stop("`n_regions` must be at least 1.", call. = FALSE)
  }

  if (!is.null(seed)) set.seed(seed)

  if (is.null(adjacency)) {
    # Simple sequential partition
    region_id <- rep(seq_len(n_regions), length.out = n)
    return(as.integer(region_id))
  }

  adjacency <- as.matrix(adjacency)
  stopifnot(nrow(adjacency) == n, ncol(adjacency) == n)

  # Greedy region-growing: pick n_regions seed districts, grow outwards
  seeds     <- sample.int(n, n_regions)
  region_id <- integer(n)
  region_id[seeds] <- seq_len(n_regions)

  unassigned <- setdiff(seq_len(n), seeds)

  # Iteratively assign unassigned districts to an adjacent region
  max_pass <- n * n_regions
  pass     <- 0L

  while (length(unassigned) > 0 && pass < max_pass) {
    pass <- pass + 1L
    newly_assigned <- integer(0)

    for (i in unassigned) {
      adj_regions <- region_id[adjacency[i, ] > 0 & region_id > 0]
      if (length(adj_regions) > 0) {
        # assign to the most common adjacent region
        region_id[i] <- as.integer(names(sort(table(adj_regions),
                                              decreasing = TRUE))[1])
        newly_assigned <- c(newly_assigned, i)
      }
    }

    unassigned <- setdiff(unassigned, newly_assigned)
    if (length(newly_assigned) == 0L) break   # no progress; fall back below
  }

  # Any remaining unassigned districts get the nearest (by index) region
  if (length(unassigned) > 0) {
    assigned_idx <- which(region_id > 0)
    for (i in unassigned) {
      closest   <- assigned_idx[which.min(abs(assigned_idx - i))]
      region_id[i] <- region_id[closest]
    }
  }

  as.integer(region_id)
}


#' Evaluate a Redistricting Plan
#'
#' Computes a comprehensive set of segregation statistics for a given
#' district-to-region assignment.
#'
#' @param enrollment A data frame with one row per district containing at
#'   least the columns named in `group_a` and `group_b`.
#' @param region_id Integer vector of length `nrow(enrollment)` giving the
#'   region assignment.
#' @param group_a Character string; reference group column name.
#' @param group_b Character string; comparison group column name.
#'
#' @return A list with elements:
#'   \describe{
#'     \item{`ep_overall`}{Population-weighted EP index across all regions.}
#'     \item{`ep_by_region`}{Named numeric vector of per-region EP values.}
#'     \item{`dissimilarity_overall`}{Population-weighted dissimilarity index.}
#'     \item{`region_summary`}{Data frame with one row per region.}
#'   }
#'
#' @examples
#' enroll <- data.frame(white = c(200, 50, 10, 5),
#'                      nonwhite = c(50, 200, 5, 10))
#' plan   <- c(1, 1, 2, 2)
#' evaluate_plan(enroll, plan, "white", "nonwhite")
#'
#' @export
evaluate_plan <- function(enrollment, region_id, group_a, group_b) {
  enrollment <- as.data.frame(enrollment)
  .validate_groups(enrollment, c(group_a, group_b))
  stopifnot(length(region_id) == nrow(enrollment))

  ep_res <- aggregate_ep(enrollment, group_a, group_b, region_id)

  # Weighted dissimilarity
  regions  <- sort(unique(region_id))
  d_vals   <- vapply(regions, function(r) {
    sub <- enrollment[region_id == r, , drop = FALSE]
    compute_dissimilarity(sub, group_a, group_b)
  }, numeric(1))
  a_totals <- vapply(regions, function(r) {
    sum(enrollment[[group_a]][region_id == r])
  }, numeric(1))
  A     <- sum(enrollment[[group_a]])
  d_avg <- if (A > 0) sum(d_vals * a_totals / A, na.rm = TRUE) else NA_real_

  # Region-level summary
  reg_summary <- lapply(regions, function(r) {
    sub  <- enrollment[region_id == r, , drop = FALSE]
    n_d  <- nrow(sub)
    tot  <- sum(sub[[group_a]]) + sum(sub[[group_b]])
    pct_a <- if (tot > 0) sum(sub[[group_a]]) / tot else NA_real_
    data.frame(region = r, n_districts = n_d, total_enrollment = tot,
               pct_group_a = round(pct_a, 4),
               ep = round(ep_res$ep_by_region[as.character(r)], 4),
               dissimilarity = round(d_vals[regions == r], 4),
               stringsAsFactors = FALSE)
  })

  list(
    ep_overall           = ep_res$ep_overall,
    ep_by_region         = ep_res$ep_by_region,
    dissimilarity_overall = d_avg,
    region_summary       = do.call(rbind, reg_summary)
  )
}


#' Build an Adjacency Matrix from Coordinates
#'
#' Creates a binary adjacency matrix by treating two districts as neighbours if
#' their centroids are within `radius` of each other (using Euclidean distance
#' on the supplied coordinates). This is a practical fallback when a formal
#' spatial adjacency matrix is unavailable.
#'
#' @param enrollment A data frame with one row per district.
#' @param id_col Character string; column with district IDs. If `NULL`, row
#'   indices are used as identifiers.
#' @param lat_col Character string; name of the latitude column
#'   (default `"lat"`).
#' @param lon_col Character string; name of the longitude column
#'   (default `"lon"`).
#' @param radius Numeric; maximum Euclidean distance (in the units of
#'   `lat_col`/`lon_col`) between two centroids to count as adjacent.
#'   Defaults to twice the median nearest-neighbour distance.
#'
#' @return A symmetric 0/1 matrix with dimnames equal to the district
#'   identifiers. The diagonal is 0.
#'
#' @examples
#' enroll <- data.frame(
#'   district_id = c("A", "B", "C"),
#'   white    = c(100, 200, 150),
#'   nonwhite = c(50,  80,  120),
#'   lat      = c(40.0, 40.1, 40.05),
#'   lon      = c(-74.0, -74.0, -74.05)
#' )
#' build_adjacency(enroll, id_col = "district_id")
#'
#' @export
build_adjacency <- function(enrollment,
                            id_col  = "district_id",
                            lat_col = "lat",
                            lon_col = "lon",
                            radius  = NULL) {

  enrollment <- as.data.frame(enrollment)

  missing_cols <- setdiff(c(lat_col, lon_col), names(enrollment))
  if (length(missing_cols) > 0) {
    stop("Columns not found: ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }

  ids  <- if (!is.null(id_col) && id_col %in% names(enrollment)) {
    as.character(enrollment[[id_col]])
  } else {
    as.character(seq_len(nrow(enrollment)))
  }

  lat  <- enrollment[[lat_col]]
  lon  <- enrollment[[lon_col]]
  n    <- nrow(enrollment)

  # Euclidean distance matrix
  coords <- cbind(lon, lat)
  dmat   <- as.matrix(stats::dist(coords))

  if (is.null(radius)) {
    # Use twice the median nearest-neighbour distance
    nn_dist <- apply(dmat + diag(Inf, n), 1, min)
    radius  <- 2 * stats::median(nn_dist)
  }

  adj <- (dmat > 0 & dmat <= radius) * 1L
  dimnames(adj) <- list(ids, ids)
  adj
}
