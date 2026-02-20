#' Run the SegSwarmEP Redistricting Optimization
#'
#' Core heuristic of the package. Starting from an initial assignment of
#' districts to regions, the algorithm iteratively improves the assignment by
#' proposing single-district moves that respect geographic contiguity. At each
#' step a candidate move is accepted if it improves the objective (minimizing
#' the population-weighted EP segregation index) or—when using the simulated-
#' annealing schedule—with a probability that decreases over time.
#'
#' @param enrollment A numeric data frame with one row per district. Must
#'   contain at least the columns named in `group_a` and `group_b`. Row names
#'   (or a `district_id` column) are used as district labels.
#' @param adjacency A square symmetric 0/1 matrix (or data frame) of size
#'   n × n, where `adjacency[i, j] == 1` means districts i and j share a
#'   border. Row and column names must match the row names of `enrollment` (or
#'   the `district_id` column when present).
#' @param region_id Integer vector of length n giving the initial region
#'   assignment for each district. Districts with the same integer belong to the
#'   same proposed region.
#' @param group_a Character string; reference group column name in
#'   `enrollment`.
#' @param group_b Character string; comparison group column name in
#'   `enrollment`.
#' @param n_iter Integer; maximum number of iterations (default 5 000).
#' @param cooling_rate Numeric in (0, 1); rate at which the simulated-annealing
#'   temperature decreases each iteration (default 0.995). Use `cooling_rate =
#'   0` for pure greedy hill-climbing.
#' @param temp_init Numeric; initial temperature for the simulated-annealing
#'   schedule (default 0.1). Ignored when `cooling_rate == 0`.
#' @param seed Integer; random seed for reproducibility (default `NULL`).
#' @param verbose Logical; if `TRUE` a progress message is printed every 500
#'   iterations (default `FALSE`).
#'
#' @return An object of class `"seg_swarm_result"` (a list) containing:
#'   \describe{
#'     \item{`region_id`}{Integer vector of final region assignments.}
#'     \item{`ep_final`}{Population-weighted EP index for the final solution.}
#'     \item{`ep_initial`}{Population-weighted EP index for the initial
#'       solution.}
#'     \item{`ep_trace`}{Numeric vector of EP values at every iteration.}
#'     \item{`enrollment`}{The `enrollment` data frame passed in (with a
#'       `region_id` column appended).}
#'     \item{`group_a`}{Value of the `group_a` argument.}
#'     \item{`group_b`}{Value of the `group_b` argument.}
#'     \item{`n_iter_run`}{Number of iterations actually performed.}
#'     \item{`call`}{The matched call.}
#'   }
#'
#' @seealso [compute_ep()], [evaluate_plan()], [plot_ep_trace()],
#'   [print.seg_swarm_result()], [summary.seg_swarm_result()]
#'
#' @examples
#' \donttest{
#' data(nj_districts)
#'
#' # Use the bundled New Jersey district data
#' adj <- build_adjacency(nj_districts, id_col = "district_id",
#'                        lat_col = "lat", lon_col = "lon")
#' init_regions <- create_regions(nj_districts, n_regions = 21,
#'                                id_col = "district_id")
#'
#' result <- seg_swarm(
#'   enrollment  = nj_districts,
#'   adjacency   = adj,
#'   region_id   = init_regions,
#'   group_a     = "white",
#'   group_b     = "nonwhite",
#'   n_iter      = 2000,
#'   seed        = 42
#' )
#' summary(result)
#' plot(result)
#' }
#'
#' @export
seg_swarm <- function(enrollment,
                      adjacency,
                      region_id,
                      group_a,
                      group_b,
                      n_iter      = 5000L,
                      cooling_rate = 0.995,
                      temp_init   = 0.1,
                      seed        = NULL,
                      verbose     = FALSE) {

  cl <- match.call()

  # --- Input validation -------------------------------------------------------
  enrollment <- as.data.frame(enrollment)
  .validate_groups(enrollment, c(group_a, group_b))

  n_districts <- nrow(enrollment)
  adjacency   <- as.matrix(adjacency)
  stopifnot(
    nrow(adjacency) == n_districts,
    ncol(adjacency) == n_districts,
    length(region_id) == n_districts
  )

  region_id <- as.integer(region_id)
  n_iter    <- as.integer(n_iter)

  if (!is.null(seed)) set.seed(seed)

  # --- Objective: population-weighted EP across regions ----------------------
  obj_fun <- function(rid) {
    res <- aggregate_ep(enrollment, group_a, group_b, rid)
    res$ep_overall
  }

  ep_current <- obj_fun(region_id)
  ep_initial <- ep_current
  ep_trace   <- numeric(n_iter)

  temp <- temp_init

  # --- Main optimization loop ------------------------------------------------
  for (iter in seq_len(n_iter)) {

    # Pick a random district to move
    i <- sample.int(n_districts, 1L)

    # Find the set of region labels adjacent to district i (excluding its own)
    neighbours_i  <- which(adjacency[i, ] > 0)
    adjacent_regs <- unique(region_id[neighbours_i])
    adjacent_regs <- adjacent_regs[adjacent_regs != region_id[i]]

    if (length(adjacent_regs) == 0L) {
      ep_trace[iter] <- ep_current
      temp <- temp * cooling_rate
      next
    }

    # Propose moving district i to a neighbouring region
    new_reg        <- adjacent_regs[sample.int(length(adjacent_regs), 1L)]
    old_reg        <- region_id[i]
    region_id_new  <- region_id
    region_id_new[i] <- new_reg

    # Contiguity check: moving district i must not disconnect its old region
    if (!.region_connected(i, old_reg, region_id, adjacency)) {
      ep_trace[iter] <- ep_current
      temp <- temp * cooling_rate
      next
    }

    ep_new <- obj_fun(region_id_new)

    delta <- ep_new - ep_current
    accept <- delta < 0 ||
      (cooling_rate > 0 && temp > 0 &&
         stats::runif(1) < exp(-delta / temp))

    if (accept) {
      region_id  <- region_id_new
      ep_current <- ep_new
    }

    ep_trace[iter] <- ep_current
    temp <- temp * cooling_rate

    if (verbose && iter %% 500L == 0L) {
      message(sprintf("Iteration %d / %d | EP = %.4f | Temp = %.6f",
                      iter, n_iter, ep_current, temp))
    }
  }

  # Attach final region_id to the enrollment data frame
  enrollment$region_id <- region_id

  structure(
    list(
      region_id  = region_id,
      ep_final   = ep_current,
      ep_initial = ep_initial,
      ep_trace   = ep_trace,
      enrollment = enrollment,
      group_a    = group_a,
      group_b    = group_b,
      n_iter_run = n_iter,
      call       = cl
    ),
    class = "seg_swarm_result"
  )
}


#' Print a SegSwarmEP Result
#'
#' @param x A `seg_swarm_result` object.
#' @param ... Ignored.
#' @export
print.seg_swarm_result <- function(x, ...) {
  cat("SegSwarmEP Redistricting Result\n")
  cat("--------------------------------\n")
  cat(sprintf("Groups       : %s (reference) vs %s\n", x$group_a, x$group_b))
  cat(sprintf("EP (initial) : %.4f\n", x$ep_initial))
  cat(sprintf("EP (final)   : %.4f\n", x$ep_final))
  cat(sprintf("Improvement  : %.2f%%\n",
              (x$ep_initial - x$ep_final) / x$ep_initial * 100))
  cat(sprintf("Iterations   : %d\n", x$n_iter_run))
  cat(sprintf("Regions      : %d\n", length(unique(x$region_id))))
  invisible(x)
}


#' Summarise a SegSwarmEP Result
#'
#' @param object A `seg_swarm_result` object.
#' @param ... Ignored.
#'
#' @return A `data.frame` with one row per region and columns for region id,
#'   number of districts, total enrollment, and the EP and dissimilarity
#'   indices.
#'
#' @export
summary.seg_swarm_result <- function(object, ...) {
  enroll <- object$enrollment
  ga <- object$group_a
  gb <- object$group_b

  regions <- sort(unique(object$region_id))
  result  <- lapply(regions, function(r) {
    sub <- enroll[object$region_id == r, , drop = FALSE]
    ep  <- compute_ep(sub, ga, gb)
    d   <- compute_dissimilarity(sub, ga, gb)
    n_d <- nrow(sub)
    n   <- sum(rowSums(sub[, c(ga, gb)]))
    data.frame(region = r, n_districts = n_d, total_enrollment = n,
               ep = round(ep, 4), dissimilarity = round(d, 4),
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, result)
  cat(sprintf("SegSwarmEP: EP improved from %.4f to %.4f (%.1f%% reduction)\n",
              object$ep_initial, object$ep_final,
              (object$ep_initial - object$ep_final) / object$ep_initial * 100))
  out
}


#' Plot a SegSwarmEP Result
#'
#' Dispatches to [plot_ep_trace()] when called on a `seg_swarm_result` object.
#'
#' @param x A `seg_swarm_result` object.
#' @param ... Additional arguments passed to [plot_ep_trace()].
#' @export
plot.seg_swarm_result <- function(x, ...) {
  plot_ep_trace(x, ...)
}


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

# Check whether the districts in `old_reg` (excluding district i) remain
# connected in the adjacency graph after district i is moved away.
.region_connected <- function(i, old_reg, region_id, adjacency) {
  members <- which(region_id == old_reg)
  if (length(members) <= 1L) return(FALSE)   # last member; disallow move

  remaining <- setdiff(members, i)
  if (length(remaining) == 0L) return(FALSE)

  # BFS over the remaining members
  sub_adj  <- adjacency[remaining, remaining, drop = FALSE]
  visited  <- logical(length(remaining))
  queue    <- 1L
  visited[1L] <- TRUE

  while (length(queue) > 0L) {
    cur   <- queue[1L]
    queue <- queue[-1L]
    nbs   <- which(sub_adj[cur, ] > 0)
    nbs   <- nbs[!visited[nbs]]
    visited[nbs] <- TRUE
    queue <- c(queue, nbs)
  }

  all(visited)
}
