#' Run a single ACO ant to build one candidate redistricting assignment
#'
#' This function is called internally by [aco_districting()] inside a parallel
#' worker. It builds a complete district assignment for one ant by:
#' 1. Seeding each target district with a starting node.
#' 2. Growing contiguous clusters greedily using pheromone and degree heuristics.
#' 3. Recycling any unassigned nodes into existing districts.
#'
#' @param graph An `igraph` object (currently unused internally; reserved for
#'   future graph-based heuristics). Must be present in the worker environment.
#' @param adj_matrix Adjacency matrix (from [prepare_districts()]).
#' @param num_districts Integer. Number of consolidated districts to form.
#' @param alpha Numeric. Pheromone exponent (influence of pheromone trails).
#' @param beta Numeric. Heuristic exponent (influence of node degree).
#' @param rho Numeric. Evaporation rate (not used directly here; used in outer
#'   loop).
#' @param sf_data The `sf` spatial data frame (with `center_x`, `center_y`, and
#'   a total column). Must be exported to the parallel worker environment.
#' @param total_col Character. Name of the total enrollment column.
#' @param max_district_size Numeric. Maximum enrollment per new district.
#' @param seed_districts Integer vector of length `num_districts` giving the
#'   starting node index for each new district.
#' @param iteration Integer. Current iteration number (informational).
#' @param search_radius Numeric. Maximum centroid distance (CRS units) an ant
#'   will consider when expanding a district.
#'
#' @return An integer vector of length `n` (number of school districts) where
#'   each element gives the consolidated district (1 to `num_districts`) that
#'   the school district is assigned to. May contain `NA` for unassigned nodes
#'   if recycling fails.
#'
#' @export
process_ant <- function(graph, adj_matrix, num_districts, alpha, beta, rho,
                         sf_data, total_col, max_district_size,
                         seed_districts, iteration, search_radius) {
  num_nodes        <- nrow(adj_matrix)
  assignment       <- rep(NA_integer_, num_nodes)
  unassigned_nodes <- seq_len(num_nodes)
  node_degrees     <- rowSums(adj_matrix)
  cluster_sizes    <- rep(0, num_districts)

  # pheromone is accessed from the worker's global environment (cluster export)

  .grow_district <- function(district_index, initial_node, current_search_radius) {
    assignment[initial_node]   <<- district_index
    unassigned_nodes           <<- setdiff(unassigned_nodes, initial_node)
    district_nodes              <- initial_node
    cluster_sizes[district_index] <<- cluster_sizes[district_index] + sf_data[[total_col]][initial_node]
    iteration_count             <- 0L

    seed_x      <- sf_data$center_x[initial_node]
    seed_y      <- sf_data$center_y[initial_node]
    distances   <- sqrt((sf_data$center_x - seed_x)^2 + (sf_data$center_y - seed_y)^2)
    eligible    <- unassigned_nodes[distances[unassigned_nodes] <= current_search_radius]

    while (length(eligible) > 0L) {
      iteration_count <- iteration_count + 1L

      neighbors  <- which(colSums(adj_matrix[district_nodes, , drop = FALSE]) > 0L)
      neighbors  <- setdiff(neighbors, district_nodes)
      candidates <- intersect(neighbors, eligible)

      candidates <- candidates[
        (cluster_sizes[district_index] + sf_data[[total_col]][candidates]) <= max_district_size
      ]

      if (length(candidates) == 0L) break

      probs <- (pheromone[district_nodes[1L], candidates]^alpha) *
               (node_degrees[candidates]^beta)
      if (sum(probs) == 0) probs <- rep(1 / length(probs), length(probs))
      else                  probs <- probs / sum(probs)

      next_node             <- if (length(candidates) > 1L) sample(candidates, 1L, prob = probs)
                               else                         candidates[1L]

      assignment[next_node]           <<- district_index
      unassigned_nodes                <<- setdiff(unassigned_nodes, next_node)
      district_nodes                  <- c(district_nodes, next_node)
      cluster_sizes[district_index]   <<- cluster_sizes[district_index] +
                                           sf_data[[total_col]][next_node]

      eligible <- unassigned_nodes[distances[unassigned_nodes] <= current_search_radius]

      if (iteration_count > 500000L) break
    }
  }

  # Primary pass: seed each district and grow
  for (k in seq_len(num_districts)) {
    if (length(unassigned_nodes) == 0L) break
    init <- if (k <= length(seed_districts)) seed_districts[k]
            else                             sample(unassigned_nodes, 1L)
    .grow_district(k, init, search_radius)
  }

  # Recycling pass: assign any remaining nodes
  recycle_count <- 0L
  while (length(unassigned_nodes) > 0L && recycle_count < 500000L) {
    recycle_count <- recycle_count + 1L
    for (k in seq_len(num_districts)) {
      if (length(unassigned_nodes) == 0L) break
      init <- sample(unassigned_nodes, 1L)
      .grow_district(k, init, search_radius)
    }
  }

  assignment
}


#' Run the Ant Colony Optimization redistricting algorithm
#'
#' The main entry point for SegSwarmEP. Runs an Ant Colony Optimization (ACO)
#' heuristic to find a school district grouping that minimises racial/demographic
#' segregation (Theil's H) while respecting geographic contiguity and
#' compactness constraints.
#'
#' @param sf_data An `sf` object returned and modified by [prepare_districts()]
#'   (must have `center_x` and `center_y` columns and be in a metric CRS).
#' @param adj_matrix Square adjacency matrix returned by [prepare_districts()].
#' @param graph `igraph` graph returned by [prepare_districts()].
#' @param num_districts Integer. Target number of consolidated school districts
#'   (regions) to form.
#' @param group_a Character. Column name in `sf_data` for demographic group A
#'   enrollment (e.g., `"white"`).
#' @param group_b Character. Column name in `sf_data` for demographic group B
#'   enrollment (e.g., `"nonwhite"`).
#' @param total_col Character. Column name for total enrollment. Default
#'   `"total"`.
#' @param id_col Character or `NULL`. Column name for district identifiers
#'   (used in result summaries). Default `NULL`.
#' @param county_col Character or `NULL`. Column name for county (used in
#'   result summaries). Default `NULL`.
#' @param max_district_size Numeric. Maximum total student enrollment permitted
#'   in any single consolidated district. Default `Inf` (no limit).
#' @param num_ants Integer. Number of ants per iteration. Higher values improve
#'   solution quality at a cost of runtime. Default `400`.
#' @param num_iterations Integer. Total number of ACO iterations. Default `320`.
#' @param demographic_scale Numeric. Multiplier for the demographic (Theil's H)
#'   component of the objective score. Default `45`.
#' @param compactness_scale Numeric. Multiplier for the Polsby-Popper
#'   compactness component. Default `120`.
#' @param penalty_contiguity Numeric. Score penalty per non-contiguous district
#'   (must be negative). Default `-30`.
#' @param bonus_contiguity Numeric. Score bonus per contiguous district.
#'   Default `60`.
#' @param size_scale_factor Numeric. Scaling factor for the size-based
#'   pheromone evaporation adjustment. Default `log(10)`.
#' @param num_cores Integer. Number of parallel cores. Default
#'   `parallel::detectCores() - 1`.
#' @param num_elite Integer. Initial number of elite solutions maintained.
#'   Default `5`.
#' @param verbose Logical. If `TRUE` (default), print iteration progress.
#'
#' @return A named list with:
#' \describe{
#'   \item{`best_assignment`}{Integer vector of length `n` giving the optimal
#'     district assignment found.}
#'   \item{`score_history`}{Numeric vector of the best score at each
#'     iteration.}
#'   \item{`h_values`}{Numeric vector of Theil's H for the best ant at each
#'     iteration.}
#'   \item{`demographic_history`}{List of `data.table` objects (one per
#'     iteration) with the best ant's demographic breakdown.}
#'   \item{`pre_elite_assignments`}{List of assignment vectors collected
#'     during the burn-in phase.}
#'   \item{`pre_elite_scores`}{Numeric vector of scores for the pre-elite
#'     assignments.}
#' }
#'
#' @examples
#' \dontrun{
#' prep   <- prepare_districts(sf_nj, crs = 32618)
#' result <- aco_districting(
#'   sf_data       = prep$sf_data,
#'   adj_matrix    = prep$adj_matrix,
#'   graph         = prep$graph,
#'   num_districts = 21,
#'   group_a       = "white",
#'   group_b       = "nonwhite",
#'   total_col     = "total",
#'   max_district_size = 124000,
#'   num_ants      = 400,
#'   num_iterations = 320
#' )
#' prep$sf_data$cluster <- result$best_assignment
#' }
#'
#' @export
aco_districting <- function(sf_data,
                             adj_matrix,
                             graph,
                             num_districts,
                             group_a,
                             group_b,
                             total_col          = "total",
                             id_col             = NULL,
                             county_col         = NULL,
                             max_district_size  = Inf,
                             num_ants           = 400L,
                             num_iterations     = 320L,
                             demographic_scale  = 45,
                             compactness_scale  = 120,
                             penalty_contiguity = -30,
                             bonus_contiguity   = 60,
                             size_scale_factor  = log(10),
                             num_cores          = parallel::detectCores() - 1L,
                             num_elite          = 5L,
                             verbose            = TRUE) {

  num_nodes          <- nrow(adj_matrix)
  best_assignment    <- NULL
  best_score         <- -Inf
  demographic_history <- list()
  h_values           <- numeric(num_iterations)
  score_history      <- numeric(num_iterations)
  pheromone          <- matrix(1, nrow = num_nodes, ncol = num_nodes)
  elite_assignments  <- list()
  elite_scores       <- rep(-Inf, num_elite)
  pre_elite_assignments <- list()
  pre_elite_scores   <- numeric()

  score_min <- penalty_contiguity * num_districts +
               (0 * compactness_scale) +
               (0 * demographic_scale)
  score_max <- bonus_contiguity * num_districts +
               (10 * demographic_scale) +
               (1 * compactness_scale * num_districts)

  # Adaptive elite parameters
  n_elite           <- num_elite
  n_elite_min       <- 10L
  n_elite_max       <- 50L
  elite_boost_factor <- 10
  gap_scaling_factor <- 20
  gap_baseline_low  <- 0.15
  gap_baseline_high <- 0.25

  # Adaptive ACO parameter schedules
  alpha_start        <- 0.8;   alpha_end        <- 4.5
  beta_start         <- 10;    beta_end         <- 4.5
  rho_start          <- 1;     rho_end          <- 0.1
  search_radius_start <- 1250; search_radius_end <- 12500

  # Set up parallel cluster
  cl <- parallel::makeCluster(num_cores)
  on.exit(parallel::stopCluster(cl), add = TRUE)

  parallel::clusterEvalQ(cl, {
    library(igraph)
    library(sf)
    library(reshape2)
    library(segregation)
    library(data.table)
  })

  # Export package-internal functions to workers from the package namespace.
  # This works whether SegSwarmEP is installed or loaded via devtools::load_all().
  pkg_ns <- asNamespace("SegSwarmEP")
  parallel::clusterExport(cl, c(
    "process_ant",
    "calculate_score",
    "calculate_polsby_popper",
    ".is_contiguous",
    ".create_districts_result"
  ), envir = pkg_ns)

  # Export static data objects once
  parallel::clusterExport(cl, c(
    "sf_data", "adj_matrix", "graph",
    "num_districts", "max_district_size",
    "penalty_contiguity", "bonus_contiguity",
    "compactness_scale", "demographic_scale",
    "group_a", "group_b", "total_col", "id_col", "county_col",
    "size_scale_factor"
  ), envir = environment())

  iteration <- 1L

  while (iteration <= num_iterations) {
    burn_in <- as.integer(round(num_iterations / 4))

    if (iteration <= burn_in) {
      rho           <- rho_start
      alpha         <- alpha_start
      beta          <- beta_start
      search_radius <- search_radius_start
    } else {
      scaled_iter   <- (iteration - burn_in) / (num_iterations - burn_in)
      scale_fac     <- log10(1 + 9 * scaled_iter)
      rho           <- rho_start   + (rho_end   - rho_start)   * scale_fac
      alpha         <- alpha_start + (alpha_end - alpha_start) * scale_fac
      beta          <- beta_start  + (beta_end  - beta_start)  * scale_fac
      search_radius <- search_radius_start +
                       (search_radius_end - search_radius_start) * scale_fac
    }

    seed_districts <- sample(seq_len(nrow(sf_data)), num_districts, replace = FALSE)

    parallel::clusterExport(cl,
      c("rho", "alpha", "beta", "search_radius", "pheromone", "seed_districts", "iteration"),
      envir = environment()
    )

    ant_results <- parallel::parLapply(cl, seq_len(num_ants), function(ant_idx) {
      asgn  <- process_ant(graph, adj_matrix, num_districts, alpha, beta, rho,
                           sf_data, total_col, max_district_size,
                           seed_districts, iteration, search_radius)
      score <- calculate_score(asgn, adj_matrix, sf_data,
                                group_a, group_b, total_col, id_col, county_col,
                                penalty_contiguity, bonus_contiguity,
                                compactness_scale, demographic_scale,
                                num_districts, max_district_size)
      list(assignment = asgn, score = score, iteration = iteration)
    })

    ant_assignments <- lapply(ant_results, `[[`, "assignment")
    ant_scores      <- unlist(lapply(ant_results, `[[`, "score"))

    # Filter NA scores
    valid           <- !is.na(ant_scores)
    ant_assignments <- ant_assignments[valid]
    ant_scores      <- ant_scores[valid]

    if (length(ant_scores) == 0L) {
      if (verbose) message("Iteration ", iteration, ": all ant scores NA, skipping.")
      iteration <- iteration + 1L
      next
    }

    # Standardise scores for adaptive scheduling
    .std <- function(x) pmax(0, pmin(1, (x - score_min) / (score_max - score_min)))
    ant_scores_std   <- .std(ant_scores)
    elite_scores_std <- .std(elite_scores)

    # Adaptive elite count (after burn-in)
    if (iteration > burn_in) {
      valid_elite <- elite_scores_std[!is.infinite(elite_scores_std)]
      if (length(valid_elite) > 0L && length(ant_scores_std) > 0L) {
        gap <- stats::median(valid_elite, na.rm = TRUE) -
               stats::median(ant_scores_std, na.rm = TRUE)
        if (gap < gap_baseline_high) {
          n_elite <- min(n_elite_max,
                         round(n_elite + (gap_baseline_high - gap) * elite_boost_factor))
        } else {
          n_elite <- max(n_elite_min,
                         round(n_elite * exp(-(gap - gap_baseline_high) * gap_scaling_factor)))
        }
        n_elite <- min(n_elite_max, max(n_elite_min, n_elite))

        # Resize elite pool if needed
        if (length(elite_scores) > n_elite) {
          keep <- order(elite_scores, decreasing = TRUE)[seq_len(n_elite)]
          elite_scores      <- elite_scores[keep]
          elite_assignments <- elite_assignments[keep]
          elite_scores_std  <- elite_scores_std[keep]
        } else if (length(elite_scores) < n_elite) {
          pad               <- n_elite - length(elite_scores)
          elite_scores      <- c(elite_scores,      rep(-Inf, pad))
          elite_assignments <- c(elite_assignments, vector("list", pad))
          elite_scores_std  <- c(elite_scores_std,  rep(0, pad))
        }

        # Promote best ants into elite pool
        if (length(ant_scores_std) >= n_elite) {
          top_idx <- order(ant_scores_std, decreasing = TRUE)[seq_len(n_elite)]
          for (i in seq_along(top_idx)) {
            cand_score <- ant_scores[top_idx[i]]
            if (!is.na(cand_score) && cand_score > min(elite_scores, na.rm = TRUE)) {
              slot <- which.min(elite_scores)
              elite_scores[slot]        <- cand_score
              elite_assignments[[slot]] <- ant_assignments[[top_idx[i]]]
              elite_scores_std[slot]    <- ant_scores_std[top_idx[i]]
            }
          }
        }
      }
    } else {
      n_elite <- 50L
    }

    # Update global best
    best_ant_idx <- which.max(ant_scores)
    if (!is.na(best_ant_idx) && ant_scores[best_ant_idx] > best_score) {
      best_assignment <- ant_assignments[[best_ant_idx]]
      best_score      <- ant_scores[best_ant_idx]
    }

    # Collect pre-elite solutions during burn-in
    if (iteration <= burn_in) {
      for (i in seq_along(ant_scores)) {
        if (!is.na(ant_scores[i])) {
          pre_elite_assignments <- c(pre_elite_assignments, list(ant_assignments[[i]]))
          pre_elite_scores      <- c(pre_elite_scores, ant_scores[i])
        }
      }
    }

    # Initialise elite pool at end of burn-in
    if (iteration == burn_in) {
      for (i in seq_along(pre_elite_scores)) {
        s <- pre_elite_scores[i]
        if (!is.na(s) && s > min(elite_scores, na.rm = TRUE)) {
          slot <- which.min(elite_scores)
          elite_scores[slot]        <- s
          elite_assignments[[slot]] <- pre_elite_assignments[[i]]
          if (verbose) message("  Initialized elite slot ", slot, " with score ", round(s, 2))
        }
      }
    }

    # Record H value for best ant of this iteration
    districts_result <- .create_districts_result(
      ant_assignments[[best_ant_idx]], sf_data, group_a, group_b, total_col, id_col, county_col
    )
    demographic_history[[iteration]] <- districts_result

    if (nrow(districts_result) > 0L) {
      data.table::setDT(districts_result)
      data_long <- reshape2::melt(
        districts_result,
        id.vars      = "cluster",
        measure.vars = c(group_a, group_b),
        variable.name = "race",
        value.name    = "population"
      )
      if (length(unique(data_long$cluster)) == num_districts) {
        h_res <- tryCatch(
          segregation::mutual_total(data_long, "race", "cluster", weight = "population"),
          error = function(e) NULL
        )
        h_values[iteration] <- if (!is.null(h_res) && nrow(h_res) >= 2) {
          as.numeric(h_res[2, 2])
        } else NA_real_
      } else {
        h_values[iteration] <- NA_real_
      }
    } else {
      h_values[iteration] <- NA_real_
    }

    score_history[iteration] <- best_score

    if (verbose) {
      best_std <- max(0, min(1, (best_score - score_min) / (score_max - score_min)))
      message(sprintf("Iteration %d | best score: %.1f (std: %.3f) | H: %.5f",
                      iteration, best_score, best_std,
                      if (is.na(h_values[iteration])) NaN else h_values[iteration]))
    }

    # Pheromone evaporation
    size_slide_threshold <- 0.935 * max_district_size
    pheromone <- (1 - rho) * pheromone
    pheromone[pheromone < 0] <- 0

    # Regular pheromone update (best assignment, vectorised)
    if (!is.null(best_assignment) && !all(is.na(best_assignment)) &&
        !is.nan(best_score) && best_score > 0) {
      scaled_score <- log(best_score + 1)
      for (d in seq_len(num_districts)) {
        nodes_d <- which(best_assignment == d)
        if (length(nodes_d) > 1L) {
          pairs <- utils::combn(nodes_d, 2)
          for (k in seq_len(ncol(pairs))) {
            i <- pairs[1, k]; j <- pairs[2, k]
            pheromone[i, j] <- pheromone[i, j] + scaled_score
            pheromone[j, i] <- pheromone[i, j]
          }
        }
      }
    }

    # Size-based pheromone evaporation adjustment
    dist_df <- sf::st_drop_geometry(sf_data)
    dist_df[[total_col]] <- as.numeric(dist_df[[total_col]])
    for (d in seq_len(num_districts)) {
      nodes_d       <- which(best_assignment == d)
      if (length(nodes_d) == 0L) next
      district_size <- sum(dist_df[nodes_d, total_col], na.rm = TRUE)
      if (district_size > max_district_size) {
        rho <- 1
      } else if (district_size > size_slide_threshold) {
        size_factor <- min(1, ((district_size - size_slide_threshold) * size_scale_factor) /
                               (max_district_size - size_slide_threshold))
        rho <- min(1, rho + size_factor)
      }
    }

    # Elitist pheromone update (after burn-in)
    if (iteration > burn_in && length(elite_assignments) > 0L) {
      for (idx in seq_along(elite_assignments)) {
        ea <- elite_assignments[[idx]]
        if (is.null(ea)) next
        es <- elite_scores[idx]
        scaled_elite <- if (is.nan(es) || es <= 0) 0 else log(es + 1) * 1.5
        if (scaled_elite == 0) next
        for (d in seq_len(num_districts)) {
          nodes_d <- which(ea == d)
          if (length(nodes_d) > 1L) {
            pairs <- utils::combn(nodes_d, 2)
            for (k in seq_len(ncol(pairs))) {
              i <- pairs[1, k]; j <- pairs[2, k]
              pheromone[i, j] <- pheromone[i, j] + scaled_elite
              pheromone[j, i] <- pheromone[i, j]
            }
          }
        }
      }
    }

    iteration <- iteration + 1L
  }

  list(
    best_assignment       = best_assignment,
    score_history         = score_history[seq_len(iteration - 1L)],
    h_values              = h_values[seq_len(iteration - 1L)],
    demographic_history   = demographic_history[seq_len(iteration - 1L)],
    pre_elite_assignments = pre_elite_assignments,
    pre_elite_scores      = pre_elite_scores
  )
}
