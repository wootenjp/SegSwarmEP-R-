#' Plot the EP Optimization Trace
#'
#' Plots the Exposure-Proportion (EP) segregation index at every iteration of
#' the [seg_swarm()] optimization, making it easy to assess convergence.
#'
#' @param result A `seg_swarm_result` object returned by [seg_swarm()].
#' @param xlab Character string; x-axis label (default `"Iteration"`).
#' @param ylab Character string; y-axis label.
#' @param main Character string; plot title.
#' @param col Line colour (default `"steelblue"`).
#' @param ... Additional arguments passed to [graphics::plot()].
#'
#' @return Invisibly returns `result`.
#'
#' @seealso [seg_swarm()], [plot_region_bars()]
#'
#' @examples
#' \donttest{
#' data(nj_districts)
#' adj  <- build_adjacency(nj_districts, id_col = "district_id")
#' init <- create_regions(nj_districts, n_regions = 21, id_col = "district_id")
#' res  <- seg_swarm(nj_districts, adj, init, "white", "nonwhite",
#'                   n_iter = 500, seed = 1)
#' plot_ep_trace(res)
#' }
#'
#' @export
plot_ep_trace <- function(result,
                          xlab = "Iteration",
                          ylab = "EP Segregation Index",
                          main = "SegSwarmEP Optimization Trace",
                          col  = "steelblue",
                          ...) {

  stopifnot(inherits(result, "seg_swarm_result"))

  trace <- result$ep_trace
  iters <- seq_along(trace)

  graphics::plot(iters, trace,
                 type  = "l",
                 xlab  = xlab,
                 ylab  = ylab,
                 main  = main,
                 col   = col,
                 las   = 1,
                 ...)

  graphics::abline(h = result$ep_initial, lty = 2, col = "grey50")
  graphics::abline(h = result$ep_final,   lty = 2, col = "tomato")
  graphics::legend("topright",
                   legend = c("Initial EP", "Final EP"),
                   lty    = 2,
                   col    = c("grey50", "tomato"),
                   bty    = "n")
  invisible(result)
}


#' Bar Chart of EP Index by Region
#'
#' Displays the Exposure-Proportion index for each region in a redistricting
#' plan, sorted from highest to lowest segregation.
#'
#' @param result A `seg_swarm_result` object returned by [seg_swarm()], **or**
#'   the list returned by [evaluate_plan()].
#' @param group_a Character string; reference group label used in the y-axis
#'   title. Required only when `result` is a plain `evaluate_plan()` list.
#' @param group_b Character string; comparison group label. Required only when
#'   `result` is a plain `evaluate_plan()` list.
#' @param col Bar fill colour (default `"steelblue"`).
#' @param main Character string; plot title.
#' @param ... Additional arguments passed to [graphics::barplot()].
#'
#' @return Invisibly returns the named vector of per-region EP values.
#'
#' @examples
#' \donttest{
#' data(nj_districts)
#' adj  <- build_adjacency(nj_districts, id_col = "district_id")
#' init <- create_regions(nj_districts, n_regions = 21, id_col = "district_id")
#' res  <- seg_swarm(nj_districts, adj, init, "white", "nonwhite",
#'                   n_iter = 500, seed = 1)
#' plot_region_bars(res)
#' }
#'
#' @export
plot_region_bars <- function(result,
                             group_a = NULL,
                             group_b = NULL,
                             col     = "steelblue",
                             main    = "EP Index by Region",
                             ...) {

  if (inherits(result, "seg_swarm_result")) {
    ep_vals <- aggregate_ep(result$enrollment,
                            result$group_a,
                            result$group_b,
                            result$region_id)$ep_by_region
    ga <- result$group_a
    gb <- result$group_b
  } else {
    ep_vals <- result$ep_by_region
    ga <- if (!is.null(group_a)) group_a else "group A"
    gb <- if (!is.null(group_b)) group_b else "group B"
  }

  ep_sorted <- sort(ep_vals, decreasing = TRUE)

  graphics::barplot(ep_sorted,
                    col   = col,
                    main  = main,
                    xlab  = "Region",
                    ylab  = sprintf("EP (%s exposed to %s)", ga, gb),
                    las   = 2,
                    names.arg = names(ep_sorted),
                    ...)

  graphics::abline(h = mean(ep_vals, na.rm = TRUE),
                   lty = 2, col = "tomato")
  graphics::legend("topright",
                   legend = "Overall mean",
                   lty    = 2,
                   col    = "tomato",
                   bty    = "n")
  invisible(ep_sorted)
}


#' Choropleth Map of Region Assignments
#'
#' When the `sf` package is available and `shape` is supplied, draws a simple
#' choropleth map colouring districts by their region assignment.
#'
#' @param result A `seg_swarm_result` object returned by [seg_swarm()].
#' @param shape An `sf` object whose rows correspond (in order) to the rows
#'   of `result$enrollment`.
#' @param id_col Character string; column in `shape` containing district IDs.
#'   Used to join `result$enrollment` when row order differs.
#' @param palette A vector of colours; if shorter than the number of regions it
#'   is recycled. Defaults to [grDevices::rainbow()].
#' @param main Character string; plot title.
#'
#' @return Invisibly returns the `sf` object with a `region_id` column added.
#'
#' @export
plot_region_map <- function(result,
                            shape,
                            id_col  = NULL,
                            palette = NULL,
                            main    = "Proposed Region Assignments") {

  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("The 'sf' package is required for map plots. ",
         "Install it with: install.packages('sf')", call. = FALSE)
  }

  stopifnot(inherits(result, "seg_swarm_result"))

  n_reg <- length(unique(result$region_id))
  if (is.null(palette)) {
    palette <- grDevices::rainbow(n_reg)
  }

  shape$region_id <- result$region_id

  col_vec <- palette[(shape$region_id - min(shape$region_id)) %% length(palette) + 1]

  plot(sf::st_geometry(shape), col = col_vec, main = main, border = "white")
  invisible(shape)
}
