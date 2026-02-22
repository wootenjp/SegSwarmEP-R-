#' Plot the optimisation score history
#'
#' Produces a line plot of the best objective score at each ACO iteration,
#' useful for assessing whether the algorithm has converged.
#'
#' @param aco_result List returned by [aco_districting()].
#' @param title Character. Plot title. Default `"ACO Score History"`.
#'
#' @return A `ggplot2` plot object (invisibly).
#'
#' @export
plot_score_history <- function(aco_result,
                                title = "ACO Score History") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for this function. Install it with install.packages('ggplot2').")
  }
  df <- data.frame(
    iteration = seq_along(aco_result$score_history),
    score     = aco_result$score_history
  )
  p <- ggplot2::ggplot(df, ggplot2::aes(x = iteration, y = score)) +
    ggplot2::geom_line(colour = "steelblue", linewidth = 0.8) +
    ggplot2::labs(title = title, x = "Iteration", y = "Best Score") +
    ggplot2::theme_minimal()
  print(p)
  invisible(p)
}


#' Plot Theil's H history across iterations
#'
#' Shows how the best ant's Theil's H segregation index changes across
#' iterations. Downward trends indicate the algorithm is finding less-
#' segregated configurations.
#'
#' @param aco_result List returned by [aco_districting()].
#' @param title Character. Plot title. Default `"Theil H History"`.
#'
#' @return A `ggplot2` plot object (invisibly).
#'
#' @export
plot_h_history <- function(aco_result,
                            title = "Theil H History") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for this function. Install it with install.packages('ggplot2').")
  }
  df <- data.frame(
    iteration = seq_along(aco_result$h_values),
    h         = aco_result$h_values
  )
  p <- ggplot2::ggplot(df, ggplot2::aes(x = iteration, y = h)) +
    ggplot2::geom_line(colour = "tomato", linewidth = 0.8, na.rm = TRUE) +
    ggplot2::labs(title = title, x = "Iteration", y = "Theil H") +
    ggplot2::theme_minimal()
  print(p)
  invisible(p)
}


#' Plot optimised district assignments on a map
#'
#' Draws a choropleth map colouring each school district by its assigned
#' consolidated region.
#'
#' @param sf_data An `sf` object that includes a `cluster` column produced by
#'   assigning `aco_result$best_assignment` (see example).
#' @param title Character. Plot title. Default `"Optimised District Clusters"`.
#'
#' @return A `ggplot2` plot object (invisibly).
#'
#' @examples
#' \dontrun{
#' prep$sf_data$cluster <- result$best_assignment
#' plot_aco_map(prep$sf_data)
#' }
#'
#' @export
plot_aco_map <- function(sf_data,
                          title = "Optimised District Clusters") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for this function. Install it with install.packages('ggplot2').")
  }
  if (!"cluster" %in% colnames(sf_data)) {
    stop("sf_data must have a 'cluster' column. Assign aco_result$best_assignment first.")
  }
  p <- ggplot2::ggplot(sf_data) +
    ggplot2::geom_sf(ggplot2::aes(fill = factor(cluster)),
                     colour = "black", linewidth = 0.1) +
    ggplot2::scale_fill_viridis_d(name = "Region", option = "turbo") +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = title) +
    ggplot2::theme(
      plot.title   = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
      legend.position = "right"
    )
  print(p)
  invisible(p)
}


#' Summarise demographic composition of final clusters
#'
#' Returns a data frame with total and percentage enrollment for each
#' demographic group per consolidated cluster.
#'
#' @param sf_data An `sf` object with a `cluster` column.
#' @param group_a Character. Column name for demographic group A.
#' @param group_b Character. Column name for demographic group B.
#' @param total_col Character. Column name for total enrollment. Default
#'   `"total"`.
#'
#' @return A `data.frame` with one row per cluster and columns:
#'   `cluster`, `n_districts`, `total`, `group_a`, `group_b`,
#'   `pct_group_a`, `pct_group_b`.
#'
#' @export
summarise_clusters <- function(sf_data, group_a, group_b, total_col = "total") {
  if (!"cluster" %in% colnames(sf_data)) {
    stop("sf_data must have a 'cluster' column.")
  }
  df <- sf::st_drop_geometry(sf_data)
  clusters <- sort(unique(df$cluster))

  do.call(rbind, lapply(clusters, function(cl) {
    rows      <- df[df$cluster == cl, ]
    ga        <- sum(rows[[group_a]], na.rm = TRUE)
    gb        <- sum(rows[[group_b]], na.rm = TRUE)
    tot       <- sum(rows[[total_col]], na.rm = TRUE)
    data.frame(
      cluster      = cl,
      n_districts  = nrow(rows),
      total        = tot,
      group_a      = ga,
      group_b      = gb,
      pct_group_a  = if (tot > 0) ga / tot * 100 else NA_real_,
      pct_group_b  = if (tot > 0) gb / tot * 100 else NA_real_,
      stringsAsFactors = FALSE
    )
  }))
}


#' Check contiguity of all clusters
#'
#' Returns a named logical vector indicating whether each cluster in
#' `sf_data$cluster` forms a geographically contiguous region.
#'
#' @param sf_data An `sf` object with a `cluster` column.
#' @param adj_matrix Adjacency matrix from [prepare_districts()].
#'
#' @return Named logical vector (one element per cluster).
#'
#' @export
check_contiguity <- function(sf_data, adj_matrix) {
  if (!"cluster" %in% colnames(sf_data)) {
    stop("sf_data must have a 'cluster' column.")
  }
  clusters <- sort(unique(sf_data$cluster))
  result   <- vapply(clusters, function(cl) {
    nodes <- which(sf_data$cluster == cl)
    .is_contiguous(nodes, adj_matrix)
  }, logical(1L))
  names(result) <- clusters
  result
}
