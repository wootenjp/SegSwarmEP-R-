#' Calculate the Polsby-Popper compactness score of a district geometry
#'
#' @param district_geometry An `sfc` geometry object representing the merged
#'   polygon of a single district (typically from [sf::st_union()]).
#'
#' @return A numeric value in (0, 1]. Values closer to 1 indicate a more
#'   compact (circle-like) shape.
#'
#' @export
calculate_polsby_popper <- function(district_geometry) {
  area      <- sf::st_area(district_geometry)
  perimeter <- sf::st_length(sf::st_boundary(district_geometry))
  circle_area <- (perimeter / (2 * pi))^2 * pi
  as.numeric(area / circle_area)
}


#' Score a candidate district assignment
#'
#' Evaluates a candidate assignment vector using three components:
#' geographic contiguity (penalty/bonus), compactness (Polsby-Popper), and
#' demographic segregation (Theil's H) across the proposed districts.
#'
#' @param assignment Integer vector of length `n` (number of school districts)
#'   where each element is the proposed consolidated region (1 to
#'   `num_districts`) that the school district belongs to.
#' @param adj_matrix Square 0/1 adjacency matrix (see [prepare_districts()]).
#' @param sf_data The `sf` object returned by [prepare_districts()] (must be
#'   in a metric CRS with `center_x`/`center_y` columns).
#' @param group_a Character. Column name for demographic group A enrollment.
#' @param group_b Character. Column name for demographic group B enrollment.
#' @param total_col Character. Column name for total enrollment.
#' @param id_col Character or `NULL`. Column name for district identifiers.
#' @param county_col Character or `NULL`. Column name for county.
#' @param penalty_contiguity Numeric. Score penalty applied per non-contiguous
#'   district (should be negative).
#' @param bonus_contiguity Numeric. Score bonus applied per contiguous district.
#' @param compactness_scale Numeric. Multiplier applied to the Polsby-Popper
#'   score contribution.
#' @param demographic_scale Numeric. Multiplier applied to the demographic
#'   (Theil's H) score contribution.
#' @param num_districts Integer. Expected number of consolidated districts.
#' @param max_district_size Numeric. Maximum total enrollment permitted per
#'   consolidated district. Violations incur a proportional penalty.
#'
#' @return A single numeric score (higher is better), or `NA` if scoring fails
#'   (e.g., empty districts or missing demographic data).
#'
#' @export
calculate_score <- function(assignment, adj_matrix, sf_data,
                             group_a, group_b, total_col,
                             id_col = NULL, county_col = NULL,
                             penalty_contiguity, bonus_contiguity,
                             compactness_scale, demographic_scale,
                             num_districts, max_district_size) {
  if (max(assignment, na.rm = TRUE) == -Inf) return(NA_real_)

  score <- 0

  districts_result <- .create_districts_result(
    assignment, sf_data, group_a, group_b, total_col, id_col, county_col
  )
  if (nrow(districts_result) == 0L) return(NA_real_)

  # Long-format data for segregation computation
  data_long <- reshape2::melt(
    districts_result,
    id.vars      = c("cluster"),
    measure.vars = c(group_a, group_b),
    variable.name = "race",
    value.name    = "population"
  )

  # Theil's H (only when all clusters present)
  if (length(unique(data_long$cluster)) == num_districts) {
    h_res <- tryCatch(
      segregation::mutual_total(data_long, "race", "cluster", weight = "population"),
      error = function(e) NULL
    )
    demographic_difference <- if (!is.null(h_res) && nrow(h_res) >= 2) {
      as.numeric(h_res[2, 2])
    } else {
      NA_real_
    }
  } else {
    demographic_difference <- NA_real_
  }

  if (is.na(demographic_difference)) return(NA_real_)

  # Per-district contiguity + compactness + size penalty
  for (i in seq_len(num_districts)) {
    district_nodes <- which(assignment == i)
    if (length(district_nodes) == 0L) return(NA_real_)

    if (!.is_contiguous(district_nodes, adj_matrix)) {
      score <- score + penalty_contiguity
    } else {
      score <- score + bonus_contiguity
    }

    district_size <- sum(sf_data[[total_col]][district_nodes], na.rm = TRUE)
    if (district_size > max_district_size) {
      score <- score - (district_size - max_district_size) / 1000
    }

    geom       <- sf::st_union(sf_data[district_nodes, "geometry"])
    compactness <- calculate_polsby_popper(geom)
    score       <- score + compactness * compactness_scale
  }

  # Demographic component (scaled so 0 segregation = 10, perfect H = −90)
  demo_term <- 10 - (demographic_difference * 100)
  score <- score + demo_term * demographic_scale

  score
}
