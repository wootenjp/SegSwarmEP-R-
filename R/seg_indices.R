#' Compute the Exposure-Proportion (EP) Segregation Index
#'
#' Computes the exposure-proportion index, which measures the average proportion
#' of a given group (e.g., non-white students) in the school of a member of the
#' reference group (e.g., white students). When used to measure isolation, the
#' reference group and the exposure group are the same.
#'
#' @param enrollment A numeric matrix or data frame where each row is a school
#'   district and each column is a demographic group. Column names should
#'   identify the groups.
#' @param group_a Character string; name of the reference group column (whose
#'   exposure is being measured).
#' @param group_b Character string; name of the exposed-to group column. When
#'   `group_b == group_a` the result is the isolation index.
#' @param region_id Optional integer or character vector of length
#'   `nrow(enrollment)` indicating which region each district belongs to. When
#'   supplied the index is computed for each region separately and the
#'   population-weighted average across regions is returned via
#'   [aggregate_ep()].
#'
#' @return A single numeric value between 0 and 1. Higher values indicate
#'   greater exposure (or isolation when `group_a == group_b`).
#'
#' @references
#' Massey, D. S., & Denton, N. A. (1988). The dimensions of residential
#' segregation. *Social Forces*, 67(2), 281–315.
#'
#' @examples
#' # Simple two-district example
#' enroll <- data.frame(white = c(200, 50), nonwhite = c(50, 200))
#' compute_ep(enroll, group_a = "white", group_b = "nonwhite")
#' compute_ep(enroll, group_a = "white", group_b = "white")  # isolation
#'
#' @export
compute_ep <- function(enrollment, group_a, group_b, region_id = NULL) {
  enrollment <- as.data.frame(enrollment)
  .validate_groups(enrollment, c(group_a, group_b))

  if (!is.null(region_id)) {
    stopifnot(length(region_id) == nrow(enrollment))
    return(aggregate_ep(enrollment, group_a, group_b, region_id))
  }

  a  <- enrollment[[group_a]]
  b  <- enrollment[[group_b]]
  n  <- rowSums(enrollment)

  A <- sum(a)
  if (A == 0) return(NA_real_)

  # EP_{a -> b} = sum_i [ (a_i / A) * (b_i / n_i) ]
  # Guard against empty districts (n_i == 0)
  prop_b <- ifelse(n > 0, b / n, 0)
  ep <- sum((a / A) * prop_b)
  ep
}


#' Compute the Dissimilarity Index
#'
#' The dissimilarity index (D) measures the percentage of one group that would
#' need to move to achieve an even distribution across all districts within a
#' region (or study area).
#'
#' @inheritParams compute_ep
#' @param group_a Character string; name of the first group column.
#' @param group_b Character string; name of the second group column.
#'
#' @return A single numeric value between 0 and 1.  Values near 0 indicate even
#'   distribution; values near 1 indicate complete separation.
#'
#' @references
#' Duncan, O. D., & Duncan, B. (1955). A methodological analysis of segregation
#' indexes. *American Sociological Review*, 20(2), 210–217.
#'
#' @examples
#' enroll <- data.frame(white = c(200, 50), nonwhite = c(50, 200))
#' compute_dissimilarity(enroll, group_a = "white", group_b = "nonwhite")
#'
#' @export
compute_dissimilarity <- function(enrollment, group_a, group_b) {
  enrollment <- as.data.frame(enrollment)
  .validate_groups(enrollment, c(group_a, group_b))

  a <- enrollment[[group_a]]
  b <- enrollment[[group_b]]
  A <- sum(a)
  B <- sum(b)

  if (A == 0 || B == 0) return(NA_real_)

  d <- 0.5 * sum(abs(a / A - b / B))
  d
}


#' Compute the Isolation Index
#'
#' A convenience wrapper around [compute_ep()] that returns the isolation index
#' (i.e., the exposure of a group to itself).
#'
#' @inheritParams compute_ep
#' @param group Character string; name of the group column.
#'
#' @return A single numeric value between 0 and 1.
#'
#' @examples
#' enroll <- data.frame(white = c(200, 50), nonwhite = c(50, 200))
#' compute_isolation(enroll, group = "white")
#'
#' @export
compute_isolation <- function(enrollment, group, region_id = NULL) {
  compute_ep(enrollment, group_a = group, group_b = group,
             region_id = region_id)
}


#' Compute the Theil H (Information Theory) Index
#'
#' Measures multigroup segregation using information theory. H equals 0 when
#' the composition of every district matches the overall composition, and 1
#' when every district is composed of a single group.
#'
#' @inheritParams compute_ep
#' @param groups Character vector of column names representing the demographic
#'   groups to include. Defaults to all columns of `enrollment`.
#'
#' @return A single numeric value between 0 and 1.
#'
#' @references
#' Theil, H. (1972). *Statistical Decomposition Analysis*. North-Holland.
#'
#' @examples
#' enroll <- data.frame(
#'   white    = c(200, 50,  10),
#'   hispanic = c(30,  100, 5),
#'   black    = c(20,  30,  150)
#' )
#' compute_theil_h(enroll)
#'
#' @export
compute_theil_h <- function(enrollment, groups = NULL) {
  enrollment <- as.data.frame(enrollment)
  if (is.null(groups)) groups <- names(enrollment)
  .validate_groups(enrollment, groups)

  mat   <- as.matrix(enrollment[, groups, drop = FALSE])
  n_i   <- rowSums(mat)          # district totals
  N     <- sum(n_i)              # grand total
  p_ig  <- mat / ifelse(n_i > 0, n_i, 1)  # district proportions

  p_g   <- colSums(mat) / N     # overall proportions

  # Overall entropy E
  E <- .entropy(p_g)
  if (E == 0) return(0)

  # District-level entropy
  e_i <- apply(p_ig, 1, .entropy)

  H <- sum((n_i / N) * ((E - e_i) / E))
  H
}


#' Aggregate EP Index Across Regions
#'
#' Computes the population-weighted average EP index across a set of regions.
#' Used internally by [compute_ep()] when `region_id` is supplied.
#'
#' @inheritParams compute_ep
#' @param region_id Integer or character vector identifying the region of each
#'   district.
#'
#' @return A named list with elements:
#'   \describe{
#'     \item{`ep_overall`}{Population-weighted mean EP across all regions.}
#'     \item{`ep_by_region`}{Named numeric vector of per-region EP values.}
#'   }
#'
#' @export
aggregate_ep <- function(enrollment, group_a, group_b, region_id) {
  enrollment <- as.data.frame(enrollment)
  .validate_groups(enrollment, c(group_a, group_b))
  stopifnot(length(region_id) == nrow(enrollment))

  regions <- unique(region_id)
  ep_by_region <- vapply(regions, function(r) {
    idx <- region_id == r
    sub <- enrollment[idx, , drop = FALSE]
    compute_ep(sub, group_a, group_b)
  }, numeric(1))
  names(ep_by_region) <- as.character(regions)

  # Weight by regional total of group_a
  a_totals <- vapply(regions, function(r) {
    sum(enrollment[[group_a]][region_id == r])
  }, numeric(1))

  A <- sum(enrollment[[group_a]])
  ep_overall <- if (A > 0) sum(ep_by_region * a_totals / A) else NA_real_

  list(ep_overall = ep_overall, ep_by_region = ep_by_region)
}


#' Compute a Full Suite of Segregation Indices
#'
#' Convenience function that returns all available segregation indices for a
#' given enrollment table and pair of groups.
#'
#' @inheritParams compute_ep
#' @param group_a Character string; reference group.
#' @param group_b Character string; comparison group.
#'
#' @return A named numeric vector with elements `ep`, `isolation_a`,
#'   `isolation_b`, and `dissimilarity`.
#'
#' @examples
#' enroll <- data.frame(white = c(200, 50), nonwhite = c(50, 200))
#' segregation_summary(enroll, "white", "nonwhite")
#'
#' @export
segregation_summary <- function(enrollment, group_a, group_b) {
  c(
    ep            = compute_ep(enrollment, group_a, group_b),
    isolation_a   = compute_isolation(enrollment, group_a),
    isolation_b   = compute_isolation(enrollment, group_b),
    dissimilarity = compute_dissimilarity(enrollment, group_a, group_b)
  )
}


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

.validate_groups <- function(df, groups) {
  missing <- setdiff(groups, names(df))
  if (length(missing) > 0) {
    stop("Column(s) not found in enrollment data: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  numeric_cols <- vapply(df[, groups, drop = FALSE], is.numeric, logical(1))
  if (!all(numeric_cols)) {
    stop("All group columns must be numeric.", call. = FALSE)
  }
}

.entropy <- function(p) {
  # Shannon entropy; 0*log(0) is treated as 0
  p <- p[p > 0]
  -sum(p * log(p))
}
