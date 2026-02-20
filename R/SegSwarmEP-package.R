#' SegSwarmEP: Swarm-Based Redistricting for School Desegregation
#'
#' @description
#' SegSwarmEP provides a swarm-based optimization heuristic for redistricting
#' school districts with the goal of reducing racial and socioeconomic
#' segregation.  The main function is [seg_swarm()], which accepts district
#' enrollment data, a geographic adjacency matrix, and an initial region
#' assignment, and returns an optimised redistricting plan together with
#' segregation statistics.
#'
#' ## Typical workflow
#'
#' ```r
#' library(SegSwarmEP)
#'
#' # 1. Load enrollment data
#' data(nj_districts)
#'
#' # 2. Build adjacency matrix from coordinates
#' adj <- build_adjacency(nj_districts, id_col = "district_id")
#'
#' # 3. Create initial region assignment
#' init_regions <- create_regions(nj_districts, n_regions = 21,
#'                                id_col    = "district_id",
#'                                adjacency = adj,
#'                                seed      = 42)
#'
#' # 4. Run the SegSwarmEP optimization
#' result <- seg_swarm(
#'   enrollment   = nj_districts,
#'   adjacency    = adj,
#'   region_id    = init_regions,
#'   group_a      = "white",
#'   group_b      = "nonwhite",
#'   n_iter       = 5000,
#'   seed         = 42
#' )
#'
#' # 5. Inspect and visualise results
#' summary(result)
#' plot(result)           # EP convergence trace
#' plot_region_bars(result)
#' ```
#'
#' ## Key functions
#'
#' | Function | Purpose |
#' |---|---|
#' | [seg_swarm()] | Core optimization algorithm |
#' | [compute_ep()] | Compute the EP segregation index |
#' | [compute_dissimilarity()] | Compute the dissimilarity index |
#' | [compute_isolation()] | Compute the isolation index |
#' | [compute_theil_h()] | Compute the multigroup Theil H index |
#' | [segregation_summary()] | All indices in one call |
#' | [create_regions()] | Generate initial region assignments |
#' | [build_adjacency()] | Build adjacency matrix from coordinates |
#' | [evaluate_plan()] | Evaluate a redistricting plan |
#' | [plot_ep_trace()] | Plot optimization convergence |
#' | [plot_region_bars()] | Bar chart of EP by region |
#' | [plot_region_map()] | Choropleth map (requires **sf**) |
#'
#' @keywords internal
"_PACKAGE"
