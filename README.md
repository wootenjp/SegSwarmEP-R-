# SegSwarmEP

An R package implementing Ant Colony Optimization (ACO) for school district
redistricting to reduce racial and demographic segregation.

## Overview

`SegSwarmEP` proposes consolidated school regions from individual district
clusters, minimising Theil's H segregation while keeping regions geographically
contiguous, compact (Polsby-Popper), and within an optional enrollment cap.
Designed to generalise to any US state or metropolitan area.

Original research applied to 155 out of New Jersey's 541 traditional public school districts as part of
ongoing work on regional desegregation planning.

## Installation

```r
# Install development version from GitHub
# install.packages("remotes")
remotes::install_github("wootenjp/SegSwarmEP-R-")
```

**Dependencies:** `igraph`, `sf`, `segregation`, `data.table`, `parallel`,
`reshape2`

**Suggested:** `ggplot2` (for visualisation)

## Quick start

```r
library(SegSwarmEP)

# 1. Prepare your sf data (centroids, adjacency matrix, igraph graph)
prep <- prepare_districts(your_sf_data, crs = 32618)   # UTM zone for your state

# 2. Run the ACO optimiser
result <- aco_districting(
  sf_data           = prep$sf_data,
  adj_matrix        = prep$adj_matrix,
  graph             = prep$graph,
  num_districts     = 21,
  group_a           = "white",     # column of group A enrollment counts
  group_b           = "nonwhite",  # column of group B enrollment counts
  total_col         = "total",
  max_district_size = 124000,
  num_ants          = 400,
  num_iterations    = 320
)

# 3. Attach results and explore
prep$sf_data$cluster <- result$best_assignment
summarise_clusters(prep$sf_data, group_a = "white", group_b = "nonwhite")
plot_aco_map(prep$sf_data)
plot_score_history(result)
plot_h_history(result)
```

See `vignette("introduction", package = "SegSwarmEP")` for a full walkthrough.

## Main functions

| Function | Purpose |
|----------|---------|
| `prepare_districts()` | Reproject, compute centroids, build adjacency matrix and igraph graph |
| `aco_districting()` | Run the ACO redistricting optimisation |
| `calculate_score()` | Score any assignment vector |
| `summarise_clusters()` | Demographic summary per consolidated cluster |
| `check_contiguity()` | Verify all clusters are geographically contiguous |
| `plot_aco_map()` | Choropleth map of cluster assignments |
| `plot_score_history()` | Convergence plot of objective score |
| `plot_h_history()` | Convergence plot of Theil's H |

## Using your own data

Your `sf` object needs:
- **Polygon geometry** for each school district
- **Two demographic enrollment columns** (e.g., `white` / `nonwhite`)
- **A total enrollment column**

Pass the column names as `group_a`, `group_b`, and `total_col` to
`aco_districting()`. No renaming required.

Choose the appropriate UTM EPSG code for your state (used in
`prepare_districts(crs = ...)`):

| Region | UTM Zone | EPSG |
|--------|----------|------|
| NJ / Mid-Atlantic | 18N | 32618 |
| Southeast US | 17N | 32617 |
| Midwest (west) | 15N | 32615 |
| Midwest (east) | 16N | 32616 |
| West Coast | 10N or 11N | 32610 / 32611 |

## Bundled NJ data

Run `data-raw/nj_data.R` once (see that file for instructions) to extract
the `nj_districts` dataset from the original workspace file and save it as
package data.

## License

MIT © J. P. Wooten
