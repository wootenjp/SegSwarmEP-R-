# SegSwarmEP

An R package implementing Ant Colony Optimization (ACO), with elite (E) and parallel (P) ants, for school district
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

## Quick start with example data

```r
library(SegSwarmEP)

# Download example New Jersey school district data
download_example_data("ACO_7")   # ~2MB, 7-district example (recommended for testing)
load("ACO_7_workspace.rda")

# The workspace contains pre-prepared data:
#   sf.dist     - sf spatial data frame
#   adj_matrix  - adjacency matrix
#   graph       - igraph object
#   df.dist     - data frame (non-spatial)

# Run the ACO optimizer
result <- aco_districting(
  sf_data           = sf.dist,
  adj_matrix        = adj_matrix,
  graph             = graph,
  num_districts     = 7,
  group_a           = "wa",        # white enrollment column
  group_b           = "hb",        # Hispanic + Black enrollment column
  total_col         = "total",
  max_district_size = 124000,
  num_ants          = 80,          # for quick testing; use 400+ for production
  num_iterations    = 120          # for quick testing; use 320+ for production
)

# Attach results and visualize
sf.dist$cluster <- result$best_assignment
summarise_clusters(sf.dist, group_a = "wa", group_b = "hb")
plot_aco_map(sf.dist)
plot_score_history(result)
plot_h_history(result)
```

**For larger examples:** Use `download_example_data("ACO_21")` to get a 21-district dataset (~11.8MB).

See `vignette("introduction", package = "SegSwarmEP")` for a full walkthrough.

## Main functions

| Function | Purpose |
|----------|---------|
| `download_example_data()` | Download example NJ datasets from GitHub (ACO_7 or ACO_21) |
| `prepare_districts()` | Prepare your sf data: reproject, compute centroids, build adjacency matrix & graph |
| `aco_districting()` | Run the ACO redistricting optimization algorithm |
| `calculate_score()` | Score any district assignment (segregation, compactness, contiguity) |
| `summarise_clusters()` | Generate demographic summary table per consolidated cluster |
| `check_contiguity()` | Verify all clusters are geographically contiguous |
| `plot_aco_map()` | Create choropleth map of cluster assignments (requires ggplot2) |
| `plot_score_history()` | Plot convergence of objective score over iterations |
| `plot_h_history()` | Plot convergence of Theil's H segregation index over iterations |

For detailed help on any function, use `?function_name` (e.g., `?aco_districting`).

## Using your own data

To use your own school district data:

```r
library(SegSwarmEP)
library(sf)

# 1. Load your sf spatial data (must have polygon geometries)
your_sf_data <- st_read("path/to/your/shapefile.shp")

# Your data needs:
#   - Polygon geometry for each school district
#   - Two demographic enrollment columns (e.g., "white", "nonwhite")  
#   - A total enrollment column (e.g., "total")

# 2. Prepare the data (computes centroids, adjacency, graph)
prep <- prepare_districts(
  your_sf_data, 
  crs = 32618  # Use appropriate UTM zone for your state (see table below)
)

# 3. Run the optimization
result <- aco_districting(
  sf_data           = prep$sf_data,
  adj_matrix        = prep$adj_matrix,
  graph             = prep$graph,
  num_districts     = 7,              # target number of consolidated districts
  group_a           = "white",        # your column name for demographic group A
  group_b           = "nonwhite",     # your column name for demographic group B
  total_col         = "total",        # your total enrollment column
  max_district_size = 124000,         # optional enrollment cap per district
  num_ants          = 320,            # more ants = better quality (slower)
  num_iterations    = 400             # more iterations = better convergence
)

# 4. Analyze results
prep$sf_data$cluster <- result$best_assignment
summarise_clusters(prep$sf_data, group_a = "white", group_b = "nonwhite")
```

**UTM coordinate reference systems by region:**

| Region | UTM Zone | EPSG Code |
|--------|----------|-----------|
| NJ / Mid-Atlantic | 18N | 32618 |
| Southeast US | 17N | 32617 |
| Midwest (west) | 15N | 32615 |
| Midwest (east) | 16N | 32616 |
| West Coast | 10N or 11N | 32610 / 32611 |

## License

This work is licensed under Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International, accessible at https://creativecommons.org/licenses/by-nc-sa/4.0/
© J. P. Wooten
