# SegSwarmEP <img src="man/figures/logo.png" align="right" height="139" alt="SegSwarmEP logo" style="display:none"/>

<!-- badges: start -->
[![R-CMD-check](https://github.com/wootenjp/SegSwarmEP-R-/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/wootenjp/SegSwarmEP-R-/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

**SegSwarmEP** is an R package implementing a swarm-based optimization
heuristic for redistricting school districts to reduce racial and
socioeconomic segregation.  The package was developed as part of doctoral
research examining whether voluntary *regionalization* of New Jersey school
districts could reduce between-district segregation.  It is designed so that
stakeholders in **any state** can evaluate proposals for consolidating or
regionalising their school districts.

---

## Installation

```r
# Install the development version from GitHub:
# install.packages("remotes")
remotes::install_github("wootenjp/SegSwarmEP-R-")
```

---

## Quick start

```r
library(SegSwarmEP)

# Load bundled demo data (60 simulated NJ school districts)
data(nj_districts)

# Build geographic adjacency from lat/lon centroids
adj <- build_adjacency(nj_districts, id_col = "district_id")

# Create an initial region assignment (21 regions = 1 per NJ county)
init <- create_regions(nj_districts, n_regions = 21,
                       id_col = "district_id", adjacency = adj, seed = 42)

# Run the SegSwarmEP optimization
result <- seg_swarm(
  enrollment   = nj_districts,
  adjacency    = adj,
  region_id    = init,
  group_a      = "white",
  group_b      = "nonwhite",
  n_iter       = 5000,
  seed         = 42
)

# Summarise results
summary(result)

# Visualise convergence
plot(result)

# Bar chart of EP index by region
plot_region_bars(result)
```

---

## Key functions

| Function | Description |
|---|---|
| `seg_swarm()` | Core SegSwarmEP optimization algorithm |
| `compute_ep()` | Exposure-Proportion (EP) segregation index |
| `compute_dissimilarity()` | Dissimilarity index |
| `compute_isolation()` | Isolation index |
| `compute_theil_h()` | Multigroup Theil H index |
| `segregation_summary()` | All indices in one call |
| `build_adjacency()` | Build adjacency matrix from centroid coordinates |
| `create_regions()` | Generate initial region assignments |
| `evaluate_plan()` | Evaluate a redistricting plan |
| `plot_ep_trace()` | Plot the EP convergence trace |
| `plot_region_bars()` | Bar chart of per-region EP values |
| `plot_region_map()` | Choropleth map (requires **sf**) |

---

## Using your own state's data

SegSwarmEP is state-agnostic.  To apply it to another state:

1. **Prepare enrollment data** — a data frame with one row per district, columns
   for each demographic group, and latitude/longitude centroids.
2. **Choose the number of regions** — typically the number of counties or
   regional planning districts in your state.
3. **Run the optimization** — call `seg_swarm()` with your data.
4. **Evaluate results** — use `evaluate_plan()`, `summary()`, and the plotting
   functions to analyse the proposed redistricting.

```r
my_data   <- read.csv("my_state_districts.csv")
my_adj    <- build_adjacency(my_data, id_col = "lea_code",
                              lat_col = "latitude", lon_col = "longitude")
my_init   <- create_regions(my_data, n_regions = 10, adjacency = my_adj)
my_result <- seg_swarm(my_data, my_adj, my_init,
                        group_a = "white_students",
                        group_b = "students_of_color",
                        n_iter  = 10000)
summary(my_result)
```

---

## Background

School district segregation in the United States often arises from sharp
demographic differences *between* districts rather than *within* them.
Voluntary regionalization — grouping adjacent districts into shared
administrative regions — is one policy lever that states can use to address
this.  SegSwarmEP operationalises this idea by treating district-to-region
assignments as an optimization problem, using a simulated-annealing heuristic
to minimise the Exposure-Proportion (EP) segregation index while respecting
geographic contiguity constraints.

---

## Citation

If you use SegSwarmEP in your research, please cite:

> Wooten, J. P. (2025). *SegSwarmEP: Swarm-Based Redistricting Optimization
> for School Desegregation*. R package version 0.1.0.
> https://github.com/wootenjp/SegSwarmEP-R-

---

## License

MIT © J. P. Wooten