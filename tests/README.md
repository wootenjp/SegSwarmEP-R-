# SegSwarmEP Tests

This directory contains test scripts for the SegSwarmEP package.

## Test Files

### `testthat.R`
**What it does:** Standard R package testing entry point that runs all automated tests when you execute `R CMD check` or `devtools::test()`.

**When to use:** Automatically used by R package checking tools. You don't need to run this directly.

### `testthat/test-basic-run.R`
**What it does:** Automated unit test for running ACO with your specified parameters (7 clusters, 80 ants, 120 iterations, max district size 124,000).

**When to use:** Run via `devtools::test()` or `testthat::test_file("tests/testthat/test-basic-run.R")`.

**Note:** Currently commented out and will skip until you provide sample data. Uncomment the code and add your data path to activate.

### `manual_test.R`
**What it does:** Interactive test script that demonstrates how to run the full ACO algorithm with your parameters. Includes helpful output messages and can save results.

**When to use:** For manual testing during development or to see a working example.

**How to run:**
```r
# From R console:
source("tests/manual_test.R")

# Or from command line:
Rscript tests/manual_test.R
```

**Note:** You need to uncomment the main code section and provide your spatial data first.

## Command Reference

### Key Functions and Their Parameters

#### `prepare_districts(sf_data, crs)`
**What it does:** Prepares your spatial data for the ACO algorithm by:
- Transforming to a metric coordinate system (CRS)
- Computing adjacency matrices and graph structures
- Adding centroid coordinates

**Parameters:**
- `sf_data`: Your spatial data (sf object with geometry column)
- `crs`: Coordinate reference system code (e.g., 32618 for UTM Zone 18N)

**Returns:** List with `sf_data`, `adj_matrix`, and `graph`

---

#### `aco_districting(...)`
**What it does:** Main algorithm that runs Ant Colony Optimization to find optimal school district groupings that minimize segregation.

**Key Parameters (Your Test Configuration):**
- `num_districts = 7` - **Number of clusters** to create (consolidated districts)
- `num_ants = 80` - **Number of ants** per iteration (more = better quality, slower)
- `num_iterations = 120` - **Number of iterations** to run (more = better optimization)
- `max_district_size = 124000` - **Maximum students** allowed per district

**Other Important Parameters:**
- `sf_data` - Prepared spatial data from `prepare_districts()`
- `adj_matrix` - Adjacency matrix from `prepare_districts()`
- `graph` - Graph object from `prepare_districts()`
- `group_a` - Column name for demographic group A (e.g., "white")
- `group_b` - Column name for demographic group B (e.g., "nonwhite")
- `total_col = "total"` - Column name for total enrollment
- `demographic_scale = 45` - Weight for segregation in objective function
- `compactness_scale = 120` - Weight for geographic compactness
- `verbose = TRUE` - Print progress messages

**Returns:** List with:
- `best_assignment` - District assignments for each school
- `score_history` - Best score at each iteration
- `h_values` - Theil's H segregation index over time
- `demographic_history` - Demographic breakdowns
- Additional diagnostic data

---

#### `plot_districts(sf_data, cluster_col)`
**What it does:** Creates a visualization of the district assignments on a map.

**Parameters:**
- `sf_data` - Spatial data with district assignments
- `cluster_col` - Column name containing cluster/district IDs

**Returns:** ggplot2 plot object

---

## Quick Start Guide

1. **Install the package:**
   ```r
   devtools::install()  # or devtools::load_all() for development
   ```

2. **Prepare your data:**
   ```r
   library(SegSwarmEP)
   library(sf)
   
   # Load your spatial data
   my_data <- st_read("path/to/shapefile.shp")
   
   # Ensure it has required columns: white, nonwhite, total, geometry
   ```

3. **Run the test:**
   ```r
   # Edit tests/manual_test.R to point to your data
   # Uncomment the main code section
   source("tests/manual_test.R")
   ```

4. **Or run interactively:**
   ```r
   prep <- prepare_districts(my_data, crs = 32618)
   
   result <- aco_districting(
     sf_data           = prep$sf_data,
     adj_matrix        = prep$adj_matrix,
     graph             = prep$graph,
     num_districts     = 7,
     group_a           = "white",
     group_b           = "nonwhite",
     max_district_size = 124000,
     num_ants          = 80,
     num_iterations    = 120
   )
   
   # View results
   prep$sf_data$cluster <- result$best_assignment
   plot_districts(prep$sf_data, cluster_col = "cluster")
   ```

## Understanding the Output

- **Best score:** Higher is better (combines low segregation + high compactness)
- **Theil's H:** Lower is better (0 = no segregation, higher = more segregation)
- **Iterations:** Watch the score improve over time
- **Assignments:** Each school gets a cluster number (1 to num_districts)

## Tips

- Start with fewer iterations (e.g., 50) for quick tests
- Increase ants and iterations for final production runs
- Adjust `max_district_size` based on your state's requirements
- Use `verbose = TRUE` to monitor progress
- Save results with `saveRDS()` for later analysis
