## Script to prepare the bundled `nj_districts` example dataset.
##
## Run this script ONCE (from the package root) whenever you want to
## regenerate the example data from the original NJ workspace file.
##
## Prerequisites:
##   - ACO_21_workspace.rda must be present in your working directory
##     (it is already in the repo root).
##   - Install usethis and sf if needed:
##       install.packages(c("usethis", "sf"))
##
## Usage (from package root, e.g., in an R console):
##   source("data-raw/nj_data.R")

library(sf)

# ------------------------------------------------------------------
# 1. Load from the original NJ workspace
# ------------------------------------------------------------------
message("Loading ACO_21_workspace.rda ...")
load("ACO_21_workspace.rda")   # brings in sf.dist, adj_matrix, graph, etc.

if (!exists("sf.dist") || !inherits(sf.dist, "sf")) {
  stop("Could not find an sf object named 'sf.dist' in ACO_21_workspace.rda")
}

# ------------------------------------------------------------------
# 2. Clean and rename columns for generality
#    Original NJ columns:
#      wa    -> white   (white non-Hispanic students)
#      hb    -> nonwhite (Hispanic + Black students)
#      total -> total   (unchanged)
# ------------------------------------------------------------------
nj_districts <- sf.dist

# Drop computed centroid columns (prepare_districts() will recompute them)
drop_cols <- intersect(c("center_x", "center_y"), colnames(nj_districts))
if (length(drop_cols) > 0) {
  nj_districts <- nj_districts[, !colnames(nj_districts) %in% drop_cols]
}

# Rename demographic columns
if ("wa" %in% colnames(nj_districts)) {
  colnames(nj_districts)[colnames(nj_districts) == "wa"] <- "white"
}
if ("hb" %in% colnames(nj_districts)) {
  colnames(nj_districts)[colnames(nj_districts) == "hb"] <- "nonwhite"
}

# Reproject back to geographic CRS (WGS84) for storage; prepare_districts()
# will reproject to UTM at runtime.
nj_districts <- sf::st_transform(nj_districts, crs = 4326)

message("nj_districts: ", nrow(nj_districts), " districts, columns: ",
        paste(colnames(nj_districts), collapse = ", "))

# ------------------------------------------------------------------
# 3. Save
# ------------------------------------------------------------------
usethis::use_data(nj_districts, overwrite = TRUE)
message("Saved data/nj_districts.rda")
