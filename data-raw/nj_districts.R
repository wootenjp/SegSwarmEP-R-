## Script to create the nj_districts demo dataset.
## Run this script once with `source("data-raw/nj_districts.R")`.
## Requires the package to be loaded/installed first (for the `data/` folder).

set.seed(2024)

# New Jersey has 21 counties. We create 60 simulated districts spread
# across those counties, with realistic lat/lon ranges for NJ.
# Enrollment figures are illustrative only.

counties <- c(
  "Atlantic", "Bergen", "Burlington", "Camden", "Cape May",
  "Cumberland", "Essex", "Gloucester", "Hudson", "Hunterdon",
  "Mercer", "Middlesex", "Monmouth", "Morris", "Ocean",
  "Passaic", "Salem", "Somerset", "Sussex", "Union", "Warren"
)

# Approximate county-centroid lat/lon in NJ
county_lat <- c(39.52, 40.96, 39.87, 39.80, 39.09,
                39.33, 40.78, 39.71, 40.72, 40.57,
                40.28, 40.44, 40.29, 40.86, 39.92,
                41.03, 39.58, 40.57, 41.13, 40.66, 40.87)
county_lon <- c(-74.64, -74.07, -74.72, -75.01, -74.89,
                -75.13, -74.24, -75.14, -74.08, -74.91,
                -74.70, -74.41, -74.17, -74.55, -74.22,
                -74.30, -75.37, -74.61, -74.69, -74.27, -74.95)

n_total <- 60
# Assign each simulated district to a county (roughly 2-3 per county)
district_county_idx <- sort(sample(seq_along(counties), n_total,
                                   replace = TRUE))
district_counties   <- counties[district_county_idx]

# Jitter coordinates around county centroid
lat <- county_lat[district_county_idx] + stats::rnorm(n_total, 0, 0.05)
lon <- county_lon[district_county_idx] + stats::rnorm(n_total, 0, 0.06)

# Simulate enrollment: total enrollment ~ LogNormal(7, 0.5) ≈ 1100 students
total_enroll <- round(stats::rlnorm(n_total, meanlog = 7, sdlog = 0.5))

# Simulate % white: highly variable across NJ (range ~10% – 90%)
pct_white <- stats::rbeta(n_total, shape1 = 1.5, shape2 = 1.5)
white_n    <- round(total_enroll * pct_white)
nonwhite_n <- total_enroll - white_n

nj_districts <- data.frame(
  district_id   = sprintf("NJ_%03d", seq_len(n_total)),
  district_name = paste("District", seq_len(n_total)),
  county        = district_counties,
  white         = white_n,
  nonwhite      = nonwhite_n,
  total         = total_enroll,
  lat           = round(lat, 5),
  lon           = round(lon, 5),
  stringsAsFactors = FALSE
)

save(nj_districts,
     file = file.path("data", "nj_districts.rda"),
     compress = "xz")

message("nj_districts saved to data/nj_districts.rda")
