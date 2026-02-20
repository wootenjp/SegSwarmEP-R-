test_that("build_adjacency returns a square 0/1 matrix", {
  enroll <- data.frame(
    district_id = c("A", "B", "C"),
    white    = c(100, 200, 150),
    nonwhite = c(50,  80,  120),
    lat      = c(40.0, 40.1, 40.05),
    lon      = c(-74.0, -74.0, -74.05)
  )
  adj <- build_adjacency(enroll, id_col = "district_id")
  expect_equal(nrow(adj), 3)
  expect_equal(ncol(adj), 3)
  expect_true(all(adj %in% c(0L, 1L)))
  # Diagonal must be 0
  expect_equal(diag(adj), c(0L, 0L, 0L))
  # Symmetric
  expect_equal(adj, t(adj))
})

test_that("build_adjacency uses row indices when id_col is NULL", {
  enroll <- data.frame(
    white = c(100, 200),
    nonwhite = c(50, 80),
    lat = c(40.0, 40.05),
    lon = c(-74.0, -74.0)
  )
  adj <- build_adjacency(enroll, id_col = NULL)
  expect_equal(rownames(adj), c("1", "2"))
})

test_that("build_adjacency errors when lat/lon columns are missing", {
  enroll <- data.frame(white = c(100, 200), nonwhite = c(50, 80))
  expect_error(build_adjacency(enroll), "Columns not found")
})

test_that("build_adjacency respects explicit radius", {
  enroll <- data.frame(
    district_id = c("A", "B", "C"),
    white    = c(100, 200, 150),
    nonwhite = c(50,  80,  120),
    lat      = c(40.0, 40.1, 50.0),  # C is far away
    lon      = c(-74.0, -74.0, -74.0)
  )
  # With a tiny radius only nearest neighbours are adjacent
  adj_tight <- build_adjacency(enroll, id_col = "district_id", radius = 0.05)
  adj_loose <- build_adjacency(enroll, id_col = "district_id", radius = 15)
  expect_true(adj_loose["A", "C"] == 1)
  expect_true(adj_tight["A", "C"] == 0)
})

test_that("create_regions without adjacency returns integer vector of correct length", {
  enroll <- data.frame(
    district_id = paste0("D", 1:9),
    white    = 1:9,
    nonwhite = 9:1
  )
  rid <- create_regions(enroll, n_regions = 3, id_col = "district_id")
  expect_length(rid, 9)
  expect_type(rid, "integer")
  expect_equal(length(unique(rid)), 3)
})

test_that("create_regions with adjacency assigns all districts", {
  enroll <- data.frame(
    district_id = paste0("D", 1:6),
    white    = c(100, 120, 80, 90, 110, 95),
    nonwhite = c(50, 40, 60, 55, 45, 50),
    lat      = c(40.0, 40.1, 40.2, 40.0, 40.1, 40.2),
    lon      = c(-74.0, -74.0, -74.0, -74.1, -74.1, -74.1)
  )
  adj <- build_adjacency(enroll, id_col = "district_id")
  rid <- create_regions(enroll, n_regions = 2, adjacency = adj, seed = 1)
  expect_length(rid, 6)
  expect_true(all(!is.na(rid)))
  expect_true(all(rid > 0))
})

test_that("create_regions errors when n_regions > n_districts", {
  enroll <- data.frame(district_id = "D1", white = 100, nonwhite = 50)
  expect_error(create_regions(enroll, n_regions = 5), "exceed")
})

test_that("evaluate_plan returns expected structure", {
  enroll <- data.frame(white    = c(100, 50, 80, 40),
                       nonwhite = c(20,  60, 10, 70))
  plan   <- c(1, 1, 2, 2)
  res    <- evaluate_plan(enroll, plan, "white", "nonwhite")
  expect_named(res, c("ep_overall", "ep_by_region",
                      "dissimilarity_overall", "region_summary"))
  expect_equal(nrow(res$region_summary), 2)
  expect_true(res$ep_overall >= 0 && res$ep_overall <= 1)
})

test_that("evaluate_plan region_summary has correct column names", {
  enroll <- data.frame(white = c(100, 50), nonwhite = c(20, 60))
  res    <- evaluate_plan(enroll, c(1, 2), "white", "nonwhite")
  expect_true(all(c("region", "n_districts", "total_enrollment",
                    "ep", "dissimilarity") %in% names(res$region_summary)))
})
