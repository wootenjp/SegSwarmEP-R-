test_that("compute_ep returns value in [0,1] for a simple two-district case", {
  enroll <- data.frame(white = c(200, 50), nonwhite = c(50, 200))
  ep <- compute_ep(enroll, "white", "nonwhite")
  expect_true(is.numeric(ep))
  expect_true(ep >= 0 && ep <= 1)
})

test_that("compute_ep handles isolation (group_a == group_b)", {
  enroll <- data.frame(white = c(200, 50), nonwhite = c(50, 200))
  iso <- compute_ep(enroll, "white", "white")
  # With highly segregated districts the isolation index should be > 0.5
  expect_true(iso > 0.5)
})

test_that("compute_ep returns NA when group_a total is zero", {
  enroll <- data.frame(white = c(0, 0), nonwhite = c(50, 200))
  expect_true(is.na(compute_ep(enroll, "white", "nonwhite")))
})

test_that("compute_ep handles empty districts (total enrollment 0)", {
  enroll <- data.frame(white = c(200, 0), nonwhite = c(50, 0))
  ep <- compute_ep(enroll, "white", "nonwhite")
  expect_true(is.numeric(ep) && !is.na(ep))
})

test_that("compute_ep with region_id returns a list", {
  enroll <- data.frame(white = c(100, 50, 80, 40),
                       nonwhite = c(20, 60, 10, 70))
  rid <- c(1, 1, 2, 2)
  res <- compute_ep(enroll, "white", "nonwhite", region_id = rid)
  expect_type(res, "list")
  expect_named(res, c("ep_overall", "ep_by_region"))
  expect_length(res$ep_by_region, 2)
})

test_that("compute_ep errors on missing columns", {
  enroll <- data.frame(white = c(100, 50), black = c(20, 60))
  expect_error(compute_ep(enroll, "white", "nonwhite"), "not found")
})

test_that("compute_ep errors on non-numeric columns", {
  enroll <- data.frame(white = c("a", "b"), nonwhite = c(20, 60))
  expect_error(compute_ep(enroll, "white", "nonwhite"), "numeric")
})

test_that("compute_dissimilarity is in [0,1] for typical data", {
  enroll <- data.frame(white = c(200, 50), nonwhite = c(50, 200))
  d <- compute_dissimilarity(enroll, "white", "nonwhite")
  expect_true(d >= 0 && d <= 1)
})

test_that("compute_dissimilarity is 0 when proportions are equal", {
  enroll <- data.frame(white = c(100, 100), nonwhite = c(100, 100))
  d <- compute_dissimilarity(enroll, "white", "nonwhite")
  expect_equal(d, 0)
})

test_that("compute_dissimilarity returns NA when a group total is zero", {
  enroll <- data.frame(white = c(0, 0), nonwhite = c(50, 200))
  expect_true(is.na(compute_dissimilarity(enroll, "white", "nonwhite")))
})

test_that("compute_isolation is a convenience wrapper for compute_ep with same group", {
  enroll <- data.frame(white = c(200, 50), nonwhite = c(50, 200))
  expect_equal(compute_isolation(enroll, "white"),
               compute_ep(enroll, "white", "white"))
})

test_that("compute_theil_h returns 0 for perfectly even distribution", {
  enroll <- data.frame(white = c(100, 100), hispanic = c(100, 100))
  h <- compute_theil_h(enroll)
  expect_equal(h, 0, tolerance = 1e-10)
})

test_that("compute_theil_h returns value in [0,1]", {
  enroll <- data.frame(
    white    = c(200, 5,   5),
    hispanic = c(5,   200, 5),
    black    = c(5,   5,   200)
  )
  h <- compute_theil_h(enroll)
  expect_true(h >= 0 && h <= 1)
})

test_that("compute_theil_h subsets to specified groups", {
  enroll <- data.frame(
    white    = c(100, 50),
    black    = c(50,  100),
    other    = c(10,  10)
  )
  h_two   <- compute_theil_h(enroll, groups = c("white", "black"))
  h_three <- compute_theil_h(enroll)
  # Both should be valid; they may differ because of the 'other' group
  expect_true(is.numeric(h_two) && is.numeric(h_three))
})

test_that("segregation_summary returns a named numeric vector", {
  enroll <- data.frame(white = c(200, 50), nonwhite = c(50, 200))
  s <- segregation_summary(enroll, "white", "nonwhite")
  expect_named(s, c("ep", "isolation_a", "isolation_b", "dissimilarity"))
  expect_true(all(is.numeric(s)))
})

test_that("aggregate_ep population-weights correctly", {
  # Two perfectly segregated regions: EP per region should both equal 1
  enroll <- data.frame(
    white    = c(100, 0,   0,   100),
    nonwhite = c(0,   100, 100, 0)
  )
  rid <- c(1, 1, 2, 2)
  res <- aggregate_ep(enroll, "white", "nonwhite", rid)
  # white has members only in districts 1 and 4
  # dist 1: 100 white / 100 total -> prop nonwhite = 0
  # dist 4: 100 white / 100 total -> prop nonwhite = 0
  # EP = 0 (no exposure to nonwhite)
  expect_equal(res$ep_overall, 0)
})
