test_that("seg_swarm returns a seg_swarm_result object", {
  # 4-district toy example with trivial adjacency
  enroll <- data.frame(
    white    = c(200, 10, 10, 200),
    nonwhite = c(10, 200, 200, 10)
  )
  # 0-1 adjacency: all districts are mutually adjacent
  adj <- matrix(c(0,1,1,1,
                  1,0,1,1,
                  1,1,0,1,
                  1,1,1,0), nrow = 4, byrow = TRUE)
  init <- c(1L, 1L, 2L, 2L)

  res <- seg_swarm(enroll, adj, init, "white", "nonwhite",
                   n_iter = 50, seed = 7)

  expect_s3_class(res, "seg_swarm_result")
  expect_equal(length(res$region_id), 4)
  expect_true(is.numeric(res$ep_final))
  expect_true(is.numeric(res$ep_initial))
  expect_length(res$ep_trace, 50)
})

test_that("seg_swarm ep_final is <= ep_initial (or equal) over many iterations", {
  enroll <- data.frame(
    white    = c(200, 10, 10, 200),
    nonwhite = c(10, 200, 200, 10)
  )
  adj <- matrix(c(0,1,1,1,
                  1,0,1,1,
                  1,1,0,1,
                  1,1,1,0), nrow = 4, byrow = TRUE)
  init <- c(1L, 1L, 2L, 2L)

  res <- seg_swarm(enroll, adj, init, "white", "nonwhite",
                   n_iter = 500, cooling_rate = 0, seed = 42)

  # Greedy mode (cooling_rate=0) should never increase EP
  expect_true(res$ep_final <= res$ep_initial + 1e-10)
})

test_that("seg_swarm print and summary methods work without error", {
  enroll <- data.frame(
    white    = c(200, 10, 10, 200),
    nonwhite = c(10, 200, 200, 10)
  )
  adj <- matrix(c(0,1,1,1,
                  1,0,1,1,
                  1,1,0,1,
                  1,1,1,0), nrow = 4, byrow = TRUE)
  init <- c(1L, 1L, 2L, 2L)
  res  <- seg_swarm(enroll, adj, init, "white", "nonwhite",
                    n_iter = 10, seed = 1)

  expect_output(print(res), "SegSwarmEP")
  s <- summary(res)
  expect_s3_class(s, "data.frame")
})

test_that("seg_swarm errors on mismatched adjacency dimensions", {
  enroll <- data.frame(white = c(100, 50), nonwhite = c(50, 100))
  bad_adj <- matrix(0, nrow = 3, ncol = 3)
  init    <- c(1L, 2L)
  expect_error(seg_swarm(enroll, bad_adj, init, "white", "nonwhite",
                          n_iter = 5))
})

test_that("seg_swarm errors on missing group column", {
  enroll <- data.frame(white = c(100, 50), nonwhite = c(50, 100))
  adj    <- matrix(c(0,1,1,0), nrow = 2)
  init   <- c(1L, 2L)
  expect_error(seg_swarm(enroll, adj, init, "white", "hispanic", n_iter = 5),
               "not found")
})

test_that("seg_swarm result$enrollment contains region_id column", {
  enroll <- data.frame(white = c(100, 50, 80, 40),
                       nonwhite = c(20, 60, 10, 70))
  adj    <- matrix(c(0,1,0,0,
                     1,0,1,0,
                     0,1,0,1,
                     0,0,1,0), nrow = 4)
  init   <- c(1L, 1L, 2L, 2L)
  res    <- seg_swarm(enroll, adj, init, "white", "nonwhite",
                      n_iter = 10, seed = 3)
  expect_true("region_id" %in% names(res$enrollment))
  expect_equal(length(res$enrollment$region_id), 4)
})
