test_that("2^2 design has 4 runs and correct columns", {
  d <- full_factorial(
    factors      = c("A", "B"),
    levels       = c(2, 2),
    level_values = list(A = c(-1, 1), B = c(-1, 1))
  )
  expect_s3_class(d, "doe_design")
  expect_equal(d$n_runs, 4L)
  expect_equal(d$n_factors, 2L)
  expect_equal(names(d$design_matrix), c("A", "B"))
})

test_that("2^3 design has 8 runs", {
  d <- full_factorial(
    factors      = c("A", "B", "C"),
    levels       = c(2, 2, 2),
    level_values = list(A = c(0, 1), B = c(0, 1), C = c(0, 1))
  )
  expect_equal(d$n_runs, 8L)
})

test_that("2x3 mixed design has 6 runs and is not coded", {
  d <- full_factorial(
    factors      = c("A", "B"),
    levels       = c(2, 3),
    level_values = list(A = c(1, 2), B = c(10, 20, 30))
  )
  expect_equal(d$n_runs, 6L)
  expect_false(d$coded)
  expect_null(d$coded_matrix)
  expect_equal(d$design_type, "full_factorial_multilevel")
})

test_that("all level combinations are present", {
  d <- full_factorial(
    factors      = c("A", "B"),
    levels       = c(2, 2),
    level_values = list(A = c(10, 20), B = c(1, 2))
  )
  dm <- d$design_matrix
  expect_equal(nrow(dm), 4L)
  # All combinations must appear
  combos <- paste(dm$A, dm$B)
  expect_setequal(combos, c("10 1", "10 2", "20 1", "20 2"))
})

test_that("randomize = TRUE produces the same combinations in different order", {
  set.seed(42)
  d_rand <- full_factorial(
    factors      = c("A", "B"),
    levels       = c(2, 2),
    level_values = list(A = c(-1, 1), B = c(-1, 1)),
    randomize    = TRUE
  )
  d_std <- full_factorial(
    factors      = c("A", "B"),
    levels       = c(2, 2),
    level_values = list(A = c(-1, 1), B = c(-1, 1))
  )
  expect_equal(nrow(d_rand$design_matrix), nrow(d_std$design_matrix))
  expect_setequal(
    paste(d_rand$design_matrix$A, d_rand$design_matrix$B),
    paste(d_std$design_matrix$A,  d_std$design_matrix$B)
  )
})

test_that("scalar levels is recycled", {
  d <- full_factorial(
    factors      = c("A", "B", "C"),
    levels       = 2,
    level_values = list(A = c(-1, 1), B = c(-1, 1), C = c(-1, 1))
  )
  expect_equal(d$n_runs, 8L)
})

test_that("coded matrix values are -1 or +1 for 2-level designs", {
  d <- full_factorial(
    factors      = c("Temp", "pH"),
    levels       = c(2, 2),
    level_values = list(Temp = c(60, 80), pH = c(5, 7))
  )
  expect_true(all(d$coded_matrix$Temp %in% c(-1, 1)))
  expect_true(all(d$coded_matrix$pH %in% c(-1, 1)))
})

test_that("error on mismatched level_values length", {
  expect_error(
    full_factorial(
      factors      = c("A"),
      levels       = 2,
      level_values = list(A = c(1, 2, 3))  # 3 values but levels = 2
    ),
    regexp = "3 values"
  )
})

test_that("replicates = 2 doubles the number of runs", {
  d <- full_factorial(
    factors      = c("A", "B"),
    levels       = c(2, 2),
    level_values = list(A = c(-1, 1), B = c(-1, 1)),
    replicates   = 2L
  )
  expect_equal(d$n_runs, 8L)
  expect_equal(d$n_replicates, 2L)
})

test_that("replicated design has all combos present exactly n times", {
  d <- full_factorial(
    factors      = c("A", "B"),
    levels       = c(2, 2),
    level_values = list(A = c(-1, 1), B = c(-1, 1)),
    replicates   = 3L
  )
  dm <- d$design_matrix
  combos <- paste(dm$A, dm$B)
  tbl <- table(combos)
  expect_equal(nrow(dm), 12L)
  expect_true(all(tbl == 3L))
})

test_that("coded_matrix is also replicated", {
  d <- full_factorial(
    factors      = c("A", "B"),
    levels       = c(2, 2),
    level_values = list(A = c(10, 20), B = c(1, 2)),
    replicates   = 2L
  )
  expect_equal(nrow(d$coded_matrix), 8L)
  expect_true(all(d$coded_matrix$A %in% c(-1, 1)))
  expect_true(all(d$coded_matrix$B %in% c(-1, 1)))
})

test_that("replicates = 0 errors", {
  expect_error(
    full_factorial(
      factors      = c("A"),
      levels       = 2,
      level_values = list(A = c(-1, 1)),
      replicates   = 0L
    ),
    regexp = "positive integer"
  )
})
