# Helper: fitted 2-factor design
make_fitted_2f <- function() {
  d <- full_factorial(
    factors      = c("A", "B"),
    levels       = c(2, 2),
    level_values = list(A = c(-1, 1), B = c(-1, 1))
  )
  fit_model(d, response = c(1, 2, 3, 4), interactions = "two_way")
}

# Helper: fitted 3-factor design
make_fitted_3f <- function() {
  d <- full_factorial(
    factors      = c("A", "B", "C"),
    levels       = c(2, 2, 2),
    level_values = list(A = c(-1, 1), B = c(-1, 1), C = c(-1, 1))
  )
  fit_model(d, response = c(1, 2, 3, 4, 5, 6, 7, 8), interactions = "two_way")
}

# Helper: fitted 4-factor design
make_fitted_4f <- function() {
  d <- full_factorial(
    factors      = c("A", "B", "C", "D"),
    levels       = c(2, 2, 2, 2),
    level_values = list(A = c(-1, 1), B = c(-1, 1), C = c(-1, 1), D = c(-1, 1))
  )
  fit_model(d, response = seq_len(16L), interactions = "two_way")
}

# ---------------------------------------------------------------------------
# is_saturated()
# ---------------------------------------------------------------------------

test_that("is_saturated returns FALSE when no model is fitted", {
  d <- full_factorial(
    factors      = c("A", "B"),
    levels       = c(2, 2),
    level_values = list(A = c(-1, 1), B = c(-1, 1))
  )
  expect_false(NormicDoE:::is_saturated(d))
})

test_that("is_saturated returns TRUE for 2^2 with all interactions", {
  d <- full_factorial(
    factors      = c("A", "B"),
    levels       = c(2, 2),
    level_values = list(A = c(-1, 1), B = c(-1, 1))
  )
  d <- fit_model(d, response = c(1, 2, 3, 4), interactions = "all")
  expect_true(NormicDoE:::is_saturated(d))
})

test_that("is_saturated returns FALSE for 2^3 with two_way interactions", {
  d <- make_fitted_3f()
  expect_false(NormicDoE:::is_saturated(d))
})

# ---------------------------------------------------------------------------
# plot_interaction() — 2, 3, 4 factors
# ---------------------------------------------------------------------------

test_that("plot_interaction with 2 factors returns a ggplot object", {
  d <- make_fitted_2f()
  p <- NormicDoE:::plot_interaction(d, "A", "B")
  expect_s3_class(p, "gg")
})

test_that("plot_interaction with 3 factors returns a ggplot object", {
  d <- make_fitted_3f()
  p <- NormicDoE:::plot_interaction(d, "A", "B", factor3 = "C")
  expect_s3_class(p, "gg")
})

test_that("plot_interaction with 4 factors returns a ggplot object", {
  d <- make_fitted_4f()
  p <- NormicDoE:::plot_interaction(d, "A", "B", factor3 = "C", factor4 = "D")
  expect_s3_class(p, "gg")
})

# ---------------------------------------------------------------------------
# plot_pareto() — normal and saturated
# ---------------------------------------------------------------------------

test_that("plot_pareto returns a ggplot object for a non-saturated model", {
  d <- make_fitted_3f()
  p <- NormicDoE:::plot_pareto(d)
  expect_s3_class(p, "gg")
})

test_that("plot_pareto warns on saturated model and still returns a ggplot", {
  d <- full_factorial(
    factors      = c("A", "B"),
    levels       = c(2, 2),
    level_values = list(A = c(-1, 1), B = c(-1, 1))
  )
  d <- fit_model(d, response = c(1, 2, 3, 4), interactions = "all")
  expect_warning(
    p <- NormicDoE:::plot_pareto(d),
    regexp = "saturated"
  )
  expect_s3_class(p, "gg")
})
