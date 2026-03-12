# ---------------------------------------------------------------------------
# bayes_suggest()
# ---------------------------------------------------------------------------

test_that("bayes_suggest errors clearly when DiceKriging not installed", {
  skip_if(requireNamespace("DiceKriging", quietly = TRUE),
          "DiceKriging is installed; skip error-path test")
  d <- full_factorial(
    factors      = c("A", "B"),
    levels       = c(2, 2),
    level_values = list(A = c(-1, 1), B = c(-1, 1))
  )
  d <- fit_model(d, response = c(1, 2, 3, 4), interactions = "two_way")
  expect_error(bayes_suggest(d), regexp = "DiceKriging")
})

test_that("bayes_suggest errors on unfitted design", {
  d <- full_factorial(
    factors      = c("A", "B"),
    levels       = c(2, 2),
    level_values = list(A = c(-1, 1), B = c(-1, 1))
  )
  expect_error(bayes_suggest(d), regexp = "fit_model")
})

test_that("bayes_suggest errors when goal = 'target' but no target", {
  d <- full_factorial(
    factors      = c("A", "B"),
    levels       = c(2, 2),
    level_values = list(A = c(-1, 1), B = c(-1, 1))
  )
  d <- fit_model(d, response = c(1, 2, 3, 4), interactions = "two_way")
  expect_error(bayes_suggest(d, goal = "target"), regexp = "target")
})

# ---------------------------------------------------------------------------
# bayes_update()
# ---------------------------------------------------------------------------

test_that("bayes_update errors on unfitted design", {
  d <- full_factorial(
    factors      = c("A", "B"),
    levels       = c(2, 2),
    level_values = list(A = c(-1, 1), B = c(-1, 1))
  )
  expect_error(bayes_update(d, c(A = 0, B = 0), 5), regexp = "fit_model")
})

test_that("bayes_update appends run and re-fits model", {
  d <- full_factorial(
    factors      = c("A", "B"),
    levels       = c(2, 2),
    level_values = list(A = c(-1, 1), B = c(-1, 1))
  )
  d   <- fit_model(d, response = c(1, 2, 3, 4), interactions = "two_way")
  nd  <- bayes_update(d, new_settings = c(A = 0, B = 0), new_response = 2.5)
  expect_equal(nd$n_runs, 5L)
  expect_length(nd$response, 5L)
  expect_false(is.null(nd$model))
})

test_that("bayes_update preserves coded_matrix when present", {
  d <- full_factorial(
    factors      = c("A", "B"),
    levels       = c(2, 2),
    level_values = list(A = c(10, 20), B = c(1, 2))
  )
  d  <- fit_model(d, response = c(1, 2, 3, 4), interactions = "two_way")
  nd <- bayes_update(d, new_settings = c(A = 15, B = 1.5), new_response = 2.5)
  expect_equal(nrow(nd$coded_matrix), 5L)
})
