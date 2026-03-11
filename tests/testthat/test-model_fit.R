make_2level_design <- function() {
  full_factorial(
    factors      = c("A", "B"),
    levels       = c(2, 2),
    level_values = list(A = c(-1, 1), B = c(-1, 1))
  )
}

test_that("fit_model populates model fields", {
  d <- make_2level_design()
  d <- fit_model(d, response = c(-1.5, 3.5, -0.5, 6.5))
  expect_false(is.null(d$model))
  expect_false(is.null(d$response))
  expect_equal(d$response_name, "y")
  expect_equal(length(d$response), 4L)
})

test_that("fit_model known-answer: half-effects match theoretical values", {
  # True model: y = 2 + 3*A + 1*B + 0.5*A*B
  # Responses at (A,B) = (-1,-1),(-1,+1),(+1,-1),(+1,+1)
  # = 2-3-1+0.5, 2-3+1-0.5, 2+3-1-0.5, 2+3+1+0.5
  # = -1.5, -0.5, 3.5, 6.5  (order: A cycles fastest in expand.grid)
  # full_factorial puts A slow, B fast (Yates order), so:
  # (-1,-1)=-1.5, (-1,+1)=-0.5, (+1,-1)=3.5, (+1,+1)=6.5
  d <- make_2level_design()

  # Check the actual run order
  dm <- d$design_matrix
  resp <- numeric(nrow(dm))
  for (i in seq_len(nrow(dm))) {
    resp[i] <- 2 + 3 * dm$A[i] + 1 * dm$B[i] + 0.5 * dm$A[i] * dm$B[i]
  }
  d <- fit_model(d, response = resp)
  eff <- extract_effects(d, type = "half")

  a_eff <- eff$effect[eff$term == "A"]
  b_eff <- eff$effect[eff$term == "B"]
  ab_eff <- eff$effect[eff$term == "A:B"]

  expect_equal(a_eff, 3.0, tolerance = 1e-10)
  expect_equal(b_eff, 1.0, tolerance = 1e-10)
  expect_equal(ab_eff, 0.5, tolerance = 1e-10)
})

test_that("extract_effects returns correct columns", {
  d <- make_2level_design()
  d <- fit_model(d, response = c(1, 2, 3, 4))
  eff <- extract_effects(d)
  expect_true(all(c("term", "effect", "std_error", "t_value", "p_value",
                     "significant") %in% names(eff)))
})

test_that("model_summary returns expected keys", {
  d <- make_2level_design()
  d <- fit_model(d, response = c(1, 2, 3, 4))
  ms <- model_summary(d)
  expect_true(all(c("r_squared", "adj_r_squared", "rmse", "effects_table",
                     "n_runs", "degrees_of_freedom", "is_saturated") %in% names(ms)))
})

test_that("model_summary r_squared is 1 for perfect fit", {
  d <- make_2level_design()
  dm <- d$design_matrix
  resp <- 2 + 3 * dm$A + 1 * dm$B + 0.5 * dm$A * dm$B
  d <- fit_model(d, response = resp)
  ms <- model_summary(d)
  expect_equal(ms$r_squared, 1.0, tolerance = 1e-10)
})

test_that("fit_model errors on wrong response length", {
  d <- make_2level_design()
  expect_error(
    fit_model(d, response = c(1, 2, 3)),
    regexp = "number of runs"
  )
})

test_that("assert_model_fitted stops without model", {
  d <- make_2level_design()
  expect_error(extract_effects(d), regexp = "fit_model")
})

test_that("model_summary is_saturated = TRUE for 2^2 with all interactions", {
  # 2^2 with 4 runs and 4 parameters (intercept, A, B, A:B) => df_residual = 0
  d <- make_2level_design()
  d <- fit_model(d, response = c(1, 2, 3, 4), interactions = "all")
  ms <- model_summary(d)
  expect_true(ms$is_saturated)
  expect_equal(ms$degrees_of_freedom, 0L)
})

test_that("model_summary is_saturated = FALSE for 2^3 with two_way interactions", {
  # 2^3 = 8 runs; two_way uses 7 params (intercept + 3 main + 3 two-way) => df = 1
  d <- full_factorial(
    factors      = c("A", "B", "C"),
    levels       = c(2, 2, 2),
    level_values = list(A = c(-1, 1), B = c(-1, 1), C = c(-1, 1))
  )
  d <- fit_model(d, response = c(1, 2, 3, 4, 5, 6, 7, 8), interactions = "two_way")
  ms <- model_summary(d)
  expect_false(ms$is_saturated)
})

test_that("model_summary is_saturated = FALSE for replicated 2^2 design", {
  # 2^2 with 2 replicates = 8 runs; all interactions uses 4 params => df = 4
  d <- full_factorial(
    factors      = c("A", "B"),
    levels       = c(2, 2),
    level_values = list(A = c(-1, 1), B = c(-1, 1)),
    replicates   = 2L
  )
  d <- fit_model(d, response = c(1, 2, 3, 4, 1.1, 2.1, 3.1, 4.1), interactions = "all")
  ms <- model_summary(d)
  expect_false(ms$is_saturated)
  expect_gt(ms$degrees_of_freedom, 0L)
})
