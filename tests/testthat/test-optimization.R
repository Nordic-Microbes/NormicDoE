make_fitted_design <- function() {
  d <- full_factorial(
    factors      = c("A", "B"),
    levels       = c(2, 2),
    level_values = list(A = c(-1, 1), B = c(-1, 1))
  )
  dm <- d$design_matrix
  resp <- 10 + 3 * dm$A + 2 * dm$B
  fit_model(d, response = resp)
}

test_that("maximize finds settings with highest predicted response", {
  d   <- make_fitted_design()
  res <- optimize_response(d, goal = "max")
  expect_equal(res$goal, "max")
  expect_named(res$optimal_settings, c("A", "B"))
  # Both factors have positive effects, so optimum should be at high levels
  expect_true(res$optimal_settings["A"] > 0)
  expect_true(res$optimal_settings["B"] > 0)
})

test_that("minimize finds settings with lowest predicted response", {
  d   <- make_fitted_design()
  res <- optimize_response(d, goal = "min")
  expect_equal(res$goal, "min")
  expect_true(res$optimal_settings["A"] < 0)
  expect_true(res$optimal_settings["B"] < 0)
})

test_that("target reaches the target value", {
  d   <- make_fitted_design()
  tgt <- 10  # intercept; achievable at A=B=0
  res <- optimize_response(d, goal = "target", target = tgt)
  expect_equal(res$predicted_response, tgt, tolerance = 0.01)
})

test_that("error when goal = 'target' without target value", {
  d <- make_fitted_design()
  expect_error(optimize_response(d, goal = "target"), regexp = "`target`")
})

test_that("predicted_response at max is >= predicted at any design point", {
  d   <- make_fitted_design()
  res <- optimize_response(d, goal = "max")
  dm  <- d$coded_matrix
  dm[[d$response_name]] <- d$response
  preds <- stats::predict(d$model, newdata = d$coded_matrix)
  expect_true(res$predicted_response >= max(preds) - 1e-6)
})
