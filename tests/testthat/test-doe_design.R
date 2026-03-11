test_that("new_doe_design creates a valid doe_design object", {
  lv <- list(A = c(-1, 1), B = c(-1, 1))
  dm <- expand.grid(A = c(-1, 1), B = c(-1, 1))
  d  <- NormicDoE:::new_doe_design(
    factors       = c("A", "B"),
    levels        = c(2L, 2L),
    level_values  = lv,
    design_matrix = dm,
    coded_matrix  = dm,
    coded         = TRUE,
    design_type   = "full_factorial_2level"
  )

  expect_s3_class(d, "doe_design")
  expect_equal(d$n_factors, 2L)
  expect_equal(d$n_runs, 4L)
  expect_true(d$coded)
  expect_null(d$model)
})

test_that("print.doe_design returns invisibly and outputs text", {
  d <- full_factorial(
    factors      = c("X", "Y"),
    levels       = c(2, 2),
    level_values = list(X = c(0, 1), Y = c(10, 20))
  )
  expect_invisible(print(d))
  out <- capture.output(print(d))
  expect_true(any(grepl("Runs", out)))
})

test_that("as.data.frame.doe_design returns actual values by default", {
  d <- full_factorial(
    factors      = c("A", "B"),
    levels       = c(2, 2),
    level_values = list(A = c(5, 10), B = c(1, 2))
  )
  df <- as.data.frame(d)
  expect_true(all(df$A %in% c(5, 10)))
  expect_true(all(df$B %in% c(1, 2)))
})

test_that("as.data.frame.doe_design returns coded values when coded = TRUE", {
  d <- full_factorial(
    factors      = c("A", "B"),
    levels       = c(2, 2),
    level_values = list(A = c(5, 10), B = c(1, 2))
  )
  df_coded <- as.data.frame(d, coded = TRUE)
  expect_true(all(df_coded$A %in% c(-1, 1)))
  expect_true(all(df_coded$B %in% c(-1, 1)))
})

test_that("as.data.frame includes response when present", {
  d <- full_factorial(
    factors      = c("A", "B"),
    levels       = c(2, 2),
    level_values = list(A = c(-1, 1), B = c(-1, 1))
  )
  d <- fit_model(d, response = c(1, 2, 3, 4))
  df <- as.data.frame(d)
  expect_true("y" %in% names(df))
})
