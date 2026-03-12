# Helpers (re-use from test-utils.R via sourcing is not needed — helpers are in a separate file)
make_2f_design_a <- function() {
  d <- full_factorial(
    factors      = c("A", "B"),
    levels       = c(2, 2),
    level_values = list(A = c(-1, 1), B = c(-1, 1))
  )
  fit_model(d, response = c(1, 2, 3, 4), interactions = "two_way")
}

make_2f_design_b <- function() {
  d <- full_factorial(
    factors      = c("A", "B"),
    levels       = c(2, 2),
    level_values = list(A = c(-1, 1), B = c(-1, 1))
  )
  fit_model(d, response = c(2, 3, 4, 5), interactions = "two_way")
}

# ---------------------------------------------------------------------------
# compare_effects()
# ---------------------------------------------------------------------------

test_that("compare_effects returns expected columns", {
  d1 <- make_2f_design_a()
  d2 <- make_2f_design_b()
  df <- compare_effects(Batch1 = d1, Batch2 = d2)
  expect_true(all(c("term", "effect", "std_error", "t_value",
                    "p_value", "significant", "sample") %in% names(df)))
})

test_that("compare_effects uses names as labels", {
  d1 <- make_2f_design_a()
  d2 <- make_2f_design_b()
  df <- compare_effects(Batch1 = d1, Batch2 = d2)
  expect_equal(sort(unique(df$sample)), c("Batch1", "Batch2"))
})

test_that("compare_effects uses default labels for unnamed designs", {
  d1 <- make_2f_design_a()
  d2 <- make_2f_design_b()
  df <- compare_effects(d1, d2)
  expect_equal(sort(unique(df$sample)), c("Sample 1", "Sample 2"))
})

test_that("compare_effects respects explicit labels argument", {
  d1 <- make_2f_design_a()
  d2 <- make_2f_design_b()
  df <- compare_effects(d1, d2, labels = c("Alpha", "Beta"))
  expect_equal(sort(unique(df$sample)), c("Alpha", "Beta"))
})

# ---------------------------------------------------------------------------
# plot_effect_comparison()
# ---------------------------------------------------------------------------

test_that("plot_effect_comparison returns a ggplot object", {
  d1 <- make_2f_design_a()
  d2 <- make_2f_design_b()
  df <- compare_effects(Batch1 = d1, Batch2 = d2)
  p  <- plot_effect_comparison(df)
  expect_s3_class(p, "gg")
})
