make_design <- function(resp, reps = 1L) {
  d <- full_factorial(
    factors      = c("A", "B"),
    levels       = c(2, 2),
    level_values = list(A = c(-1, 1), B = c(-1, 1)),
    replicates   = reps
  )
  fit_model(d, response = resp, interactions = "two_way")
}

# ---------------------------------------------------------------------------
# compute_fold_changes()
# ---------------------------------------------------------------------------

test_that("returns expected columns", {
  d  <- make_design(c(1, 2, 3, 4))
  fc <- compute_fold_changes(d, reference_label = "A=-1, B=-1")
  expect_true(all(c("label", "response", "mean_response",
                    "fold_change", "mean_fold_change", "is_reference") %in% names(fc)))
})

test_that("reference row has mean_fold_change == 1", {
  d  <- make_design(c(1, 2, 3, 4))
  fc <- compute_fold_changes(d, reference_label = "A=-1, B=-1")
  expect_equal(unique(fc$mean_fold_change[fc$is_reference]), 1)
})

test_that("fold changes are numerically correct", {
  d  <- make_design(c(2, 4, 6, 8))
  fc <- compute_fold_changes(d, reference_label = "A=-1, B=-1")
  expect_equal(fc$mean_fold_change[fc$label == "A=1, B=1"], 4)
})

test_that("non-reference combination labels are FALSE for is_reference", {
  d  <- make_design(c(1, 2, 3, 4))
  fc <- compute_fold_changes(d, reference_label = "A=-1, B=-1")
  expect_true(all(!fc$is_reference[fc$label != "A=-1, B=-1"]))
})

test_that("errors when response is NULL", {
  d <- full_factorial(c("A", "B"), c(2, 2), list(A = c(-1, 1), B = c(-1, 1)))
  expect_error(compute_fold_changes(d, "A=-1, B=-1"), "No response")
})

test_that("errors on unknown reference label", {
  d <- make_design(c(1, 2, 3, 4))
  expect_error(compute_fold_changes(d, "X=99"), "not found")
})

test_that("replicates: nrow equals n_runs, means aggregated correctly", {
  d  <- make_design(c(1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5), reps = 2L)
  fc <- compute_fold_changes(d, reference_label = "A=-1, B=-1")
  expect_equal(nrow(fc), d$n_runs)
  ref_rows <- fc[fc$is_reference, ]
  expect_equal(unique(ref_rows$mean_fold_change), 1)
})

test_that("replicates: individual fold_change differs from mean_fold_change", {
  d  <- make_design(c(1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5), reps = 2L)
  fc <- compute_fold_changes(d, reference_label = "A=-1, B=-1")
  # Individual run values (1, 1.5) / mean(1, 1.5) = 1 / 1.25 and 1.5 / 1.25
  ref_fc <- fc$fold_change[fc$is_reference]
  expect_false(all(ref_fc == 1))
})

# ---------------------------------------------------------------------------
# plot_fold_changes()
# ---------------------------------------------------------------------------

test_that("returns a ggplot object", {
  d  <- make_design(c(1, 2, 3, 4))
  fc <- compute_fold_changes(d, "A=-1, B=-1")
  expect_s3_class(plot_fold_changes(fc), "gg")
})

test_that("replicate design also produces a ggplot object", {
  d  <- make_design(c(1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5), reps = 2L)
  fc <- compute_fold_changes(d, "A=-1, B=-1")
  expect_s3_class(plot_fold_changes(fc), "gg")
})
