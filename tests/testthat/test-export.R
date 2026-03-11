test_that("export_design creates a file", {
  d   <- full_factorial(
    factors      = c("A", "B"),
    levels       = c(2, 2),
    level_values = list(A = c(60, 80), B = c(5, 7))
  )
  tmp <- withr::local_tempfile(fileext = ".csv")
  export_design(d, file = tmp)
  expect_true(file.exists(tmp))
})

test_that("exported CSV has correct dimensions", {
  d <- full_factorial(
    factors      = c("A", "B", "C"),
    levels       = c(2, 2, 2),
    level_values = list(A = c(0, 1), B = c(0, 1), C = c(0, 1))
  )
  tmp <- withr::local_tempfile(fileext = ".csv")
  export_design(d, file = tmp)

  csv <- utils::read.csv(tmp)
  expect_equal(nrow(csv), 8L)
  expect_equal(ncol(csv), 3L)
  expect_equal(names(csv), c("A", "B", "C"))
})

test_that("export with coded = TRUE produces -1/+1 values", {
  d <- full_factorial(
    factors      = c("Temp", "pH"),
    levels       = c(2, 2),
    level_values = list(Temp = c(60, 80), pH = c(5, 7))
  )
  tmp <- withr::local_tempfile(fileext = ".csv")
  export_design(d, file = tmp, coded = TRUE)

  csv <- utils::read.csv(tmp)
  expect_true(all(csv$Temp %in% c(-1, 1)))
  expect_true(all(csv$pH   %in% c(-1, 1)))
})

test_that("export includes response when include_response = TRUE", {
  d <- full_factorial(
    factors      = c("A", "B"),
    levels       = c(2, 2),
    level_values = list(A = c(-1, 1), B = c(-1, 1))
  )
  d <- fit_model(d, response = c(1, 2, 3, 4))
  tmp <- withr::local_tempfile(fileext = ".csv")
  export_design(d, file = tmp, include_response = TRUE)

  csv <- utils::read.csv(tmp)
  expect_true("y" %in% names(csv))
})

test_that("export_design returns file path invisibly", {
  d <- full_factorial(
    factors      = c("A"),
    levels       = 2,
    level_values = list(A = c(0, 1))
  )
  tmp <- withr::local_tempfile(fileext = ".csv")
  result <- withVisible(export_design(d, file = tmp))
  expect_false(result$visible)
  expect_equal(result$value, tmp)
})
