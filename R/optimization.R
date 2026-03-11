#' Find optimal factor settings for a fitted model
#'
#' Searches for the factor combination that maximizes, minimizes, or hits a
#' target value for the response. The search uses a grid over the factor ranges
#' (taken from the design's `level_values`) refined with `stats::optim()`.
#'
#' For 2-level designs the prediction uses coded factor values internally but
#' returns actual-scale optimal settings.
#'
#' @param design A `doe_design` object with a fitted model (see [fit_model()]).
#' @param goal Character. One of `"max"`, `"min"`, or `"target"`.
#' @param target Numeric. Required when `goal = "target"`.
#' @param constraints Named list of length-2 numeric vectors giving lower and
#'   upper bounds for each factor, e.g.
#'   `list(Temperature = c(60, 80), pH = c(5, 7))`.
#'   Defaults to the full range defined by `level_values`.
#'
#' @return A list with:
#'   * `optimal_settings` — named numeric vector of factor values.
#'   * `predicted_response` — predicted response at the optimal settings.
#'   * `goal` — the goal used.
#'   * `target` — the target value (or `NULL`).
#'
#' @examples
#' d <- full_factorial(
#'   factors      = c("A", "B"),
#'   levels       = c(2, 2),
#'   level_values = list(A = c(-1, 1), B = c(-1, 1))
#' )
#' d <- fit_model(d, response = c(-1.5, 3.5, -0.5, 6.5))
#' optimize_response(d, goal = "max")
#'
#' @export
optimize_response <- function(design, goal = "max", target = NULL,
                               constraints = NULL) {
  assert_model_fitted(design)
  goal <- match.arg(goal, c("max", "min", "target"))

  if (goal == "target" && is.null(target)) {
    stop("`target` must be provided when `goal = 'target'`.", call. = FALSE)
  }

  # Build factor ranges from level_values (or user-supplied constraints)
  ranges <- lapply(design$factors, function(f) {
    if (!is.null(constraints) && !is.null(constraints[[f]])) {
      constraints[[f]]
    } else {
      range(design$level_values[[f]])
    }
  })
  names(ranges) <- design$factors

  # Determine whether the model was fitted on coded or actual values
  use_coded <- isTRUE(design$coded) && !is.null(design$coded_matrix)

  # Build a prediction function that accepts a numeric vector of actual values
  predict_fn <- function(x_actual) {
    newdata <- as.list(x_actual)
    names(newdata) <- design$factors

    if (use_coded) {
      for (f in design$factors) {
        lo <- min(design$level_values[[f]])
        hi <- max(design$level_values[[f]])
        newdata[[f]] <- encode_to_coded(newdata[[f]], lo, hi)
      }
    }

    stats::predict(design$model, newdata = as.data.frame(newdata))
  }

  # Objective function for optim()
  obj_fn <- switch(goal,
    max    = function(x) -predict_fn(x),
    min    = function(x)  predict_fn(x),
    target = function(x) (predict_fn(x) - target)^2
  )

  # Grid search to find a good starting point
  grid_size  <- min(11L, 5L)   # points per factor; keep it small for >3 factors
  grid_lists <- lapply(ranges, function(r) seq(r[1], r[2], length.out = grid_size))
  grid       <- expand.grid(grid_lists)

  grid_vals <- apply(grid, 1, obj_fn)
  best_idx  <- which.min(grid_vals)
  x0        <- unlist(grid[best_idx, ])

  # Refine with L-BFGS-B (supports box constraints)
  lower <- sapply(ranges, `[`, 1)
  upper <- sapply(ranges, `[`, 2)

  opt <- stats::optim(
    par     = x0,
    fn      = obj_fn,
    method  = "L-BFGS-B",
    lower   = lower,
    upper   = upper
  )

  optimal_actual <- stats::setNames(opt$par, design$factors)

  list(
    optimal_settings   = optimal_actual,
    predicted_response = unname(predict_fn(optimal_actual)),
    goal               = goal,
    target             = target
  )
}
