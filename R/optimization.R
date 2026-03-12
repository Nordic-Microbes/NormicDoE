#' Find optimal factor settings for a fitted model
#'
#' Searches for the factor combination that maximizes, minimizes, or hits a
#' target value for the response. The search uses a grid over the factor ranges
#' (taken from the design's `level_values`) refined with `stats::optim()`.
#'
#' Categorical factors (non-numeric levels) are handled by enumeration:
#' all combinations of categorical levels are evaluated and, for each,
#' any continuous factors are optimised. The combination with the best
#' predicted response is returned.
#'
#' For 2-level designs the prediction uses coded factor values internally but
#' returns actual-scale optimal settings.
#'
#' @param design A `doe_design` object with a fitted model (see [fit_model()]).
#' @param goal Character. One of `"max"`, `"min"`, or `"target"`.
#' @param target Numeric. Required when `goal = "target"`.
#' @param constraints Named list of length-2 numeric vectors giving lower and
#'   upper bounds for each numeric factor, e.g.
#'   `list(Temperature = c(60, 80), pH = c(5, 7))`.
#'   Defaults to the full range defined by `level_values`.
#'
#' @return A list with:
#'   * `optimal_settings` - named list of factor values (numeric or character).
#'   * `predicted_response` - predicted response at the optimal settings.
#'   * `goal` - the goal used.
#'   * `target` - the target value (or `NULL`).
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

  # Classify factors as numeric or categorical
  is_numeric_fac <- vapply(design$factors, function(f) {
    !anyNA(suppressWarnings(as.numeric(range(design$level_values[[f]]))))
  }, logical(1L))

  numeric_factors     <- design$factors[ is_numeric_fac]
  categorical_factors <- design$factors[!is_numeric_fac]

  # Build ranges for numeric factors (or use user-supplied constraints)
  ranges <- lapply(numeric_factors, function(f) {
    if (!is.null(constraints) && !is.null(constraints[[f]])) {
      constraints[[f]]
    } else {
      suppressWarnings(as.numeric(range(design$level_values[[f]])))
    }
  })
  names(ranges) <- numeric_factors

  # Determine whether the model was fitted on coded or actual values
  use_coded <- isTRUE(design$coded) && !is.null(design$coded_matrix)

  # Prediction function: accepts a named list of actual (decoded) factor values
  predict_fn <- function(settings) {
    nd <- as.list(settings)
    if (use_coded) {
      for (f in numeric_factors) {
        lo <- min(as.numeric(design$level_values[[f]]))
        hi <- max(as.numeric(design$level_values[[f]]))
        nd[[f]] <- encode_to_coded(as.numeric(nd[[f]]), lo, hi)
      }
    }
    stats::predict(design$model,
                   newdata = as.data.frame(nd, stringsAsFactors = FALSE))
  }

  # Build all combinations of categorical factor levels to enumerate
  if (length(categorical_factors) > 0L) {
    cat_lvls <- lapply(categorical_factors, function(f) design$level_values[[f]])
    names(cat_lvls) <- categorical_factors
    cat_grid <- expand.grid(cat_lvls, stringsAsFactors = FALSE)
  } else {
    cat_grid <- data.frame(dummy__ = 1L)   # single row for all-numeric designs
  }

  # Separate numeric factors into free (range > 0) and fixed (single level)
  is_free <- vapply(numeric_factors, function(f) {
    r <- ranges[[f]]; r[1] < r[2]
  }, logical(1L))
  free_factors  <- numeric_factors[ is_free]
  fixed_factors <- numeric_factors[!is_free]
  free_ranges   <- ranges[is_free]
  fixed_values  <- lapply(fixed_factors, function(f) ranges[[f]][1])
  names(fixed_values) <- fixed_factors

  best_obj      <- Inf
  best_settings <- NULL

  for (i in seq_len(nrow(cat_grid))) {
    # Current categorical assignments (empty list for all-numeric designs)
    cat_row <- if (length(categorical_factors) > 0L)
      as.list(cat_grid[i, categorical_factors, drop = FALSE])
    else
      list()

    # Merge fixed numeric values with categorical for this iteration
    fixed_row <- c(cat_row, fixed_values)

    if (length(free_factors) == 0L) {
      # No free numeric factors: evaluate directly (all-categorical or all-fixed)
      settings <- c(fixed_row, list())[design$factors]
      pred     <- predict_fn(settings)
      obj_val  <- switch(goal,
        max    = -pred,
        min    =  pred,
        target = (pred - target)^2
      )
      if (obj_val < best_obj) {
        best_obj      <- obj_val
        best_settings <- settings
      }
    } else {
      # Optimise over free numeric factors.
      # Use local() so fixed_row is captured by value, not by reference.
      obj_fn <- local({
        fixed_row_ <- fixed_row
        function(x_free) {
          free_list <- stats::setNames(as.list(x_free), free_factors)
          settings  <- c(fixed_row_, free_list)[design$factors]
          pred      <- predict_fn(settings)
          switch(goal,
            max    = -pred,
            min    =  pred,
            target = (pred - target)^2
          )
        }
      })

      # Grid search to find a good starting point
      grid_size  <- if (length(free_factors) > 3L) 5L else 11L
      grid_lists <- lapply(free_ranges, function(r) seq(r[1], r[2], length.out = grid_size))
      grid       <- expand.grid(grid_lists)

      grid_vals <- apply(grid, 1, obj_fn)
      best_idx  <- which.min(grid_vals)
      x0        <- unlist(grid[best_idx, ])

      lower <- sapply(free_ranges, `[`, 1)
      upper <- sapply(free_ranges, `[`, 2)

      opt <- stats::optim(
        par    = x0,
        fn     = obj_fn,
        method = "L-BFGS-B",
        lower  = lower,
        upper  = upper
      )

      if (opt$value < best_obj) {
        best_obj      <- opt$value
        free_settings <- stats::setNames(as.list(opt$par), free_factors)
        best_settings <- c(fixed_row, free_settings)[design$factors]
      }
    }
  }

  list(
    optimal_settings   = best_settings,
    predicted_response = unname(predict_fn(best_settings)),
    goal               = goal,
    target             = target
  )
}
