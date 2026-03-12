#' Suggest next experimental run via Bayesian optimisation
#'
#' Fits a Gaussian Process surrogate using DiceKriging and returns the factor
#' settings that maximise Expected Improvement (via DiceOptim).
#'
#' @param design A `doe_design` object with a fitted response
#'   (see [fit_model()]).
#' @param goal Character. One of `"max"`, `"min"`, or `"target"`.
#' @param target Numeric. Required when `goal = "target"`.
#' @param n_suggestions Integer. Number of points to suggest (default 1).
#' @return A data frame with one row per suggestion; columns equal factor names.
#' @examples
#' \dontrun{
#' d <- full_factorial(
#'   factors = c("A", "B"), levels = c(2, 2),
#'   level_values = list(A = c(-1, 1), B = c(-1, 1))
#' )
#' d <- fit_model(d, response = c(1, 2, 3, 4))
#' bayes_suggest(d, goal = "max")
#' }
#' @export
bayes_suggest <- function(design, goal = "max", target = NULL,
                           n_suggestions = 1L) {
  assert_model_fitted(design)
  goal <- match.arg(goal, c("max", "min", "target"))

  if (goal == "target" && is.null(target))
    stop("`target` must be provided when `goal = 'target'`.", call. = FALSE)

  if (!requireNamespace("DiceKriging", quietly = TRUE) ||
      !requireNamespace("DiceOptim",   quietly = TRUE)) {
    stop(
      "Bayesian optimisation requires DiceKriging and DiceOptim. ",
      "Install with: install.packages(c(\"DiceKriging\", \"DiceOptim\"))",
      call. = FALSE
    )
  }

  use_coded <- isTRUE(design$coded) && !is.null(design$coded_matrix)
  X <- if (use_coded) as.matrix(design$coded_matrix) else as.matrix(design$design_matrix)
  y <- design$response

  y_train <- switch(goal,
    max    = y,
    min    = -y,
    target = -(y - target)^2
  )

  km_model <- DiceKriging::km(
    design       = as.data.frame(X),
    response     = y_train,
    nugget.estim = TRUE
  )

  lower <- if (use_coded) rep(-1, ncol(X)) else
    sapply(design$factors, function(f) min(design$level_values[[f]]))
  upper <- if (use_coded) rep(1, ncol(X)) else
    sapply(design$factors, function(f) max(design$level_values[[f]]))

  suggestions <- lapply(seq_len(n_suggestions), function(i) {
    opt <- DiceOptim::max_EI(model = km_model, lower = lower, upper = upper)
    par <- stats::setNames(as.numeric(opt$par), colnames(X))
    if (use_coded) {
      for (f in design$factors) {
        lo   <- min(design$level_values[[f]])
        hi   <- max(design$level_values[[f]])
        par[f] <- decode_from_coded(par[f], lo, hi)
      }
    }
    as.data.frame(t(par))
  })

  do.call(rbind, suggestions)
}

#' Update design with a new observation for Bayesian optimisation
#'
#' Appends a new factor setting and response to an existing fitted design and
#' re-fits the model.
#'
#' @param design A `doe_design` object with a fitted response.
#' @param new_settings Named numeric vector or single-row data frame of factor
#'   settings at actual (non-coded) scale.
#' @param new_response Numeric. The observed response value.
#' @return An updated `doe_design` object with the new run appended and the
#'   model re-fitted.
#' @examples
#' \dontrun{
#' d <- full_factorial(
#'   factors = c("A", "B"), levels = c(2, 2),
#'   level_values = list(A = c(-1, 1), B = c(-1, 1))
#' )
#' d <- fit_model(d, response = c(1, 2, 3, 4))
#' suggestion <- bayes_suggest(d, goal = "max")
#' d_updated  <- bayes_update(d, suggestion, new_response = 4.8)
#' }
#' @export
bayes_update <- function(design, new_settings, new_response) {
  assert_model_fitted(design)

  if (is.numeric(new_settings))
    new_settings <- as.data.frame(t(new_settings))

  new_dm        <- rbind(design$design_matrix, new_settings)
  rownames(new_dm) <- NULL

  new_cm <- if (!is.null(design$coded_matrix)) {
    coded_row <- new_settings
    for (f in design$factors) {
      lo <- min(design$level_values[[f]])
      hi <- max(design$level_values[[f]])
      coded_row[[f]] <- encode_to_coded(coded_row[[f]], lo, hi)
    }
    rbind(design$coded_matrix, coded_row)
  } else NULL

  new_response_vec <- c(design$response, new_response)

  updated_design <- new_doe_design(
    factors       = design$factors,
    levels        = design$levels,
    level_values  = design$level_values,
    design_matrix = new_dm,
    coded_matrix  = new_cm,
    coded         = design$coded,
    design_type   = design$design_type,
    n_replicates  = design$n_replicates
  )

  # Infer interactions from stored model formula
  fmla <- sub(paste0(design$response_name, " ~ "), "",
              design$model_formula, fixed = TRUE)
  interactions <- if (grepl("[*]", fmla)) "all" else
    if (grepl(":", fmla)) "two_way" else "none"

  fit_model(updated_design,
            response      = new_response_vec,
            response_name = design$response_name,
            interactions  = interactions)
}
