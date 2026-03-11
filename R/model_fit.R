#' Fit a linear model to a factorial design
#'
#' Fits a linear model with the specified interaction structure and stores the
#' result in the `doe_design` object. For 2-level designs, the model is fitted
#' on coded (-1/+1) values so that coefficients equal the half-effects.
#'
#' @param design A `doe_design` object.
#' @param response Numeric vector of measured responses (length must equal the
#'   number of runs), or the name of an existing column in the design matrix.
#' @param response_name Character. Label for the response variable. Default
#'   is `"y"`.
#' @param interactions Character. One of:
#'   * `"all"` (default) — all main effects and all interaction terms (`A*B*C`).
#'   * `"two_way"` — main effects plus explicit two-factor interactions.
#'   * `"none"` — main effects only.
#' @param coded Logical. If `TRUE` (default) and the design is a 2-level design,
#'   fit on coded (-1/+1) values.
#'
#' @return The same `doe_design` object with `$response`, `$response_name`,
#'   `$model`, and `$model_formula` populated.
#'
#' @examples
#' d <- full_factorial(
#'   factors      = c("A", "B"),
#'   levels       = c(2, 2),
#'   level_values = list(A = c(-1, 1), B = c(-1, 1))
#' )
#' d <- fit_model(d, response = c(-1.5, 3.5, -0.5, 6.5))
#'
#' @export
fit_model <- function(design, response, response_name = "y",
                       interactions = "all", coded = TRUE) {
  if (!inherits(design, "doe_design")) {
    stop("`design` must be a `doe_design` object.", call. = FALSE)
  }
  interactions <- match.arg(interactions, c("all", "two_way", "none"))

  # Allow response as a column name already in design_matrix
  if (is.character(response) && length(response) == 1 &&
      response %in% names(design$design_matrix)) {
    response_name <- response
    response      <- design$design_matrix[[response]]
  }

  if (!is.numeric(response)) {
    stop("`response` must be a numeric vector or a column name in the design matrix.",
         call. = FALSE)
  }
  if (length(response) != design$n_runs) {
    stop(
      sprintf("`response` length (%d) must equal the number of runs (%d).",
              length(response), design$n_runs),
      call. = FALSE
    )
  }

  # Choose design matrix for fitting
  use_coded <- coded && isTRUE(design$coded) && !is.null(design$coded_matrix)
  fit_df <- if (use_coded) design$coded_matrix else design$design_matrix
  fit_df[[response_name]] <- response

  formula_str <- build_formula(response_name, design$factors, interactions)
  fit <- stats::lm(stats::as.formula(formula_str), data = fit_df)

  design$response      <- response
  design$response_name <- response_name
  design$model         <- fit
  design$model_formula <- formula_str

  design
}

#' Extract effects from a fitted design
#'
#' Returns a tidy data frame of effects and their statistics.
#'
#' @param design A `doe_design` object with a fitted model (see [fit_model()]).
#' @param type Character. Type of effect to return:
#'   * `"half"` (default) — raw coefficient (equals half-effect for 2-level
#'     coded designs).
#'   * `"standardized"` — t-value (`coefficient / SE`).
#'
#' @return A data.frame with columns `term`, `effect`, `std_error`, `t_value`,
#'   `p_value`, and `significant`.
#'
#' @examples
#' d <- full_factorial(
#'   factors      = c("A", "B"),
#'   levels       = c(2, 2),
#'   level_values = list(A = c(-1, 1), B = c(-1, 1))
#' )
#' d <- fit_model(d, response = c(-1.5, 3.5, -0.5, 6.5))
#' extract_effects(d)
#'
#' @export
extract_effects <- function(design, type = "half") {
  assert_model_fitted(design)
  type <- match.arg(type, c("half", "standardized"))

  s   <- summary(design$model)
  ct  <- as.data.frame(s$coefficients)
  ct$term <- rownames(ct)
  ct  <- ct[ct$term != "(Intercept)", , drop = FALSE]
  rownames(ct) <- NULL

  result <- data.frame(
    term      = ct$term,
    effect    = if (type == "half") ct[, "Estimate"] else ct[, "t value"],
    std_error = ct[, "Std. Error"],
    t_value   = ct[, "t value"],
    p_value   = ct[, "Pr(>|t|)"],
    significant = ct[, "Pr(>|t|)"] < 0.05,
    stringsAsFactors = FALSE
  )

  result[order(abs(result$effect), decreasing = TRUE), ]
}

#' Summarize model fit statistics
#'
#' Returns a list of key statistics for the fitted model.
#'
#' @param design A `doe_design` object with a fitted model (see [fit_model()]).
#'
#' @return A list with elements:
#'   * `r_squared` — coefficient of determination R².
#'   * `adj_r_squared` — adjusted R².
#'   * `rmse` — residual standard error.
#'   * `f_statistic` — overall F-statistic.
#'   * `f_p_value` — p-value of the F-test.
#'   * `effects_table` — data.frame from [extract_effects()].
#'   * `n_runs` — number of experimental runs.
#'   * `degrees_of_freedom` — residual degrees of freedom.
#'
#' @examples
#' d <- full_factorial(
#'   factors      = c("A", "B"),
#'   levels       = c(2, 2),
#'   level_values = list(A = c(-1, 1), B = c(-1, 1))
#' )
#' d <- fit_model(d, response = c(-1.5, 3.5, -0.5, 6.5))
#' ms <- model_summary(d)
#' ms$r_squared
#'
#' @export
model_summary <- function(design) {
  assert_model_fitted(design)

  s   <- summary(design$model)
  fst <- s$fstatistic

  list(
    r_squared          = s$r.squared,
    adj_r_squared      = s$adj.r.squared,
    rmse               = s$sigma,
    f_statistic        = if (!is.null(fst)) unname(fst["value"]) else NA_real_,
    f_p_value          = if (!is.null(fst))
                           stats::pf(fst["value"], fst["numdf"], fst["dendf"],
                                     lower.tail = FALSE)
                         else NA_real_,
    effects_table      = extract_effects(design),
    n_runs             = design$n_runs,
    degrees_of_freedom = s$df[2]
  )
}
