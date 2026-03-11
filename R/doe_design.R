# S3 class definition and methods for doe_design objects

# ---------------------------------------------------------------------------
# Internal constructor
# ---------------------------------------------------------------------------

#' Construct a doe_design object (internal)
#'
#' @param factors Character vector of factor names.
#' @param levels Integer vector, number of levels per factor.
#' @param level_values Named list of numeric vectors (actual values per factor).
#' @param design_matrix data.frame of actual values (rows = runs).
#' @param coded_matrix data.frame of -1/+1 coded values, or NULL.
#' @param coded Logical; TRUE for 2-level coded designs.
#' @param design_type Character; `"full_factorial_2level"` or
#'   `"full_factorial_multilevel"`.
#' @return An object of class `"doe_design"`.
#' @noRd
new_doe_design <- function(factors, levels, level_values, design_matrix,
                            coded_matrix, coded, design_type) {
  structure(
    list(
      factors       = factors,
      n_factors     = length(factors),
      levels        = levels,
      level_values  = level_values,
      coded         = coded,
      design_matrix = design_matrix,
      coded_matrix  = coded_matrix,
      response      = NULL,
      response_name = NULL,
      model         = NULL,
      model_formula = NULL,
      n_runs        = nrow(design_matrix),
      design_type   = design_type
    ),
    class = "doe_design"
  )
}

#' Validate a doe_design object (internal)
#' @noRd
validate_doe_design <- function(x) {
  if (!is.character(x$factors) || length(x$factors) == 0) {
    stop("`factors` must be a non-empty character vector.", call. = FALSE)
  }
  if (length(x$levels) != length(x$factors)) {
    stop("`levels` must have the same length as `factors`.", call. = FALSE)
  }
  if (!all(x$levels >= 2)) {
    stop("Each factor must have at least 2 levels.", call. = FALSE)
  }
  if (!is.data.frame(x$design_matrix) || nrow(x$design_matrix) == 0) {
    stop("`design_matrix` must be a non-empty data.frame.", call. = FALSE)
  }
  x
}

# ---------------------------------------------------------------------------
# S3 methods
# ---------------------------------------------------------------------------

#' Print a doe_design object
#'
#' @param x A `doe_design` object.
#' @param ... Ignored.
#' @return Invisibly returns `x`.
#' @export
print.doe_design <- function(x, ...) {
  cat("Design of Experiments: Full Factorial\n")
  cat("  Type   :", x$design_type, "\n")
  cat("  Factors:", x$n_factors, "\n")
  cat("  Runs   :", x$n_runs, "\n\n")

  cat("Factor summary:\n")
  for (i in seq_along(x$factors)) {
    vals <- paste(x$level_values[[x$factors[i]]], collapse = ", ")
    cat(sprintf("  %-20s %d levels: [%s]\n", x$factors[i], x$levels[i], vals))
  }

  if (!is.null(x$model)) {
    cat("\nModel fitted:", x$model_formula, "\n")
    r2 <- summary(x$model)$r.squared
    cat(sprintf("  R\u00b2 = %.4f\n", r2))
  }

  invisible(x)
}

#' Summarize a doe_design object
#'
#' @param object A `doe_design` object.
#' @param ... Ignored.
#' @return Invisibly returns `object`.
#' @export
summary.doe_design <- function(object, ...) {
  print(object)

  cat("\nDesign matrix (first 6 rows):\n")
  print(utils::head(object$design_matrix, 6))

  if (!is.null(object$model)) {
    cat("\nModel summary:\n")
    print(summary(object$model))
  }

  invisible(object)
}

#' Coerce a doe_design to a data.frame
#'
#' @param x A `doe_design` object.
#' @param row.names `NULL` or a character vector of row names. Ignored.
#' @param optional Logical. Ignored (included for S3 generic compatibility).
#' @param coded Logical. If `TRUE`, return coded (-1/+1) values (only for
#'   2-level designs). Default is `FALSE` (actual values).
#' @param ... Ignored.
#' @return A data.frame.
#' @export
as.data.frame.doe_design <- function(x, row.names = NULL, optional = FALSE,
                                      coded = FALSE, ...) {
  if (coded) {
    if (is.null(x$coded_matrix)) {
      stop("No coded matrix available (design is not a 2-level coded design).",
           call. = FALSE)
    }
    df <- x$coded_matrix
  } else {
    df <- x$design_matrix
  }

  if (!is.null(x$response)) {
    df[[x$response_name]] <- x$response
  }

  df
}
