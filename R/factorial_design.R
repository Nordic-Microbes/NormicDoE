#' Create a full factorial experimental design
#'
#' Generates all combinations of factor levels for a full factorial design.
#' For 2-level designs, a coded matrix (values -1 and +1) is also produced.
#'
#' @param factors Character vector of factor names,
#'   e.g. `c("Temperature", "pH", "Catalyst")`.
#' @param levels Integer vector specifying the number of levels per factor.
#'   A single integer is recycled to all factors.
#' @param level_values Named list of numeric vectors giving the actual values
#'   for each factor, e.g.
#'   `list(Temperature = c(60, 80), pH = c(5, 7), Catalyst = c(0.1, 0.5))`.
#'   If `NULL`, defaults to `1:n` for each factor.
#' @param randomize Logical. If `TRUE`, randomize the run order. Default is
#'   `FALSE`.
#'
#' @return An object of class `"doe_design"`.
#'
#' @examples
#' d <- full_factorial(
#'   factors      = c("Temperature", "pH"),
#'   levels       = c(2, 2),
#'   level_values = list(Temperature = c(60, 80), pH = c(5, 7))
#' )
#' print(d)
#'
#' @export
full_factorial <- function(factors, levels, level_values = NULL,
                            randomize = FALSE) {
  # --- Input validation ---
  if (!is.character(factors) || length(factors) == 0) {
    stop("`factors` must be a non-empty character vector.", call. = FALSE)
  }
  n_factors <- length(factors)

  # Recycle a scalar levels value
  if (length(levels) == 1) levels <- rep(levels, n_factors)
  levels <- as.integer(levels)

  if (length(levels) != n_factors) {
    stop("`levels` must have length 1 or the same length as `factors`.",
         call. = FALSE)
  }
  if (any(levels < 2)) {
    stop("All factors must have at least 2 levels.", call. = FALSE)
  }

  # --- Default level values ---
  if (is.null(level_values)) {
    level_values <- stats::setNames(
      lapply(levels, function(k) seq_len(k)),
      factors
    )
  }

  # Validate level_values
  if (!is.list(level_values)) {
    stop("`level_values` must be a named list.", call. = FALSE)
  }
  # Ensure all factors are present
  missing_factors <- setdiff(factors, names(level_values))
  if (length(missing_factors) > 0) {
    stop("Missing `level_values` entries for: ",
         paste(missing_factors, collapse = ", "), call. = FALSE)
  }
  # Ensure length matches declared levels
  for (f in factors) {
    if (length(level_values[[f]]) != levels[factors == f]) {
      stop(
        sprintf(
          "Factor '%s' declared with %d levels but `level_values` has %d values.",
          f, levels[factors == f], length(level_values[[f]])
        ),
        call. = FALSE
      )
    }
  }

  # --- Build design matrix via expand.grid ---
  # expand.grid cycles the first argument fastest; for Yates order we want the
  # last factor to cycle fastest, so we reverse the list and then fix columns.
  grid_input <- rev(level_values[factors])
  dm <- expand.grid(grid_input, stringsAsFactors = FALSE)
  dm <- dm[, rev(seq_len(ncol(dm))), drop = FALSE]  # restore original column order
  rownames(dm) <- NULL

  # Randomize if requested
  if (randomize) {
    dm <- dm[sample(nrow(dm)), , drop = FALSE]
    rownames(dm) <- NULL
  }

  # --- Coded matrix (only for uniform 2-level designs) ---
  all_two_level <- all(levels == 2L)
  if (all_two_level) {
    cm <- dm
    for (f in factors) {
      lo <- min(level_values[[f]])
      hi <- max(level_values[[f]])
      cm[[f]] <- encode_to_coded(dm[[f]], lo, hi)
    }
    design_type <- "full_factorial_2level"
  } else {
    cm <- NULL
    design_type <- "full_factorial_multilevel"
  }

  design <- new_doe_design(
    factors       = factors,
    levels        = levels,
    level_values  = level_values[factors],
    design_matrix = dm,
    coded_matrix  = cm,
    coded         = all_two_level,
    design_type   = design_type
  )

  validate_doe_design(design)
}
