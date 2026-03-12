# Internal utility functions for NormicDoE

# Suppress R CMD check note for ggplot2 .data pronoun
utils::globalVariables(".data")

# ---------------------------------------------------------------------------
# Formula builder
# ---------------------------------------------------------------------------

#' Build an lm() formula string
#'
#' @param response_name Character. Name of the response variable.
#' @param factor_names Character vector. Names of factors.
#' @param interactions Character. One of `"none"`, `"two_way"`, or `"all"`.
#' @return A character string suitable for `stats::as.formula()`.
#' @noRd
build_formula <- function(response_name, factor_names, interactions) {
  interactions <- match.arg(interactions, c("none", "two_way", "all"))

  if (interactions == "none") {
    rhs <- paste(factor_names, collapse = " + ")
  } else if (interactions == "all") {
    rhs <- paste(factor_names, collapse = " * ")
  } else {
    # two_way: main effects + explicit two-way interactions
    main <- factor_names
    pairs <- utils::combn(factor_names, 2, FUN = function(x) paste(x, collapse = ":"))
    rhs <- paste(c(main, pairs), collapse = " + ")
  }

  paste(response_name, "~", rhs)
}

# ---------------------------------------------------------------------------
# Coding / decoding
# ---------------------------------------------------------------------------

#' Encode actual values to -1/+1 coded values (2-level designs)
#'
#' @param x Numeric vector of actual values.
#' @param low Numeric. The low-level value.
#' @param high Numeric. The high-level value.
#' @return Numeric vector in -1/+1 coding.
#' @noRd
encode_to_coded <- function(x, low, high) {
  centre <- (low + high) / 2
  half_range <- (high - low) / 2
  (x - centre) / half_range
}

#' Decode coded values back to actual values
#'
#' @param x_coded Numeric vector in -1/+1 coding.
#' @param low Numeric. The low-level value.
#' @param high Numeric. The high-level value.
#' @return Numeric vector of actual values.
#' @noRd
decode_from_coded <- function(x_coded, low, high) {
  centre <- (low + high) / 2
  half_range <- (high - low) / 2
  x_coded * half_range + centre
}

# ---------------------------------------------------------------------------
# Guards
# ---------------------------------------------------------------------------

#' Stop with an informative message if no model has been fitted
#' @noRd
assert_model_fitted <- function(design) {
  if (is.null(design$model)) {
    stop(
      "No model fitted yet. Call `fit_model()` first.",
      call. = FALSE
    )
  }
  invisible(design)
}

#' Stop if the design is not a 2-level coded design
#' @noRd
assert_two_level <- function(design) {
  if (!isTRUE(design$coded)) {
    stop(
      "This function requires a 2-level coded design (levels = 2 for all factors).",
      call. = FALSE
    )
  }
  invisible(design)
}

#' Check whether a fitted model is saturated (df_residual == 0)
#'
#' A saturated model has as many parameters as observations, leaving zero
#' residual degrees of freedom. Standard errors and p-values are undefined.
#' @noRd
is_saturated <- function(design) {
  if (is.null(design$model)) return(FALSE)
  summary(design$model)$df[2] == 0L
}

# ---------------------------------------------------------------------------
# Plot helpers (return ggplot objects; used by Shiny server)
# ---------------------------------------------------------------------------

#' Main effects plot for a single factor
#'
#' @param design A `doe_design` object with response data.
#' @param factor_name Character. Name of the factor to plot.
#' @return A ggplot2 object.
#' @noRd
plot_main_effects <- function(design, factor_name) {
  assert_model_fitted(design)

  df <- design$design_matrix
  df[[design$response_name]] <- design$response

  # Compute mean response at each level of the chosen factor
  means <- stats::aggregate(
    df[[design$response_name]],
    by = list(level = df[[factor_name]]),
    FUN = mean
  )
  names(means) <- c("level", "mean_response")
  means$level <- as.factor(means$level)

  ggplot2::ggplot(means, ggplot2::aes(x = .data$level, y = .data$mean_response, group = 1)) +
    ggplot2::geom_point(pch = 21, size = 3, fill = ggNormic::normic_colors$greens[[2]]) +
    ggplot2::geom_line(col = ggNormic::normic_colors$greens[[2]]) +
    ggplot2::labs(
      title = paste("Main Effect:", factor_name),
      x = factor_name,
      y = design$response_name
    ) +
    tryCatch(
      if (requireNamespace("ggNormic", quietly = TRUE)) ggNormic::theme_normic() else ggplot2::theme_bw(),
      error = function(e) ggplot2::theme_bw()
    )
}

#' All-factors main effects overview
#'
#' @param design A `doe_design` object with response data.
#' @return A ggplot2 object with one panel per factor (facet_wrap).
#' @noRd
plot_all_main_effects <- function(design) {
  assert_model_fitted(design)

  df <- design$design_matrix
  df[[design$response_name]] <- design$response

  rows <- lapply(design$factors, function(f) {
    m <- stats::aggregate(
      df[[design$response_name]],
      by  = list(level = df[[f]]),
      FUN = mean
    )
    names(m) <- c("level", "mean_response")
    m$factor  <- f
    m
  })
  long_df        <- do.call(rbind, rows)
  long_df$level  <- as.character(long_df$level)
  long_df$factor <- factor(long_df$factor, levels = design$factors)

  ggplot2::ggplot(
    long_df,
    ggplot2::aes(x = .data$level, y = .data$mean_response, group = 1)
  ) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~ factor, scales = "free_x") +
    ggplot2::labs(
      title = "All Main Effects",
      x     = "Level",
      y     = design$response_name
    ) +
    ggplot2::theme_bw()
}

#' Interaction plot for two to four factors
#'
#' Visualizes mean response across factor level combinations.
#' - 2 factors: lines plot (x = factor1, colour = factor2)
#' - 3 factors: lines plot with `facet_wrap` by factor3
#' - 4 factors: lines plot with `facet_grid(factor3 ~ factor4)`
#'
#' @param design A `doe_design` object with response data.
#' @param factor1 Character. Factor on x-axis.
#' @param factor2 Character. Factor defining line colour/groups.
#' @param factor3 Character or `NULL`. Third factor for facet panels.
#' @param factor4 Character or `NULL`. Fourth factor for facet grid rows
#'   (requires `factor3`).
#' @return A ggplot2 object.
#' @noRd
plot_interaction <- function(design, factor1, factor2,
                              factor3 = NULL, factor4 = NULL) {
  assert_model_fitted(design)

  # Collect active factors (drop NULLs)
  all_factors <- c(factor1, factor2, factor3, factor4)
  all_factors <- all_factors[!vapply(all_factors, is.null, logical(1L))]
  n_active    <- length(all_factors)

  df <- design$design_matrix
  df[[design$response_name]] <- design$response

  # Dynamic aggregation over all grouping factors
  by_list        <- lapply(all_factors, function(f) df[[f]])
  names(by_list) <- all_factors
  means          <- stats::aggregate(
    df[[design$response_name]],
    by  = by_list,
    FUN = mean
  )
  names(means)[ncol(means)] <- "mean_response"

  means[[factor2]] <- as.factor(means[[factor2]])

  p <- ggplot2::ggplot(
    means,
    ggplot2::aes(
      x     = .data[[factor1]],
      y     = .data$mean_response,
      color = .data[[factor2]],
      fill = .data[[factor2]],
      group = .data[[factor2]]
    )
  ) +
    ggplot2::geom_point(pch = 21, col = "black", size = 3) +
    ggplot2::geom_line() +
    ggplot2::labs(
      title = paste("Interaction:", paste(all_factors, collapse = " \u00d7 ")),
      x     = factor1,
      y     = design$response_name,
      color = factor2
    ) +
    tryCatch(
      if (requireNamespace("ggNormic", quietly = TRUE)) ggNormic::theme_normic() else ggplot2::theme_bw(),
      error = function(e) ggplot2::theme_bw()
    )

  if (n_active == 3L) {
    means[[factor3]] <- as.factor(means[[factor3]])
    p <- p + ggplot2::facet_wrap(stats::as.formula(paste("~", factor3)))
  } else if (n_active >= 4L) {
    means[[factor3]] <- as.factor(means[[factor3]])
    means[[factor4]] <- as.factor(means[[factor4]])
    p <- p + ggplot2::facet_grid(
      stats::as.formula(paste(factor3, "~", factor4))
    )
  }

  if (requireNamespace("ggNormic", quietly = TRUE)) {
    p <- tryCatch(
      p + ggNormic::scale_color_normic_d(palette = "lights") + ggNormic::scale_fill_normic_d(palette = "lights"),
      error = function(e) p
    )
  }

  p
}

#' Pareto diagram of standardized effects
#'
#' @param design A `doe_design` object with fitted model.
#' @param alpha Numeric. Significance level for the reference line (default 0.05).
#' @return A ggplot2 object.
#' @noRd
plot_pareto <- function(design, alpha = 0.05) {
  assert_model_fitted(design)

  s        <- summary(design$model)
  df_resid <- s$df[2]

  coef_table       <- as.data.frame(s$coefficients)
  coef_table$term  <- rownames(coef_table)
  coef_table       <- coef_table[coef_table$term != "(Intercept)", , drop = FALSE]
  coef_table$abs_t <- abs(coef_table[, "t value"])
  coef_table       <- coef_table[order(coef_table$abs_t, decreasing = FALSE), ]
  coef_table$term  <- factor(coef_table$term, levels = coef_table$term)

  if (df_resid <= 0L) {
    warning(
      "Model is saturated (df_residual = 0). The Pareto chart cannot display ",
      "a significance reference line. Use 'Two-way only' or 'Main effects only' ",
      "interactions, or add replicates to obtain residual degrees of freedom.",
      call. = FALSE
    )
    return(
      ggplot2::ggplot(coef_table,
                      ggplot2::aes(x = .data$abs_t, y = .data$term), fill = ggNormic::normic_colors$greens[[2]]) +
        ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
        ggplot2::labs(
          title   = "Pareto Chart of Standardized Effects",
          x       = "|Standardized Effect| (t-value)",
          y       = "Term",
          caption = "Saturated model \u2014 no significance line (df_residual = 0)"
        ) +
        tryCatch(
      if (requireNamespace("ggNormic", quietly = TRUE)) ggNormic::theme_normic() else ggplot2::theme_bw(),
      error = function(e) ggplot2::theme_bw()
    )
    )
  }

  t_crit <- stats::qt(1 - alpha / 2, df = df_resid)

  ggplot2::ggplot(coef_table, ggplot2::aes(x = .data$abs_t, y = .data$term)) +
    ggplot2::geom_bar(stat = "identity", fill = ggNormic::normic_colors$greens[[2]]) +
    ggplot2::geom_vline(xintercept = t_crit, linetype = "dashed", color = ggNormic::normic_colors$reds[[1]]) +
    ggplot2::labs(
      title   = "Pareto Chart of Standardized Effects",
      x       = "|Standardized Effect| (t-value)",
      y       = "Term",
      caption = paste0("Red line = t\u2080.", round(alpha / 2, 3),
                       " (df = ", df_resid, ")")
    ) +
    tryCatch(
      if (requireNamespace("ggNormic", quietly = TRUE)) ggNormic::theme_normic() else ggplot2::theme_bw(),
      error = function(e) ggplot2::theme_bw()
    )
}
