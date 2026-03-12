#' Compare effects from multiple fitted designs
#'
#' Combines the effects tables from multiple fitted `doe_design` objects into
#' a single data frame for comparison.
#'
#' @param ... Named `doe_design` objects with fitted models. Names are used as
#'   sample labels. If unnamed, labels default to `"Sample 1"`, `"Sample 2"`, etc.
#' @param labels Character vector of labels. Overrides names from `...`.
#' @return A data.frame with columns `term`, `effect`, `std_error`, `t_value`,
#'   `p_value`, `significant`, and `sample`.
#' @examples
#' d1 <- full_factorial(
#'   factors = c("A", "B"), levels = c(2, 2),
#'   level_values = list(A = c(-1, 1), B = c(-1, 1))
#' )
#' d1 <- fit_model(d1, response = c(1, 2, 3, 4), interactions = "two_way")
#' d2 <- fit_model(d1, response = c(2, 3, 4, 5), interactions = "two_way")
#' compare_effects(Batch1 = d1, Batch2 = d2)
#' @export
compare_effects <- function(..., labels = NULL) {
  designs <- list(...)
  if (is.null(labels)) labels <- names(designs)
  if (is.null(labels) || any(is.na(labels)) || any(labels == ""))
    labels <- paste0("Sample ", seq_along(designs))

  tables <- lapply(seq_along(designs), function(i) {
    eff        <- extract_effects(designs[[i]])
    eff$sample <- labels[[i]]
    eff
  })
  do.call(rbind, tables)
}

#' Plot overlaid effect estimates from multiple samples
#'
#' Creates a grouped bar chart of effect estimates with error bars.
#'
#' @param comparison_df Data frame from [compare_effects()].
#' @return A ggplot2 object.
#' @examples
#' d1 <- full_factorial(
#'   factors = c("A", "B"), levels = c(2, 2),
#'   level_values = list(A = c(-1, 1), B = c(-1, 1))
#' )
#' d1 <- fit_model(d1, response = c(1, 2, 3, 4), interactions = "two_way")
#' d2 <- fit_model(d1, response = c(2, 3, 4, 5), interactions = "two_way")
#' df <- compare_effects(Batch1 = d1, Batch2 = d2)
#' plot_effect_comparison(df)
#' @export
plot_effect_comparison <- function(comparison_df) {
  comparison_df$sample <- factor(comparison_df$sample,
                                  levels = unique(comparison_df$sample))

  ggplot2::ggplot(
    comparison_df,
    ggplot2::aes(
      x    = stats::reorder(.data$term, abs(.data$effect)),
      y    = .data$effect,
      fill = .data$sample,
      ymin = .data$effect - .data$std_error,
      ymax = .data$effect + .data$std_error
    )
  ) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::geom_errorbar(
      position = ggplot2::position_dodge(0.9), width = 0.25
    ) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Effect Comparison Across Samples",
      x     = "Term",
      y     = "Effect (half-effect)",
      fill  = "Sample"
    ) +
    ggplot2::theme_bw()
}
