# Internal label builder — one label string per run
.make_labels <- function(design) {
  df    <- design$design_matrix[, design$factors, drop = FALSE]
  parts <- lapply(design$factors, function(f) paste0(f, "=", as.character(df[[f]])))
  do.call(paste, c(parts, list(sep = ", ")))
}

#' Compute fold changes relative to a reference treatment combination
#'
#' Groups runs by unique factor-level combination, computes the mean response
#' per group, and expresses each group's mean as a fold change relative to the
#' chosen reference combination.  When replicates are present the returned
#' data frame has one row per experimental run so that individual values can be
#' overlaid by [plot_fold_changes()].
#'
#' @param design A fitted \code{doe_design} object (must have \code{$response}
#'   set via [fit_model()]).
#' @param reference_label A character string identifying the reference
#'   combination; must match one of the labels produced internally, e.g.
#'   \code{"A=-1, B=1"}.
#' @return A data frame with one row per run and columns:
#'   \describe{
#'     \item{label}{Human-readable factor-level combination string.}
#'     \item{response}{Individual run response value.}
#'     \item{mean_response}{Mean response for this combination.}
#'     \item{fold_change}{Individual run response / reference mean response.}
#'     \item{mean_fold_change}{Mean response / reference mean response.}
#'     \item{is_reference}{Logical; \code{TRUE} for the reference combination.}
#'   }
#' @examples
#' d <- full_factorial(
#'   factors      = c("A", "B"),
#'   levels       = c(2, 2),
#'   level_values = list(A = c(-1, 1), B = c(-1, 1))
#' )
#' d  <- fit_model(d, response = c(1, 2, 3, 4), interactions = "two_way")
#' fc <- compute_fold_changes(d, reference_label = "A=-1, B=-1")
#' @export
compute_fold_changes <- function(design, reference_label) {
  if (is.null(design$response))
    stop("No response fitted. Call fit_model() first.", call. = FALSE)

  df          <- design$design_matrix[, design$factors, drop = FALSE]
  df$response <- design$response
  df$label    <- .make_labels(design)

  means <- tapply(df$response, df$label, mean)

  if (!reference_label %in% names(means))
    stop(sprintf("Reference '%s' not found in design combinations.",
                 reference_label), call. = FALSE)

  ref_mean <- means[[reference_label]]
  if (ref_mean == 0)
    stop("Reference mean response is zero; fold change is undefined.", call. = FALSE)

  df$mean_response    <- as.numeric(means[df$label])
  df$fold_change      <- df$response      / ref_mean
  df$mean_fold_change <- df$mean_response / ref_mean
  df$is_reference     <- df$label == reference_label

  df[, c("label", "response", "mean_response",
         "fold_change", "mean_fold_change", "is_reference")]
}

#' Plot fold changes as a horizontal barplot ordered by fold change
#'
#' Draws a horizontal bar chart where each bar represents a treatment
#' combination's mean fold change relative to the reference (shown in purple).
#' When replicates are present, individual run fold changes are overlaid as
#' points.  The maximum fold change value is added as an extra break on the
#' x-axis and is visible in the hover tooltip.
#'
#' @param fc_df A data frame from [compute_fold_changes()].
#' @param show_y_labels Logical. When `FALSE`, the y-axis (combination labels)
#'   text is hidden. Default `FALSE`.
#' @return A ggplot2 object.
#' @examples
#' d <- full_factorial(
#'   factors      = c("A", "B"),
#'   levels       = c(2, 2),
#'   level_values = list(A = c(-1, 1), B = c(-1, 1))
#' )
#' d  <- fit_model(d, response = c(1, 2, 3, 4), interactions = "two_way")
#' fc <- compute_fold_changes(d, reference_label = "A=-1, B=-1")
#' plot_fold_changes(fc)
#' @export
plot_fold_changes <- function(fc_df, show_y_labels = FALSE) {
  summ         <- unique(fc_df[, c("label", "mean_fold_change", "is_reference")])
  ord          <- summ$label[order(summ$mean_fold_change)]
  summ$label   <- factor(summ$label,  levels = ord)
  fc_df$label  <- factor(fc_df$label, levels = ord)

  # Pre-label the fill aesthetic so plotly inherits the proper legend names
  summ$fill_label <- factor(
    summ$is_reference,
    levels = c(FALSE, TRUE),
    labels = c("Sample", "Reference")
  )

  # Hover text for bars
  summ$hover_text <- sprintf(
    "<b>%s</b><br>Mean fold change: %s\u00d7",
    summ$label, .fmt_val(summ$mean_fold_change)
  )

  # Max fold change axis break (value also visible in hover and on axis)
  max_fc      <- max(summ$mean_fold_change)
  base_breaks <- pretty(c(min(summ$mean_fold_change, 0), max_fc))
  all_breaks  <- sort(unique(c(base_breaks, round(max_fc, 3L))))

  p <- ggplot2::ggplot(
    summ,
    ggplot2::aes(
      x    = .data$label,
      y    = .data$mean_fold_change,
      fill = .data$fill_label,
      text = .data$hover_text
    )
  ) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40") +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(
      values = c("Sample"    = ggNormic::normic_colors$greens[[2]],
                 "Reference" = ggNormic::normic_colors$purples[[2]]),
      name   = NULL
    ) +
    ggplot2::scale_y_continuous(
      labels = .fmt_val,
      breaks = all_breaks,
      expand = ggplot2::expansion(mult = c(0, 0.15))
    ) +
    ggplot2::labs(
      title = "Fold Change Relative to Reference",
      x     = "Treatment Combination",
      y     = "Fold Change"
    ) +
    ggplot2::theme_bw()

  # Overlay individual run values when replicates are present
  if (anyDuplicated(fc_df$label) > 0L) {
    p <- p + ggplot2::geom_point(
      data        = fc_df,
      mapping     = ggplot2::aes(x = .data$label, y = .data$fold_change),
      colour      = "black",
      size        = 2L,
      inherit.aes = FALSE,
      position    = ggplot2::position_jitter(height = 0.1, seed = 1L)
    )
  }

  if (!show_y_labels)
    p <- p + ggplot2::theme(axis.text.y = ggplot2::element_blank())

  p
}
