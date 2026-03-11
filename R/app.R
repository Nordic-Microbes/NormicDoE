#' Launch the NormicDoE interactive Shiny application
#'
#' Opens an interactive web application for visualizing and analyzing a
#' factorial design. The app provides three visualization tabs (main effects,
#' interaction effects, Pareto diagram) plus a model summary panel and an
#' optimization tool.
#'
#' @param design A `doe_design` object to pre-load into the app, or `NULL`
#'   (default) to start with an empty session. When `NULL`, you must paste
#'   response values in the sidebar before any plots appear.
#'
#' @return Called for its side effect of launching a Shiny app. Returns
#'   invisibly.
#'
#' @examples
#' \dontrun{
#' d <- full_factorial(
#'   factors      = c("Temperature", "pH"),
#'   levels       = c(2, 2),
#'   level_values = list(Temperature = c(60, 80), pH = c(5, 7))
#' )
#' launch_app(d)
#' }
#'
#' @export
launch_app <- function(design = NULL) {
  if (!is.null(design) && !inherits(design, "doe_design")) {
    stop("`design` must be a `doe_design` object or NULL.", call. = FALSE)
  }

  factors <- if (!is.null(design)) design$factors else character(0)

  shiny::shinyApp(
    ui     = app_ui(factors),
    server = app_server(design)
  )
}
