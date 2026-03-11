#' Export a design to a CSV file
#'
#' Writes the design matrix (and optionally the response column) to a
#' comma-separated values file.
#'
#' @param design A `doe_design` object.
#' @param file Character. Path for the output `.csv` file.
#' @param include_response Logical. If `TRUE` and a response has been added,
#'   include it as the last column. Default is `FALSE`.
#' @param coded Logical. If `TRUE`, export coded (-1/+1) values instead of
#'   actual values (only valid for 2-level designs). Default is `FALSE`.
#'
#' @return Invisibly returns the file path.
#'
#' @examples
#' d <- full_factorial(
#'   factors      = c("Temperature", "pH"),
#'   levels       = c(2, 2),
#'   level_values = list(Temperature = c(60, 80), pH = c(5, 7))
#' )
#' tmp <- tempfile(fileext = ".csv")
#' export_design(d, file = tmp)
#'
#' @export
export_design <- function(design, file, include_response = FALSE,
                           coded = FALSE) {
  if (!inherits(design, "doe_design")) {
    stop("`design` must be a `doe_design` object.", call. = FALSE)
  }

  df <- as.data.frame(design, coded = coded)

  if (!include_response && !is.null(design$response_name)) {
    df[[design$response_name]] <- NULL
  }

  utils::write.csv(df, file = file, row.names = FALSE)
  invisible(file)
}
