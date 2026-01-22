#' Start PDF Output (Bera Style)
#'
#' @name plot.pdf.start.bera
#'
#' Opens PDF with Bera font settings.
#'
#' @param filename PDF filename.
#' @param ... Additional arguments.
#'
#' @return Invisible NULL.
#'
#' @family plotting
#' @keywords internal

iaw$pdf.start.bera <- function(filename, ...) {
    iaw$plot.pdf.start(filename, ...)
}
