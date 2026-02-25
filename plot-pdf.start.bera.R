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
#' @examples
#' \dontrun{
#' # Open a PDF with Bera-style font settings
#' iaw$pdf.start.bera("output/figure.pdf")
#' plot(1:10, main = "Bera-style PDF")
#' iaw$pdf.end()
#' }
#'
#' @family plotting
#' @keywords internal

iaw$pdf.start.bera <- function(filename, ...) {
    iaw$pdf.start(filename, ...)
}
