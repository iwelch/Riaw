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
#'
#' # Bera style with custom dimensions for a presentation slide
#' iaw$pdf.start.bera("slide_chart.pdf", wd = 12, ht = 6)
#' barplot(c(3.2, 4.1, 2.8, 5.0), names.arg = paste0("Q", 1:4),
#'         col = "steelblue", main = "Quarterly Revenue ($M)")
#' iaw$pdf.end()
#'
#' # Bera style delegates to pdf.start(), so all pdf.start() args work
#' iaw$pdf.start.bera("small_plot.pdf", scale = 0.5, ht = 4)
#' plot(rnorm(100), pch = 16, col = "gray40", main = "Small Bera PDF")
#' iaw$pdf.end()
#' }
#'
#' @family plotting
#' @keywords internal

iaw$pdf.start.bera <- function(filename, ...) {
    iaw$pdf.start(filename, ...)
}
