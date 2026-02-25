#' Embed Fonts in PDF
#'
#' @name plot.pdf.embedfonts
#'
#' Embeds fonts in a PDF file.
#'
#' @param filename PDF filename.
#'
#' @return Invisible NULL.
#'
#' @examples
#' \dontrun{
#' # Create a PDF then embed fonts for portability
#' iaw$pdf.start("myplot.pdf")
#' plot(1:10, main = "Test plot")
#' iaw$pdf.end()
#' iaw$pdf.embedfonts("myplot.pdf")
#'
#' # Embed fonts in an existing PDF file
#' iaw$pdf.embedfonts("results/figure1.pdf")
#' }
#'
#' @family plotting
#' @export

iaw$pdf.embedfonts <- function(filename) {
    stopifnot(is.character(filename), length(filename) == 1L)
    grDevices::embedFonts(filename)
    invisible(NULL)
}
