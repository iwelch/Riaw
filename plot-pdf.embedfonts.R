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
#'
#' # Embed fonts after batch-generating multiple PDFs
#' for (f in list.files("figs/", pattern = "\\.pdf$", full.names = TRUE)) {
#'   iaw$pdf.embedfonts(f)
#' }
#'
#' # Typical publication workflow: create, close, then embed
#' pdf("draft_figure.pdf", width = 6, height = 4)
#' hist(rnorm(500), main = "Simulation draws", col = "steelblue")
#' dev.off()
#' iaw$pdf.embedfonts("draft_figure.pdf")
#' }
#'
#' @family plotting
#' @export

iaw$pdf.embedfonts <- function(filename) {
    stopifnot(is.character(filename), length(filename) == 1L)
    grDevices::embedFonts(filename)
    invisible(NULL)
}
