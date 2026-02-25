#' End PDF Output
#'
#' @name pdf.end
#'
#' Closes the PDF device opened by \code{iaw$pdf.start}.
#'
#' @param verbose Numeric or logical. Print status message (default 1).
#'
#' @return Invisible \code{NULL}.
#'
#' @examples
#' \dontrun{
#' # Normal open/plot/close cycle
#' iaw$pdf.start("output")
#' plot(1:5)
#' iaw$pdf.end()
#'
#' # pdf.end() can also be called without a matching pdf.start()
#' # to close any currently open graphics device
#' iaw$pdf.end()
#'
#' # Suppress the status message with verbose = 0
#' iaw$pdf.start("output_quiet")
#' plot(1:5, main = "Quiet close")
#' iaw$pdf.end(verbose = 0)
#'
#' # Multi-page PDF: draw several plots then close once
#' iaw$pdf.start("multipage")
#' for (i in 1:4) plot(rnorm(50), main = paste("Page", i))
#' iaw$pdf.end()
#' }
#'
#' @family plotting
#' @export
#'
#' @seealso [iaw$pdf.start()]

iaw$pdf.end <- function(verbose = 1) {
    grDevices::dev.off()
    if (verbose) iaw$cat.stderr("[pdf.end]\n")
    options(pdf.openfile = NULL)
    invisible(NULL)
}
