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
