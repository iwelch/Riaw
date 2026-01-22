#' End PDF Output
#'
#' @name plot.pdf.end
#'
#' Closes the PDF device.
#'
#' @return Invisible NULL.
#'
#' @family plotting
#' @export

iaw$pdf.end <- function(verbose = 1) {
    grDevices::dev.off()
    if (verbose) iaw$cat.stderr("[pdf.end]\n")
    options(pdf.openfile = NULL)
    invisible(NULL)
}
