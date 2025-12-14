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

iaw$plot.pdf.end <- function() {
    grDevices::dev.off()
    invisible(NULL)
}
