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
#' @family plotting
#' @export

iaw$pdf.embedfonts <- function(filename) {
    stopifnot(is.character(filename), length(filename) == 1L)
    grDevices::embedFonts(filename)
    invisible(NULL)
}
