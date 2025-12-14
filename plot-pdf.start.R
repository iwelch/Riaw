#' Start PDF Output
#'
#' @name plot.pdf.start
#'
#' Opens a PDF device for plotting.
#'
#' @param filename PDF filename.
#' @param width Width in inches.
#' @param height Height in inches.
#' @param ... Additional pdf() arguments.
#'
#' @return Invisible NULL.
#'
#' @family plotting
#' @export
#'
#' @examples
#' \dontrun{
#' iaw$plot.pdf.start("plot.pdf")
#' plot(1:10)
#' iaw$plot.pdf.end()
#' }

iaw$plot.pdf.start <- function(filename, width = 7, height = 7, ...) {
    stopifnot(is.character(filename), length(filename) == 1L)
    stopifnot(is.numeric(width), is.numeric(height))
    
    if (Sys.info()["sysname"] == "Darwin") {
        grDevices::quartz(type = "pdf", file = filename, 
                         width = width, height = height, ...)
    } else {
        grDevices::pdf(file = filename, width = width, height = height, ...)
    }
    invisible(NULL)
}
