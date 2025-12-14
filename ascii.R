#' Convert to ASCII
#'
#' @name ascii
#'
#' Converts object to ASCII representation.
#'
#' @param x Object to convert.
#'
#' @return ASCII string.
#'
#' @family utilities
#' @export

iaw$ascii <- function(x) {
    if (is.character(x)) {
        iconv(x, to = "ASCII//TRANSLIT")
    } else {
        as.character(x)
    }
}
