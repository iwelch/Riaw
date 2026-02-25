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
#' @examples
#' # Transliterate accented characters to ASCII
#' iaw$ascii("caf\u00e9")        # "cafe"
#' iaw$ascii("na\u00efve")       # "naive"
#'
#' # Non-character input is coerced via as.character()
#' iaw$ascii(3.14)               # "3.14"
#' iaw$ascii(TRUE)               # "TRUE"
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
