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
#' # Clean company names with special characters for file-safe strings
#' iaw$ascii("Nestl\u00e9 S.A.")        # "Nestle S.A."
#' iaw$ascii("Cr\u00e9dit Agricole")     # "Credit Agricole"
#'
#' # Sanitize a vector of city names from international datasets
#' cities <- c("Z\u00fcrich", "Montr\u00e9al", "S\u00e3o Paulo")
#' vapply(cities, iaw$ascii, character(1))  # "Zurich" "Montreal" "Sao Paulo"
#'
#' # Plain ASCII passes through unchanged
#' iaw$ascii("Goldman Sachs")    # "Goldman Sachs"
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
