#' ASCII Code Conversion
#'
#' Converts between ASCII character codes (integers) and character strings.
#' Automatically detects input type and converts accordingly.
#'
#' @param input.vector Either:
#'   \itemize{
#'     \item Numeric vector of ASCII codes (0-127) to convert to characters
#'     \item Character vector to convert to ASCII codes
#'   }
#'
#' @return A vector of the same length:
#'   \itemize{
#'     \item If input is numeric: character vector
#'     \item If input is character: named numeric vector of ASCII codes
#'   }
#'
#' @export
#'
#' @seealso \code{\link{charToRaw}}, \code{\link{rawToChar}}, \code{\link{strtoi}}
#'
#' @examples
#' # Convert ASCII codes to characters
#' ascii(c(72, 101, 108, 108, 111))
#' # "H" "e" "l" "l" "o"
#'
#' # Convert characters to ASCII codes
#' ascii(c("H", "e", "l", "l", "o"))
#' #  H   e   l   l   o
#' # 72 101 108 108 111
#'
#' # Common codes
#' ascii(c(60, 61, 62))
#' # "<" "=" ">"

ascii <- function(input.vector) {
    if (is.character(input.vector)) {
        return(sapply(input.vector, function(s) strtoi(charToRaw(s), 16L)))
    }
    if (is.numeric(input.vector)) {
        return(sapply(input.vector, function(n) rawToChar(as.raw(n))))
    }
    stop("input must be numeric or character, not ", class(input.vector))
}

#' @export
iaw$ascii <- ascii
