#' Test if Pattern Exists in String
#'
#' @name is.instring
#'
#' A wrapper for grepl.
#'
#' @param needle Character pattern to search for.
#' @param heystack Character vector to search within.
#'
#' @return Logical vector.
#'
#' @family type-checking
#' @export
#'
#' @examples
#' iaw$is.instring("ab", c("abc", "def", "ab"))

iaw$is.instring <- function(needle, heystack) {
    stopifnot(is.character(needle), length(needle) == 1L)
    stopifnot(is.character(heystack))
    grepl(needle, heystack)
}
