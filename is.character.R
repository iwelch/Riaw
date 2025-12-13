
#' IS.CHARACTER
#'
#' @name is.character
#'
#' extends is.character to accept an optional argument prescribing the length of the object.
#'
#' @usage is.character( c("a","b","c"), 3)
#'
#' @param cvec a character vector to test
#' @param len the necessary length of the vector
#'
#' @return true or false
#'

iaw$is.character <- function( cvec, required.length.of.cvec =0 ) {
    c1 <- (.Primitive("is.character")(cvec))
    c2 <- (length(cvec) == required.length.of.cvec)
    return (c1&c2)
}
