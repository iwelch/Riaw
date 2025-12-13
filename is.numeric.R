
#' IS.NUMERIC
#'
#' @name is.numeric
#'
#'  extends is.numeric to accept an argument prescribing the length of the object.
#'
#' @usage is.numeric( nvec, required.length.of.nvec=0 )
#'
#' @param nvec the numeric vector
#' @param required.length an integer of how many are needed
#'
#' @return Boolean
#'

iaw$is.numeric <- function( nvec, required.length.of.nvec =0 ) {
    c1 <- (.Primitive("is.numeric")(nvec))
    c2 <- (length(nvec) == required.length.of.nvec)
    return (c1&c2)
}
