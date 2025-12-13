
#' Concatenate vector of strings into one string
#'
#' @name strcat
#'
#'  @usage strcat( c("a", "b"), sep="," ) -> "a,b"
#'
#'  @param a character vector
#'
#'  @return a character string
#'

iaw$strcat <- function( svec, sep=" ") paste( svec, collapse=sep )
