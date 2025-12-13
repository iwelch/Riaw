
#' ASCII To Character and Reverse Conversion
#'
#' convert an integer vector of ascii codes into ascii characters, or vice-versa.
#'
#' @name ascii
#'
#' @param input.vector either a vector of ASCII code integers (to convert to characters) or a vector of characters (to convert to ASCII code integers)
#'
#' @return returns a vector of equal length
#'
#' @examples
#'   > all(ascii( c(60,61) ) == c("<","="))
#'    [1] TRUE
#'
#'   > ascii( c("<","=") )
#'     <  =
#'     60 61
#'
#' @seealso strtoi

ascii <- function ( input.vector ) {
#  (is.vector(input.vector)) %or% "input is not a vector but a {{class(input.vector}}"
  if (is.character( input.vector )) return(sapply( input.vector, function(s) strtoi(charToRaw(s),16L) ))
  if (is.numeric( input.vector )) return(sapply( input.vector, function(n) { rawToChar(as.raw(n)) } ))

  (FALSE) %and% "input was a vector, but neither numeric nor character"
}

iaw$ascii <- ascii
