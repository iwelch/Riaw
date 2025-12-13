
#' IS.INSTRING
#'
#' @name is.instring("ab", c("this is ab in", "nada", "abracadabra"))
#'         [1]  TRUE FALSE  TRUE
#'
#'  a wrapper for grepl
#'
#' @usage is.instring (needle, heystack)
#'
#' @param needle the string to search for
#' @param heystack the string(s) we are searching for
#'
#' @return a vector of booleans
#'
#' @examples
#'    iaw$is.instring("ab", c("this is ab in", "nada", "abracadabra"))
#'         [1]  TRUE FALSE  TRUE
#'

iaw$is.instring <- function (needle, heystack) grepl(needle, heystack)
