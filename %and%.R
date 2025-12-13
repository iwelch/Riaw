
#' AND
#'
#'  A perl-like 'and' condition that works with estrings as errors
#'
#' @name %and%
#'
#' @usage condition %and% estring.or.statement
#'
#' @param estring.or.statement: if an estring, abort is called with the argument.  if a statement, it is executed.
#'
#' @return
#'
#' @seealso  %or%
#'
#' @examples
#' 	(!is.character(x)) %and% "Unfortunately, x is not a character"
#' 	(x!="A") %and% "Unfortunately, x is not the character A but {{x}}"
#' 	(is.numeric(y)) %and% { y <- ascii(y) }


`%and%` <- function(e1, e2) {
  if (e1) { if (is.character(e2)) iaw$abort(e2) else eval(e2) }
}
