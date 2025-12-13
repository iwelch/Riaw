
#' OR
#'
#' @name %or%
#'
#'  A perl-like 'and' condition that works with estrings as errors
#'
#' @usage condition %or% estring.or.statement
#'
#' @param estring.or.statement: if an estring, abort is called with the argument.  if a statement, it is executed.
#'
#' @return
#'
#' @seealso %and%
#'
#' @examples
#' 	(is.character(x)) %or% "Unfortunately, x is not a character"
#' 	(x=="A") %or% "Unfortunately, x is not the character A but {{x}}"
#' 	(!is.numeric(y)) %or% { y <- ascii(y) }


`%or%` <- function(e1, e2) {
    if (!e1) {
        if (is.character(e2)) {
            iaw$abort(e2)
        } else {
            eval(e2) }
    }
}
