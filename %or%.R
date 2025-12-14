#' Conditional OR Operator with Error Handling
#'
#' @name %or%
#'
#' A Perl-like 'or' condition that executes the right-hand side only if the
#' left-hand side is FALSE. Primary use is for assertions.
#'
#' @param e1 A logical scalar condition to evaluate.
#' @param e2 Either an error message string or an expression to execute.
#'
#' @return Invisibly returns NULL.
#'
#' @family operators
#' @export
#'
#' @seealso \code{\link{%and%}}
#'
#' @examples
#' x <- "hello"
#' (is.character(x)) %or% "x must be a character"

`%or%` <- function(e1, e2) {
    stopifnot(is.logical(e1), length(e1) == 1L)
    if (!e1) {
        if (is.character(e2)) iaw$abort(e2) else eval(e2)
    }
    invisible(NULL)
}
