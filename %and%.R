#' Conditional AND Operator with Error Handling
#'
#' @name %and%
#'
#' A Perl-like 'and' condition that executes the right-hand side only if the
#' left-hand side is TRUE. Commonly used with stop for conditional error
#' messages.
#'
#' @param e1 A logical scalar condition to evaluate.
#' @param e2 an expression to execute.
#'
#' @return Invisibly returns NULL.
#'
#' @family operators
#' @export
#'
#' @seealso \code{\link{%or%}}
#'
#' @examples
#' x <- 5
#' (x > 3) %and% message("x is greater than 3")

`%and%` <- function(e1, e2) {
    stopifnot(is.logical(e1), length(e1) == 1L)
    if (e1) eval(e2)
    invisible(NULL)
}
