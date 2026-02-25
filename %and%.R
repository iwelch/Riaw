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
#' # Execute right-hand side only when condition is TRUE
#' x <- 5
#' (x > 3) %and% message("x is greater than 3")
#'
#' # Condition is FALSE: right-hand side is never evaluated
#' (x > 10) %and% message("this will not print")
#'
#' # Common pattern: conditional abort on bad input
#' \dontrun{
#' validate <- function(df) {
#'   (nrow(df) == 0) %and% iaw$abort("data frame is empty")
#' }
#' validate(data.frame(x = 1:5))   # passes silently
#' validate(data.frame())           # aborts
#' }

`%and%` <- function(e1, e2) {
    e2 <- substitute(e2)
    stopifnot(is.logical(e1), length(e1) == 1L, !is.na(e1))
    if (e1) {
        if (is.character(e2)) stop("Use (cond) %and% abort('message'), not (cond) %and% 'message'")
        eval.parent(e2)
    }
    invisible(NULL)
}
