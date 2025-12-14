#' Conditional AND Operator with Error Handling
#'
#' A Perl-like 'and' condition that executes the right-hand side only if the
#' left-hand side is TRUE. Commonly used with estrings for conditional error
#' messages.
#'
#' @param e1 A logical condition to evaluate.
#' @param e2 Either an error message string (with optional \code{\{\{var\}\}}
#'   interpolation) or an expression to execute.
#'
#' @return Invisibly returns NULL. Side effect: if \code{e1} is TRUE and
#'   \code{e2} is a character string, calls \code{iaw$abort()} with the message.
#'   If \code{e2} is an expression, evaluates it.
#'
#' @export
#'
#' @seealso \code{\link{\%or\%}} for the opposite behavior (execute on FALSE)
#'
#' @examples
#' # Execute code block when condition is TRUE
#' x <- 5
#' (x > 3) %and% message("x is greater than 3")
#'
#' # Abort with error message when condition is TRUE
#' \dontrun{
#' y <- "not a number"
#' (!is.numeric(y)) %and% "y must be numeric, but got {{class(y)}}"
#' }
#'
#' # Conditional assignment
#' result <- NULL
#' (is.null(result)) %and% { result <- "default" }

`%and%` <- function(e1, e2) {
    if (e1) {
        if (is.character(e2)) iaw$abort(e2) else eval(e2)
    }
    invisible(NULL)
}
