#' Conditional OR Operator with Error Handling
#'
#' A Perl-like 'or' condition that executes the right-hand side only if the
#' left-hand side is FALSE. The primary use case is for assertions: if the
#' condition fails, abort with an error message.
#'
#' @param e1 A logical condition to evaluate.
#' @param e2 Either an error message string (with optional \code{\{\{var\}\}}
#'   interpolation) or an expression to execute.
#'
#' @return Invisibly returns NULL. Side effect: if \code{e1} is FALSE and
#'   \code{e2} is a character string, calls \code{iaw$abort()} with the message.
#'   If \code{e2} is an expression, evaluates it.
#'
#' @export
#'
#' @seealso \code{\link{\%and\%}} for the opposite behavior (execute on TRUE)
#'
#' @examples
#' # Assert that a condition is true (abort if false)
#' x <- "hello"
#' (is.character(x)) %or% "x must be a character"
#'
#' # With variable interpolation in error message
#' \dontrun{
#' y <- list(1, 2, 3)
#' (is.numeric(y)) %or% "y must be numeric, not {{class(y)}}"
#' }
#'
#' # Provide default value when condition fails
#' value <- NULL
#' (!is.null(value)) %or% { value <- 0 }
#' print(value)  # 0

`%or%` <- function(e1, e2) {
    if (!e1) {
        if (is.character(e2)) {
            iaw$abort(e2)
        } else {
            eval(e2)
        }
    }
    invisible(NULL)
}
