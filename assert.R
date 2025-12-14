#' Assert a Condition is True
#'
#' Checks that a condition is TRUE; if not, aborts with an error message.
#' Useful for validating function inputs and intermediate states.
#'
#' @param condition A logical scalar that should be TRUE.
#' @param ... Error message strings (passed to \code{iaw$abort}). Supports
#'   \code{\{\{expr\}\}} interpolation syntax.
#'
#' @return Invisible NULL if the condition is TRUE. Does not return if FALSE.
#'
#' @export
#'
#' @seealso \code{\link{iaw$abort}}, \code{\link{stopifnot}}
#'
#' @examples
#' # Basic assertion
#' x <- 5
#' iaw$assert(x > 0, "x must be positive")
#'
#' \dontrun{
#' # Assertion that fails
#' y <- -3
#' iaw$assert(y > 0, "y must be positive, got {{y}}")
#' }
#'
#' # Multiple assertions in function
#' my_sqrt <- function(x) {
#'     iaw$assert(is.numeric(x), "x must be numeric")
#'     iaw$assert(all(x >= 0), "x must be non-negative")
#'     sqrt(x)
#' }
#' my_sqrt(4)  # Returns 2

iaw$assert <- function(condition, ...) {
    if (!condition) {
        iaw$abort(...)
        stop()
    }
    invisible(NULL)
}
