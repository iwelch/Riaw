#' Assert Condition
#'
#' @name assert
#'
#' Checks a condition and aborts with an error message if FALSE.
#'
#' @param cond Logical scalar to test.
#' @param ... Error message arguments passed to abort().
#'
#' @return Invisible NULL if condition is TRUE; otherwise stops.
#'
#' @family error-handling
#' @export
#'
#' @examples
#' # Basic assertion that passes silently
#' iaw$assert(1 + 1 == 2, "math is broken")
#'
#' # Glue-style interpolation in the message
#' n <- 5
#' iaw$assert(n > 0, paste("n must be positive, got", n))
#'
#' # Common pattern: validate function arguments
#' \dontrun{
#' check_input <- function(x) {
#'   iaw$assert(is.numeric(x), paste("x must be numeric, got", class(x)))
#'   iaw$assert(length(x) > 0, "x must not be empty")
#' }
#' check_input(c(1, 2, 3))  # passes
#' check_input("oops")      # aborts with message
#' }

iaw$assert <- function(cond, ...) {
    stopifnot(is.logical(cond), length(cond) == 1L)
    if (!cond) {
        msg <- paste(...)
        if (nchar(msg) == 0) msg <- paste(deparse(substitute(cond)), collapse = " ")
        iaw$abort(msg)
    }
    invisible(NULL)
}
