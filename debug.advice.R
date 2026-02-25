#' Debug Advice
#'
#' @name debug.advice
#'
#' Prints debugging tips.
#'
#' @return Invisible NULL.
#'
#' @examples
#' # Print R debugging tips to the console
#' iaw$debug.advice()
#'
#' \dontrun{
#' # Call at the start of an interactive debugging session
#' iaw$debug.advice()
#' # Then follow the tips, e.g.:
#' options(error = recover)
#' }
#'
#' @family utilities
#' @export

iaw$debug.advice <- function() {
    cat("Debug tips:\n")
    cat("1. Use traceback() after error\n")
    cat("2. Use browser() to set breakpoint\n")
    cat("3. Use debug(fn) to trace function\n")
    cat("4. Use options(error = recover)\n")
    invisible(NULL)
}
