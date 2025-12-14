#' Assert a Condition is True
#'
#' @name assert
#'
#' Checks that a condition is TRUE; if not, aborts with an error message.
#'
#' @param condition A logical scalar that should be TRUE.
#' @param ... Error message strings passed to iaw$abort.
#'
#' @return Invisible NULL if condition is TRUE.
#'
#' @family error-handling
#' @export
#'
#' @examples
#' x <- 5
#' iaw$assert(x > 0, "x must be positive")

iaw$assert <- function(condition, ...) {
    stopifnot(is.logical(condition), length(condition) == 1L)
    if (!condition) {
        iaw$abort(...)
    }
    invisible(NULL)
}
