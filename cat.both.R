#' Cat to Both stdout and stderr
#'
#' @name cat.both
#'
#' Prints to both standard output and error.
#'
#' @param ... Items to print.
#'
#' @return Invisible NULL.
#'
#' @family utilities
#' @export

iaw$cat.both <- function(...) {
    cat(...)
    cat(..., file = stderr())
    invisible(NULL)
}
