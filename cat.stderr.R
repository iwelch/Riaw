#' Cat to stderr
#'
#' @name cat.stderr
#'
#' Prints to standard error.
#'
#' @param ... Items to print.
#'
#' @return Invisible NULL.
#'
#' @family utilities
#' @export

iaw$cat.stderr <- function(...) {
    cat(..., file = stderr())
    invisible(NULL)
}
