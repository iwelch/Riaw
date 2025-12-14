#' Print to stderr Only
#'
#' Prints output to the standard error stream. Useful for log messages
#' that should appear in error logs but not standard output.
#'
#' @param ... Arguments passed to \code{cat()}.
#'
#' @return Invisible NULL.
#'
#' @export
#'
#' @seealso \code{\link{iaw$cat.both}}, \code{\link{cat}}, \code{\link{message}}
#'
#' @examples
#' iaw$cat.stderr("Debug info\n")

iaw$cat.stderr <- function(...) {
    cat(..., file = stderr())
    invisible(NULL)
}
