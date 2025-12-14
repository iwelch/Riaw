#' Print to Both stdout and stderr
#'
#' Prints output to both standard output and standard error streams.
#' Useful for ensuring messages appear both in console and in log files.
#'
#' @param ... Arguments passed to \code{cat()}.
#'
#' @return Invisible NULL.
#'
#' @export
#'
#' @seealso \code{\link{iaw$cat.stderr}}, \code{\link{cat}}, \code{\link{message}}
#'
#' @examples
#' iaw$cat.both("Processing step 1\n")
#' # Prints to both stdout and stderr

iaw$cat.both <- function(...) {
    cat(..., file = stderr())
    cat(...)
    invisible(NULL)
}
