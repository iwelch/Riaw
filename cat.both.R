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
#' @examples
#' \dontrun{
#' # Print a status message that appears on both stdout and stderr
#' iaw$cat.both("Processing file...\n")
#'
#' # Useful when output is captured but warnings should still reach the console
#' iaw$cat.both("Warning: missing values found\n")
#' }
#'
#' @family utilities
#' @export

iaw$cat.both <- function(...) {
    cat(...)
    cat(..., file = stderr())
    invisible(NULL)
}
