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
#' @examples
#' \dontrun{
#' # Send a diagnostic message to stderr without polluting stdout
#' iaw$cat.stderr("Loading data...\n")
#'
#' # Report progress in a script whose stdout is redirected to a file
#' iaw$cat.stderr(sprintf("Processed %d rows\n", nrow(mydf)))
#' }
#'
#' @family utilities
#' @export

iaw$cat.stderr <- function(...) {
    cat(..., file = stderr())
    invisible(NULL)
}
