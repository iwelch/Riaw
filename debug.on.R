#' Enable Debug Mode
#'
#' @name debug.on
#'
#' Enables error recovery mode.
#'
#' @return Invisible NULL.
#'
#' @examples
#' \dontrun{
#' # Enable interactive error recovery so R drops into recover() on errors
#' iaw$debug.on()
#'
#' # Restore normal error handling when done debugging
#' options(error = NULL)
#' }
#'
#' @family utilities
#' @export

iaw$debug.on <- function() {
    options(error = recover)
    message("Debug mode enabled")
    invisible(NULL)
}
