#' Enable Debug Mode
#'
#' @name debug.on
#'
#' Enables error recovery mode.
#'
#' @return Invisible NULL.
#'
#' @family utilities
#' @export

iaw$debug.on <- function() {
    options(error = recover)
    message("Debug mode enabled")
    invisible(NULL)
}
