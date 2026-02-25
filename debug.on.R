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
#' \dontrun{
#' # Typical debugging workflow: enable, run failing code, then disable
#' iaw$debug.on()
#' tryCatch(log("not a number"), error = function(e) message(e$message))
#' options(error = NULL)  # disable when done
#' }
#'
#' \dontrun{
#' # Pair with debug.advice() for a guided debugging session
#' iaw$debug.advice()
#' iaw$debug.on()
#' }
#'
#' @family utilities
#' @export

iaw$debug.on <- function() {
    options(error = recover)
    message("Debug mode enabled")
    invisible(NULL)
}
