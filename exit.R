#' Exit Script Silently
#'
#' Stops script execution without printing an error message. Useful for
#' clean exits in scripts where you don't want the standard "Error" prefix.
#'
#' @param ... Ignored arguments.
#'
#' @return Does not return.
#'
#' @export
#'
#' @seealso \code{\link{iaw$done}}, \code{\link{stop}}
#'
#' @examples
#' \dontrun{
#' if (condition_met) {
#'     message("Early exit")
#'     exit()
#' }
#' }

exit <- function(...) {
    blankMsg <- sprintf("\r%s\r", paste(rep(" ", getOption("width") - 1L), collapse = " "))
    stop(simpleError(blankMsg))
}
