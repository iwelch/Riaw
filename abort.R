#' Abort Program Execution with Error Message
#'
#' @name abort
#'
#' Terminates program execution with an informative error message.
#'
#' @param errstring A character string describing the error.
#'
#' @return Does not return; stops execution.
#'
#' @family error-handling
#' @export
#'
#' @examples
#' \dontrun{
#' iaw$abort("File not found")
#' }

iaw$abort <- function(errstring) {
    stopifnot(is.character(errstring), length(errstring) == 1L)
    message("iaw$abort")
    browser()
}
