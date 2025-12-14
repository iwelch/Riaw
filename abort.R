#' Abort Program Execution with Error Message
#'
#' Terminates program execution with an informative error message. The message
#' can contain \code{\{\{variable\}\}} syntax for interpolation via
#' \code{iaw$estring()}.
#'
#' @param errstring A character string describing the error. Supports
#'   \code{\{\{expr\}\}} syntax for variable interpolation.
#'
#' @return Does not return; stops execution with an error.
#'
#' @export
#'
#' @seealso \code{\link{iaw$assert}}, \code{\link{iaw$estring}},
#'   \code{\link{iaw$debug.advice}}
#'
#' @examples
#' \dontrun{
#' # Simple error message
#' iaw$abort("File not found")
#'
#' # With variable interpolation
#' x <- 42
#' iaw$abort("Expected positive value, got {{x}}")
#'
#' # Common usage with %or%
#' (length(data) > 0) %or% "Data cannot be empty"
#' }

iaw$abort <- function(errstring) {
    (iaw$is.character(errstring, 1)) %or% "argument must be a single string, not {{class(errstring)}}"
    errstring <- paste(iaw$estring(errstring), "\n")
    if (interactive()) message(errstring) else cat(errstring)
    stop(simpleError("see debug advice on start or iaw$debug.advice()"))
}
