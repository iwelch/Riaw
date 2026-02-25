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
#' # Basic abort with a plain message
#' \dontrun{
#' iaw$abort("File not found")
#' }
#'
#' # Abort with a dynamic message built from context
#' \dontrun{
#' load_data <- function(path) {
#'   if (!file.exists(path))
#'     iaw$abort(paste("Cannot find file:", path))
#'   readRDS(path)
#' }
#' load_data("/no/such/file.rds")
#' }
#'
#' # Combined with %or% for readable guard clauses
#' \dontrun{
#' (is.numeric(x)) %or% iaw$abort("x must be numeric")
#' }

iaw$abort <- function(errstring) {
    stopifnot(is.character(errstring), length(errstring) == 1L)
    message("iaw$abort: ", errstring)
    stop(errstring, call. = FALSE)
}
