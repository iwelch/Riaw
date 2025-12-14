#' Enable Line Number Reporting in Errors
#'
#' Sets R options to include source line numbers in error messages and
#' tracebacks. Very helpful for debugging sourced scripts.
#'
#' @return Invisible NULL. Sets \code{options(error = ...)}.
#'
#' @export
#'
#' @seealso \code{\link{iaw$debug.on}}, \code{\link{iaw$debug.advice}},
#'   \code{\link{traceback}}
#'
#' @examples
#' \dontrun{
#' iaw$enable.error.line.num()
#' source("my_script.R")
#' # Errors now show line numbers
#' }

iaw$enable.error.line.num <- function() {
    options(error = quote({
        print(attr(dump.frames(), "error.message"))
        traceback()
    }))
    invisible(NULL)
}
