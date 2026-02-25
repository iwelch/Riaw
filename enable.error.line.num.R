#' Enable Error Line Numbers
#'
#' @name enable.error.line.num
#'
#' Enables line numbers in error messages.
#'
#' @return Invisible NULL.
#'
#' @examples
#' \dontrun{
#' # Turn on line-number reporting in error messages
#' iaw$enable.error.line.num()
#'
#' # Now errors show the source line where they occurred
#' f <- function() stop("something went wrong")
#' f()
#' }
#'
#' @family utilities
#' @export

iaw$enable.error.line.num <- function() {
    options(show.error.locations = TRUE)
    invisible(NULL)
}
