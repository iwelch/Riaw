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
#' \dontrun{
#' # Useful at the top of a batch script for easier post-mortem debugging
#' iaw$enable.error.line.num()
#' source("my_analysis.R")  # errors now include line numbers
#' }
#'
#' \dontrun{
#' # Check the current setting, enable if not already on
#' getOption("show.error.locations")  # NULL or FALSE by default
#' iaw$enable.error.line.num()
#' getOption("show.error.locations")  # TRUE
#' }
#'
#' @family utilities
#' @export

iaw$enable.error.line.num <- function() {
    options(show.error.locations = TRUE)
    invisible(NULL)
}
