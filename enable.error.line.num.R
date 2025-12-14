#' Enable Error Line Numbers
#'
#' @name enable.error.line.num
#'
#' Enables line numbers in error messages.
#'
#' @return Invisible NULL.
#'
#' @family utilities
#' @export

iaw$enable.error.line.num <- function() {
    options(show.error.locations = TRUE)
    invisible(NULL)
}
