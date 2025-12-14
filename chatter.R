#' Conditional Message Printing
#'
#' @name chatter
#'
#' Prints message only if verbose mode is on.
#'
#' @param ... Message components.
#' @param verbose Print message if TRUE.
#'
#' @return Invisibly returns TRUE.
#'
#' @family utilities
#' @export
#'
#' @examples
#' iaw$chatter("Processing...", verbose = TRUE)

iaw$chatter <- function(..., verbose = TRUE) {
    stopifnot(is.logical(verbose), length(verbose) == 1L)
    if (verbose) cat(...)
    invisible(TRUE)
}
