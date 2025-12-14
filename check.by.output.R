#' Check Output of By Operation
#'
#' @name check.by.output
#'
#' Validates output from by/mc.by operations.
#'
#' @param output Output from by operation.
#'
#' @return Invisibly returns TRUE if valid.
#'
#' @family utilities
#' @keywords internal

iaw$check.by.output <- function(output) {
    stopifnot(is.list(output))
    invisible(TRUE)
}
