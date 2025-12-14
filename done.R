#' Print Done Message
#'
#' @name done
#'
#' Prints completion message with timestamp.
#'
#' @return Invisible NULL.
#'
#' @family utilities
#' @export

iaw$done <- function() {
    cat("Done at", format(Sys.time()), "\n")
    invisible(NULL)
}
