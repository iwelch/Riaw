#' Print Heartbeat
#'
#' @name heartbeat
#'
#' Prints periodic progress indicator.
#'
#' @param i Current iteration.
#' @param every Print every N iterations.
#'
#' @return Invisible NULL.
#'
#' @family utilities
#' @export

iaw$heartbeat <- function(i, every = 100) {
    stopifnot(is.numeric(i), is.numeric(every))
    if (i %% every == 0) cat(".")
    invisible(NULL)
}
