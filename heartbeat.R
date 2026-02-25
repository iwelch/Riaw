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
#' @examples
#' # Print a dot every 10 iterations to show a loop is alive
#' for (i in 1:50) {
#'   iaw$heartbeat(i, every = 10)
#' }
#' cat("\n")
#'
#' # Default: print every 100 iterations
#' for (i in 1:300) {
#'   iaw$heartbeat(i)
#' }
#' cat("\n")
#'
#' # Fine-grained progress: print every 5 iterations
#' for (i in 1:25) {
#'   iaw$heartbeat(i, every = 5)
#' }
#' cat("\n")                          # prints 5 dots
#'
#' # Typical use: monitor a long simulation run
#' results <- numeric(500)
#' for (i in seq_along(results)) {
#'   results[i] <- rnorm(1)
#'   iaw$heartbeat(i, every = 100)   # one dot per 100 draws
#' }
#' cat("\n")                          # prints 5 dots
#'
#' @family utilities
#' @export

iaw$heartbeat <- function(i, every = 100) {
    stopifnot(is.numeric(i), is.numeric(every))
    if (i %% every == 0) cat(".")
    invisible(NULL)
}
