#' Print Done Message
#'
#' @name done
#'
#' Prints completion message with timestamp.
#'
#' @return Invisible NULL.
#'
#' @examples
#' \dontrun{
#' # Place at the end of a script to print a timestamp and quit cleanly
#' # (when run non-interactively, also calls q(save = "no"))
#' iaw$done()
#' # Prints: Done at 2025-01-15 14:32:07
#' }
#'
#' \dontrun{
#' # Typical batch script ending: write results, then done()
#' write.csv(results, "output.csv")
#' iaw$done()  # prints timestamp and exits
#' }
#'
#' \dontrun{
#' # In interactive mode, done() prints but does not quit
#' iaw$done()  # safe to call -- only quits in non-interactive sessions
#' }
#'
#' @family utilities
#' @export

iaw$done <- function() {
    cat("Done at", format(Sys.time()), "\n")
    while (sink.number() >= 1) sink()
    if (!interactive()) q(save = "no")
    invisible(NULL)
}
