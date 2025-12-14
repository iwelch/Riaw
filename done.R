#' Exit Script Gracefully
#'
#' Stops execution gracefully, closing all sinks first. In non-interactive
#' mode, quits R. In interactive mode, stops with a friendly message.
#'
#' @return Does not return.
#'
#' @export
#'
#' @seealso \code{\link{iaw$exit}}, \code{\link{stop}}, \code{\link{q}}
#'
#' @examples
#' \dontrun{
#' # At end of script
#' message("Analysis complete")
#' iaw$done()
#' }

iaw$done <- function() {
    if (!interactive()) q()
    while (sink.number() >= 1) sink()
    .Internal(stop(as.logical(TRUE), "The program has ended nicely."))
}
