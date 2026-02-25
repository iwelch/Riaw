#' Toggle Console Output Verbosity
#'
#' @name chatter
#'
#' Controls whether console messages and output are printed (\code{"verbose"})
#' or suppressed (\code{"quiet"}). In quiet mode, both output and message
#' streams are redirected to \code{/tmp/R-suppressed.Rout}.
#'
#' Implemented as a closure factory (\code{make_chatter}). Call with no
#' arguments to query the current state.
#'
#' @param quiet.verbose Character scalar: \code{"quiet"}, \code{"verbose"},
#'   \code{""} (no-op), or missing (query mode).
#'
#' @return The current state (\code{"quiet"} or \code{"verbose"}).
#'
#' @family utilities
#' @export
#'
#' @examples
#' # Query current verbosity mode (no argument)
#' \dontrun{
#' iaw$chatter()
#' }
#'
#' # Suppress noisy library loading, then restore
#' \dontrun{
#' iaw$chatter("quiet")
#' library(data.table)   # startup messages go to file, not console
#' library(lfe)
#' iaw$chatter("verbose")
#' }
#'
#' # Typical pattern: wrap a block of code that produces output
#' \dontrun{
#' iaw$chatter("quiet")
#' source("setup_heavy_libs.R")
#' iaw$chatter("verbose")
#' iaw$msg("all libraries loaded")
#' }
#'
#' # Silence output during data loading in a backtest script
#' \dontrun{
#' iaw$chatter("quiet")
#' prices <- read.csv("prices.csv")
#' factors <- read.csv("factors.csv")
#' iaw$chatter("verbose")
#' }
#'
#' # Check current state before toggling
#' \dontrun{
#' state <- iaw$chatter()         # returns "quiet" or "verbose"
#' if (state == "verbose") iaw$chatter("quiet")
#' }
#'
#' @seealso [sink()], [message()]

make_chatter <- function() {

  # internal persistent variables
  toggle_state <- "verbose"
  filecon <- NULL

  chatter_fun <- function(quiet.verbose) {

    # allow query mode
    if (missing(quiet.verbose))
      return(toggle_state)

    if (toggle_state == "")
      return(0)  # no action if disabled

    stopifnot(quiet.verbose %in% c("quiet", "verbose", ""))

    if (quiet.verbose == "quiet") {
      if (toggle_state != "verbose")
        message("warning --- chatter was not verbose")

      toggle_state <<- "quiet"
      filecon <<- file("/tmp/R-suppressed.Rout", "w")

      message("[suppressing chatter now (quiet) for loading of standard R libraries]")
      sink(file = filecon, type = "message")
      sink(file = filecon, type = "output")

    } else if (quiet.verbose == "verbose") {
      if (toggle_state != "quiet")
        message("warning -- chatter was not quiet")

      toggle_state <<- "verbose"
      sink(file = NULL, type = "output")
      sink(file = NULL, type = "message")

      if (!is.null(filecon) && isOpen(filecon)) {
        close(filecon)
        filecon <<- NULL
      }

      message("[chatter turned back on (verbose) for iaw progress]\n")
    }

      toggle_state
  }

  return(chatter_fun)
}

iaw$chatter <- make_chatter()
