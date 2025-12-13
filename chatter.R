#' Toggle Console Output Verbosity ("Chatter")
#'
#' Creates a closure-based function that controls whether console messages and
#' output are printed (`"verbose"`) or temporarily suppressed (`"quiet"`).
#'
#' When in `"quiet"` mode, both standard output and message streams are redirected
#' to a temporary file (`/tmp/R-suppressed.Rout`), allowing silent loading of
#' packages or functions that normally print a lot of information.
#' When switched back to `"verbose"`, both sinks are closed, the connection is
#' released, and normal console output resumes.
#'
#' If called with no arguments, the current chatter state is returned invisibly
#' (`"quiet"` or `"verbose"`), allowing you to query the mode.
#'
#' @return
#' A function that toggles or queries the current verbosity state.
#' Each call returns (invisibly) the active state.
#'
#' @param quiet.verbose Character scalar; must be one of:
#'   \describe{
#'     \item{"quiet"}{Suppress console chatter by redirecting output and messages.}
#'     \item{"verbose"}{Re-enable normal console output and close the sink.}
#'     \item{""}{No effect (used internally).}
#'     \item{missing}{If no argument is provided, returns the current state.}
#'   }
#'
#' @details
#' Internally, two closure variables persist between calls:
#' \itemize{
#'   \item `toggle_state` — the current verbosity mode.
#'   \item `filecon` — the connection used for temporary output suppression.
#' }
#' No global variables are created or modified.
#'
#' @examples
#' \dontrun{
#' # Attach to existing iaw environment or list
#' iaw$chatter <- make_chatter()
#'
#' iaw$chatter("quiet")     # Suppress output
#' iaw$chatter("verbose")   # Restore normal output
#'
#' iaw$chatter()            # Query current mode (returns "verbose" or "quiet")
#' }
#'
#' @seealso [sink()], [message()]
#'
#' @export
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
