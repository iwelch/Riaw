#' Toggle Console Output Verbosity
#'
#' Controls whether console messages and output are printed ("verbose") or
#' temporarily suppressed ("quiet"). Useful for silencing noisy package
#' loading or function output.
#'
#' @return A function with signature \code{chatter(quiet.verbose)} that:
#'   \itemize{
#'     \item When called with "quiet": suppresses output
#'     \item When called with "verbose": restores output
#'     \item When called with no arguments: returns current state
#'   }
#'
#' @param quiet.verbose One of:
#'   \itemize{
#'     \item "quiet": Suppress console output
#'     \item "verbose": Restore normal output
#'     \item missing: Query current state
#'   }
#'
#' @details
#' In "quiet" mode, both stdout and message streams are redirected to
#' \code{/tmp/R-suppressed.Rout}. When switched back to "verbose",
#' normal console output resumes.
#'
#' @export
#'
#' @seealso \code{\link{sink}}, \code{\link{suppressMessages}}
#'
#' @examples
#' \dontrun{
#' # Suppress output during package loading
#' iaw$chatter("quiet")
#' library(ggplot2)  # Silent loading
#' iaw$chatter("verbose")
#'
#' # Check current state
#' iaw$chatter()  # Returns "verbose" or "quiet"
#' }

make_chatter <- function() {
    toggle_state <- "verbose"
    filecon <- NULL

    chatter_fun <- function(quiet.verbose) {
        if (missing(quiet.verbose))
            return(toggle_state)

        if (toggle_state == "")
            return(invisible(NULL))

        stopifnot(quiet.verbose %in% c("quiet", "verbose", ""))

        if (quiet.verbose == "quiet") {
            if (toggle_state != "verbose")
                message("warning: chatter was not verbose")

            toggle_state <<- "quiet"
            filecon <<- file("/tmp/R-suppressed.Rout", "w")

            message("[suppressing chatter (quiet)]")
            sink(file = filecon, type = "message")
            sink(file = filecon, type = "output")

        } else if (quiet.verbose == "verbose") {
            if (toggle_state != "quiet")
                message("warning: chatter was not quiet")

            toggle_state <<- "verbose"
            sink(file = NULL, type = "output")
            sink(file = NULL, type = "message")

            if (!is.null(filecon) && isOpen(filecon)) {
                close(filecon)
                filecon <<- NULL
            }

            message("[chatter restored (verbose)]")
        }

        invisible(toggle_state)
    }

    chatter_fun
}

#' @export
iaw$chatter <- make_chatter()
