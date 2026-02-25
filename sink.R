#' Sink Output to .Rout File
#'
#' @name sink
#'
#' Redirects R output to an \code{.Rout} file, with timing and auto-close.
#' Call with a filename to start sinking, call with no arguments (or
#' \code{file = NULL}) to stop.
#'
#' Implemented as a closure factory (\code{make_iaw_sink}) that tracks start
#' time and current file internally.
#'
#' @param file Character. Path to \code{.Rout} file, or \code{NULL} to close.
#' @param verbose Logical. Print timing info (default \code{FALSE}).
#' @param ... Additional arguments passed to \code{base::sink}.
#'
#' @return When starting: the file path (invisibly). When stopping: \code{NULL} (invisibly).
#'
#' @family utilities
#' @export
#'
#' @examples
#' \dontrun{
#' # Redirect output to a file, run some code, then close
#' iaw$sink("output.Rout")
#' print(summary(mtcars))
#' iaw$sink()   # closes the file
#'
#' # Use verbose = TRUE to print timing info when closing
#' iaw$sink("results.Rout", verbose = TRUE)
#' cat("processing...\n")
#' Sys.sleep(0.1)
#' iaw$sink(verbose = TRUE)   # prints elapsed time
#'
#' # Append to existing file instead of overwriting
#' iaw$sink("log.Rout", append = TRUE)
#' cat("run 2 results\n")
#' iaw$sink()
#' }
make_iaw_sink <- function(withpid = FALSE, msg = message) {

  # internal persistent variables (closure state)
  startingtime <- NULL
  current_file <- NULL

  sink_fun <- function(file = NULL, verbose = FALSE, ...) {

    sink_goodbye <- function(verbose = FALSE) {
      if (!sink.number()) {
        warning("no sink file to close")
        return(invisible(NULL))
      }

      herenow <- Sys.time()
      if (!is.null(startingtime)) {
        delta <- herenow - startingtime
        if (verbose)
          msg("[pid=", Sys.getpid(),
              ": now=", format(herenow),
              "; started=", format(startingtime),
              ". elapsed=", delta, units(delta), "]")
      } else if (verbose) {
        msg("[sink bye]")
      }

      base::sink(NULL)
      startingtime <<- NULL
      current_file <<- NULL
      invisible(NULL)
    }

    # ---------------- MAIN LOGIC ----------------
    if (!is.null(file)) {
      if (grepl("\\.R$", file))
        stop("you cannot sink to an .R file; use an .Rout file instead.")
      if (!grepl("\\.Rout$", file))
        stop("you can sink only to an .Rout file.")

      if (sink.number()) {
        warning("I am closing your previous sink file ", current_file,
                " for you first, before opening another.\n")
        sink_goodbye(verbose)
      }

      herenow <- Sys.time()
      if (withpid)
        file <- paste0(file, "-", Sys.getpid())

      base::sink(file = file, ...)
      if (verbose)
        msg("[pid=", Sys.getpid(), ": logfile=", file, ": now=", herenow, "]")

      startingtime <<- herenow
      current_file <<- file

      invisible(file)  # return path when starting

    } else {
      sink_goodbye(verbose)
    }
  }

  return(sink_fun)
}


# ---- Example use ----

iaw$sink <- make_iaw_sink(withpid = FALSE, msg = message)

# same interface:
# iaw$sink("output.Rout", verbose = TRUE)  # start sink (returns "output.Rout")
# iaw$sink(verbose = TRUE)                 # stop sink (returns NULL)
