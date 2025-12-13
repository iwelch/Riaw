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
