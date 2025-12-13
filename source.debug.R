
#' SOURCE.DEBUG
#'
#' @name source.debug
#'
#'   a debugging helper that prints the call stack on error.
#'
#' @usage source.debug (sourcefilename)
#'
#' @param sourcefilename the function to run
#'
#' @return
#'

iaw$source.debug <- function (sourcefilename)
  tryCatch({
    base::source(sourcefilename)
  }, finally = {
      iaw$sink(stderr())
      traceback()
      iaw$sink()
  })
