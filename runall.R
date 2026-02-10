#' Runall R project files, in numerical start order
#'
#' @name runall
#'

iaw$runall <- function() {
  message( getwd() )
  filenames <- Sys.glob("[0-9]*.R")
  message( paste(filenames) )
  for (fnm in filenames) {
    msgboth("Running ", fnm)
    result <- tryCatch({
      base::source(fnm)
      NULL
    }, error = function(e) e)
    if (inherits(result, "error")) {
      message("\n*** FAILED: ", fnm, " ***")
      message("Error: ", conditionMessage(result))
      stop("Aborting runall due to error in ", fnm, call. = FALSE)
    }
    msgboth("Done ", fnm, "\n")
  }

  message("All Executed OK")
}
