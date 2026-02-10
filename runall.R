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
    source(fnm)
    msgboth("Done ", fnm, "\n")
  }

  message("All Executed OK")
}
