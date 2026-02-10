#' Runall R project files, in numerical start order
#'
#' @name runall
#'

iaw$runall <- function() {
  message( "iaw$runall: ", getwd() )
  rfiles <- Sys.glob("[0-9]*.R")
  plfiles <- Sys.glob("[0-9]*.pl")

  if (length(plfiles) > 0) {
    warning("Some scripts are external (Perl): ", paste(plfiles, collapse = ", "))
  }

  ## Combine and sort by numeric prefix
  allfiles <- c(rfiles, plfiles)
  allfiles <- allfiles[order(as.numeric(sub("^([0-9]+).*", "\\1", allfiles)))]

  message( "iaw$runall: executing ", paste(allfiles, collapse = " ") )

  for (fnm in allfiles) {
    msgboth("Running ", fnm)

    if (grepl("\\.pl$", fnm)) {
      ## Perl script
      result <- tryCatch({
        retcode <- system(paste("perl", shQuote(fnm)))
        if (retcode != 0) stop("Perl script exited with code ", retcode)
        NULL
      }, error = function(e) e)
    } else {
      ## R script
      result <- tryCatch({
        base::source(fnm)
        NULL
      }, error = function(e) e)
    }

    if (inherits(result, "error")) {
      message("\n*** FAILED: ", fnm, " ***")
      message("Error: ", conditionMessage(result))
      stop("Aborting runall due to error in ", fnm, call. = FALSE)
    }
    msgboth("Done ", fnm, "\n")
  }

  message("All Executed OK")
}
