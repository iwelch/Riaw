#' Run All Numbered Scripts in Working Directory
#'
#' @name runall
#'
#' Sources all R and Perl scripts matching \code{[0-9]*.R} and \code{[0-9]*.pl}
#' in the current directory, ordered by numeric prefix. Aborts on first error.
#'
#' @return Called for side effect. Prints status messages.
#'
#' @examples
#' \dontrun{
#' # In a project directory with scripts 01-load.R, 02-clean.R, 03-model.R:
#' setwd("/path/to/project")
#' iaw$runall()
#' # Sources 01-load.R, then 02-clean.R, then 03-model.R in order.
#' # Stops immediately if any script throws an error.
#'
#' # Mixed R and Perl scripts are also supported (Perl emits a warning)
#' # 01-download.pl, 02-parse.R -> runs perl 01-download.pl, then sources 02-parse.R
#'
#' # Typical project layout:
#' #   01-download.R  -- fetch raw data
#' #   02-clean.R     -- merge and filter
#' #   03-analyze.R   -- regressions
#' #   04-tables.R    -- output LaTeX tables
#' # iaw$runall() executes them sequentially, aborting on first error.
#'
#' # Scripts without a leading digit are ignored (e.g., helper.R, utils.R)
#' # Only files matching [0-9]*.R or [0-9]*.pl are sourced.
#' }
#'
#' @family utilities
#' @export

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
