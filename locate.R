## #7 locate.R

#' Locate a File
#'
#' @name locate
#'
#' Search for file locally, one directory up, one down, then system-wide.
#'
#' @param global.filename Filename to find.
#' @param restrict.pattern Optional regex the path must match.
#' @param strict If TRUE, error when multiple matches. Default TRUE.
#' @param omit.backup Exclude /backup/ and /old/ paths. Default TRUE.
#'
#' @return Full pathname, or NULL if not found.
#'
#' @examples
#' \dontrun{
#' # Find a data file anywhere on disk (searches locally first, then system-wide)
#' iaw$locate("mydata.csv")
#'
#' # Restrict matches to a particular project directory
#' iaw$locate("config.R", restrict.pattern = "/myproject/")
#'
#' # Allow multiple matches (returns most recently modified)
#' iaw$locate("README.md", strict = FALSE)
#'
#' # Find a configuration file scoped to a project
#' iaw$locate("settings.json", restrict.pattern = "/analytics/")
#'
#' # Include backup directories in search results
#' iaw$locate("archive.csv", omit.backup = FALSE)
#' }
#'
#' @family io
#' @export

iaw$locate <- function(global.filename, restrict.pattern = NULL,
                       strict = TRUE, omit.backup = TRUE) {
  ## local trumps
  if (length(Sys.glob(global.filename)) == 1)
    return(global.filename)

  ## one up
  up1 <- paste0("../", global.filename)
  if (length(Sys.glob(up1)) == 1)
    return(up1)

  ## one down
  down1 <- Sys.glob(paste0("*/", global.filename))
  if (length(down1) == 1)
    return(down1)

  ## system-wide search
  locate.cmd <- if (Sys.info()["sysname"] == "Darwin") {
    paste("mdfind -name", shQuote(global.filename))
  } else {
    paste("locate -b -l 20 -r", shQuote(global.filename))
  }

  fitting <- suppressWarnings(system(locate.cmd, intern = TRUE))

  if (length(fitting) == 0) {
    warning("locate could not find ", global.filename,
            " -- do you need to run updatedb?")
    return(NULL)
  }

  ## filter out backups
  if (omit.backup) {
    fitting <- fitting[!grepl("/backup/|/old/", fitting)]
    if (length(fitting) == 0) {
      warning("only backup/old copies found for ", global.filename)
      return(NULL)
    }
  }

  ## apply restriction pattern
  if (!is.null(restrict.pattern))
    fitting <- fitting[grepl(restrict.pattern, fitting)]

  if (length(fitting) == 0) {
    warning("no files match restrict.pattern")
    return(NULL)
  }

  ## strict mode
  if (strict && length(fitting) > 1)
    stop(sprintf("%d files match '%s': %s",
                 length(fitting), global.filename,
                 paste(head(fitting, 5), collapse = ", ")))

  ## return most recently modified
  fitting[order(file.info(fitting)$mtime, decreasing = TRUE)][1]
}
