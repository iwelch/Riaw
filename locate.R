
#' LOCATE A FILE
#'
#' @name read.csv( iaw$locate( "somewhere.csv.gz" ) )
#'
#' first look for a file in the local directory.  if it is not there, then look in the directory one up.  if it is not there, either, then search for a unique file elsewhere in the filesystem.  otherwise, complain.
#'
#' @usage locate (global.filename, restrict.pattern = NULL, strict = TRUE, omit.backup = TRUE)
#'
#' @param global.filename
#' @param restrict.pattern optional required part in the pathname
#' @param strict do not pick an arbitrary file if more than one filename matches in a search
#' @param omit.backup do not consider files ending with ~
#'
#' @return the full pathname
#'
#' @examples
#'        iaw$read.csv( iaw$locate( "somewhere.csv.gz" ) )
#'


iaw$locate <- function (global.filename, restrict.pattern = NULL, strict = TRUE, omit.backup = TRUE) {
  if (length(Sys.glob(global.filename))==1) return(global.filename);  ## local always trumps!
  if (length(Sys.glob(paste0("../",global.filename)))==1) return(paste0("../",global.filename))  ## up above one always trumps!
  if (length(fnms <- Sys.glob(paste0("*/",global.filename)))==1) return(fnms)  ## below one always trumps!

  ## now anywhere else in the file system
  locate.command <- if (options("uname") == "Darwin") "mdfind -name" else "locate -b -l10 -r"
  fitting.files <- suppressWarnings(system(paste0(locate.command, " ", global.filename), intern=TRUE))
  ## PS: it would be better if we disallowed file fragment names

  if (length(fitting.files)==0) {
    warning(paste("locate could not find", global.filename, " --- do you need to run updatedb or /usr/libexec/locate.updatedb?"))
    return(NULL)
  }

  if (omit.backup) {
    fitting.files <- fitting.files[!grepl("/backup/", fitting.files)]
    fitting.files <- fitting.files[!grepl("/old/", fitting.files)]
    (length(fitting.files)>0) %or% "you have backup and old copies, but no useful copies";
  }
  if (!is.null(restrict.pattern)) fitting.files <- fitting.files[grepl(restrict.pattern, fitting.files)]

  if (strict) {
      (length(fitting.files)==1) %or% "Sorry, but you have {{length(fitting.files)}} != 1 choices for {{global.filename}} in {{fitting.files}}"
  }

  ## we have multiple files, so we have to sort them by time now

  (fitting.files[order(file.info(fitting.files)$mtime)])[1]
}
