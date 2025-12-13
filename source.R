
#' SOURCE
#'
#' @name source
#'
#' replaces the original source but adds the current source file name as an global attr to source.  Ergo, it keeps track
#' of the source call sequences.  Note---this is *not* the call stack.
#'
#' further, this source also recognizes Rmd files
#'
#' @usage source (filename, ...)
#'
#' @param Rsourcefilename
#'
#' @return
#'

## iaw$source <- function (Rsourcefilename, ...) {
##     if (grepl("\\.Rinclude", Rsourcefilename)) return(base:::source(Rsourcefilename))
##     (grepl("\\.R$",Rsourcefilename)) %or% "can only iaw$source file .R files [or .Rinclude] not {{Rsourcefilename}} with such ## ## ## ## ## an extension"
##     (is.null(iaw$ARGV0)) %or% "cannot nest iaw$source.  please use base:::source() instead."
## 
##     {
##         message("[Sourcing Working R file ", Rsourcefilename, "]")
##         iaw$msg.firstcall <- NULL
##         iaw$ARGV0 <<- Rsourcefilename
##         base:::source(Rsourcefilename, ...)
##         iaw$ARGV0 <<- NULL
##     }
## }

################################################################

## automatically log last invokation to an .Rout file --- NOTE: NO IAW PREFIX
source <- function(file, verbose = FALSE, ...) {
    iaw$ARGV0 <- file

    iaw$sink(paste0(file, "out"), split = TRUE, verbose)

    if (Rscriptname == "Rscriptname unknown")  Rscriptname <<- paste(getwd(), file)

    on.exit({
        iaw$ARGV0 <- NULL
        iaw$sink(NULL, verbose)
        if (verbose)
            iaw$msg("[automatic source sinking into ", file, "out was closed]")
    }, add = TRUE)

    base::source(file, keep.source = TRUE, ...)
}




