
#' CACHE
#'
#' @name cache
#'
#' check a number of input files for changes, and recreate an Rdata file if required
#'
#' @usage cache("store.Rdata", c("in1.csv", "in2.csv.gz"), function( infiles ) { merge( read.csv(infiles[1]), read.csv.gz(infiles[2]) ) }
#'   the return value is a data frame (or list).  this is the same data structure that the function.recreation produces, usually a data.frame.
#'
#' @param Rdata.filename where the data should be stored
#' @param input.filenames list of filenames on which the Rdata file depends
#' @param function.recreation a regenerator of the Rdata file
#'
#' @return the contents of the Rdata filename
#'
#' @examples
#' 	a <- data.frame( x=c(1,2,3), y=c(1,3,5) ); write.csv(a, file="a.csv")
#' 	b <- data.frame( x=c(4,5,6), y=c(-1,-3,-5) ); write.csv(b, file="b.csv")
#' 	rr <- function( x ) rbind(read.csv(x[1]), read.csv(x[2]))
#'
#' 	cat("First read --- this time should be recreated \n")
#' 	print(cache( "test.Rdata", c("a.csv", "b.csv"), rr ))
#' 	cat("Second read --- this time should be from cache \n")
#' 	print(cache( "test.Rdata", c("a.csv", "b.csv"), rr ))
#'


iaw$cache <- function( Rdata.filename, input.filenames, function.recreation =NULL, verbose=TRUE ) {
    stop("use memoise package")
  (is.character(Rdata.filename)) %or% "Rdata.filename must be a character (filename name)"
  (is.character(input.filenames)) %or% "input filenames must be a character (filename name)"
  (grepl("\\.Rdata$", Rdata.filename)) %or% "Rdata.filename must end with Rdata, not {{Rdata.filename}}\n"
  if (is.null(function.recreation)) {
    if (length(input.filenames)==1) {
      if (grepl("\\.csv\\.(g|b)z2$", input.filenames)) function.recreation <- iaw$read.csv.any
      if (grepl("\\.csv\\.(g|b)z$", input.filenames)) function.recreation <- iaw$read.csv.any
      if (grepl("\\.csv", input.filenames)) function.recreation <- read.csv
    } else { stop("If you do not give a function.recreation, you must specify a single csv filename") }
  }
  (class(function.recreation) == "function") %or% "function.recreation in cache needs to be a function"


  ## we allow empty files that are later.  they make it possible not to recreate
  globbed.infilenames <- Sys.glob(input.filenames)
  globbed.infilenames.nonempty <- globbed.infilenames[ file.info(globbed.infilenames)$size > 0 ]

  if (file.exists(Rdata.filename) & all( (file.info(Rdata.filename)$mtime > file.info(globbed.infilenames.nonempty)$mtime ) )){
    (verbose) %and% cat("[cache.R: Reloading cached data structures from '", Rdata.filename, "'...", sep="")
      name.of.new.ds <- load(Rdata.filename)
    (verbose) %and% cat(" --> read", name.of.new.ds, "done]\n")
    return(environment()[[name.of.new.ds]])
  }

  (verbose) %and% cat("[cache.R: Rebuilding cached data frame for", Rdata.filename, "]\n")

  ## yikes --- we have to rebuild the Rdata filename
  saved.cached.datalist <- function.recreation( input.filenames )

  (is.list(saved.cached.datalist)) %or% "for safety, your recreation function needs to create a list or data frame, not {{class(saveme)}}"
  save( saved.cached.datalist , file= Rdata.filename )
  saved.cached.datalist
}
