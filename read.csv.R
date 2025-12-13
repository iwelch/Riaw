
#' Reading (Gzipped) CSV Files
#'
#' @name read.csv
#'
#' transparent and fast reading of compressed csv files.  by default, uses data.table for quicker csv reading.
#' for replicability, prints out information about the file being read.
#'
#' @usage read.csv (filename, ..., verbose = FALSE)
#'
#' @param filename -- a csv or csv.gz file
#' @param ... -- other arguments to fread
#' @param verbose=TRUE --- print information about the file for replicability.
#'
#' @note
#'     please do not use bzip2 compression.  It is *MUCH* slower for both compression and decompression.
#'     the most recent version of fread() can transparently read .csv.gz files, provided that R.utils has
#'     been installed (does not need to be loaded!).
#'
#' @return a data frame
#'
#' @aliases read.csv.gz
#'
#' @seealso write.csv, fwrite
#'
#' @export


library(data.table)

iaw$searchdir <- c(".")

iaw$strcat0 <- function(svec, sep="") paste(svec,collapse=sep)
iaw$strcat <- function(svec, sep=" ") paste(svec,collapse=sep)

iaw$read.csv <- function (filename, ..., search=NULL, allow.fst.cache= TRUE, verbose = 1) {
    (iaw$is.character(filename, 1)) %or% "read.csv expects a filename"
    (grepl("sv$", filename) | grepl("sv.gz$", filename) | grepl(".fst$", filename))  %or% "read.csv: Filename {{filename}} should end in [ct]sv or [ct]sv.gz (or fst)"

    ## on the first run, add the directory at the end
    if (!is.null(search)) {
        search <- ifelse( iaw$substrRight(search,1) == "/", search, paste0(search, "/") ) ## append a trailing '/'
        iaw$searchdir <<- unique(c(iaw$searchdir, search))
    }

    basic.filename <- filename
    if (!file.exists(filename)) {
        for (nm in paste0(iaw$searchdir, basic.filename)) {
            if (file.exists(nm)) {
                filename <- nm
                break
            }
        }
    }
    (file.exists(filename)) %or% "read.csv: sorry, but {{filename}} does not exist anywhere in {{ iaw$strcat(iaw$searchdir)}}";

    ## .fst can be a cached version of the same file in the same directory
    cachefilename <- sub(".csv.gz", ".fst", filename)

    if (grepl(".fst$", filename)) {
        library(fst)
        object <- read.fst( filename )
    } else if (((cachefilename != filename) & (file.exists( cachefilename ))) & (allow.fst.cache)) {
        message("Using cached fst file ", cachefilename, " instead.  (For md5, set verbose to 2)")
        if (file.info(cachefilename)$mtime < file.info(filename)$mtime ) stop("Please remove the older ", cachefilename, " first")
        library(fst)
        object <- read.fst( cachefilename )
        filename <- cachefilename
    } else {
        object <-  data.table::fread(filename, nThread=8, data.table=FALSE,  integer64="numeric", ...)
        ## if integer64 fails, use:
        ## coln.int64 <- names(which(sapply(df, bit64::is.integer64)))
        ## if (length(coln.int64) > 0L) df[, c(coln.int64) := lapply(.SD, as.numeric), .SDcols = coln.int64]
    }

    if (verbose) {
        cat("\n[read from", filename, ":", nrow(object), "rows, ", ncol(object), "cols]\n")
        cat("#---------------- Input File: ", filename)
        if (verbose>=2) cat(" ", md5sum(filename))
        cat("\n")
        print( file.info(filename) )
        cat("#---------------- epoch=", Sys.time(), "\n")
    }

    invisible(as.data.frame(object))
}

iaw$read.csv.gz <- iaw$read.csv  ## an alias
iaw$read.fst <- iaw$read.csv  ## an alias
iaw$fread <- iaw$read.csv  ## an alias

iaw$substrRight <- function(x, n){ substr(x, nchar(x)-n+1, nchar(x)) }

