#' Fast CSV/TSV File Reading
#'
#' Reads CSV, TSV, gzipped CSV/TSV, and FST files with automatic format
#' detection. Uses \code{data.table::fread()} for speed. Supports caching
#' with FST format and directory search paths.
#'
#' @param filename Path to the file. Supported extensions: \code{.csv}, \code{.tsv},
#'   \code{.csv.gz}, \code{.tsv.gz}, \code{.fst}.
#' @param ... Additional arguments passed to \code{data.table::fread()}.
#' @param search Optional directory to add to the search path.
#' @param allow.fst.cache Logical; if TRUE (default), uses cached \code{.fst} file
#'   if available and newer than the original.
#' @param verbose Integer controlling output:
#'   \itemize{
#'     \item 0: silent
#'     \item 1: print file info and dimensions (default)
#'     \item 2+: also print MD5 checksum
#'   }
#'
#' @return A data frame.
#'
#' @details
#' The function automatically:
#' \itemize{
#'   \item Searches for the file in the current directory and any directories
#'     added via the \code{search} parameter
#'   \item Uses FST cache files when available (much faster for repeated reads)
#'   \item Handles gzipped files transparently (requires R.utils package)
#'   \item Converts integer64 columns to numeric
#' }
#'
#' @note Avoid bzip2 compression as it's much slower than gzip for both
#' compression and decompression.
#'
#' @section Search Path:
#' Directories added via \code{search} are remembered across calls in
#' \code{iaw$searchdir}. Reset with \code{iaw$searchdir <- c(".")}.
#'
#' @export
#'
#' @seealso \code{\link{iaw$write.csv}}, \code{\link{fread}}, \code{\link{read.fst}}
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' df <- iaw$read.csv("data.csv")
#'
#' # Read gzipped file
#' df <- iaw$read.csv("data.csv.gz")
#'
#' # Add search directory
#' df <- iaw$read.csv("data.csv", search = "/data/project/")
#'
#' # Silent reading
#' df <- iaw$read.csv("data.csv", verbose = 0)
#'
#' # Pass options to fread
#' df <- iaw$read.csv("data.csv", select = c("id", "value"), nrows = 1000)
#' }

library(data.table)

iaw$searchdir <- c(".")

iaw$strcat0 <- function(svec, sep = "") paste(svec, collapse = sep)
iaw$strcat <- function(svec, sep = " ") paste(svec, collapse = sep)

#' @rdname read.csv
#' @export
iaw$read.csv <- function(filename, ..., search = NULL, allow.fst.cache = TRUE, verbose = 1) {
    (iaw$is.character(filename, 1)) %or% "read.csv expects a filename"
    (grepl("sv$", filename) | grepl("sv.gz$", filename) | grepl(".fst$", filename)) %or%
        "read.csv: filename {{filename}} should end in .csv, .tsv, .csv.gz, .tsv.gz, or .fst"

    # Add search directory if provided
    if (!is.null(search)) {
        search <- ifelse(iaw$substrRight(search, 1) == "/", search, paste0(search, "/"))
        iaw$searchdir <<- unique(c(iaw$searchdir, search))
    }

    # Search for file
    basic.filename <- filename
    if (!file.exists(filename)) {
        for (nm in paste0(iaw$searchdir, basic.filename)) {
            if (file.exists(nm)) {
                filename <- nm
                break
            }
        }
    }
    (file.exists(filename)) %or%
        "read.csv: {{filename}} not found in {{ iaw$strcat(iaw$searchdir) }}"

    # Check for FST cache
    cachefilename <- sub(".csv.gz", ".fst", filename)

    if (grepl(".fst$", filename)) {
        library(fst)
        object <- read.fst(filename)
    } else if (cachefilename != filename && file.exists(cachefilename) && allow.fst.cache) {
        if (file.info(cachefilename)$mtime < file.info(filename)$mtime) {
            stop("FST cache ", cachefilename, " is older than source. Please remove it.")
        }
        message("Using cached FST file: ", cachefilename)
        library(fst)
        object <- read.fst(cachefilename)
        filename <- cachefilename
    } else {
        object <- data.table::fread(filename, nThread = 8, data.table = FALSE,
                                     integer64 = "numeric", ...)
    }

    if (verbose) {
        cat("\n[read from", filename, ":", nrow(object), "rows,", ncol(object), "cols]\n")
        cat("#---------------- Input File:", filename)
        if (verbose >= 2) cat(" ", tools::md5sum(filename))
        cat("\n")
        print(file.info(filename))
        cat("#---------------- epoch=", as.character(Sys.time()), "\n")
    }

    invisible(as.data.frame(object))
}

#' @rdname read.csv
#' @export
iaw$read.csv.gz <- iaw$read.csv

#' @rdname read.csv
#' @export
iaw$read.fst <- iaw$read.csv

#' @rdname read.csv
#' @export
iaw$fread <- iaw$read.csv

iaw$substrRight <- function(x, n) {
    substr(x, nchar(x) - n + 1, nchar(x))
}
