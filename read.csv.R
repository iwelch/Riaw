#' Fast CSV File Reading
#'
#' @name read.csv
#'
#' Reads CSV files using data.table::fread for speed.
#'
#' @param filename Path to CSV or CSV.GZ file.
#' @param ... Arguments passed to fread.
#' @param search Directory to add to search path.
#' @param allow.fst.cache Use FST cache if available.
#' @param verbose Print file info.
#'
#' @return Data frame.
#'
#' @family io
#' @export
#'
#' @importFrom data.table fread
#'
#' @examples
#' \dontrun{
#' df <- iaw$read.csv("data.csv")
#' }

iaw$searchdir <- c(".")

iaw$read.csv <- function(filename, ..., search = NULL,
                          allow.fst.cache = TRUE, verbose = FALSE) {
    stopifnot(is.character(filename), length(filename) == 1L)
    stopifnot(grepl("sv$", filename) | grepl("sv.gz$", filename) | grepl(".fst$", filename))

    if (!is.null(search)) {
        search <- ifelse(substr(search, nchar(search), nchar(search)) == "/",
                         search, paste0(search, "/"))
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
    stopifnot(file.exists(filename))

    cachefilename <- sub(".csv.gz", ".fst", filename)

    if (grepl(".fst$", filename)) {
        if (!requireNamespace("fst", quietly = TRUE)) stop("Package 'fst' required")
        object <- fst::read.fst(filename)
    } else if ((cachefilename != filename) && file.exists(cachefilename) && allow.fst.cache) {
        if (file.info(cachefilename)$mtime < file.info(filename)$mtime) {
            stop("FST cache older than source")
        }
        if (!requireNamespace("fst", quietly = TRUE)) stop("Package 'fst' required")
        object <- fst::read.fst(cachefilename)
        filename <- cachefilename
    } else {
        object <- data.table::fread(filename, nThread = 8, data.table = FALSE, integer64 = "numeric", ...)
    }

    if (file.exists("Rio.log")) cat("[I]\t", filename, "\t->\t", getOption(Rscriptname), "\t", Sys.time(), "\n", file= "Rio.log", append=TRUE)

    if (verbose) {
        cat("\n[read from", filename, ":", nrow(object), "rows,", ncol(object), "cols]\n")
    } else {
        message("\n[read from ", filename, ": ", nrow(object), " rows, ", ncol(object), " cols]\n")
    }

    invisible(as.data.frame(object))
}

iaw$read.csv.gz <- iaw$read.csv
iaw$read.fst <- iaw$read.csv
iaw$fread <- iaw$read.csv
