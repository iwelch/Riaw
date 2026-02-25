#' Fast CSV File Reading
#'
#' @name read.csv
#'
#' Reads CSV files using data.table::fread for speed.
#'
#' @param filename Path to CSV, CSV.GZ, or FST file.
#' @param ... Arguments passed to \code{data.table::fread}.
#' @param select Character vector of column names to read, or \code{NULL} for all (default).
#' @param search Directory to add to search path.
#' @param allow.fst.cache Logical. Use FST cache if available (default \code{TRUE}).
#' @param quiet Logical. If \code{TRUE}, suppress file info message (default \code{FALSE}).
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
#' # Basic CSV read
#' df <- iaw$read.csv("data.csv")
#'
#' # Read only selected columns (fread-style column selection)
#' df <- iaw$read.csv("data.csv", select = c("date", "price", "volume"))
#'
#' # Read compressed file, suppress progress message
#' df <- iaw$read.csv("data.csv.gz", quiet = TRUE)
#'
#' # Add a search directory so relative filenames resolve automatically
#' df <- iaw$read.csv("prices.csv", search = "~/data/")
#'
#' # Read a gzipped CSV directly (auto-detected by extension)
#' df <- iaw$read.csv("monthly_returns.csv.gz")
#'
#' # Read an FST file for columnar speed
#' df <- iaw$read.csv("panel.fst", select = c("permno", "date", "ret"))
#'
#' # Override FST cache to force reading from original CSV
#' df <- iaw$read.csv("data.csv.gz", allow.fst.cache = FALSE)
#' }

iaw$searchdir <- c(".")

iaw$read.csv <- function(filename, ..., select = NULL, search = NULL,
                          allow.fst.cache = TRUE, quiet = FALSE) {
    stopifnot(is.character(filename), length(filename) == 1L)
    stopifnot(grepl("sv$", filename) | grepl("sv.gz$", filename) | grepl(".fst$", filename))

    if (!is.null(search)) {
        search <- ifelse(substr(search, nchar(search), nchar(search)) == "/",
                         search, paste0(search, "/"))
        assign("searchdir", unique(c(iaw$searchdir, search)), envir = iaw)
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
        object <- fst::read.fst(filename, columns = select)
    } else if ((cachefilename != filename) && file.exists(cachefilename) && allow.fst.cache) {
        if (file.info(cachefilename)$mtime < file.info(filename)$mtime) {
            stop("FST cache older than source")
        }
        if (!requireNamespace("fst", quietly = TRUE)) stop("Package 'fst' required")
        object <- fst::read.fst(cachefilename, columns = select)
        filename <- cachefilename
    } else {
        object <- data.table::fread(filename, nThread = getOption("mc.cores", data.table::getDTthreads()), data.table = FALSE, integer64 = "numeric", select = select, ...)
    }

    iaw$.Riolog("I", filename)

    if (!quiet) {
        message("[read from ", filename, ": ", nrow(object), " rows, ", ncol(object), " cols]")
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
