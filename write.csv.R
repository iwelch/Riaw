#' Fast CSV/TSV File Writing
#'
#' Writes data frames to CSV or gzipped CSV files using \code{data.table::fwrite()}
#' for speed. Supports automatic gzip compression and verbose logging.
#'
#' @param object Data frame to write.
#' @param filename Output file path. Should end in \code{.csv} or \code{.csv.gz}.
#' @param ... Additional arguments passed to \code{data.table::fwrite()}.
#' @param allow.overwrite Logical; if FALSE, skips writing if file exists.
#'   Default is TRUE.
#' @param abort.on.overwrite Logical; if TRUE, aborts if file exists. Default FALSE.
#' @param verbose Logical; if TRUE, prints file info. Default TRUE.
#' @param use.data.table Logical; retained for compatibility. Always uses fwrite.
#' @param gzip.in.background Logical; retained for compatibility.
#'
#' @return Invisibly returns the input object.
#'
#' @details
#' Modern versions of \code{data.table::fwrite()} support gzip compression
#' natively when the filename ends in \code{.gz}.
#'
#' @export
#'
#' @seealso \code{\link{iaw$read.csv}}, \code{\link{fwrite}}
#'
#' @examples
#' \dontrun{
#' df <- data.frame(x = 1:1000, y = rnorm(1000))
#'
#' # Write CSV
#' iaw$write.csv(df, "output.csv")
#'
#' # Write gzipped CSV
#' iaw$write.csv(df, "output.csv.gz")
#'
#' # Silent writing
#' iaw$write.csv(df, "output.csv", verbose = FALSE)
#'
#' # Prevent overwriting
#' iaw$write.csv(df, "output.csv", allow.overwrite = FALSE)
#' }

iaw$write.csv <- function(object, filename, ...,
                           allow.overwrite = TRUE,
                           abort.on.overwrite = FALSE,
                           verbose = TRUE,
                           use.data.table = TRUE,
                           gzip.in.background = FALSE) {

    if (file.exists(filename)) {
        (abort.on.overwrite) %and% "write.csv: {{filename}} exists and abort.on.overwrite=TRUE"
        if (!allow.overwrite) {
            cat("[write.csv: not overwriting existing file", filename, "]\n")
            return(invisible(object))
        }
    }

    iaw$is.character(filename, 1) %or% "filename must be a single string"

    is.gz <- grepl("sv.gz$", filename)
    (grepl(".csv$", filename) | is.gz) %or% "filename {{filename}} should end in .csv or .csv.gz"

    if (verbose) {
        cat("[write.csv: saving", filename, "via fwrite]\n", file = stderr())
    }

    data.table::fwrite(object, file = filename, ...)

    if (verbose) {
        cat("[wrote data frame:", nrow(object), "rows,", ncol(object), "cols to", filename, "]\n",
            file = stderr())
        cat("#---------------- Output File:", filename, "\n")
        print(file.info(filename))
        cat("#----------------", as.character(Sys.time()), ", dim=", paste(dim(object), collapse = "x"), "\n")
    }

    invisible(object)
}

#' @rdname write.csv
#' @export
iaw$write.csv.gz <- iaw$write.csv

#' @rdname write.csv
#' @export
iaw$fwrite <- iaw$write.csv
