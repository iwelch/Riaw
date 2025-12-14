#' Fast CSV File Writing
#'
#' @name write.csv
#'
#' Writes CSV files using data.table::fwrite for speed.
#'
#' @param object Data frame to write.
#' @param filename Output file path.
#' @param ... Arguments passed to fwrite.
#' @param allow.overwrite Allow overwriting existing files.
#' @param abort.on.overwrite Abort if file exists.
#' @param verbose Print file info.
#' @param use.data.table Retained for compatibility.
#' @param gzip.in.background Retained for compatibility.
#'
#' @return Invisibly returns input object.
#'
#' @family io
#' @export
#'
#' @importFrom data.table fwrite
#'
#' @examples
#' \dontrun{
#' iaw$write.csv(df, "output.csv")
#' }

iaw$write.csv <- function(object, filename, ...,
                           allow.overwrite = TRUE,
                           abort.on.overwrite = FALSE,
                           verbose = TRUE,
                           use.data.table = TRUE,
                           gzip.in.background = FALSE) {
    stopifnot(is.data.frame(object))
    stopifnot(is.character(filename), length(filename) == 1L)
    
    if (file.exists(filename)) {
        if (abort.on.overwrite) stop("File exists: ", filename)
        if (!allow.overwrite) {
            cat("[write.csv: not overwriting", filename, "]\n")
            return(invisible(object))
        }
    }
    
    stopifnot(grepl(".csv$", filename) | grepl("sv.gz$", filename))
    
    if (verbose) cat("[write.csv: saving", filename, "]\n")
    
    data.table::fwrite(object, file = filename, ...)
    
    if (verbose) {
        cat("[wrote:", nrow(object), "rows,", ncol(object), "cols to", filename, "]\n")
    }
    
    invisible(object)
}

iaw$write.csv.gz <- iaw$write.csv
iaw$fwrite <- iaw$write.csv
