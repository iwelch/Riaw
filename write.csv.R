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
#' @param quiet If TRUE, suppress file info message. Default FALSE.
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
#' # Basic write
#' iaw$write.csv(df, "output.csv")
#'
#' # Write compressed CSV, suppress confirmation message
#' iaw$write.csv(df, "output.csv.gz", quiet = TRUE)
#'
#' # Protect against accidental overwrites - abort if file already exists
#' iaw$write.csv(df, "results.csv", abort.on.overwrite = TRUE)
#'
#' # Pass fwrite options: use tab separator
#' iaw$write.csv(df, "output.csv", sep = "\t")
#'
#' # Write daily returns to a dated file
#' ret <- data.frame(date = 20240101:20240105, ret = rnorm(5, 0, 0.01))
#' iaw$write.csv(ret, file.path(tempdir(), "daily_returns.csv"))
#'
#' # Use the alias iaw$fwrite (same function)
#' iaw$fwrite(ret, file.path(tempdir(), "returns_copy.csv"), quiet = TRUE)
#'
#' # Prevent accidental overwrite of a production file
#' iaw$write.csv(ret, file.path(tempdir(), "prod.csv"))
#' iaw$write.csv(ret, file.path(tempdir(), "prod.csv"),
#'               allow.overwrite = FALSE)   # silently skips
#' }

iaw$write.csv <- function(object, filename, ...,
                           allow.overwrite = TRUE,
                           abort.on.overwrite = FALSE,
                           quiet = FALSE) {
    stopifnot(is.data.frame(object))
    stopifnot(is.character(filename), length(filename) == 1L)

    if (file.exists(filename)) {
        if (abort.on.overwrite) stop("File exists: ", filename)
        if (!allow.overwrite) {
            cat("[write.csv: not overwriting", filename, "]\n")
            return(invisible(object))
        }
    }

    stopifnot(grepl("\\.csv$", filename) | grepl("\\.csv\\.gz$", filename))

    # if (verbose) cat("[write.csv: saving", filename, "]\n")

    data.table::fwrite(object, file = filename, ...)
    iaw$.Riolog("O", filename)

    if (!quiet) {
        message("[wrote: ", nrow(object), " rows & ", ncol(object), " cols to ", filename, "]")
    }

    invisible(object)
}

#' @rdname write.csv
#' @export
iaw$write.csv.gz <- iaw$write.csv

#' @rdname write.csv
#' @export
iaw$fwrite <- iaw$write.csv
