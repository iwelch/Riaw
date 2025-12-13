
#' Writing (Gzipped) CSV Files
#'
#' @name write.csv
#'
#' transparent and fast writing of compressed csv files.  by default, uses data.table for quicker csv reading.
#' for replicability, prints out information about the file being written.
#'
#' @usage write.csv (filename, ..., verbose = FALSE)
#'
#' @param filename -- a csv or csv.gz file
#' @param ... -- other arguments to fwrite
#' @param verbose=TRUE --- print information about the file for replicability.
#'
#' @aliases write.csv.gz
#'
#' @seealso read.csv.gz, fread
#'
#' @export

iaw$write.csv <- function(object, filename, ..., allow.overwrite=TRUE, abort.on.overwrite=FALSE, verbose = TRUE, use.data.table=TRUE, gzip.in.background=FALSE) {
    ## update: fwrite now supports gzip directly

    if (file.exists(filename)) {
        (abort.on.overwrite) %and% "write.csv: filename {{filename}} exists AND abort.on.overwrite requested";
        if (!allow.overwrite) {
            cat("[write.csv --- not overwriting because file ", filename, " already exists]")
            return(object)
        }
    }

    iaw$is.character(filename,1) %or% "write.csv expects a filename"

    is.gz <- grepl("sv.gz$", filename)
    (grepl(".csv$", filename) | is.gz) %or% "Filename {{filename}} should contain .csv or .csv.gz"

    ## plaincsv <- sub('\\.gz$','', filename)

    if (verbose) cat("[write.csv: saving", filename, " via fwrite]\n", file=stderr())

    data.table::fwrite(object, file=filename, ...)  ## no longer plaincsv, because data.table::fwrite now supports .gz natively

    if ((verbose)) {
        ## md5sum.unzipped <- (if (is.gz) NA else md5sum(plaincsv))
        ## md5sum.zipped <- (if (is.gz) NA else md5sum(plaincsv))
        md5sum.zipped <- md5sum.unzipped <- NA

        cat("[wrote data frame:", nrow(object), "rows, ", ncol(object), "cols to", filename, "]\n", file=stderr())
        cat("#---------------- Output File: ", filename, " unzipped=", md5sum.unzipped, "\n")
        print( file.info(filename) )
        cat("#---------------- ", Sys.time(), " zipped=", md5sum.zipped, ", dim=", paste(dim(object)), "\n")
    }

    invisible(object)
}

iaw$write.csv.gz <- iaw$write.csv  ## an alias
iaw$fwrite <- iaw$write.csv  ## an alias
