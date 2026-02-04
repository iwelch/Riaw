#' Log to Rio.log
#'
#' @name .Riolog
#'
#' Writes a line to Rio.log if it exists.
#'
#' @param code Single character code (e.g., "I" for input, "O" for output, "P" for plot).
#' @param msg Message to log (typically a filename).
#'
#' @return NULL invisibly.
#'
#' @family io
#' @export
#'
#' @examples
#' \dontrun{
#' iaw$.Riolog("I", "data.csv")
#' }

iaw$.Riolog <- function(code, msg) {
    if (!file.exists("Rio.log")) return(invisible(NULL))
    now <- Sys.time()
    cat(as.integer(now), "\t",
        getOption("Rscriptname"), "\t",
        code, "\t",
        msg, "\t",
        format(now, "%Y-%m-%d %H:%M:%S"), "\n",
        sep = "", file = "Rio.log", append = TRUE)
    invisible(NULL)
}
