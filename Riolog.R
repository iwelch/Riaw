#' Log to Rio.log
#'
#' @name Riolog
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
#' # Log an input file read event (only writes if Rio.log exists in working dir)
#' iaw$.Riolog("I", "data.csv")
#'
#' # Log an output file write event
#' iaw$.Riolog("O", "results.csv")
#'
#' # Log a plot output
#' iaw$.Riolog("P", "figures/returns.pdf")
#'
#' # Log a generic message for debugging
#' iaw$.Riolog("M", "Starting portfolio optimization")
#'
#' # Log entries include timestamp, PID, and script name for traceability
#' # Typical Rio.log line: 1706112000  01-run.R  I  data.csv  2024-01-24 12:00:00
#'
#' # No-op if Rio.log does not exist (safe to call unconditionally)
#' iaw$.Riolog("I", "nonexistent.csv")  # silently returns NULL
#' }

iaw$.Riolog <- function(code, msg) {
    if (!file.exists("Rio.log")) return(invisible(NULL))
    starttime <- getOption("Rscriptstarttime")
    if (is.null(starttime)) starttime <- Sys.time()
    scriptname <- getOption("Rscriptname")
    ## Strip current directory prefix if present
    wd_prefix <- paste0(getwd(), "/")
    if (!is.null(scriptname) && startsWith(scriptname, wd_prefix)) {
        scriptname <- substring(scriptname, nchar(wd_prefix) + 1)
    }
    cat(as.integer(starttime), "\t",
        scriptname, "\t",
        code, "\t",
        msg, "\t",
        format(starttime, "%Y-%m-%d %H:%M:%S"), "\n",
        sep = "", file = "Rio.log", append = TRUE)
    invisible(NULL)
}
