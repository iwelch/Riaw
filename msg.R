#' Enhanced Message Printer with Timing
#'
#' @name msg
#'
#' Prints timestamped messages with elapsed time since first call and
#' the name of the calling function. Implemented as a closure factory
#' (\code{make_msg}).
#'
#' @param ... Message components (passed to \code{cat}).
#' @param len.of.funname Integer. Width for caller name formatting (default 12).
#'
#' @return \code{TRUE} (invisibly).
#'
#' @family utilities
#' @export
#'
#' @examples
#' # Print a timestamped progress message
#' \dontrun{
#' iaw$msg("Starting process")
#' }
#'
#' # Multiple messages show elapsed time since first call
#' \dontrun{
#' iaw$msg("Loading data")
#' Sys.sleep(1)
#' iaw$msg("Processing", 1000, "rows")
#' Sys.sleep(1)
#' iaw$msg("Done")
#' }
#'
#' # Use inside a function - caller name is shown automatically
#' \dontrun{
#' run_model <- function() {
#'   iaw$msg("fitting model")
#'   # ... model code ...
#'   iaw$msg("model complete")
#' }
#' run_model()
#'
#' # Track progress in a loop
#' process_files <- function(files) {
#'   for (f in files) {
#'     iaw$msg("Processing ", f)
#'     # ... work ...
#'   }
#'   iaw$msg("All files done")
#' }
#' process_files(c("data1.csv", "data2.csv"))
#'
#' # Custom caller-name width for long function names
#' iaw$msg("step complete", len.of.funname = 20)
#' }

make_msg <- function() {
    firstcall <- NULL
    
    messageln <- function(...) {
        cat("\n")
        cat(..., sep = "")
        cat("\n")
    }
    
    msg_fun <- function(..., len.of.funname = 12) {
        lnm <- as.list(sys.call(-1))
        snm <- as.character(if (length(lnm) == 0) "global" else lnm[[1]])
        sformat <- paste0("%", len.of.funname, ".", len.of.funname, "s")
        
        curtime <- Sys.time()
        
        if (is.null(firstcall)) {
            firstcall <<- curtime
            elapsed <- 0
        } else {
            elapsed <- as.numeric(difftime(curtime, firstcall, units = "secs"))
            curtime <- substr(as.character(curtime), 12, 100)
        }
        
        messageln(
            "\n####----####----####\n",
            as.character(curtime),
            ":[", sprintf("%04d", elapsed), "s]",
            ":", sprintf(sformat, snm), ": ", ...,
            "\n####----####----####"
        )
        invisible(TRUE)
    }
    return(msg_fun)
}

iaw$msg <- make_msg()
