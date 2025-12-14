#' Enhanced Message Printer with Timing and Caller Info
#'
#' Creates a closure-based message function that prints user messages along with
#' the time since its first invocation and the calling function name. Useful for
#' logging progress and measuring elapsed time across function calls.
#'
#' @return A function that can be called like \code{iaw$msg("text")} to print
#'   enhanced messages with timing information.
#'
#' @details
#' The returned function:
#' \itemize{
#'   \item Prints the current time and elapsed seconds since first call
#'   \item Shows the name of the calling function
#'   \item Uses ANSI color codes in interactive sessions for visibility
#'   \item Outputs to stdout via \code{cat()} (not stderr like \code{message()})
#' }
#'
#' The closure maintains state between calls to track the first invocation time.
#'
#' @section Usage:
#' \preformatted{
#' iaw$msg("Starting analysis")
#' # ... long computation ...
#' iaw$msg("Checkpoint 1")
#' # ... more computation ...
#' iaw$msg("Done")
#' }
#'
#' @export
#'
#' @seealso \code{\link{message}}, \code{\link{cat}}, \code{\link{iaw$chatter}}
#'
#' @examples
#' # Basic usage
#' iaw$msg("Starting process")
#' Sys.sleep(1)
#' iaw$msg("Step 1 complete")
#' Sys.sleep(1)
#' iaw$msg("All done")
#'
#' # Output shows elapsed time:
#' # 2024-01-15 10:30:45:[0000s]: global: Starting process
#' # 10:30:46:[0001s]: global: Step 1 complete
#' # 10:30:47:[0002s]: global: All done
#'
#' # Inside a function, shows function name
#' my_analysis <- function() {
#'     iaw$msg("Beginning analysis")
#'     Sys.sleep(1)
#'     iaw$msg("Analysis complete")
#' }
#' my_analysis()
#' # Shows "my_analysis" as the caller

make_msg <- function() {

    # Persistent variable for first-call timestamp
    firstcall <- NULL

    # Internal helper for colored message output
    messageln <- function(...) {
        cat("\n")
        boldblue <- "\033[34;1m"
        ansioff  <- "\033[0m"

        if (interactive()) message(boldblue)
        cat(..., sep = "")
        if (interactive()) message(ansioff)
        cat("\n")
    }

    #' @param ... Text or objects to print (passed to \code{cat()}).
    #' @param len.of.funname Integer; width for formatting the caller function name.
    #'   Default is 12.
    msg_fun <- function(..., len.of.funname = 12) {
        # Identify caller
        lnm <- as.list(sys.call(-1))
        snm <- as.character(if (length(lnm) == 0) "global" else lnm[[1]])
        sformat <- paste0("%", len.of.funname, ".", len.of.funname, "s")

        curtime <- Sys.time()

        # Initialize or compute elapsed time since first call
        if (is.null(firstcall)) {
            firstcall <<- curtime
            elapsed <- 0
        } else {
            elapsed <- as.numeric(difftime(curtime, firstcall, units = "secs"))
            curtime <- substr(as.character(curtime), 12, 100)
        }

        messageln(
            "\n####----####----####----####----####----####----####----####----####----####----####----\n",
            as.character(curtime),
            ":[", sprintf("%04d", elapsed), "s]",
            ":", sprintf(sformat, snm), ": ", ...,
            "\n####----####----####----####----####----####----####----####----####----####----####----"
        )

        invisible(TRUE)
    }

    return(msg_fun)
}

#' @rdname make_msg
#' @export
iaw$msg <- make_msg()
