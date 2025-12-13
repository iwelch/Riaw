#' Enhanced Message Printer with Timing and Caller Info
#'
#' Creates a closure-based message function that prints user messages along with
#' the time since its first invocation and the calling function name.
#' This is useful for logging progress and measuring elapsed time across function calls.
#'
#' Unlike [message()], output is directed to standard output (`stdout`) via [cat()]
#' for better logging in files or pipelines. ANSI color codes are used in
#' interactive sessions to highlight the message visually.
#'
#' @return
#' A function that can be called like `msg("text")` to print enhanced messages.
#'
#' @param ... Any text or objects accepted by [cat()] or [message()].
#' @param len.of.funname Integer; width used when formatting the caller function name.
#'   Defaults to `12`.
#'
#' @details
#' Internally, the closure keeps:
#' \itemize{
#'   \item `firstcall` — the timestamp of the first message call.
#'   \item a helper function `messageln()` that prints with color formatting.
#' }
#' Each subsequent message prints the time since the first call and the calling
#' function name.
#'
#' @examples
#' \dontrun{
#' iaw$msg <- make_msg()
#'
#' iaw$msg("Hello")
#' Sys.sleep(2)
#' iaw$msg("Again")
#' }
#'
#' @seealso [message()], [cat()]
#'
#' @export
make_msg <- function() {

  # persistent variable for first-call timestamp
  firstcall <- NULL

  # internal helper for colored message output
  messageln <- function(...) {
    cat("\n")
    boldblue <- "\033[34;1m"
    boldred  <- "\033[30;1m"
    bold     <- "\033[30;1m"
    ansioff  <- "\033[0m"

    if (interactive()) message(boldblue)
    cat(..., sep = "")
    if (interactive()) message(ansioff)
    cat("\n")
  }

  msg_fun <- function(..., len.of.funname = 12) {
    # identify caller
    lnm <- as.list(sys.call(-1))
    snm <- as.character(if (length(lnm) == 0) "global" else lnm[[1]])
    sformat <- paste0("%", len.of.funname, ".", len.of.funname, "s")

    curtime <- Sys.time()

    # initialize or compute elapsed time since first call
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

    TRUE
  }

  return(msg_fun)
}

iaw$msg <- make_msg()
