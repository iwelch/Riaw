#' Base Utility Functions
#'
#' Core utility functions that are loaded before all other Riaw functions.
#' These provide basic system information and terminal configuration.
#'
#' @name base-utilities
#' @rdname base-utilities
#'
#' @examples
#' # Get current hostname
#' hostname()
#'
#' # Set terminal width for proper formatting
#' if (interactive()) tty.setcols()

#' Get System Hostname
#'
#' Returns the hostname of the current machine.
#'
#' @return Character string with the hostname.
#'
#' @export
hostname <- function() {
    as.character(Sys.info()["nodename"])
}

#' Print Message to Both stdout and stderr
#'
#' Prints a message with highlighting to both standard output and message streams.
#'
#' @param ... Text to print.
#' @param msgcat Category label for the message.
#'
#' @return Invisible NULL.
#'
#' @export
msgboth <- function(..., msgcat = "") {
    cat("\n----------------")
    message("\n\033[43m ==> ", msgcat, "\033[0m", appendLF = FALSE)
    cat("\n", ..., "\n")
    invisible(NULL)
}

#' Set Terminal Column Width
#'
#' Automatically sets R's width option based on the current terminal width.
#' Only works in interactive sessions with a proper terminal.
#'
#' @param rightmargin Integer; characters to reserve for right margin. Default 8.
#'
#' @return Invisible NULL. Sets \code{options(width = ...)}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' tty.setcols()
#' tty.setcols(rightmargin = 4)  # Narrower margin
#' }
tty.setcols <- function(rightmargin = 8) {
    cols <- system('tput cols', intern = TRUE)
    if (is.character(cols)) {
        options(width = as.integer(cols) - rightmargin)
    }
    message("[tty.setcols: Terminal width now ", getOption("width"), " characters]\n")
    invisible(NULL)
}
