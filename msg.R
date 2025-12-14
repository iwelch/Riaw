#' Enhanced Message Printer with Timing
#'
#' @name msg
#'
#' Prints messages with elapsed time and caller info.
#'
#' @return A message function.
#'
#' @family utilities
#' @export
#'
#' @examples
#' iaw$msg("Starting process")

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
