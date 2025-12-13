## these functions are basic and need to be included before everything else

hostname <- function() as.character(Sys.info()["nodename"])

msgboth <- function(..., msgcat = "") {
    cat("\n----------------")
    message("\n\033[43m ==> ", msgcat, "\033[0m", appendLF = FALSE)
    cat("\n", ..., "\n")
}

tty.setcols <- function( rightmargin= 8) {
    cols <- system('tput cols', intern = TRUE)
    if (is.character(cols)) options(width = as.integer(cols) - rightmargin)  ## default
    message("[iaw.autosetcols: Col Widths of Terminal now ", getOption("width"), " characters]\n")
}
    
