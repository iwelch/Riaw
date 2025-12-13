
#' Exit Nicely
#'
#' @name exit
#'

## not sure if I'd prefer having this iaw$exit
exit <- function(...) {
    blankMsg <- sprintf("\r%s\r", paste(rep(" ", getOption("width")-1L), collapse=" "));
    stop(simpleError(blankMsg));
}
