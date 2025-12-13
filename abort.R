
#' abort the program execution, with a message and a call stack
#'
#' @param errstring An error string
#' @examples
#'   abort("You have made a mistake")

iaw$abort <- function(errstring) {
  (iaw$is.character(errstring, 1)) %or% "argument must be a single string, not {{class(errstring)}}";
  errstring <- paste(iaw$estring(errstring),"\n")  ## up one more level and evaluate
  if (interactive()) message( errstring ) else cat( errstring )
  ## cat("\nCall Stack:\n") -- discard <- traceback()  ## gives the call stack post-mortem.  not used.
  stop( simpleError("see debug advice on start or iaw$debug.advice()") )
}
