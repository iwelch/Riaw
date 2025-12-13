
#' Give Some Debugging Help
#'
#' @name debug.advice
#'
#' read carefully!
#'
#' @seealso iaw$debug.onerr
#'

iaw$debug.advice <-
    paste("\nDebugging:",
          "\t: debug(f) = stop when reaching function f (also debugonce, undebug, setBreakpoint)",
          "\t: trace(f, f2) = call f2 upon entering f(); e.g., trace(f, browser).  see also untrace()",
          "\t: if (condition) browser( \"debug now\" ) = enter browser if condition",
          "\t: traceback() = last error call stack post-mortem",
          "\t: options(error=recover) / options(error=NULL)",
          "\t: my own functions: sourcedebug(), debugon(), enable.error.line.num()",
          "\t: where() = current call stack",
          "\t: also, Bravington debug package, or edtdbg\n",
          sep="\n\t::")

