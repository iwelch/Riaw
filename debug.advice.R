#' Debugging Help and Tips
#'
#' Displays a summary of R debugging techniques and functions. Useful as
#' a quick reference when debugging R code.
#'
#' @format A character string containing debugging tips.
#'
#' @details
#' Key debugging techniques covered:
#' \itemize{
#'   \item \code{debug(f)}: Stop when reaching function f
#'   \item \code{trace(f, browser)}: Enter browser upon entering f()
#'   \item \code{browser()}: Insert breakpoint in code
#'   \item \code{traceback()}: View call stack after error
#'   \item \code{options(error=recover)}: Enter debug mode on error
#' }
#'
#' @export
#'
#' @seealso \code{\link{iaw$debug.on}}, \code{\link{iaw$enable.error.line.num}},
#'   \code{\link{debug}}, \code{\link{browser}}, \code{\link{traceback}}
#'
#' @examples
#' cat(iaw$debug.advice)

iaw$debug.advice <- paste(
    "\nDebugging:",
    "\t: debug(f) = stop when reaching function f (also debugonce, undebug, setBreakpoint)",
    "\t: trace(f, f2) = call f2 upon entering f(); e.g., trace(f, browser). see also untrace()",
    "\t: if (condition) browser(\"debug now\") = enter browser if condition",
    "\t: traceback() = last error call stack post-mortem",
    "\t: options(error=recover) / options(error=NULL)",
    "\t: Riaw functions: iaw$source.debug(), iaw$debug.on(), iaw$enable.error.line.num()",
    "\t: where() = current call stack (in browser)",
    "\t: Also see: Bravington debug package, edtdbg\n",
    sep = "\n\t::"
)
