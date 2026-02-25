#' Cat to Both stdout and stderr
#'
#' @name cat.both
#'
#' Prints to both standard output and error.
#'
#' @param ... Items to print.
#'
#' @return Invisible NULL.
#'
#' @examples
#' \dontrun{
#' # Print a status message that appears on both stdout and stderr
#' iaw$cat.both("Processing file...\n")
#'
#' # Useful when output is captured but warnings should still reach the console
#' iaw$cat.both("Warning: missing values found\n")
#'
#' # Log regression progress to both streams during a backtest
#' for (yr in 2020:2023) {
#'   iaw$cat.both(sprintf("Running regression for %d...\n", yr))
#' }
#'
#' # Report summary statistics that need to appear in log files and terminal
#' iaw$cat.both(sprintf("Portfolio Sharpe: %.2f\n", 1.45))
#' }
#'
#' @family utilities
#' @export

iaw$cat.both <- function(...) {
    cat(...)
    cat(..., file = stderr())
    invisible(NULL)
}
