#' Concatenate Strings
#'
#' @name strcat
#'
#' Concatenates strings without separator.
#'
#' @param ... Strings to concatenate.
#' @param sep Separator between elements (default \code{""}).
#'
#' @return Single string.
#'
#' @family utilities
#' @export
#'
#' @examples
#' # Basic concatenation with no separator
#' iaw$strcat("Hello", "World")
#'
#' # Multiple arguments joined into one string
#' iaw$strcat("file", "_", "2024", ".csv")
#'
#' # Use sep to join with a delimiter
#' iaw$strcat("a", "b", "c", sep = "-")
#'
#' # Build a file path from parts
#' iaw$strcat("/data", "/raw", "/prices.csv")
#'
#' # Build a SQL WHERE clause dynamically
#' tickers <- c("AAPL", "MSFT", "GOOG")
#' iaw$strcat("'", tickers, "'", sep = ", ")   # "'AAPL', 'MSFT', 'GOOG'"
#'
#' # Join column names for a header line
#' iaw$strcat(names(iris), sep = "|")   # "Sepal.Length|Sepal.Width|..."
#'
#' # Single argument returns it unchanged
#' iaw$strcat("hello")   # "hello"

iaw$strcat <- function(..., sep = "") {
    args <- c(...)
    paste(args, collapse = sep)
}
