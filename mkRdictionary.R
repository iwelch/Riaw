#' Make R Dictionary
#'
#' @name mkRdictionary
#'
#' Creates named vector dictionary.
#'
#' @param keys Key values.
#' @param values Value values.
#'
#' @return Named vector.
#'
#' @examples
#' # Map state abbreviations to full names
#' d <- iaw$mkRdictionary(c("CA", "NY", "TX"),
#'                         c("California", "New York", "Texas"))
#' d["NY"]    # "New York"
#'
#' # Use as a lookup table
#' codes <- c("CA", "TX", "CA", "NY")
#' d[codes]   # returns the full names for each code
#'
#' # Map numeric sector codes to labels
#' sec <- iaw$mkRdictionary(c(10, 20, 30),
#'                           c("Energy", "Materials", "Industrials"))
#' sec["20"]  # "Materials"
#'
#' # Build a currency-pair dictionary for FX data
#' fx <- iaw$mkRdictionary(c("EURUSD", "GBPUSD", "USDJPY"),
#'                          c(1.08, 1.27, 149.5))
#' fx["EURUSD"]  # 1.08
#'
#' # Single-element dictionary
#' iaw$mkRdictionary("key1", "value1")  # named character vector of length 1
#'
#' @family utilities
#' @export

iaw$mkRdictionary <- function(keys, values) {
    stopifnot(length(keys) == length(values))
    setNames(values, keys)
}
