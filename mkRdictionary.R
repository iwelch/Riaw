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
#' @family utilities
#' @export

iaw$mkRdictionary <- function(keys, values) {
    stopifnot(length(keys) == length(values))
    setNames(values, keys)
}
