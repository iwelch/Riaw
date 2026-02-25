#' Grep Column Names
#'
#' @name grepcolname
#'
#' Finds column names matching pattern.
#'
#' @param pattern Regex pattern.
#' @param df Data frame.
#'
#' @return Matching column names.
#'
#' @examples
#' df <- data.frame(ret_1m = 1, ret_3m = 2, price = 3, volume = 4)
#'
#' # Find all return columns
#' iaw$grepcolname("^ret", df)       # "ret_1m" "ret_3m"
#'
#' # Case-insensitive search using regex
#' iaw$grepcolname("(?i)price|vol", df)   # "price" "volume"
#'
#' @family utilities
#' @export

iaw$grepcolname <- function(pattern, df) {
    grep(pattern, names(df), value = TRUE)
}
