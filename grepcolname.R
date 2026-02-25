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
#' # Find date-related columns in a financial panel
#' trades <- data.frame(date = 1, yyyymm = 2, ret = 3, date_filed = 4)
#' iaw$grepcolname("date", trades)   # "date" "date_filed"
#'
#' # Use anchored regex to match columns ending with a suffix
#' stats <- data.frame(mean_ret = 1, sd_ret = 2, mean_vol = 3, n_obs = 4)
#' iaw$grepcolname("_ret$", stats)   # "mean_ret" "sd_ret"
#'
#' # No matches returns character(0)
#' iaw$grepcolname("zzz", df)        # character(0)
#'
#' @family utilities
#' @export

iaw$grepcolname <- function(pattern, df) {
    grep(pattern, names(df), value = TRUE)
}
