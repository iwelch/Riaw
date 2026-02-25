#' Test if Pattern Exists in String
#'
#' @name is.instring
#'
#' A wrapper for grepl.
#'
#' @param needle Character pattern to search for.
#' @param heystack Character vector to search within.
#'
#' @return Logical vector.
#'
#' @family type-checking
#' @export
#'
#' @examples
#' # Check which strings contain the substring "ab"
#' iaw$is.instring("ab", c("abc", "def", "ab"))
#'
#' # Filter a vector to keep only matching elements
#' tickers <- c("AAPL", "MSFT", "AMZN", "AAPLX", "META")
#' tickers[iaw$is.instring("AAPL", tickers)]
#'
#' # Case-sensitive: "AB" does not match "abc"
#' iaw$is.instring("AB", c("abc", "ABC", "Ab"))
#'
#' # Useful for filtering data frame rows by a string column
#' df <- data.frame(name = c("alpha", "beta", "alphabet"), val = 1:3,
#'                  stringsAsFactors = FALSE)
#' df[iaw$is.instring("alpha", df$name), ]

iaw$is.instring <- function(needle, heystack) {
    stopifnot(is.character(needle), length(needle) == 1L)
    stopifnot(is.character(heystack))
    grepl(needle, heystack, fixed = TRUE)
}
