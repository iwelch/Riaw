#' Test if Pattern Exists in String(s)
#'
#' A wrapper for \code{grepl()} that checks whether a pattern (needle) exists
#' within one or more strings (haystack).
#'
#' @param needle Character string pattern to search for.
#' @param heystack Character vector of strings to search within.
#'
#' @return A logical vector the same length as \code{heystack}.
#'
#' @export
#'
#' @seealso \code{\link{grepl}}, \code{\link{grep}}
#'
#' @examples
#' iaw$is.instring("ab", c("this is ab in", "nada", "abracadabra"))
#' # TRUE FALSE TRUE
#'
#' # Check if column names contain "price"
#' df <- data.frame(stock_price = 1, volume = 2, adj_price = 3)
#' iaw$is.instring("price", names(df))
#' # TRUE FALSE TRUE
#'
#' # Filter columns
#' price_cols <- names(df)[iaw$is.instring("price", names(df))]

iaw$is.instring <- function(needle, heystack) {
    grepl(needle, heystack)
}
