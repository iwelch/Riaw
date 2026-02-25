#' Sprint Data Frame
#'
#' @name sprint.data.frame
#'
#' Prints data frame in compact format.
#'
#' @param df Data frame.
#' @param n Number of rows.
#'
#' @return Invisible data frame.
#'
#' @examples
#' df <- data.frame(id = 1:3, name = c("Alice", "Bob", "Carol"), score = c(88, 92, 79))
#' s <- iaw$sprint.data.frame(df)
#' cat(s, "\n")
#'
#' # Limit to first 2 rows
#' big <- data.frame(x = 1:10, y = letters[1:10])
#' cat(iaw$sprint.data.frame(big, n = 2), "\n")
#'
#' # Capture a summary snapshot for logging
#' trades <- data.frame(ticker = c("AAPL", "MSFT", "GOOG"),
#'                      qty = c(100, 200, 50),
#'                      price = c(175.3, 415.2, 141.8))
#' msg <- iaw$sprint.data.frame(trades)
#' nchar(msg) > 0   # TRUE
#'
#' # Single-row data frame
#' iaw$sprint.data.frame(data.frame(a = 1, b = "x"), n = 1)
#'
#' # Combine with strcat for composite log messages
#' header <- "=== Portfolio ==="
#' body <- iaw$sprint.data.frame(trades, n = 2)
#' paste(header, body, sep = "\n")
#'
#' @family utilities
#' @export

iaw$sprint.data.frame <- function(df, n = NULL) {
    stopifnot(is.data.frame(df))
    if (!is.null(n)) df <- head(df, n)
    paste0(capture.output(df), collapse = "\n")
}
