#' Create Tabular Summary
#'
#' @name tabularsummary
#'
#' Creates frequency table with statistics.
#'
#' @param x Factor or character vector.
#' @param sort Sort by frequency.
#'
#' @return Data frame with counts and percentages.
#'
#' @family statistics
#' @export
#'
#' @examples
#' iaw$tabularsummary(c("a", "b", "a", "c", "a"))
#'
#' # Summarise a factor column from a data frame
#' df <- data.frame(sector = c("Tech", "Finance", "Tech", "Energy",
#'                              "Finance", "Tech", NA))
#' iaw$tabularsummary(df$sector)   # shows NA as its own row
#'
#' # Unsorted output preserves original level order
#' iaw$tabularsummary(c("low", "high", "med", "low", "low"), sort = FALSE)
#'
#' # Frequency breakdown of trade directions
#' trades <- c("buy", "sell", "buy", "buy", "sell", "hold", "buy")
#' tab <- iaw$tabularsummary(trades)
#' tab$count[1]   # 4 (buy is most frequent)
#'
#' # Rating distribution with percentages
#' ratings <- sample(c("AAA", "AA", "A", "BBB", "BB"), 200, replace = TRUE)
#' iaw$tabularsummary(ratings)
#'
#' # Single unique value
#' iaw$tabularsummary(rep("constant", 10))   # one row, count=10, pct=100

iaw$tabularsummary <- function(x, sort = TRUE) {
    tb <- table(x, useNA = "ifany")
    df <- data.frame(
        value = names(tb),
        count = as.integer(tb),
        pct = round(100 * as.integer(tb) / sum(tb), 1),
        stringsAsFactors = FALSE
    )
    if (sort) df <- df[order(-df$count), ]
    df
}
