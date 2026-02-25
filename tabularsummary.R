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
