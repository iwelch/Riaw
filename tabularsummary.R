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
#' iaw$tabular(c("a", "b", "a", "c", "a"))

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
