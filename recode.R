#' Recode Values in a Vector
#'
#' @name recode
#'
#' Replaces values according to mapping.
#'
#' @param x Vector to recode.
#' @param from Original values.
#' @param to Replacement values.
#' @param non.from.becomes.na If TRUE, unmatched become NA.
#'
#' @return Recoded vector.
#'
#' @family data-manipulation
#' @export
#'
#' @examples
#' x <- c(1, 2, 3, 1, 2)
#' iaw$recode(x, c(1, 2), c(10, 20))
#'
#' # Recode character values (e.g. standardise country name variants)
#' country <- c("US", "USA", "United States", "UK")
#' iaw$recode(country, c("USA", "United States"), c("US", "US"))
#'
#' # Set unmatched values to NA with non.from.becomes.na
#' x <- c(1, 2, 3, 4, 5)
#' iaw$recode(x, c(1, 2, 3), c(10, 20, 30), non.from.becomes.na = TRUE)
#'
#' # Recode numeric exchange codes to readable labels
#' exchange <- c(1, 2, 3, 1, 2)
#' iaw$recode(exchange, c(1, 2, 3), c(11, 12, 13))  # c(11, 12, 13, 11, 12)
#'
#' # Map month abbreviations to quarter labels
#' months <- c("Jan", "Apr", "Jul", "Oct", "Jan")
#' iaw$recode(months, c("Jan","Apr","Jul","Oct"), c("Q1","Q2","Q3","Q4"))
#' # c("Q1", "Q2", "Q3", "Q4", "Q1")
#'
#' # Only keep mapped values; everything else becomes NA
#' ratings <- c("AAA", "AA", "A", "BBB", "BB", "B")
#' iaw$recode(ratings, c("AAA", "AA", "A"), c("IG", "IG", "IG"),
#'            non.from.becomes.na = TRUE)  # c("IG", "IG", "IG", NA, NA, NA)

iaw$recode <- function(x, from, to, non.from.becomes.na = FALSE) {
    stopifnot(length(from) == length(to))
    stopifnot(is.numeric(from) == is.numeric(to), is.character(from) == is.character(to))
    stopifnot(is.numeric(x) == is.numeric(from), is.character(x) == is.character(from))

    y <- if (non.from.becomes.na) rep(NA, length(x)) else x

    for (f in seq_along(from)) {
        y[x == from[f]] <- to[f]
    }
    y
}
