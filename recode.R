#' Recode Values in a Vector
#'
#' Replaces values in a vector according to a mapping. Useful for recoding
#' factor levels or categorical variables.
#'
#' @param x A vector to recode.
#' @param from Vector of original values to replace.
#' @param to Vector of replacement values (same length as \code{from}).
#' @param non.from.becomes.na Logical; if TRUE, values not in \code{from}
#'   become NA. If FALSE (default), they remain unchanged.
#'
#' @return A vector of the same length as \code{x} with recoded values.
#'
#' @export
#'
#' @seealso \code{\link{iaw$rename.columns}}, \code{\link{factor}},
#'   \code{\link[dplyr]{recode}}
#'
#' @examples
#' # Recode numeric values
#' x <- c(1, 2, 3, 1, 2, 3)
#' iaw$recode(x, from = c(1, 2, 3), to = c(10, 20, 30))
#' # 10 20 30 10 20 30
#'
#' # Recode character values
#' colors <- c("r", "g", "b", "r", "g")
#' iaw$recode(colors, from = c("r", "g", "b"), to = c("red", "green", "blue"))
#' # "red" "green" "blue" "red" "green"
#'
#' # Set unmatched values to NA
#' x <- c(1, 2, 3, 4, 5)
#' iaw$recode(x, from = c(1, 2), to = c(10, 20), non.from.becomes.na = TRUE)
#' # 10 20 NA NA NA
#'
#' # Partial recoding (keep unmatched values)
#' iaw$recode(x, from = c(1, 2), to = c(10, 20), non.from.becomes.na = FALSE)
#' # 10 20 3 4 5

iaw$recode <- function(x, from, to, non.from.becomes.na = FALSE) {
    (length(from) == length(to)) %or% "from and to must have same length"

    y <- if (non.from.becomes.na) rep(NA, length(x)) else x

    for (f in seq_along(from)) {
        y[x == from[f]] <- to[f]
    }

    y
}
