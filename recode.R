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

iaw$recode <- function(x, from, to, non.from.becomes.na = FALSE) {
    stopifnot(length(from) == length(to))
    stopifnot(typeof(from)==typeof(to))
    stopifnot(typeof(x)==typeof(from))

    y <- if (non.from.becomes.na) rep(NA, length(x)) else x

    for (f in seq_along(from)) {
        y[x == from[f]] <- to[f]
    }
    y
}
