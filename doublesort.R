#' Double Sort
#'
#' @name doublesort
#'
#' Sorts by two variables (for portfolio sorts).
#'
#' @param df Data frame.
#' @param var1 First sort variable.
#' @param var2 Second sort variable.
#' @param n1 Number of groups for var1.
#' @param n2 Number of groups for var2.
#'
#' @return Data frame with portfolio assignments.
#'
#' @family data-manipulation
#' @export

iaw$doublesort <- function(df, var1, var2, n1 = 5, n2 = 5) {
    stopifnot(is.data.frame(df))
    stopifnot(var1 %in% names(df), var2 %in% names(df))

    # ntile <- function(x, n) floor((rank(x, na.last = "keep") - 1) / length(x[!is.na(x)]) * n) + 1

    ntile <- dplyr::ntile(df[[var1]], n1)

    df$port1 <- ntile(df[[var1]], n1)
    df$port2 <- ntile(df[[var2]], n2)
    df
}
