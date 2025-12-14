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
#' @family utilities
#' @export

iaw$sprint.data.frame <- function(df, n = 10) {
    stopifnot(is.data.frame(df))
    print(head(df, n))
    invisible(df)
}
