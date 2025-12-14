#' Number of Observations by Group
#'
#' @name numobs.by
#'
#' Counts observations per group.
#'
#' @param df Data frame.
#' @param by Grouping variable.
#'
#' @return Table of counts.
#'
#' @family utilities
#' @export

iaw$numobs.by <- function(df, by) {
    stopifnot(is.data.frame(df))
    table(df[[by]])
}
