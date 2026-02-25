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
#' @examples
#' # Count observations per industry group
#' df <- data.frame(
#'   firm   = c("A", "B", "C", "D", "E"),
#'   sector = c("Tech", "Finance", "Tech", "Finance", "Tech")
#' )
#' iaw$numobs.by(df, by = "sector")
#'
#' # Works with numeric group codes too
#' df2 <- data.frame(ret = rnorm(6), year = c(2020, 2020, 2021, 2021, 2022, 2022))
#' iaw$numobs.by(df2, by = "year")
#'
#' @family utilities
#' @export

iaw$numobs.by <- function(df, by) {
    stopifnot(is.data.frame(df))
    table(df[[by]])
}
