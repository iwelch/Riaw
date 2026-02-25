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
#' # Panel data: count firm-year observations per country
#' panel <- data.frame(
#'   firm = c("AAPL", "MSFT", "SAP", "NESN", "AAPL", "SAP"),
#'   country = c("US", "US", "DE", "CH", "US", "DE"),
#'   year = c(2020, 2020, 2020, 2020, 2021, 2021)
#' )
#' iaw$numobs.by(panel, by = "country")  # CH=1, DE=2, US=3
#'
#' # Single-group edge case
#' df3 <- data.frame(x = 1:5, grp = rep("all", 5))
#' iaw$numobs.by(df3, by = "grp")  # all=5
#'
#' # With factor grouping variable (preserves empty levels)
#' df4 <- data.frame(
#'   val = 1:4,
#'   size = factor(c("S", "M", "S", "M"), levels = c("S", "M", "L"))
#' )
#' iaw$numobs.by(df4, by = "size")  # S=2, M=2, L=0
#'
#' @family utilities
#' @export

iaw$numobs.by <- function(df, by) {
    stopifnot(is.data.frame(df))
    table(df[[by]])
}
