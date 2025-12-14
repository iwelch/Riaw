#' Calculate Days Between Dates
#'
#' @name diffdays
#'
#' Calculates difference in days between dates.
#'
#' @param d1 First date.
#' @param d2 Second date.
#'
#' @return Numeric days.
#'
#' @family datetime
#' @export
#'
#' @examples
#' iaw$diffdays(as.Date("2021-01-01"), as.Date("2021-01-15"))

iaw$diffdays <- function(d1, d2) {
    as.numeric(difftime(d2, d1, units = "days"))
}
