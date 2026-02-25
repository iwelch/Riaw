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
#' # Days between two specific dates
#' iaw$diffdays(as.Date("2021-01-01"), as.Date("2021-01-15"))  # 14
#'
#' # Negative result when d2 is before d1
#' iaw$diffdays(as.Date("2021-12-31"), as.Date("2021-01-01"))  # -364
#'
#' # Vectorized: days from a base date to each element of a vector
#' base <- as.Date("2020-01-01")
#' dates <- as.Date(c("2020-01-15", "2020-02-01", "2020-03-01"))
#' iaw$diffdays(base, dates)

iaw$diffdays <- function(d1, d2) {
    as.numeric(difftime(d2, d1, units = "days"))
}
