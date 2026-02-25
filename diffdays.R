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
#'
#' # Days to expiry for an options portfolio
#' today <- as.Date("2024-06-15")
#' expiries <- as.Date(c("2024-07-19", "2024-08-16", "2024-09-20"))
#' iaw$diffdays(today, expiries)  # 34, 62, 97
#'
#' # Same date returns zero
#' iaw$diffdays(as.Date("2024-01-01"), as.Date("2024-01-01"))  # 0
#'
#' # Leap year: Feb 28 to Mar 1 in 2024 is 2 days
#' iaw$diffdays(as.Date("2024-02-28"), as.Date("2024-03-01"))  # 2

iaw$diffdays <- function(d1, d2) {
    as.numeric(difftime(d2, d1, units = "days"))
}
