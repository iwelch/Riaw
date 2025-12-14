#' Calculate Days Between Dates
#'
#' Calculates the number of days between two dates given in YYYYMMDD format.
#'
#' @param yyyymmdd1 Integer; first date in YYYYMMDD format.
#' @param yyyymmdd2 Integer; second date in YYYYMMDD format.
#'
#' @return Integer; number of days from yyyymmdd1 to yyyymmdd2 (positive if
#'   yyyymmdd2 is later).
#'
#' @export
#'
#' @seealso \code{\link{difftime}}, \code{\link{iaw$yyyymmdd.toggle}}
#'
#' @examples
#' # Days in January 2024
#' iaw$diffdays(20240101, 20240131)
#' # 30
#'
#' # Days between years
#' iaw$diffdays(20200101, 20210101)
#' # 366 (2020 was a leap year)

iaw$diffdays <- function(yyyymmdd1, yyyymmdd2) {
    d1 <- as.Date(strptime(yyyymmdd1, "%Y%m%d"))
    d2 <- as.Date(strptime(yyyymmdd2, "%Y%m%d"))
    as.integer(d2 - d1)
}
