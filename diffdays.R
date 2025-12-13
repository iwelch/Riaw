
#' DIFFDAYS
#'
#' @name diffdays
#'
#' the difference between two days
#'
#' @usage diffdays (yyyymmdd1, yyyymmdd2)
#'
#' @param yyyymmdd1 the first day
#' @param yyyymmdd2 the second day
#'
#' @return integer number of days
#'


iaw$diffdays <- function (yyyymmdd1, yyyymmdd2) {
  d1 <- as.Date(strptime(yyyymmdd1, "%Y%m%d"))
  d2 <- as.Date(strptime(yyyymmdd2, "%Y%m%d"))
  as.integer(d2 - d1)
}
