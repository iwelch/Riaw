#' Convert Date to YYYYMMDD Integer
#'
#' @name yyyymmdd2int
#'
#' Converts Date to integer format.
#'
#' @param d Date object or character.
#'
#' @return Integer YYYYMMDD.
#'
#' @family datetime
#' @export
#'
#' @examples
#' iaw$yyyymmdd2int(as.Date("2021-01-15"))

iaw$yyyymmdd2int <- function(d) {
    if (is.character(d)) d <- as.Date(d)
    stopifnot(inherits(d, "Date"))
    as.integer(format(d, "%Y%m%d"))
}

iaw$yyyymmdd.to.int <- iaw$yyyymmdd2int
