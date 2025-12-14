#' Convert YYYYMMDD to Plot Date
#'
#' @name as.PlotDate.yyyymmdd
#'
#' Converts integer date to Date for plotting.
#'
#' @param yyyymmdd Integer date.
#'
#' @return Date object.
#'
#' @family plotting
#' @export

iaw$as.PlotDate.yyyymmdd <- function(yyyymmdd) {
    stopifnot(is.numeric(yyyymmdd))
    as.Date(as.character(yyyymmdd), format = "%Y%m%d")
}
