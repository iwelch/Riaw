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
#' @examples
#' \dontrun{
#' # Convert a single integer date
#' iaw$as.PlotDate.yyyymmdd(20230101)
#' # [1] "2023-01-01"
#'
#' # Convert a vector of dates for use as plot axis values
#' dates <- c(20200101, 20200601, 20210101)
#' plot(iaw$as.PlotDate.yyyymmdd(dates), c(1, 2, 3), type = "l",
#'      xlab = "Date", ylab = "Value")
#' }
#'
#' @family plotting
#' @export

iaw$as.PlotDate.yyyymmdd <- function(yyyymmdd) {
    stopifnot(is.numeric(yyyymmdd))
    as.Date(as.character(yyyymmdd), format = "%Y%m%d")
}
