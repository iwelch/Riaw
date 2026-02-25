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
#' # Convert a single integer date
#' iaw$as.PlotDate.yyyymmdd(20230101)       # as.Date("2023-01-01")
#'
#' # Convert a vector of quarterly end-dates
#' qdates <- c(20200331, 20200630, 20200930, 20201231)
#' iaw$as.PlotDate.yyyymmdd(qdates)
#' # Date[1:4] "2020-03-31" "2020-06-30" "2020-09-30" "2020-12-31"
#'
#' # Compute number of calendar days between two dates
#' d1 <- iaw$as.PlotDate.yyyymmdd(20200101)
#' d2 <- iaw$as.PlotDate.yyyymmdd(20201231)
#' as.integer(d2 - d1)                       # 365
#'
#' \dontrun{
#' # Convert a vector of dates for use as plot axis values
#' dates <- c(20200101, 20200601, 20210101)
#' plot(iaw$as.PlotDate.yyyymmdd(dates), c(1, 2, 3), type = "l",
#'      xlab = "Date", ylab = "Value")
#'
#' # Time-series plot with proper date axis
#' months <- seq(20190101, 20191201, by = 100)
#' vals <- cumsum(rnorm(12))
#' plot(iaw$as.PlotDate.yyyymmdd(months), vals, type = "l",
#'      xlab = "Month", ylab = "Cumulative return")
#' }
#'
#' @family plotting
#' @export

iaw$as.PlotDate.yyyymmdd <- function(yyyymmdd) {
    stopifnot(is.numeric(yyyymmdd))
    as.Date(as.character(yyyymmdd), format = "%Y%m%d")
}
