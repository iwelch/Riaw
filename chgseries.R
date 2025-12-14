#' Calculate Element-to-Element Changes in a Time Series
#'
#' Computes the first difference (or nth difference) of a time series:
#' \code{x[t] - x[t-n]}. For panel data, differences do not cross panel
#' boundaries.
#'
#' @param seriesin A numeric vector (time series).
#' @param numlags Integer number of periods to difference. Default is 1.
#' @param panelid Optional vector of panel identifiers (e.g., firm IDs).
#' @param timeid Optional vector of time identifiers (required with panelid).
#'
#' @return A numeric vector of the same length as \code{seriesin}, with NA
#'   values for the first \code{numlags} observations (or at panel boundaries).
#'
#' @export
#'
#' @seealso \code{\link{iaw$pchgseries}} for percent changes,
#'   \code{\link{iaw$lagseries}}, \code{\link{diff}}
#'
#' @examples
#' # Simple differencing
#' x <- c(100, 102, 105, 103, 108)
#' iaw$chgseries(x)
#' # NA 2 3 -2 5
#'
#' # Two-period difference
#' iaw$chgseries(x, numlags = 2)
#' # NA NA 5 1 3
#'
#' # Panel data (differences don't cross firms)
#' d <- data.frame(
#'     firm = c("A", "A", "A", "B", "B", "B"),
#'     year = c(2020, 2021, 2022, 2020, 2021, 2022),
#'     sales = c(100, 110, 120, 200, 220, 250)
#' )
#' iaw$chgseries(d$sales, panelid = d$firm, timeid = d$year)
#' # NA 10 10 NA 20 30

iaw$chgseries <- function(seriesin, numlags = 1, panelid = NULL, timeid = NULL) {
    (is.numeric(seriesin)) %or% "seriesin must be numeric, not {{class(seriesin)}}"
    (length(seriesin) > 1) %or% "Need more than {{length(seriesin)}} observations"
    seriesin - iaw$lagseries(seriesin, numlags, panelid, timeid)
}

#' @rdname chgseries
#' @export
iaw$dchgseries <- iaw$chgseries
