#' Lag a Time Series
#'
#' @name lagseries
#'
#' Shifts series backward by numlags periods.
#'
#' @param series A numeric vector.
#' @param numlags Integer number of lags. Default 1.
#' @param panelid Optional panel identifier vector.
#' @param timeid Optional time identifier vector.
#'
#' @return Numeric vector of same length.
#'
#' @family time-series
#' @export
#'
#' @examples
#' # Lag by 1: first value becomes NA, others shift right
#' x <- c(1, 2, 3, 4, 5)
#' iaw$lagseries(x)          # NA 1 2 3 4
#'
#' # Lag by 2 periods
#' iaw$lagseries(x, numlags = 2)  # NA NA 1 2 3
#'
#' # Panel-aware lag: respects group boundaries
#' panel <- data.frame(
#'   firm = c("A","A","A","B","B","B"),
#'   t    = c(1, 2, 3, 1, 2, 3),
#'   ret  = c(0.01, 0.02, 0.03, 0.10, 0.11, 0.12)
#' )
#' iaw$lagseries(panel$ret, panelid = panel$firm, timeid = panel$t)

iaw$lagseries <- function(series, numlags = 1, panelid = NULL, timeid = NULL) {
    stopifnot(is.numeric(series) || is.factor(series))
    stopifnot(is.numeric(numlags), length(numlags) == 1L)
    
    iaw$funseries(
        list(
            fun = function(series, numlags) {
                if (numlags == 0) return(series)
                if (numlags < 0) return(iaw$leadseries(series, -numlags, panelid, timeid))
                x <- series[1:(length(series) - numlags)]
                if (is.factor(x)) {
                    factor(c(rep(NA, numlags), levels(x)[x]))
                } else {
                    c(rep(NA, numlags), x)
                }
            },
            param = numlags,
            series = series
        ),
        series, panelid, timeid
    )
}
