#' Lead a Time Series
#'
#' @name leadseries
#'
#' Shifts series forward by numleads periods.
#'
#' @param series A numeric vector.
#' @param numleads Integer number of leads. Default 1.
#' @param panelid Optional panel identifier vector.
#' @param timeid Optional time identifier vector.
#'
#' @return Numeric vector of same length.
#'
#' @family time-series
#' @export
#'
#' @examples
#' # Lead by 1: first value becomes the second element, last becomes NA
#' x <- c(1, 2, 3, 4, 5)
#' iaw$leadseries(x)          # 2 3 4 5 NA
#'
#' # Lead by 2 periods
#' iaw$leadseries(x, numleads = 2)  # 3 4 5 NA NA
#'
#' # Panel-aware lead: does not bleed across group boundaries
#' panel <- data.frame(
#'   firm = c("A","A","A","B","B","B"),
#'   t    = c(1, 2, 3, 1, 2, 3),
#'   ret  = c(0.01, 0.02, 0.03, 0.10, 0.11, 0.12)
#' )
#' iaw$leadseries(panel$ret, panelid = panel$firm, timeid = panel$t)
#'
#' # Compute forward return: next period minus current
#' prices <- c(100, 102, 101, 105, 108)
#' fwd_ret <- iaw$leadseries(prices) - prices  # 2 -1 4 3 NA
#' fwd_ret
#'
#' # Lead of zero returns the original series unchanged
#' iaw$leadseries(c(10, 20, 30), numleads = 0)  # 10 20 30
#'
#' # Negative lead is equivalent to a lag
#' x <- c(1, 2, 3, 4, 5)
#' iaw$leadseries(x, numleads = -1)  # NA 1 2 3 4

iaw$leadseries <- function(series, numleads = 1, panelid = NULL, timeid = NULL) {
    stopifnot(is.numeric(series) || is.factor(series))
    stopifnot(is.numeric(numleads), length(numleads) == 1L)
    
    iaw$funseries(
        list(
            fun = function(series, numleads) {
                if (numleads == 0) return(series)
                if (numleads < 0) return(iaw$lagseries(series, -numleads, panelid, timeid))
                x <- series[(1 + numleads):(length(series))]
                if (is.factor(x)) {
                    factor(c(levels(x)[x], rep(NA, numleads)))
                } else {
                    c(x, rep(NA, numleads))
                }
            },
            param = numleads,
            series = series
        ),
        series, panelid, timeid
    )
}
