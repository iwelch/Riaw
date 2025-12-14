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
#' x <- c(1, 2, 3, 4, 5)
#' iaw$leadseries(x)

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
