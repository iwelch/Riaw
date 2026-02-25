#' Lag Data Frame Columns
#'
#' @name lagdataframe
#'
#' Creates lagged versions of data frame columns.
#'
#' @param df Data frame.
#' @param vars Variables to lag.
#' @param nlags Number of lags.
#' @param panelid Panel identifier.
#' @param timeid Time identifier.
#'
#' @return Data frame with lagged columns.
#'
#' @examples
#' df <- data.frame(t = 1:5, ret = c(0.01, 0.02, -0.01, 0.03, 0.00))
#'
#' # Add a one-period lag of "ret" (new column "ret.L1")
#' iaw$lagdataframe(df, vars = "ret")
#'
#' # Add a two-period lag
#' iaw$lagdataframe(df, vars = "ret", nlags = 2)
#'
#' # Lag multiple columns at once
#' df2 <- data.frame(t = 1:4, ret = 1:4, vol = 10:13)
#' iaw$lagdataframe(df2, vars = c("ret", "vol"))
#'
#' @family time-series
#' @export

iaw$lagdataframe <- function(df, vars, nlags = 1, panelid = NULL, timeid = NULL) {
    stopifnot(is.data.frame(df))
    stopifnot(is.character(vars))
    stopifnot(all(vars %in% names(df)))
    
    for (v in vars) {
        newname <- paste0(v, ".L", nlags)
        df[[newname]] <- iaw$lagseries(df[[v]], nlags, panelid, timeid)
    }
    df
}
