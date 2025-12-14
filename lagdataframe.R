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
