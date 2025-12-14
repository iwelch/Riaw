#' Lead/Lag Panel Data
#'
#' @name leadlagpanel
#'
#' Creates lead/lag variables for panel data.
#'
#' @param df Data frame.
#' @param vars Variables to transform.
#' @param nlags Number of lags (negative for leads).
#' @param panelid Panel ID column name.
#' @param timeid Time column name.
#'
#' @return Data frame with new columns.
#'
#' @family time-series
#' @export

iaw$leadlagpanel <- function(df, vars, nlags, panelid, timeid) {
    stopifnot(is.data.frame(df))
    stopifnot(is.character(vars))
    stopifnot(is.numeric(nlags), length(nlags) == 1L)
    stopifnot(is.character(panelid), length(panelid) == 1L)
    stopifnot(is.character(timeid), length(timeid) == 1L)
    
    for (v in vars) {
        suffix <- if (nlags >= 0) paste0(".L", nlags) else paste0(".F", abs(nlags))
        df[[paste0(v, suffix)]] <- iaw$lagseries(df[[v]], nlags, 
                                                  df[[panelid]], df[[timeid]])
    }
    df
}
