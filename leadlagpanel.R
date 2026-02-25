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
#' @examples
#' panel <- data.frame(
#'   firm = c("A","A","A","B","B","B"),
#'   t    = c(1, 2, 3, 1, 2, 3),
#'   ret  = c(0.01, 0.02, 0.03, 0.10, 0.11, 0.12)
#' )
#'
#' # One-period lag within each firm (adds column "ret.L1")
#' iaw$leadlagpanel(panel, vars = "ret", nlags = 1,
#'                  panelid = "firm", timeid = "t")
#'
#' # One-period lead (negative nlags; adds column "ret.F1")
#' iaw$leadlagpanel(panel, vars = "ret", nlags = -1,
#'                  panelid = "firm", timeid = "t")
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
