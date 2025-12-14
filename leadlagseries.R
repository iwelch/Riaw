#' Lead and Lag Time Series Support Functions
#'
#' @name leadlagseries
#'
#' Support functions for panel-aware time series operations.
#'
#' @param seriesin Numeric vector.
#' @param panelid Optional panel identifier vector.
#' @param timeid Optional time identifier vector.
#' @param errormsg Error message for validation failures.
#' @param fun.and.data List with function, parameter, and series.
#'
#' @return Various depending on function.
#'
#' @family time-series
#' @keywords internal

iaw$panelcheck <- function(seriesin, panelid, timeid, errormsg = "unused") {
    if (is.null(panelid)) return(0)
    
    stopifnot(!is.null(timeid))
    stopifnot(!is.null(seriesin))
    stopifnot(length(seriesin) > 1L)
    stopifnot(length(panelid) > 1L)
    stopifnot(length(timeid) > 1L)
    stopifnot(length(panelid) == length(timeid))
    stopifnot(length(timeid) == length(seriesin))
    stopifnot(is.numeric(timeid))
    stopifnot(!any(is.na(panelid)))
    stopifnot(!any(is.na(timeid)))
    
    if (!(all(panelid >= iaw$lagseries(panelid), na.rm = TRUE))) {
        problemids <- head(which(panelid < iaw$lagseries(panelid)), na.rm = TRUE)
        message("Panel not sorted by panel id. Problem IDs: ")
        print(problemids)
    }
    
    (all(panelid >= iaw$lagseries(panelid), na.rm = TRUE)) %or%
        "Panel is not sorted by panel id"
    
    noproblem <- (panelid != iaw$lagseries(panelid)) | (timeid > iaw$lagseries(timeid))
    
    if (!all(noproblem, na.rm = TRUE)) {
        noproblem <- ifelse(is.na(noproblem), TRUE, noproblem)
        problems <- which(!noproblem, arr.ind = TRUE)
        cat("First problem at index", problems[1], "\n")
        d <- data.frame(seriesin, panelid, timeid)
        print(d[problems[1] + (-1:0), ])
        (FALSE) %or% "Panel not sorted by time within panel"
    }
}

iaw$seriescheck <- function(seriesin) {
    if (!is.null(getOption("strict"))) {
        stopifnot(is.vector(seriesin) || is.factor(seriesin) || is.ts(seriesin))
        stopifnot(length(seriesin) > 1L)
    }
}

iaw$funseries <- function(fun.and.data, seriesin, panelid, timeid) {
    iaw$seriescheck(seriesin)
    stopifnot(length(seriesin) > fun.and.data$param)
    rv <- with(fun.and.data, fun(series, param))
    if (!is.null(panelid)) {
        iaw$panelcheck(seriesin, panelid, timeid)
        rv <- ifelse(panelid != with(fun.and.data, fun(panelid, param)), NA, rv)
    }
    rv
}
