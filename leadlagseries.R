#' Lead and Lag Time Series Support Functions
#'
#' @name leadlagseries
#'
#' Internal support functions for panel-aware time series operations.
#' \code{panelcheck} validates panel sort order.
#' \code{seriescheck} validates series input.
#' \code{funseries} applies a lag/lead function respecting panel boundaries.
#'
#' @param seriesin Numeric vector.
#' @param panelid Optional panel identifier vector.
#' @param timeid Optional time identifier vector.
#' @param errormsg Error message for validation failures (default \code{"unused"}).
#'
#' @return \code{panelcheck}: 0 (invisibly) if no panel, or stops on error.
#'   \code{seriescheck}: \code{NULL} (invisibly).
#'   \code{funseries}: transformed numeric vector.
#'
#' @examples
#' \dontrun{
#' # panelcheck: validate that a panel is sorted by id then time
#' series  <- c(1, 2, 3, 4, 5, 6)
#' firm    <- c("A", "A", "A", "B", "B", "B")
#' time    <- c(1, 2, 3, 1, 2, 3)
#' iaw$panelcheck(series, firm, time)  # returns 0 silently when valid
#'
#' # funseries: apply a lag function respecting panel boundaries
#' lag_fun <- list(fun = function(x, p) c(rep(NA, p), head(x, -p)), param = 1,
#'                 series = series)
#' iaw$funseries(lag_fun, series, firm, time)
#' }
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
        problemids <- head(which(panelid < iaw$lagseries(panelid)))
        message("Panel not sorted by panel id. Problem IDs: ")
        print(problemids)
    }

    (all(panelid >= iaw$lagseries(panelid), na.rm = TRUE)) %or%
      stop("Panel is not sorted by panel id")

    noproblem <- (panelid != iaw$lagseries(panelid)) | (timeid > iaw$lagseries(timeid))

    if (!all(noproblem, na.rm = TRUE)) {
        noproblem <- ifelse(is.na(noproblem), TRUE, noproblem)
        problems <- which(!noproblem, arr.ind = TRUE)
        cat("First problem at index", problems[1], "\n")
        d <- data.frame(seriesin, panelid, timeid)
        print(d[problems[1] + (-1:0), ])
        (FALSE) %or% stop("Panel not sorted by time within panel")
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
