#' Lead and Lag Time Series
#'
#' Functions for leading and lagging time series data, with proper handling of
#' panel data structures. The lagged/led values are shifted, with NA values
#' filling the gaps.
#'
#' @name leadlagseries
#'
#' @section Functions:
#' \describe{
#'   \item{\code{lagseries}}{Shift series backward (lag): value at t becomes value at t-1}
#'   \item{\code{leadseries}}{Shift series forward (lead): value at t becomes value at t+1}
#'   \item{\code{panelcheck}}{Validate panel data sorting}
#'   \item{\code{seriescheck}}{Validate series input}
#' }
#'
#' @param series A numeric vector, factor, or time series object.
#' @param numlags,numleads Integer number of periods to shift. Negative values
#'   reverse the direction.
#' @param panelid Optional vector identifying panel units (e.g., firm IDs).
#'   When provided, lagged values will not cross panel boundaries.
#' @param timeid Optional vector of time identifiers. Required with \code{panelid}
#'   to verify proper sort order.
#'
#' @return A vector of the same length as \code{series}, with values shifted
#'   and NA values filling the created gaps.
#'
#' @details
#' When \code{panelid} is provided, the data must be sorted by panel ID and then
#' by time within each panel. The functions verify this and will error if the
#' sort order is incorrect.
#'
#' For panel data, lagged values from one panel unit will not "leak" into
#' another panel unit's observations.
#'
#' @export
#'
#' @seealso \code{\link{iaw$chgseries}}, \code{\link{iaw$pchgseries}},
#'   \code{\link{iaw$compoundseries}}, \code{\link{iaw$lagdataframe}}
#'
#' @examples
#' # Simple lag
#' x <- c(1, 2, 3, 4, 5)
#' iaw$lagseries(x)
#' # NA 1 2 3 4
#'
#' iaw$lagseries(x, 2)
#' # NA NA 1 2 3
#'
#' # Simple lead
#' iaw$leadseries(x)
#' # 2 3 4 5 NA
#'
#' # Panel data example
#' d <- data.frame(
#'     value = c(10, 20, 30, 100, 200, 300),
#'     firm = c("A", "A", "A", "B", "B", "B"),
#'     year = c(2001, 2002, 2003, 2001, 2002, 2003)
#' )
#' # Lag within panels (values don't cross firms)
#' iaw$lagseries(d$value, panelid = d$firm, timeid = d$year)
#' # NA 10 20 NA 100 200
#'
#' # Use in regression
#' x <- rnorm(100)
#' xlag <- iaw$lagseries(x)
#' # Estimate AR(1) model
#' coef(lm(x ~ xlag, na.action = na.omit))

#' @rdname leadlagseries
iaw$panelcheck <- function(seriesin, panelid, timeid, errormsg = "unused") {

    if (is.null(panelid)) return(0)
    (is.null(timeid)) %and% "panelcheck: you have a panelid but no timeid"
    (is.null(seriesin)) %and% "panelcheck: you have a panelid but no seriesin"

    (length(seriesin) > 1) %or% "panelcheck: you cannot take a lead or lag of a series of length 1"
    (length(panelid) > 1) %or% "panelcheck: panelid is not series, but {{iaw$whatis(panelid)}}"
    (length(timeid) > 1) %or% "panelcheck: timeid is not series, but {{iaw$whatis(timeid)}}"

    (length(panelid) == length(timeid)) %or%
        "panelcheck: the panelid length {{length(panelid)}} does not match timeid length {{length(timeid)}}"
    (length(timeid) == length(seriesin)) %or%
        "panelcheck: the timeid length {{length(timeid)}} does not match series length {{length(seriesin)}}"

    (is.numeric(timeid)) %or% "panelcheck: timeid must be numeric, not {{class(timeid)}}"
    (any(is.na(panelid))) %and% "panelcheck: panel id must not contain any missing values"
    (any(is.na(timeid))) %and% "panelcheck: time id must not contain any missing values"

    # Check panel sorting
    if (!(all(panelid >= iaw$lagseries(panelid), na.rm = TRUE))) {
        problemids <- head(which(panelid < iaw$lagseries(panelid)), na.rm = TRUE)
        message("panelcheck: Panel is not sorted by panel id. Problem IDs: ")
        print(problemids)
    }

    (all(panelid >= iaw$lagseries(panelid), na.rm = TRUE)) %or%
        "panelcheck: Panel is not sorted by panel id"

    # Check time sorting within panels
    noproblem <- (panelid != iaw$lagseries(panelid)) | (timeid > iaw$lagseries(timeid))

    if (!all(noproblem, na.rm = TRUE)) {
        noproblem <- ifelse(is.na(noproblem), TRUE, noproblem)
        problems <- which(!noproblem, arr.ind = TRUE)
        cat("First problem at index", problems[1], "\n")
        d <- data.frame(seriesin, panelid, timeid)
        print(d[problems[1] + (-1:0), ])
        (FALSE) %or% "{{errormsg}} Panel is not sorted by time within panel. First problem: {{problems[1]}}"
    }
}


#' @rdname leadlagseries
iaw$seriescheck <- function(seriesin) {
    if (!is.null(getOption("strict"))) {
        ((is.vector(seriesin)) | (is.factor(seriesin)) | (is.ts(seriesin))) %or%
            "seriescheck: series must be a vector, factor, or ts, not {{class(seriesin)}}"
        (length(seriesin) > 1) %or% "seriescheck: series needs more than {{length(seriesin)}} observations"
    }
}


iaw$funseries <- function(fun.and.data, seriesin, panelid, timeid) {
    iaw$seriescheck(seriesin)
    (length(seriesin) > fun.and.data$param) %or%
        "leadlagseries: cannot shift {{fun.and.data$param}} periods on series of length {{length(seriesin)}}"
    rv <- with(fun.and.data, fun(series, param))
    if (!is.null(panelid)) {
        iaw$panelcheck(seriesin, panelid, timeid)
        rv <- ifelse(panelid != with(fun.and.data, fun(panelid, param)), NA, rv)
    }
    rv
}


#' @rdname leadlagseries
#' @export
iaw$lagseries <- function(series, numlags = 1, panelid = NULL, timeid = NULL) {
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


#' @rdname leadlagseries
#' @export
iaw$leadseries <- function(series, numleads = 1, panelid = NULL, timeid = NULL) {
    iaw$funseries(
        list(
            fun = function(series, numleads) {
                if (numleads == 0) return(series)
                if (numleads < 0) return(iaw$lagseries(series, -numleads, panelid, timeid))
                x <- series[(1 + numleads):(length(series))]
                # BUG FIX: was is.factor(numleads), should be is.factor(x)
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
