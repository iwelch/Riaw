
#' LEADLAGSERIES
#'
#' @name leadlagseries
#'
#'   "leadlagseries" takes a vector and shift its contents numleadlags items to the left, filling in appropriate missing values to retain the length of the vector.  If panelid is named, then lagged value from another panelid will not be assigned to be the lag.  (Usually, the panelid will be the firm id, and the panel must be sorted by firmid. Naturally, it makes little sense to use this unless the observations are also sorted by the time of the observation.  This is, after all, a lagseries function.)
#'
#'  @details None
#'
#'  @usage leadlagseries(seriesin, direction, numleadlags =1, panelid=NULL)
#'
#'  @param seriesin: a numeric vector.
#' 	numleadlags: an integer, can be negative
#' 	direction: either "lead" or "lag",
#' 	panelid: an optional panel id
#'
#'  @return
#'
#'  @seealso chgseries, compoundseries, lagdataframe
#'  @examples
#' 	x <- rnorm(10)
#' 	xlag <- lagseries(x,2)
#' 	lm( x ~ xlag )
#' 	d <- data.frame( x <- c( rnorm(20), runif(30), rcauchy(40) ),
#' 	                 who= c( rep("firm1",20), rep("firm2", 30), rep("firm3",40)),
#' 	                 year= c( 1961:1980, 1971:2000, 1971:2010 ) )
#' 	lagd <- data.frame( x=lagseries(d$x, panelid=who), who=d$who, year=lagseries(d$x, panelid=who) )
#'


iaw$panelcheck <- function( seriesin, panelid, timeid, errormsg="unused" ) {

    if (is.null(panelid)) return(0)  ## ok
    (is.null(timeid)) %and% "panelcheck: you have a panelid but no timeid"  ## ok
    (is.null(seriesin)) %and% "panelcheck: you have a panelid but no seriesin"  ## ok

    (length(seriesin)>1) %or% "panelcheck: you cannot take a lead or lag of a series of length 1"
    (length(panelid)>1) %or% "panelcheck: panelid is not series, but {{iaw$whatis(panelid)}}"
    (length(timeid)>1) %or% "panelcheck: timeid is not series, but {{iaw$whatis(timeid)}}"

    (length(panelid) == length(timeid)) %or% "panelcheck: the panelid length p{{length(panelid)}} does not have the length of the timeid {{length(timeid)}}. did you give a name instead of a series?"
    (length(timeid) == length(seriesin)) %or% "panelcheck: the timeid length p{{length(timeid)}} does not have the length of the series {{length(seriesin)}}. did you give a name instead of a series?"

    (is.numeric(timeid)) %or% "panelcheck: sorry, but timeid is not a numeric vector, but a {{class(timeid)}}\n";
    ##  (is.numeric(panelid)) %or% "panelcheck: sorry, but panelid is not a numeric vector, but a {{class(panelid)}}\n";
    (any(is.na(panelid))) %and% "panelcheck: panel id must not contain any missing values in panel data\n";
    (any(is.na(timeid))) %and% "panelcheck: time id must not contain any missing values in panel data\n";

    ## ok, we have a panel data set.  here, we want to be really cautious and force extra checks
    if (!(all(panelid >= iaw$lagseries(panelid), na.rm = TRUE))) {
        message("panelcheck: Panel is not sorted up by panel id: {{head(which(panelid < iaw$lagseries(panelid)), na.rm=T))}")
        problemids <- head(which(panelid < iaw$lagseries(panelid)), na.rm=T)
        message("Problems on Panel IDs: ")
        print( problemids )
        message("Containing Values of the Series: ")
        print( seriesin[ problemids ]  )
    }

    (all(panelid >= iaw$lagseries(panelid), na.rm = TRUE)) %or%
        "panelcheck: Panel is not sorted up by panel id: {{head(which(panelid < iaw$lagseries(panelid)), na.rm=T))}"

    (is.null(timeid)) %and% "panelcheck: in leadlagseries, for panel data, we require a timeid also for checking proper sort order"

    noproblem <- (panelid != iaw$lagseries(panelid) ) | (timeid>iaw$lagseries(timeid))

    if (! all( noproblem, na.rm=TRUE ) ) {
        noproblem <- ifelse( is.na(noproblem), TRUE, noproblem )
        problems <- which(!noproblem, arr.ind=TRUE)
        cat("The first problem is at index ", problems[1], ", with id as follows:\n")
        ## sadly, we do not know the name of d$seriesin...
        d <- data.frame( seriesin, panelid, timeid )
        print(d[problems[1]+(-1:0),])
        (FALSE) %or% "{{errormsg}} Panel is sorted by panel id, but not by time id.  First problem Obs= {{ problems[1] }}\n"
    }

}


iaw$seriescheck <- function( seriesin ) {
    if (!is.null(getOption("strict"))) {
        ((is.vector(seriesin))|(is.factor(seriesin))|(is.ts(seriesin))) %or% "seriescheck: Your series is not a vector, but a {{class(seriesin)}} --- do not give just name; needs series itself."
        (length(seriesin) > 1) %or% "seriescheck: needs more observations than {{length(seriesin)}}"
    }
}



iaw$funseries <- function( fun.and.data, seriesin, panelid, timeid ) {
    iaw$seriescheck( seriesin )
    (length(seriesin) > fun.and.data$param) %or% "leadlagseries: cannot leadlag {{fun.and.data$param}} leads/lags on series of length {{length(seriesin)}}"
    rv <- with(fun.and.data, fun( series, param ))
    if (!is.null(panelid)) {
        iaw$panelcheck( seriesin, panelid, timeid )
        rv <- ifelse( panelid != with(fun.and.data, fun( panelid, param )) , NA, rv)
    }
    rv
}



iaw$lagseries <- function( series, numlags = 1, panelid = NULL, timeid = NULL )
    iaw$funseries(
            list(
                fun= function( series, numlags ) {
                    if (numlags == 0) return( series )
                    if (numlags < 0) return( iaw$leadseries( series, -numlags, panelid, timeid ))
                    x <- series[1:(length(series) - numlags)]
                    if (is.factor(x)) factor( c( levels(x), rep(NA, numleads))[x] ) else c(x, rep(NA, numleads))
                },
                param= numlags,
                series= series
            ),
            series, panelid, timeid )


iaw$leadseries <- function( series, numleads = 1, panelid = NULL, timeid = NULL )
    iaw$funseries(
            list(
                fun= function( series, numleads ) {
                    if (numleads == 0) return( series )
                    if (numleads < 0) return( iaw$lagseries( series, -numleads, panelid, timeid ) )
                    x <- series[(1+numleads):(length(series))]
                    if (is.factor(numleads)) factor( c( levels(x), rep(NA, numleads))[x] ) else c(x, rep(NA, numleads))
                },
                param= numleads,
                series= series
            ),
            series, panelid, timeid )

