
#' CMPSUM
#'
#' @name cmpsum
#'
#' cumulate a rate of return series and return only the final value.
#'
#' An optional parameter allows to convert to a frequency.  For example, if the inputs are monthly returns, adding MPY=12 (months per year) will give annualized values.  MPY=0 means just raw numbers, and is a standing for MPY=length(x)
#'
#' @param rseries  the rates of return
#'
#' @return the cumulated rate of return
#'
#' @seealso compoundseries
#'
#' @aliases compsum, geomean


iaw$cmpsum <- function( rseries, MPY =0 ) exp(ifelse(MPY<=0, 1, MPY/length(rseries))*sum(log(1+rseries)))-1

iaw$compsum <- iaw$cmpsum

iaw$geomean <- function(x) iaw$cmpsum(x, MPY=1)
