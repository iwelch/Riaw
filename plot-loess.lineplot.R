#' Loess Smoothed Line Plot
#'
#' @name plot.loess.lineplot
#'
#' Plots loess-smoothed line.
#'
#' @param x X values.
#' @param y Y values.
#' @param lo default number of points for which to draw
#' @param span Smoothing span.
#' @param ... Plot arguments.
#'
#' @return Invisible NULL.
#'
#' @family plotting
#' @export

iaw$plot.loess.lineplot <- function( x, y, lo=200, span=0.75, mean.hatch=NA, ... ) {
  (is.numeric(x)) %or% stop("sorry, x is not numeric")
  (is.numeric(y)) %or% stop("sorry, y is not numeric")
  (is.vector(x)) %or% stop("sorry, x is not a vector")
  (is.vector(y)) %or% stop("sorry, y is not a vector")
  d <- data.frame( y, x )
  d <- d[complete.cases(d),]
  d <- d[order(d$x),]
  ll <- loess( y ~ x, span=span, data=d )
  xnew <-  with(d, seq( min(x),max(x),length.out=lo ))
  ynew <- predict( ll, newdata=xnew )
  lines(xnew, ynew, ...)
  if (!is.na(mean.hatch)) {
    xmean <- mean.hatch
    y.at.xmean <- predict( ll, newdata=c(xmean) )
    xd <- with(d, (max(x)-min(x))/50)
    yd <- with(d, (max(y)-min(y))/50)
    lines( c(xmean, xmean), c(y.at.xmean-yd, y.at.xmean+yd), ... )
    lines( c(xmean-xd, xmean+xd), c(y.at.xmean, y.at.xmean), ... )
  }
  invisible( data.frame( xnew, ynew ) )
}
