#' Loess Smoothed Line Plot
#'
#' @name plot.loess.lineplot
#'
#' Plots loess-smoothed line.
#'
#' @param x Numeric vector of x values.
#' @param y Numeric vector of y values.
#' @param lo Number of points for the smoothed line (default 200).
#' @param span Smoothing span for \code{loess} (default 0.75).
#' @param mean.hatch X-value at which to draw a cross-hatch mark, or \code{NA}
#'   to skip (default \code{NA}).
#' @param ... Additional arguments passed to \code{lines}.
#'
#' @return Data frame of smoothed x and y values (invisibly).
#'
#' @examples
#' \dontrun{
#' # Smooth a noisy sine wave and overlay on scatter plot
#' set.seed(1)
#' x <- runif(100, 0, 2 * pi)
#' y <- sin(x) + rnorm(100, sd = 0.3)
#' plot(x, y, pch = 16, col = "gray70", main = "Loess smoother")
#' iaw$plot.loess.lineplot(x, y, col = "blue", lwd = 2)
#'
#' # Add a cross-hatch mark at the mean of x
#' plot(x, y, pch = 16, col = "gray70")
#' iaw$plot.loess.lineplot(x, y, mean.hatch = pi, col = "red", lwd = 2)
#'
#' # Tighter span for less smoothing
#' plot(x, y, pch = 16, col = "gray70")
#' iaw$plot.loess.lineplot(x, y, span = 0.3, col = "darkgreen", lwd = 2)
#' }
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
