preamble <- c(doc= '
@TITLE loess.lineplot
@AUTHOR ivo.welch@gmail.com
@DATE 2013
@DESCRIPTION
@USAGE loss.lineplot (x, y, lo=200, ... )
@ARGUMENTS
@DETAILS
@SEEALSO
@EXAMPLES
x <- rnorm(100)
y <- x^2+sin(x)*x+rnorm(100)
plot( x, y )
iaw$loess.lineplot( x, y )
', test= '
', changes= '
')

iaw$loess.lineplot <- function( x, y, lo=200, span=0.75, mean.hatch=NA, ... ) {
  (is.numeric(x)) %or% "sorry, x is not numeric"
  (is.numeric(y)) %or% "sorry, y is not numeric"
  (is.vector(x)) %or% "sorry, x is not a vector"
  (is.vector(y)) %or% "sorry, y is not a vector"
  d <- data.frame( y, x )
  d <- d[complete.cases(d),]
  d <- d[order(d$x),]
  ll <- loess( y ~ x, span=span, data=d )
  xnew <-  with(d, seq( min(x),max(x),length.out=lo ))
  lines(xnew, predict( ll, newdata=xnew ), ...)
  if (!is.na(mean.hatch)) {
    xmean <- mean.hatch
    y.at.xmean <- predict( ll, newdata=c(xmean) )
    xd <- with(d, (max(x)-min(x))/50)
    yd <- with(d, (max(y)-min(y))/50)
    lines( c(xmean, xmean), c(y.at.xmean-yd, y.at.xmean+yd), ... )
    lines( c(xmean-xd, xmean+xd), c(y.at.xmean, y.at.xmean), ... )
  }
}
