preamble <- c(doc= '
@TITLE ecdf.lineplot
@AUTHOR ivo.welch@gmail.com
@DATE 2013
@DESCRIPTION
@USAGE ecdf.lineplot(x)
@ARGUMENTS
@DETAILS
@SEEALSO
@EXAMPLES

plot( 0, type="n", xlim=c(-3,3), ylim=c(0,1) )
ecdf.lineplot(rnorm(100), col="gray")

', test= '
', changes= '
')


iaw$ecdf.lineplot <- function (x, ...) {
  x <- sort(x)
  n <- length(x)
  if (n < 1) stop("'x' must have 1 or more non-missing values")
  vals <- unique(x)
  y <- cumsum(tabulate(match(x, vals)))/n
  x <- vals
  iaw$loess.lineplot( x, y, span=0.01, lo=1000, mean.hatch=mean(x), ... )
}
