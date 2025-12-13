preamble <- c(doc= '
@TITLE yearly.tickmarks
@AUTHOR ivo.welch@gmail.com
@DATE 2013
@DESCRIPTION
@USAGE yearly.tickmarks (yyyymm, decades = 0) 
@ARGUMENTS
@DETAILS
@SEEALSO
@EXAMPLES
', test= '
', changes= '
')

iaw$yearly.tickmarks <- function (yyyymm, decades = 0) 
{
  tr <- if (decades) 1000 else 100
  mi <- yyyymm
  yi <- as.integer(mi/tr)
  yl <- iaw$lagseries(yi)
  newyear <- (yi != yl)
  tickloc <- (1:length(mi))[newyear]
  tickval <- yi[newyear]
  if (decades) tickval <- tickval * 10
  return(list(ticklocs = tickloc, tickvals = tickval))
}
