preamble <- c(doc= '
@TITLE as.PlotDate.yyyymmdd
@AUTHOR ivo.welch@gmail.com
@DATE 2013
@DESCRIPTION

an easier to remember wrapper that converts yyyymmdd dates into plottable values.
actually, the output are a seconds-based representation of time.

@USAGE as.PlotDate.yyyymmdd(yyyymmdd)
@ARGUMENTS
@DETAILS
@SEEALSO unix2POSIXct as.Date as.POSIXlt
@EXAMPLES
   plot( as.PlotDate.yyyymmdd( (198001:198012)*100+15, 1:12 ) )
   plot( as.PlotDate.yyyymmdd( (198001:198012)*100+15, 1:12 ) )
', test= '
', changes= '
')

iaw$as.PlotDate.yyyymmdd <- function (yyyymmdd) {
  as.POSIXct(strptime(yyyymmdd, "%Y%m%d", tz="UTC"))  ## need UTC or differences are weird due to DST
}
