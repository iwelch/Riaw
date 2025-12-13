preamble <- c(doc= '
@TITLE vline
@AUTHOR ivo.welch@gmail.com
@DATE 2013
@DESCRIPTION
@USAGE vline (xloc, yloc = rational.domain, ...) 
@ARGUMENTS
@DETAILS
@SEEALSO
@EXAMPLES
', test= '
', changes= '
')


iaw$vline <- function (atxloc, yrange = NULL, ...) 
{
    if (length(yrange) != 2) {
        yrange <- if (par("ylog")) c(9e-99,9e99) else c(-9e99, 9e99)
    }

    for (i in 1:length(atxloc)) {
        lines(c(atxloc[i], atxloc[i]), yrange, ...)
    }
}
