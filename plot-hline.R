preamble <- c(doc= '
@TITLE hline
@AUTHOR ivo.welch@gmail.com
@DATE 2013
@DESCRIPTION
@USAGE hline (yloc, xloc = rational.domain, ...) 
@ARGUMENTS
@DETAILS
@SEEALSO
@EXAMPLES
', test= '
', changes= '
')

iaw$hline <- function (atyloc, xrange = NULL, ...) 
{
    if (length(xrange) != 2) {
        xrange <- if (par("xlog")) c(9e-99,9e99) else c(-9e99, 9e99)
    }

    for (i in 1:length(atyloc)) {
        lines(xrange, c(atyloc[i], atyloc[i]), ...)
    }
}
