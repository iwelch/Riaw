preamble <- c(doc= '
@TITLE text.bf
@AUTHOR ivo.welch@gmail.com
@DATE 2022
@DESCRIPTION
@USAGE test.bg (... , bgcol= )
@ARGUMENTS
@DETAILS
@SEEALSO
@EXAMPLES
', test= '
', changes= '
')

iaw$text.bg <- function( x, y, labels, bgcol="white", ... ) {
    w <- strwidth( labels )
    h <- strheight( labels )
    buf <- 0.1
    rect( xleft= x - 0.5*w - buf, ybottom= y - 0.5*h - buf,
         xright= x + 0.5*w + buf, ytop= y + 0.5*h + buf,
         col = bgcol, border = NA)
    text(x = x, y = y, labels = labels, adj = c(0.5, 0.5), ...)
}
