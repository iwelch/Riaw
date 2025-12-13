
iaw$text.onbg <- function( x, y, s, bgcol="white", ... ) {
    w <- strwidth(s)
    h <- strheight(s)
    buf <- 0.1
    rect(xleft  = x - 0.5 * w - buf, ybottom = y - 0.5 * h - buf,
         xright = x + 0.5 * w + buf, ytop    = y + 0.5 * h + buf,
         col = bgcol, border = "white", lwd=3)
    text(x = x, y = y, labels = s, adj = c(0.5, 0.5), ...)
}
