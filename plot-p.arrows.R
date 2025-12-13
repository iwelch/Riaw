preamble <- c(doc= '
@TITLE p.arrows
@AUTHOR ivo.welch@gmail.com
@DATE 2013
@DESCRIPTION
@USAGE p.arrows (x1, y1, x2, y2, size = 0.75, width = (sqrt(5) - 1)/4/cin, 
@ARGUMENTS
@DETAILS
@SEEALSO
@EXAMPLES
', test= '
', changes= '
')

iaw$p.arrows <- function (x1, y1, x2, y2, size = 0.75, width = (sqrt(5) - 1)/4/cin, 
          col = "black", backward.compatibility= FALSE, ...) 
{
    if (!backward.compatibility) stop("please install shape library (and then use Arrows() instead")

  cin <- size * par("cin")[2]
  uin <- 1/xyinch()
  segments(x1, y1, x2, y2, col = col, ...)
  x <- sqrt(seq(0, cin^2, length = floor(35 * cin) + 2))
  delta <- 0.005/2.54
  x.arr <- c(-x, -rev(x))
  wx2 <- width * x^2
  y.arr <- c(-wx2 - delta, rev(wx2) + delta)
  deg.arr <- c(atan2(y.arr, x.arr), NA)
  r.arr <- c(sqrt(x.arr^2 + y.arr^2), NA)
  theta <- atan2((y2 - y1) * uin[2], (x2 - x1) * uin[1])
  lx <- length(x1)
  Rep <- rep.int(length(deg.arr), lx)
  x2 <- rep.int(x2, Rep)
  y2 <- rep.int(y2, Rep)
  theta <- rep.int(theta, Rep) + rep.int(deg.arr, lx)
  r.arr <- rep.int(r.arr, lx)
  polygon(x2 + r.arr * cos(theta)/uin[1], y2 + r.arr * sin(theta)/uin[2], 
          col = col, ...)
}
