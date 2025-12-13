preamble <- c(doc= '
@TITLE shade.xyy
@AUTHOR ivo.welch@gmail.com
@DATE 2013
@DESCRIPTION
@USAGE shade.xyy (x, ylo, yhi, col.plus = "blue", col.minus = "red", 
@ARGUMENTS
@DETAILS
@SEEALSO
@EXAMPLES
', test= '
', changes= '
')

iaw$shade.xyy <- function (x, ylo, yhi, col.plus = "blue", col.minus = "red", col.indif = "gray") 
{
  n <- length(x)
  if (length(ylo)==1) ylo <- rep(ylo, n)
  if (length(yhi)==1) yhi <- rep(yhi, n)
  stopifnot(all(!is.na(x)))
  stopifnot(all(!is.na(ylo)))
  stopifnot(all(!is.na(yhi)))

  for (i in 1:(n - 1)) {
    if ((ylo[i] < yhi[i]) && (ylo[i + 1] < yhi[i + 1])) {
      polygon(c(x[i], x[i + 1], x[i + 1], x[i]),
              c(ylo[i], ylo[i + 1], yhi[i + 1], yhi[i]), col = col.minus, ## for hatch: angle=45, density=20, border = FALSE)
              border = FALSE)
    }
    else if ((ylo[i] > yhi[i]) && (ylo[i + 1] > yhi[i + 1])) {
      polygon(c(x[i], x[i + 1], x[i + 1], x[i]),
              c(ylo[i], ylo[i + 1], yhi[i + 1], yhi[i]), col = col.plus,  ## for hatch:  angle=-45, density=20, col="red",
              border = FALSE)
    }
    else {
#      polygon(c(x[i], x[i + 1], x[i + 1], x[i]),
#              c(ylo[i], ylo[i + 1], yhi[i + 1], yhi[i]), col = col.indif, 
#              border = FALSE)
    }
  }
}
