
#' WINSORIZE.PERCENTILE
#'
#' @name winsorize.percentile
#'
#'  winsorize a vector by a percentile
#'
#' @usage winsorize.percentile (x, xperc.min = 0.01, xperc.max = 0.99, name = NULL)
#'
#' @param x a vector to winsorize
#' @param xperc.min the minimum percentile, often 1% or 5%
#' @param xperc.max the maximum percentile, often 95% or 99%
#'
#' @return a winsorized vector
#'
#' @seealso winsorize.level


iaw$winsorize.percentile <- function (x, xperc.min = 0.01, xperc.max = 0.99, verbose= FALSE, name = "") {

  if (length(xperc.min) == 2) {
      xperc.max <- xperc.min[2]
      xperc.min <- xperc.min[1]
  }

  (is.numeric(x)) %or% "x needs to be a vector"
  (is.vector(x)) %or% "x is not a vector: it is a {{class(x)}}\n"
  (xperc.min < xperc.max) %or% "xmin {{xperc.min}} < {{xperc.max}}"
  (xperc.min < 0.75) %or% "Choose a lower minimum percentile, not {{xperc.min}}"
  (xperc.max > 0.25) %or% "Choose a higher maximum percentile, not {{xperc.max}}"
  x.nona <- x[!is.na(x)]
  lowest.set <- x.nona[rank(x.nona) < xperc.min * length(x.nona)]
  highest.set <- x.nona[rank(x.nona) > xperc.max * length(x.nona)]
  winsor.level.xperc.min <- if (length(lowest.set) == 0) (-Inf) else max(lowest.set)

  winsor.level.xperc.max <- if (length(highest.set) == 0) (+Inf) else min(highest.set)
  if (!is.null(name)) name <- paste("\\%", name)
  if (verbose) cat("Winsorized ", name, ": [", xperc.min, "=", winsor.level.xperc.min, " , ", xperc.max, "=", winsor.level.xperc.max, "]\n")
  iaw$winsorize.level(x, winsor.level.xperc.min, winsor.level.xperc.max, name = name)
}
