
#' WINSORIZE.LEVEL
#'
#' @name winsorize.level
#'
#'  winsorizes a single x vector at predetermined levels
#'
#' @usage winsorize.level (x, xmin, xmax = NULL, name = NULL)
#'
#' @param x: the vector to be winsorized
#' @param xmin: either a number or a 2-length vector.
#' @param xmax: ignored if xmin is a 2-length vector.
#' @param name: for debugging, allows you to name the variable and then prints the request
#'
#' @return a winsorized vector
#'
#' @seealso winsorize.percentile
#'


iaw$winsorize.level <- function (x, xmin, xmax = NULL, name = NULL, verbose=F) {
  if (length(xmin) == 2) {
    xmax <- xmin[2]
    xmin <- xmin[1]
  }
  (!is.null(xmax)) %or% "winsorize.level needs a max argument\n"
  (all(xmin <= xmax)) %or% "winsorize.level range error {{head(xmin)}} > {{head(xmax)}}"
  ## "xmax for at least one obs", which(xmin > xmax))
  (is.vector(x)) %or% "winsorize.level data error: x is not a scalar or vector: {{class(x)}}"
  if (verbose) {
      if (!is.null(name)) name <- "unknown"
      cat("[winsorize.level]", name, ": ", length(x), "vector with ", (sum(!is.na(x))), "non-na observations: ")
      tb <- sum((x < xmin), na.rm = T)
      tt <- sum((x > xmax), na.rm = T)
      cat("Bottom[ <", xmin, "]: N=", tb, "Top[ >", xmax, "]: N=", tt, "\n")
  }
  return(pmin(pmax(x, xmin), xmax))
}
