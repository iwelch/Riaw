
#' Rename List Names or Columns of Data Frame
#'
#' @name recode
#'
#' @examples d
#'
#' change the names of the variables in a data frame, either in list form or vector form
#'
#' @usage d <- rename.columns(d, from, to =NULL)
#' @usage names(d) <- rename.columns(names(d), from, to =NULL)
#'
#' @param d.or.n a names vector or a data frame
#' @param from a character string vector.
#' @param to an equal-sized character string vector
#'
#' If to is left empty, then the 'from' vector  must have both names and values.  see the example
#'
#' @return the same data type as d.o.rn
#'
#' @seealso iaw$recode
#'
#' @examples d <- rename.columns( d, c(a=x, b=y) )
#'

iaw$rename.columns <- function (d.or.n, from, to =NULL) {
  nms <- if (is.data.frame(d.or.n)) names(d.or.n) else d.or.n

  if (is.null(to)) { to <- as.character(from); from <- names(from) }

  ## maybe I should rewrite this using function 'recode'
  for (i in 1:length(from)) nms[nms == from[i]] <- to[i]
  if (is.data.frame(d.or.n)) {
    names(d.or.n) <- as.character(nms)
    return(d.or.n)
  }
  else return(as.character(nms))
}

iaw$rename.column <- iaw$rename.columns
