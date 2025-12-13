
#' Rename List Names or Columns of Data Frame
#'

iaw$grepcolname <- function(d.or.n, name) {
  nms <- if (is.data.frame(d.or.n)) names(d.or.n) else d.or.n
  nms[grepl(name, nms)]
}
