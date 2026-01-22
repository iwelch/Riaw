#' Nice correlation printout
#'
#' @name print.cor
#'
#' prints nice correlation output
#'
#' @param d either a data frame or a matrix
#' @param digits number of significant digits.
#'
#' @return an invisible correlation
#'
#' @family statistics
#' @export
#'
#' @examples
#' x <- sin(1:50) + rnorm(50, sd = 0.1)
#' y <- sin(2:51) + rnorm(50, sd = 0.1)
#' iaw$cor( data.frame(x,y) )


iaw$print.cor <- function(d, digits = 2, use =  "pairwise.complete.obs") {
  stopifnot(is.data.frame(d) | is.matrix(d))

  nums <- sapply(d, is.numeric)
  cm <- cor(d[, nums], use = use)

  oold <- options(knitr.kable.NA = "")
  cm[upper.tri(cm, diag = TRUE)] <- NA
  diag(cm) <- 1

  print(knitr::kable(cm, digits = digits), na.print = "")
  options( oold )
  invisible(cm)
}
