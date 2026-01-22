## #8 check.by.output.R

#' Validate by() Output is Rectangular
#'
#' Checks that all elements of a by/mc.by result have same number of columns.
#' Catches silent failures where by() returns ragged lists.
#'
#' @param mc.rv Return value from by() or mc.by().
#'
#' @return Invisible TRUE if valid, otherwise stops with diagnostic.
#'
#' @family utilities
#' @export

iaw$by.check.rectangular <- function(mc.rv) {
  stopifnot(is.list(mc.rv))

  ncols <- NULL
  first.idx <- NULL
  first.el <- NULL

  for (i in seq_along(mc.rv)) {
    el <- mc.rv[[i]]
    if (is.null(el)) next

    if (!is.matrix(el) && !is.data.frame(el))
      stop(sprintf("Element %d is not a matrix/data.frame but a %s",
                   i, class(el)[1]))

    nc <- ncol(el)

    if (is.null(ncols)) {
      ncols <- nc
      first.idx <- i
      first.el <- el
    } else if (nc != ncols) {
      cat("\nby.check.rectangular: column mismatch\n", file = stderr())
      cat("First element (index", first.idx, ") had", ncols, "cols:\n", file = stderr())
      print(head(first.el, 3))
      cat("\nElement", i, "has", nc, "cols:\n", file = stderr())
      print(head(el, 3))
      stop(sprintf("Element %d has %d columns, expected %d. Usually means function returns different values for some groups (e.g., N=1).",
                   i, nc, ncols))
    }
  }

  invisible(TRUE)
}
