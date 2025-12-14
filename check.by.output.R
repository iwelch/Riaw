
#' BY.CHECK.RECTANGULAR
#'
#' @name by.check.rectangular
#'
#' by can silently return a list with different elements when we want an array.
#'
#' @usage check.by.output(mc.rv)
#'
#' @param mc.rv a return value from a by
#'
#' @return
#'

iaw$by.check.rectangular <- function(mc.rv) {
  numofcols <- (-1)
  for (i in 1:length(mc.rv)) {
    if (is.null(mc.rv[[i]]))
      next
    (!(is.matrix(mc.rv[[i]]) | is.data.frame(mc.rv[[i]]))) %or%
       "iaw-mc.R:check.output: Element {{i}} is not a matrix/dataframe, but a {{class(mc.rv[[i]])}}"
    if (numofcols < 0) {
      numofcols <- ncol(mc.rv[[i]])
      firstnumofcolselement <- mc.rv[[i]]
    }
    if (numofcols < 0)
      next
    if (ncol(mc.rv[[i]]) != numofcols) {
      cat("
iaw-mc.R: check.output problem
", file=stderr())
      cat("First Element was:", file=stderr())
      print(head(firstnumofcolselement))
      cat("Element", i, " was:", file=stderr())
      print(head(mc.rv[[i]]))
      iaw$abort("iaw-mc.R: check.output: Element {{i}} should have {{numofcols}} columns, but has {{ncol(mc.rv[[i]])}} columns instead.  Usually, this means that your function returns different values (e.g., when a group may have N=1 elements)")
    }
  }
}
