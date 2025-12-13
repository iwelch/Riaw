
#' MULTIMERGE
#'
#' @name multimerge
#'
#' Merge multiple data frames by a common variable
#'
#' @usage multimerge (list.of.df, by, suffixlist = NULL, ...)
#'
#' @param list.of.df a list of data frames
#' @param by the common columns by which they should be merged
#'
#' @return a data frame that combines all of the above
#'


iaw$multimerge <- function (list.of.df, by, suffixlist = NULL, ...) {
  for (i in 1:length(list.of.df)) {
      (is.data.frame(list.of.df[[i]])) %or% "sorry, but list member {{i}} is not a data frame"
      (exists(by[1], list.of.df[[i]])) %or% "sorry, but variable {{by}} does not exist in data frame list {{i}}"
      if (i == 1) {
          d <- list.of.df[[i]]
          next
      }
      d <- if (is.null(suffixlist)) merge(d, list.of.df[[i]], by = by, all = "T", ...) else merge(d, list.of.df[[i]], by = by, all = "T", suffix = c((if (i == 2) suffixlist[1] else ""), suffixlist[i]), ...)
  }
  d
}
