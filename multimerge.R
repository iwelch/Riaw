#' Merge Multiple Data Frames
#'
#' @name multimerge
#'
#' Merges list of data frames.
#'
#' @param dflist List of data frames.
#' @param by Merge keys.
#' @param ... Additional merge arguments.
#'
#' @return Merged data frame.
#'
#' @family data-reshaping
#' @export

iaw$multimerge <- function(dflist, by, ...) {
    stopifnot(is.list(dflist))  ## list of data frames
    stopifnot(length(dflist) > 1)

    stopifnot(all(sapply(dflist, is.data.frame)))

    Reduce(function(x, y) merge(x, y, by = by, ...), dflist)
}
