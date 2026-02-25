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
#' @examples
#' d1 <- data.frame(id = 1:3, x = c(10, 20, 30))
#' d2 <- data.frame(id = 1:3, y = c(40, 50, 60))
#' d3 <- data.frame(id = 1:3, z = c(70, 80, 90))
#'
#' # Merge three data frames on a common key
#' iaw$multimerge(list(d1, d2, d3), by = "id")
#'
#' # Works with any number of frames; all.x = TRUE for left join behaviour
#' d4 <- data.frame(id = c(1, 2), w = c(100, 200))
#' iaw$multimerge(list(d1, d2, d4), by = "id", all.x = TRUE)
#'
#' @family data-reshaping
#' @export

iaw$multimerge <- function(dflist, by, ...) {
    stopifnot(is.list(dflist))  ## list of data frames
    stopifnot(length(dflist) > 1)

    stopifnot(all(sapply(dflist, is.data.frame)))

    Reduce(function(x, y) merge(x, y, by = by, ...), dflist)
}
