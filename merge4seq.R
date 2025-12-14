#' Merge for Sequential Data
#'
#' @name merge4seq
#'
#' Merges data frames maintaining order.
#'
#' @param x First data frame.
#' @param y Second data frame.
#' @param by Merge keys.
#' @param ... Additional merge arguments.
#'
#' @return Merged data frame.
#'
#' @family data-reshaping
#' @export

iaw$merge4seq <- function(x, y, by, ...) {
    stopifnot(is.data.frame(x), is.data.frame(y))
    stopifnot(is.character(by))
    
    x$.order <- seq_len(nrow(x))
    result <- merge(x, y, by = by, ...)
    result <- result[order(result$.order), ]
    result$.order <- NULL
    result
}
