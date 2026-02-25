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
#' @examples
#' # Basic order-preserving merge
#' x <- data.frame(id = c(3, 1, 2), val = c("c", "a", "b"))
#' y <- data.frame(id = c(1, 2, 3), extra = c(10, 20, 30))
#' result <- iaw$merge4seq(x, y, by = "id")
#' result  # rows match original order of x: 3, 1, 2
#'
#' # Left join preserving x row order
#' x2 <- data.frame(date = c(20230103, 20230101, 20230102), ret = c(0.3, 0.1, 0.2))
#' y2 <- data.frame(date = c(20230101, 20230102, 20230103), mkt = c(0.5, 0.6, 0.7))
#' iaw$merge4seq(x2, y2, by = "date")
#'
#' @family data-reshaping
#' @export

iaw$merge4seq <- function(x, y, by, ...) {
    stopifnot(is.data.frame(x), is.data.frame(y))
    stopifnot(is.character(by))

    ordcol <- ".merge4seq.order."
    stopifnot(!ordcol %in% names(x))
    x[[ordcol]] <- seq_len(nrow(x))
    result <- merge(x, y, by = by, ...)
    result <- result[order(result[[ordcol]]), ]
    result[[ordcol]] <- NULL
    result
}
