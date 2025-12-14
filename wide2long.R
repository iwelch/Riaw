#' Reshape Wide to Long Format
#'
#' @name wide2long
#'
#' Converts wide matrix/data frame to long format.
#'
#' @param d Matrix or data frame with row/column names.
#' @param valname.is Name for value column. Default "val".
#' @param row.is Name for row identifier. Default "time".
#' @param col.is Name for column identifier. Default "unit".
#'
#' @return Data frame in long format.
#'
#' @family data-reshaping
#' @export
#'
#' @examples
#' mat <- matrix(1:6, nrow = 2)
#' rownames(mat) <- c("r1", "r2")
#' colnames(mat) <- c("c1", "c2", "c3")
#' iaw$wide2long(mat)

iaw$wide2long <- function(d, valname.is = "val", row.is = "time", col.is = "unit") {
    stopifnot(is.matrix(d) || is.data.frame(d))
    stopifnot(is.character(valname.is), length(valname.is) == 1L)
    stopifnot(is.character(col.is), length(col.is) == 1L)
    stopifnot(is.character(row.is), length(row.is) == 1L)
    
    rv <- reshape(
        as.data.frame(d),
        direction = "long",
        idvar = col.is,
        timevar = row.is,
        varying = list(colnames(d)),
        times = colnames(d),
        ids = rownames(d)
    )
    
    rv <- data.frame(rv[, 1], rv[, 3], rv[, 2])
    names(rv) <- c(row.is, col.is, valname.is)
    rv
}
