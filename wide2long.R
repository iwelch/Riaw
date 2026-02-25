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
#' # Matrix input: wide panel to long format
#' mat <- matrix(1:6, nrow = 2)
#' rownames(mat) <- c("r1", "r2")
#' colnames(mat) <- c("c1", "c2", "c3")
#' iaw$wide2long(mat)
#'
#' # Data frame with custom column names
#' df <- data.frame(AAPL = c(150, 155, 160),
#'                  MSFT = c(300, 310, 305),
#'                  row.names = c("2024-01", "2024-02", "2024-03"))
#' iaw$wide2long(df, valname.is = "price", row.is = "date", col.is = "ticker")
#'
#' # Prices panel: each row a date, each column a stock
#' prices <- matrix(c(1.0, 1.1, 1.05, 2.0, 1.9, 2.1), nrow = 3,
#'                  dimnames = list(c("d1","d2","d3"), c("A","B")))
#' iaw$wide2long(prices, valname.is = "ret", row.is = "date", col.is = "firm")

iaw$wide2long <- function(d, valname.is = "val", row.is = "time", col.is = "unit") {
    stopifnot(is.matrix(d) || is.data.frame(d))
    stopifnot(is.character(valname.is), length(valname.is) == 1L)
    stopifnot(is.character(col.is), length(col.is) == 1L)
    stopifnot(is.character(row.is), length(row.is) == 1L)
    
    rv <- reshape(
        as.data.frame(d),
        direction = "long",
        idvar = row.is,
        timevar = col.is,
        varying = list(colnames(d)),
        times = colnames(d),
        ids = rownames(d)
    )

    rv <- data.frame(rv[, 1], rv[, 2], rv[, 3])
    names(rv) <- c(row.is, col.is, valname.is)
    rv
}
