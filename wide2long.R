#' Reshape Matrix/Data Frame from Wide to Long Format
#'
#' Converts a wide-format matrix or data frame (with variables in columns) to
#' long format (one observation per row). Row names become one identifier,
#' column names become another, and cell values become the value column.
#'
#' @param d A matrix or data frame to reshape. Should have meaningful row and
#'   column names.
#' @param valname.is Character; name for the value column in output. Default \code{"val"}.
#' @param row.is Character; name for the column containing original row names.
#'   Default \code{"time"}.
#' @param col.is Character; name for the column containing original column names.
#'   Default \code{"unit"}.
#'
#' @return A data frame in long format with three columns: the row identifier,
#'   the column identifier, and the value.
#'
#' @details
#' This is a convenience wrapper around \code{reshape()} for the common case
#' of converting a matrix of values (e.g., time series for multiple units) into
#' a "tidy" long format suitable for regression or plotting.
#'
#' @export
#'
#' @seealso \code{\link{iaw$long2wide}} for the reverse transformation,
#'   \code{\link{reshape}}, \code{\link[tidyr]{pivot_longer}}
#'
#' @examples
#' # Create a wide matrix: rows are years, columns are firms
#' mat <- matrix(rnorm(15), nrow = 5, ncol = 3)
#' rownames(mat) <- 2015:2019
#' colnames(mat) <- c("FirmA", "FirmB", "FirmC")
#' mat
#' #           FirmA      FirmB      FirmC
#' # 2015  0.1234567 -0.5432101  0.9876543
#' # ...
#'
#' # Convert to long format
#' df_long <- iaw$wide2long(mat, valname.is = "return",
#'                          row.is = "year", col.is = "firm")
#' head(df_long)
#' #   year  firm     return
#' # 1 2015 FirmA  0.1234567
#' # 2 2016 FirmA  ...
#' # ...
#'
#' # The long format is suitable for regression
#' # lm(return ~ year + firm, data = df_long)

iaw$wide2long <- function(d, valname.is = "val", row.is = "time", col.is = "unit") {
    stopifnot(is.matrix(d) | is.data.frame(d))
    stopifnot(is.character(valname.is) && length(valname.is) == 1)
    stopifnot(is.character(col.is) && length(col.is) == 1)
    stopifnot(is.character(row.is) && length(row.is) == 1)

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
