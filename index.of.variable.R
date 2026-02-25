#' Get Column Index
#'
#' @name index.of.variable
#'
#' Returns column index by name.
#'
#' @param varname Variable name.
#' @param df Data frame.
#'
#' @return Integer index.
#'
#' @examples
#' df <- data.frame(firm = 1:3, date = 4:6, ret = 7:9)
#'
#' # Find the column position of "ret"
#' iaw$index.of.variable("ret", df)    # 3
#'
#' # Use the index for programmatic subsetting
#' idx <- iaw$index.of.variable("date", df)
#' df[, idx]                           # 4 5 6
#'
#' # Returns integer(0) when variable is missing
#' iaw$index.of.variable("price", df)  # integer(0)
#'
#' # Locate a column among many in a wide financial dataset
#' wide <- data.frame(permno = 1, yyyymm = 2, ret = 3, vol = 4, mktcap = 5)
#' iaw$index.of.variable("mktcap", wide)  # 5
#'
#' # Use with column reordering: move "ret" to the first position
#' idx <- iaw$index.of.variable("ret", df)
#' df[, c(idx, setdiff(seq_along(df), idx))]
#'
#' @family utilities
#' @export

iaw$index.of.variable <- function(varname, df) {
    which(names(df) == varname)
}
