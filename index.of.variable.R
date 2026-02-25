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
#' @family utilities
#' @export

iaw$index.of.variable <- function(varname, df) {
    which(names(df) == varname)
}
