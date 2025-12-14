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
#' @family utilities
#' @export

iaw$index.of.variable <- function(varname, df) {
    which(names(df) == varname)
}
