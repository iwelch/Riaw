#' Grep Column Names
#'
#' @name grepcolname
#'
#' Finds column names matching pattern.
#'
#' @param pattern Regex pattern.
#' @param df Data frame.
#'
#' @return Matching column names.
#'
#' @family utilities
#' @export

iaw$grepcolname <- function(pattern, df) {
    grep(pattern, names(df), value = TRUE)
}
