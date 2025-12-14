#' Make R Dictionary
#'
#' @name mkRdictionary
#'
#' Creates named vector dictionary.
#'
#' @param keys Key values.
#' @param values Value values.
#'
#' @return Named vector.
#'
#' @family utilities
#' @export

iaw$mkRdictionary <- function(keys, values) {
    stopifnot(length(keys) == length(values))
    setNames(values, keys)
}
