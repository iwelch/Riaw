#' Concatenate Strings
#'
#' @name strcat
#'
#' Concatenates strings without separator.
#'
#' @param ... Strings to concatenate.
#'
#' @return Single string.
#'
#' @family utilities
#' @export
#'
#' @examples
#' iaw$strcat("Hello", "World")

iaw$strcat <- function(...) {
    paste0(...)
}
