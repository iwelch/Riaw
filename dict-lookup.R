#' Dictionary Lookup
#'
#' @name dict.lookup
#'
#' Looks up values in a dictionary.
#'
#' @param keys Keys to look up.
#' @param dict Named vector (dictionary).
#' @param default Default for missing.
#'
#' @return Looked up values.
#'
#' @family utilities
#' @export
#'
#' @examples
#' d <- c(a = 1, b = 2, c = 3)
#' iaw$dict.lookup(c("a", "c", "x"), d, NA)

iaw$dict.lookup <- function(keys, dict, default = NA) {
    stopifnot(is.character(keys) || is.numeric(keys))
    result <- dict[as.character(keys)]
    result[is.na(result)] <- default
    result
}
