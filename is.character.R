#' Test if Object is Character Vector of Specific Length
#'
#' @name is.character
#'
#' Extends base is.character to also check length.
#'
#' @param cvec Object to test.
#' @param required.length.of.cvec Required length (default 0).
#'
#' @return Logical scalar.
#'
#' @family type-checking
#' @export
#'
#' @examples
#' iaw$is.character("hello", 1)
#' iaw$is.character(c("a", "b"), 2)

iaw$is.character <- function(cvec, required.length.of.cvec = NULL) {

    c1 <- is.character(cvec)
    if (is.null(required.length.of.cvec)) return(c1)

    stopifnot(is.numeric(required.length.of.cvec), length(required.length.of.cvec) == 1L)
    c1 && (length(cvec) == required.length.of.cvec)
}
