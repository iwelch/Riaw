#' Exit R Session
#'
#' @name exit
#'
#' Exits R without saving.
#'
#' @param status Exit status code.
#'
#' @return Does not return.
#'
#' @family utilities
#' @export

iaw$exit <- function(status = 0) {
    q(save = "no", status = status)
}
