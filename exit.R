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
#' @examples
#' \dontrun{
#' # Exit R cleanly (no save prompt)
#' iaw$exit()
#'
#' # Exit with a non-zero status code to signal failure to the calling shell
#' iaw$exit(1)
#' }
#'
#' @family utilities
#' @export

iaw$exit <- function(status = 0) {
    q(save = "no", status = status)
}
