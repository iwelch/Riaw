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
#' \dontrun{
#' # In a batch pipeline: exit with status 2 on validation failure
#' if (nrow(bad_rows) > 0) {
#'   message("Validation failed: ", nrow(bad_rows), " bad rows")
#'   iaw$exit(2)
#' }
#' }
#'
#' \dontrun{
#' # Default status 0 indicates success to the calling process
#' iaw$exit()   # equivalent to q(save = "no", status = 0)
#' }
#'
#' @family utilities
#' @export

iaw$exit <- function(status = 0) {
    q(save = "no", status = status)
}
