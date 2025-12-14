#' Operating System Information
#'
#' @name osinfo
#'
#' Returns OS information.
#'
#' @return List with OS details.
#'
#' @family utilities
#' @export
#'
#' @examples
#' iaw$osinfo()

iaw$osinfo <- function() {
    list(
        sysname = Sys.info()["sysname"],
        release = Sys.info()["release"],
        machine = Sys.info()["machine"],
        R_version = R.version.string
    )
}
