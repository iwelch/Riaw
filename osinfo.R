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
#'
#' # Access individual fields
#' info <- iaw$osinfo()
#' info$sysname     # e.g. "Darwin" or "Linux"
#' info$R_version   # e.g. "R version 4.3.2 (2023-10-31)"
#'
#' # Conditionally run platform-specific code
#' if (iaw$osinfo()$sysname == "Darwin") {
#'   message("Running on macOS")
#' }

iaw$osinfo <- function() {
    list(
        sysname = Sys.info()["sysname"],
        release = Sys.info()["release"],
        machine = Sys.info()["machine"],
        R_version = R.version.string
    )
}
