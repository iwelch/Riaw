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
#'
#' # Check R version meets minimum requirement
#' info <- iaw$osinfo()
#' grepl("^R version [4-9]", info$R_version)  # TRUE if R >= 4.x
#'
#' # Log environment details at script startup
#' info <- iaw$osinfo()
#' msg <- paste(info$sysname, info$machine, info$R_version, sep = " | ")
#' msg  # e.g. "Darwin | x86_64 | R version 4.3.2 (2023-10-31)"
#'
#' # Return value is a named list with 4 elements
#' length(iaw$osinfo())      # 4
#' names(iaw$osinfo())       # "sysname" "release" "machine" "R_version"

iaw$osinfo <- function() {
    list(
        sysname = Sys.info()["sysname"],
        release = Sys.info()["release"],
        machine = Sys.info()["machine"],
        R_version = R.version.string
    )
}
