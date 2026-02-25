#' Touch File
#'
#' @name touch
#'
#' Creates empty file or updates timestamp.
#'
#' @param filename File path.
#'
#' @return Invisible TRUE.
#'
#' @examples
#' \dontrun{
#' # Create an empty sentinel file
#' iaw$touch(file.path(tempdir(), "done.flag"))
#'
#' # Update the modification timestamp of an existing file
#' f <- tempfile()
#' writeLines("hello", f)
#' iaw$touch(f)
#' file.info(f)$mtime   # now equals (approximately) Sys.time()
#' }
#'
#' @family io
#' @export

iaw$touch <- function(filename) {
    stopifnot(is.character(filename), length(filename) == 1L)
    
    if (!file.exists(filename)) {
        file.create(filename)
    } else {
        Sys.setFileTime(filename, Sys.time())
    }
    invisible(TRUE)
}
