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
