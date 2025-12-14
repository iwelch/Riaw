#' Locate File in Search Paths
#'
#' @name locate
#'
#' Searches for file in multiple directories.
#'
#' @param filename Filename to find.
#' @param paths Search paths.
#'
#' @return Full path if found, NULL otherwise.
#'
#' @family io
#' @export

iaw$locate <- function(filename, paths = c(".", getwd())) {
    stopifnot(is.character(filename), length(filename) == 1L)
    stopifnot(is.character(paths))
    
    for (p in paths) {
        full <- file.path(p, filename)
        if (file.exists(full)) return(full)
    }
    NULL
}
