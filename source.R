#' Enhanced Source
#'
#' @name source
#'
#' Sources R file with verbose option.
#'
#' @param file R file to source.
#' @param verbose Print filename.
#' @param ... Additional source arguments.
#'
#' @return Invisible result.
#'
#' @family io
#' @export

iaw$source <- function(file, verbose = TRUE, ...) {
    stopifnot(is.character(file), length(file) == 1L)
    
    if (verbose) message("Sourcing: ", file)
    source(file, ...)
}
