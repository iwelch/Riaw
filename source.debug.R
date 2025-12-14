#' Source with Debug
#'
#' @name source.debug
#'
#' Sources file with error recovery.
#'
#' @param file R file to source.
#'
#' @return Invisible result.
#'
#' @family io
#' @export

iaw$source.debug <- function(file) {
    stopifnot(is.character(file), length(file) == 1L)
    
    old_opt <- getOption("error")
    options(error = recover)
    on.exit(options(error = old_opt))
    
    source(file)
}
