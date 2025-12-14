#' Enhanced Sink
#'
#' @name sink
#'
#' Redirects output with automatic cleanup.
#'
#' @param file Output file.
#' @param expr Expression to evaluate.
#' @param append Append to file.
#'
#' @return Result of expression.
#'
#' @family io
#' @export

iaw$sink <- function(file, expr, append = FALSE) {
    stopifnot(is.character(file), length(file) == 1L)
    
    sink(file, append = append)
    on.exit(sink())
    
    result <- eval(expr)
    result
}
