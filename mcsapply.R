#' Enhanced Multicore Sapply
#'
#' @name mcsapply
#'
#' Wrapper for mclapply with simplification.
#'
#' @param X List or vector.
#' @param FUN Function to apply.
#' @param ... Arguments to FUN.
#'
#' @return Simplified result.
#'
#' @family parallel
#' @export
#'
#' @examples
#' \dontrun{
#' iaw$mcsapply(1:10, function(x) x^2)
#' }

iaw$mcsapply <- function(X, FUN, ...) {
    simplify2array(iaw$mclapply(X, FUN, ...))
}
