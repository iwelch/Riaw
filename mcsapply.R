#' Parallel sapply with Result Simplification
#'
#' A parallel version of \code{sapply()} that uses \code{mclapply()} for
#' computation and then simplifies the results to a vector or matrix if possible.
#'
#' @param X A vector (atomic or list) over which to apply FUN.
#' @param FUN The function to apply to each element of X.
#' @param ... Additional arguments passed to FUN.
#' @param simplify Logical; if TRUE (default), attempts to simplify the result
#'   to a vector or matrix.
#' @param USE.NAMES Logical; if TRUE (default), uses names from X for the result.
#' @param mc.cores Integer; number of cores to use.
#'
#' @return If \code{simplify=FALSE}, a list. Otherwise, attempts to return a
#'   vector (if all results are length 1) or matrix (if all results have the
#'   same length > 1).
#'
#' @export
#'
#' @seealso \code{\link{iaw$mclapply}}, \code{\link{sapply}}
#'
#' @examples
#' # Returns a numeric vector
#' iaw$mcsapply(1:10, function(x) x^2)
#'
#' # Returns a matrix (each call returns same-length vector)
#' iaw$mcsapply(1:5, function(x) c(x, x^2, x^3))
#'
#' # Returns a list (different lengths)
#' iaw$mcsapply(1:5, function(x) 1:x)

iaw$mcsapply <- function(X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE,
                          mc.cores = getOption("mc.cores", 2L)) {
    result <- mclapply(X, FUN, ..., mc.cores = mc.cores)

    if (!simplify) {
        return(result)
    }

    if (length(result) == 0L) {
        return(list())
    }

    lens <- vapply(result, length, integer(1))

    if (all(lens == 1L)) {
        result <- unlist(result, recursive = FALSE, use.names = USE.NAMES)
    } else if (all(lens == lens[1L]) && lens[1L] > 0L) {
        result <- tryCatch({
            matrix(unlist(result), nrow = lens[1L],
                   dimnames = if (USE.NAMES) {
                       list(names(result[[1L]]), names(result))
                   } else NULL)
        }, error = function(e) result)
    }

    result
}
