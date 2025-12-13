
#' MC.SAPPLY
#'
#' @name mc.sapply
#'
#' sapply is a simplied lapply that tries to return the same return type
#'
#' @param X an indexed length structure (list, array, etc.)
#' @param FUN the function to be applied to each element
#' @param simplify (must be named to distinguish from FUN arguments
#' @param use.names try to use the same names
#'
#' @return a vector or matrix
#'

library(parallel)

iaw$mc.sapply <- function (X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE) {
    FUN <- match.fun(FUN)
    answer <- mclapply(X = X, FUN = FUN, ...)
    if (USE.NAMES && is.character(X) && is.null(names(answer)))
        names(answer) <- X
    if (!identical(simplify, FALSE) && length(answer))
        simplify2array(answer, higher = (simplify == "array"))
    else answer
}
