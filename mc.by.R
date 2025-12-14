#' Parallel Apply by Groups
#'
#' @name mc.by
#'
#' Parallel version of by() using mclapply.
#'
#' @param indata Data frame to process.
#' @param INDICES Factor or list of factors for grouping.
#' @param FUNIN Function to apply to each group.
#' @param ... Additional arguments to FUNIN.
#'
#' @return List of results.
#'
#' @family parallel
#' @export
#'
#' @importFrom parallel mclapply
#'
#' @examples
#' \dontrun{
#' result <- iaw$mc.by(df, df$group, function(d) mean(d$value))
#' }

iaw$.mc.cripple <- FALSE
iaw$.mc.by.cache <- NULL
iaw$.mc.by.cache.nrow <- NULL

iaw$mc.by.cache <- function(true.or.null) {
    if (is.null(true.or.null) || isFALSE(true.or.null)) {
        message("[mc.by.cache disabled]")
        iaw$.mc.by.cache <<- NULL
        iaw$.mc.by.cache.nrow <<- NULL
    } else {
        if (is.null(iaw$.mc.by.cache)) {
            message("[mc.by.cache enabled]")
            iaw$.mc.by.cache <<- TRUE
        }
    }
}

iaw$mc.by.cripple.toggle <- function() {
    iaw$.mc.cripple <<- !iaw$.mc.cripple
    message("[mc.by parallel: ", if (iaw$.mc.cripple) "OFF" else "ON", "]")
}

iaw$mc.by <- function(indata, INDICES, FUNIN, ...) {
    stopifnot(is.data.frame(indata))
    
    if (inherits(iaw$.mc.by.cache, "list")) {
        stopifnot(nrow(indata) == iaw$.mc.by.cache.nrow)
        ssplit <- iaw$.mc.by.cache
    } else {
        ssplit <- split(seq_len(nrow(indata)), INDICES)
        if (!is.null(iaw$.mc.by.cache)) {
            iaw$.mc.by.cache <<- ssplit
            iaw$.mc.by.cache.nrow <<- nrow(indata)
        }
    }
    
    wapply <- if (iaw$.mc.cripple) lapply else parallel::mclapply
    
    wapply(ssplit, FUN = function(.index) {
        FUNIN(indata[.index, , drop = FALSE], ...)
    })
}

iaw$mc.by.data.frame <- function(indata, INDICES, FUN, ..., simplify = TRUE) {
    stopifnot(is.data.frame(indata))
    if (!is.list(INDICES)) {
        IND <- vector("list", 1L)
        IND[[1L]] <- INDICES
        names(IND) <- deparse(substitute(INDICES))[1L]
    } else {
        IND <- INDICES
    }
    FUNx <- function(x) FUN(indata[x, , drop = FALSE], ...)
    nd <- nrow(indata)
    ans <- eval(substitute(tapply(seq_len(nd), IND, FUNx, simplify = simplify)), indata)
    attr(ans, "call") <- match.call()
    class(ans) <- "by"
    ans
}
