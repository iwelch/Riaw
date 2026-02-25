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
#' # Compute group means in parallel
#' df <- data.frame(
#'   group = c("A", "A", "B", "B", "C"),
#'   value = c(1, 3, 2, 4, 5)
#' )
#' result <- iaw$mc.by(df, df$group, function(d) mean(d$value))
#' unlist(result)  # named vector: A=2, B=3, C=5
#'
#' # Return a summary data frame per group (results are a list of data frames)
#' result <- iaw$mc.by(df, df$group, function(d) {
#'   data.frame(n = nrow(d), mean = mean(d$value), sd = sd(d$value))
#' })
#' do.call(rbind, result)
#'
#' \dontrun{
#' # Toggle parallel off for debugging (runs serially via lapply)
#' iaw$mc.by.cripple.toggle()
#' result <- iaw$mc.by(df, df$group, function(d) mean(d$value))
#' iaw$mc.by.cripple.toggle()  # toggle back on
#' }
#'
#' # Group by two variables using a list of indices
#' df2 <- data.frame(sector = c("Tech","Tech","Fin","Fin"),
#'                   year   = c(2020, 2021, 2020, 2021),
#'                   rev    = c(100, 150, 80, 90))
#' result2 <- iaw$mc.by(df2, list(df2$sector, df2$year),
#'                       function(d) sum(d$rev))
#' unlist(result2)  # one sum per sector-year combination
#'
#' # Run a regression per group in parallel
#' panel <- data.frame(
#'   id  = rep(c("A","B"), each = 10),
#'   x   = rnorm(20),
#'   y   = rnorm(20)
#' )
#' fits <- iaw$mc.by(panel, panel$id, function(d) coef(lm(y ~ x, data = d)))
#' fits  # list of coefficient vectors, one per group

iaw$.mc.cripple <- FALSE
iaw$.mc.by.cache <- NULL
iaw$.mc.by.cache.nrow <- NULL

#' @rdname mc.by
#'
#' @param true.or.null Logical or \code{NULL}. Enable/disable the group split cache.
#'
#' @export
iaw$mc.by.cache <- function(true.or.null) {
    if (is.null(true.or.null) || isFALSE(true.or.null)) {
        message("[mc.by.cache disabled]")
        assign(".mc.by.cache", NULL, envir = iaw)
        assign(".mc.by.cache.nrow", NULL, envir = iaw)
    } else {
        if (is.null(iaw$.mc.by.cache)) {
            message("[mc.by.cache enabled]")
            assign(".mc.by.cache", TRUE, envir = iaw)
        }
    }
}

#' @rdname mc.by
#'
#' @details \code{mc.by.cripple.toggle} switches between parallel and serial execution.
#'
#' @export
iaw$mc.by.cripple.toggle <- function() {
    assign(".mc.cripple", !iaw$.mc.cripple, envir = iaw)
    message("[mc.by parallel: ", if (iaw$.mc.cripple) "OFF" else "ON", "]")
}

iaw$mc.by <- function(indata, INDICES, FUNIN, ...) {
    stopifnot(is.data.frame(indata))
    if (is.list(INDICES)) {
        stopifnot(all(lengths(INDICES) == nrow(indata)))
    } else if (is.vector(INDICES)) {
        stopifnot(length(INDICES) == nrow(indata))
    } else stop("INDICES must be list or vector")


    cache_key <- paste(INDICES, collapse = "\x01")
    if (inherits(iaw$.mc.by.cache, "list") &&
        identical(iaw$.mc.by.cache.nrow, nrow(indata)) &&
        identical(attr(iaw$.mc.by.cache, "key"), cache_key)) {
        ssplit <- iaw$.mc.by.cache
    } else {
        ssplit <- split(seq_len(nrow(indata)), INDICES)
        if (!is.null(iaw$.mc.by.cache)) {
            attr(ssplit, "key") <- cache_key
            assign(".mc.by.cache", ssplit, envir = iaw)
            assign(".mc.by.cache.nrow", nrow(indata), envir = iaw)
        }
    }

    wapply <- if (iaw$.mc.cripple) lapply else parallel::mclapply

    wapply(ssplit, FUN = function(.index) {
        FUNIN(indata[.index, , drop = FALSE], ...)
    })
}

#' @rdname mc.by
#'
#' @param FUN Function to apply (for \code{mc.by.data.frame}).
#' @param simplify Logical. Simplify result (default \code{TRUE}).
#'
#' @details \code{mc.by.data.frame} is a single-core variant using \code{tapply}.
#'
#' @export
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
