
#' MC.BY.DATA.FRAME
#'
#' @name mc.by.indata.frame
#'
#' the by function in parallel form.  Often used as rbind.mc.by
#'
#' @usage mc.by (indata, INDICES, FUN, ...)
#'
#' @param see by
#'
#' @return a list
#'
#' @seealso rbind.mc.by


library(parallel)

## note https://stackoverflow.com/questions/31575585/shared-memory-in-parallel-foreach-in-r
## for strategies how to put a indata frame into a big matrix for shared memory use with foreach
## http://www.stat.yale.edu/~mjk56/temp/bigmemory-vignette.pdf


iaw$mc.by.data.frame <- function (indata, INDICES, FUN, ..., simplify = TRUE) {
    if (!is.list(INDICES)) {
        IND <- vector("list", 1L)
        IND[[1L]] <- INDICES
        names(IND) <- deparse(substitute(INDICES))[1L]
    }
    else IND <- INDICES
    FUNx <- function(x) FUN(indata[x, , drop = FALSE], ...)
    nd <- nrow(indata)
    ans <- eval(substitute(tapply(seq_len(nd), IND, FUNx, simplify = simplify)),
        indata)
    attr(ans, "call") <- match.call()
    class(ans) <- "by"
    ans
}


##  --- it is up to the user to make sure the split is still the same

iaw$.mc.cripple <- FALSE
iaw$.mc.by.cache <- NULL
iaw$.mc.by.cache.nrow <- NULL
iaw$mc.by.cache <- function(true.or.null) {
    if (is.null(true.or.null)) {
        message("[mc.by.cache disabled]")
        iaw$mc.by.cache <<- iaw$.mc.by.cache.nrow <- NULL
    } else if (true.or.null==FALSE) {
        message("[mc.by.cache disabled]")
        iaw$.mc.by.cache <<- iaw$.mc.by.cache.nrow <- NULL
    } else {
        if (is.null(iaw$.mc.by.cache)) {
            message("[mc.by.cache now enabled --- make sure to put a first==last check into your code]")
            iaw$.mc.by.cache <<- TRUE
        } else {
            message("[mc.by.cache was already enabled...ignored]")
        }
    }
}

iaw$mc.by.cripple.toggle <- function() { iaw$.mc.cripple <<- (!iaw$.mc.cripple) }

iaw$mc.by <- function (indata, INDICES, FUNIN, ...)  {
    timestart <- Sys.time()

    if (class(iaw$.mc.by.cache) == "list") {
        ( nrow(indata) == iaw$.mc.by.cache.nrow ) %or% "nrow indata={{nrow(indata)}}  != stored={{iaw$.mc.by.cache.nrow }}"
        ssplit <- iaw$.mc.by.cache
        message("[mc.by fetched cached list with ", length(ssplit), " groups]")
    } else {
        ssplit <- split(1:nrow(indata), INDICES)
        if (!is.null(iaw$.mc.by.cache)) {  ## usually, just TRUE
            iaw$.mc.by.cache <<- ssplit  ## store the cache.
            iaw$.mc.by.cache.nrow <<- nrow(indata)
            message("[mc.by caching split list with ", iaw$.mc.by.cache.nrow, " obs into ", length(ssplit), " groups]")
        }
    }

    if ((Sys.time() - timestart)>10)
        message("[splitting for mc.by done: ",length(ssplit)," groups from ", nrow(indata), " observations]\n")

    wapply <- if (iaw$.mc.cripple) lapply else iaw$mclapply

    applytoeach <- (wapply(ssplit, FUN=function(.index) FUNIN(indata[.index, , drop = FALSE], ...)))
    if ((Sys.time() - timestart)>20) message("[applying mc.by done]\n")
    # cat("[mc.by] probably do 'require(indata.table); rbindlist(mc.by.result)', or 'do.call(\"rbind\", mc.by.result)', or 'iaw$rbindall.by(mc.by.result)'.\n")
    applytoeach
}

