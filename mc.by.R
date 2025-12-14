#' Multicore Apply By Groups
#'
#' A parallel version of \code{by()} that applies a function to subsets of a
#' data frame defined by one or more grouping variables. Uses \code{mclapply}
#' for parallel execution on Unix-like systems.
#'
#' @name mc.by
#'
#' @param indata A data frame to split and process.
#' @param INDICES A factor or list of factors defining groups. Each unique
#'   combination defines a subset of \code{indata}.
#' @param FUNIN A function to apply to each subset data frame.
#' @param ... Additional arguments passed to \code{FUNIN}.
#' @param simplify Logical; if TRUE, attempt to simplify the result.
#'
#' @return A list with one element per group, containing the result of applying
#'   \code{FUNIN} to each group's data.
#'
#' @details
#' The function splits the data frame by \code{INDICES}, then applies
#' \code{FUNIN} to each subset in parallel using \code{mclapply}.
#'
#' For repeated calls with the same grouping structure, use
#' \code{iaw$mc.by.cache(TRUE)} to cache the split indices, avoiding
#' redundant computation.
#'
#' @section Caching:
#' \describe{
#'   \item{\code{iaw$mc.by.cache(TRUE)}}{Enable caching of split indices}
#'   \item{\code{iaw$mc.by.cache(FALSE)}}{Disable and clear cache}
#'   \item{\code{iaw$mc.by.cripple.toggle()}}{Toggle between parallel and serial execution (for debugging)}
#' }
#'
#' @export
#'
#' @seealso \code{\link{iaw$rbind.mc.by}}, \code{\link{iaw$oc.by}},
#'   \code{\link{mclapply}}, \code{\link{by}}
#'
#' @examples
#' # Create sample panel data
#' d <- data.frame(
#'     firm = rep(c("A", "B", "C"), each = 10),
#'     year = rep(2010:2019, 3),
#'     value = rnorm(30)
#' )
#'
#' # Compute mean by firm (parallel)
#' result <- iaw$mc.by(d, d$firm, function(df) mean(df$value))
#' unlist(result)
#'
#' # Multiple grouping variables
#' d$region <- rep(c("East", "West"), 15)
#' result <- iaw$mc.by(d, list(d$firm, d$region),
#'                     function(df) c(mean = mean(df$value), n = nrow(df)))
#'
#' # With rbind for data frame results
#' result <- iaw$rbind.mc.by(d, d$firm, function(df) {
#'     data.frame(firm = df$firm[1],
#'                mean_value = mean(df$value),
#'                sd_value = sd(df$value))
#' })
#'
#' # Enable caching for repeated operations on same data
#' iaw$mc.by.cache(TRUE)
#' result1 <- iaw$mc.by(d, d$firm, function(df) mean(df$value))
#' result2 <- iaw$mc.by(d, d$firm, function(df) sd(df$value))  # Uses cached split
#' iaw$mc.by.cache(FALSE)  # Clear cache when done

library(parallel)

iaw$mc.by.data.frame <- function(indata, INDICES, FUN, ..., simplify = TRUE) {
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


# Cache management
iaw$.mc.cripple <- FALSE
iaw$.mc.by.cache <- NULL
iaw$.mc.by.cache.nrow <- NULL

#' @rdname mc.by
#' @export
iaw$mc.by.cache <- function(true.or.null) {
    if (is.null(true.or.null) || isFALSE(true.or.null)) {
        message("[mc.by.cache disabled]")
        iaw$.mc.by.cache <<- NULL
        iaw$.mc.by.cache.nrow <<- NULL
    } else {
        if (is.null(iaw$.mc.by.cache)) {
            message("[mc.by.cache enabled - ensure first==last check in your code]")
            iaw$.mc.by.cache <<- TRUE
        } else {
            message("[mc.by.cache was already enabled]")
        }
    }
}

#' @rdname mc.by
#' @export
iaw$mc.by.cripple.toggle <- function() {
    iaw$.mc.cripple <<- !iaw$.mc.cripple
    message("[mc.by parallel execution: ", if (iaw$.mc.cripple) "DISABLED" else "ENABLED", "]")
}

#' @rdname mc.by
#' @export
iaw$mc.by <- function(indata, INDICES, FUNIN, ...) {
    timestart <- Sys.time()

    # Use cached split if available
    if (inherits(iaw$.mc.by.cache, "list")) {
        (nrow(indata) == iaw$.mc.by.cache.nrow) %or%
            "nrow mismatch: indata={{nrow(indata)}} != cached={{iaw$.mc.by.cache.nrow}}"
        ssplit <- iaw$.mc.by.cache
        message("[mc.by: using cached split with ", length(ssplit), " groups]")
    } else {
        ssplit <- split(1:nrow(indata), INDICES)
        if (!is.null(iaw$.mc.by.cache)) {
            iaw$.mc.by.cache <<- ssplit
            iaw$.mc.by.cache.nrow <<- nrow(indata)
            message("[mc.by: caching split of ", nrow(indata), " obs into ", length(ssplit), " groups]")
        }
    }

    if ((Sys.time() - timestart) > 10) {
        message("[mc.by split done: ", length(ssplit), " groups from ", nrow(indata), " obs]")
    }

    # Use lapply if crippled (for debugging), otherwise mclapply
    wapply <- if (iaw$.mc.cripple) lapply else mclapply

    result <- wapply(ssplit, FUN = function(.index) {
        FUNIN(indata[.index, , drop = FALSE], ...)
    })

    if ((Sys.time() - timestart) > 20) {
        message("[mc.by apply done]")
    }

    result
}
