#' Two-Way Sorting for Portfolio Formation
#'
#' Given two criteria, first sorts observations into groups by the first
#' criterion (to hold it approximately constant), then ranks within each
#' group by the second criterion. This is commonly used in asset pricing
#' to form portfolios that vary in one characteristic while controlling
#' for another.
#'
#' @param crit.holdconstant Numeric vector of the first sorting criterion
#'   (the one to hold approximately constant within groups).
#' @param crit.spread Numeric vector of the second sorting criterion
#'   (the one to vary within each group).
#' @param NUMPERGROUP Integer; number of observations per group. Default is 5.
#'
#' @return A numeric vector of ranks (1 to NUMPERGROUP) indicating which
#'   sub-group each observation belongs to. NA for observations that don't
#'   fit evenly into groups (middle observations are removed).
#'
#' @details
#' The algorithm:
#' 1. Sorts by crit.holdconstant and divides into N/NUMPERGROUP groups
#' 2. If N is not divisible by NUMPERGROUP, removes middle observations
#' 3. Within each group, ranks by crit.spread
#'
#' This is useful for constructing 5x5 or 10x10 portfolio sorts where you
#' want to examine returns varying by one characteristic while controlling
#' for another.
#'
#' @export
#'
#' @seealso \code{\link{rank}}, \code{\link{iaw$pctrank}}
#'
#' @examples
#' # Sort 20 stocks: hold size constant, vary by book-to-market
#' set.seed(42)
#' size <- rnorm(20)
#' bm <- rnorm(20)
#'
#' # Create 4 groups of 5 stocks each
#' ranks <- iaw$doublesort(size, bm, NUMPERGROUP = 5)
#' # Within each size quartile, stocks are ranked 1-5 by B/M
#'
#' # Example from documentation
#' crit1 <- c(4, 9, 13, 11, 5, 7, 10, 1, 12, 8, 3, 6, 2)
#' crit2 <- c(-1, 10, 104, 103, 22, -99, 102, 1, 105, 11, 2, 33, -2)
#' iaw$doublesort(crit1, crit2, NUMPERGROUP = 4)
#' # 2 1 3 2 3 NA 1 3 4 2 4 4 1

iaw$.mkusefullist <- function(N, NUMPERGROUP) {
    RMNUM <- N %% NUMPERGROUP
    NUMGROUPS <- as.integer(N / NUMPERGROUP)
    HALFKEEP <- as.integer((NUMGROUPS * NUMPERGROUP) / 2)
    c(1:HALFKEEP, rep(NA, RMNUM), (HALFKEEP + 1):(NUMGROUPS * NUMPERGROUP))
}

#' @rdname doublesort
#' @export
iaw$doublesort <- function(crit.holdconstant, crit.spread, NUMPERGROUP = 5) {
    stopifnot(length(crit.holdconstant) == length(crit.spread))
    stopifnot(length(crit.holdconstant) >= NUMPERGROUP)
    stopifnot(NUMPERGROUP > 1)

    N <- length(crit.holdconstant)

    NCRIT.HOLDCONSTANT <- iaw$.mkusefullist(N, NUMPERGROUP)

    # First sort by criterion.holdconstant
    grouporder <- order(crit.holdconstant)
    group1num <- as.integer((NCRIT.HOLDCONSTANT - 1) / NUMPERGROUP) + 1
    names(grouporder) <- paste0("f", group1num)
    zerolist <- grouporder[is.na(group1num)]
    names(crit.holdconstant)[grouporder] <- names(grouporder)

    rankfun <- function(x) rank(x, ties.method = "max")
    # Within each criterion.holdconstant group, rank the observations
    r <- ave(crit.spread, as.factor(names(crit.holdconstant)), FUN = rankfun)
    r[zerolist] <- NA
    r
}
