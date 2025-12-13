
#' doublesort
#'
#' given two criteria, first sort by criterion 1, then rank within
#' groups by criterion 2.  The result is a vector that groups
#' observations that are similar in terms of the first criterion and
#' different in terms of the second criterion.
#'
#' if the observations do not fit exactly into groups, the middle
#' observations are first removed
#'
#' @name doublesort
#'
#' @param crit1 the first criterion, usually a value vector
#'
#' @param crit2 the second criterion, usually a value vector
#'
#' @return a vector from 1 to N that indicates which group each observation should go into
#'
#' @examples
#'
#'	> crit1 <- c(  4,  9,  13,  11,  5,   7,  10, 1,  12,  8, 3,  6,  2 )
#' 	> crit2 <- c( -1, 10, 104, 103, 22, -99, 102, 1, 105, 11, 2, 33, -2 )
#' 	> result <- doublesort( crit1, crit2, NUMPERGROUP=4 )
#' 	> print(result)
#'        [1]  2  1  3  2  3 NA  1  3  4  2  4  4  1
#'      > stopifnot( identical(result , c(2, 1, 3, 2, 3, NA, 1, 3, 4, 2, 4, 4, 1) ) )

iaw$.mkusefullist <- function( N, NUMPERGROUP ) {
    ## replace any middle unnecessary observations with NA
    RMNUM <- N %% NUMPERGROUP
    NUMGROUPS <- as.integer( N/NUMPERGROUP )
    HALFKEEP <- as.integer( (NUMGROUPS*NUMPERGROUP)/2 )
    c( 1:HALFKEEP, rep(NA, RMNUM), (HALFKEEP+1):(NUMGROUPS*NUMPERGROUP) )
}

iaw$doublesort <- function(crit.holdconstant, crit.spread, NUMPERGROUP=5) {
    stopifnot( length(crit.holdconstant) == length(crit.spread) )
    stopifnot( length(crit.holdconstant) >= NUMPERGROUP )
    stopifnot( NUMPERGROUP > 1 )

    N <- length(crit.holdconstant)

    NCRIT.HOLDCONSTANT <- iaw$.mkusefullist( N, NUMPERGROUP)

    ## first sort by criterion.holdconstant
    grouporder <- order(crit.holdconstant)
    group1num <- as.integer( (NCRIT.HOLDCONSTANT-1)/NUMPERGROUP ) + 1
    names(grouporder) <- paste0("f", group1num )
    zerolist <- grouporder[is.na(group1num)]
    names(crit.holdconstant)[grouporder] <- names(grouporder)  ## will be used next

    rankfun <- function(x) rank(x, ties.method="max")
    ## within each criterion.holdconstant group, rank the observations
    r <- ave(crit.spread, as.factor(names(crit.holdconstant)), FUN=rankfun)
    r[zerolist] <- NA
    r
}

if (FALSE) {


    stopifnot( identical( iaw$.mkusefullist( 9, 3 ) , c(1:9) ) )
    stopifnot( identical( iaw$.mkusefullist( 10, 3 ) , c(1:4,NA,5:9) ) )
    stopifnot( identical( iaw$.mkusefullist( 11, 3 ) , c(1:4,NA,NA,5:9) ) )
    stopifnot( identical( iaw$.mkusefullist( 15, 5 ) , 1:15 ) )
    stopifnot( identical( iaw$.mkusefullist( 16, 5 ) , c(1:7,NA,8:15) ) )
    stopifnot( identical( iaw$.mkusefullist( 17, 5 ) , c(1:7,NA,NA,8:15) ) )
    stopifnot( identical( iaw$.mkusefullist( 18, 5 ) , c(1:7,NA,NA,NA,8:15) ) )
    stopifnot( identical( iaw$.mkusefullist( 19, 5 ) , c(1:7,NA,NA,NA,NA,8:15) ) )
    stopifnot( identical( iaw$.mkusefullist( 20, 5 ) , c(1:20) ) )

    crit1 <- c(  4,  9,  13,  11,  5,   7,  10, 1,  12,  8, 3,  6,  2 )
    crit2 <- c( -1, 10, 104, 103, 22, -99, 102, 1, 105, 11, 2, 33, -2 )
    result <- iaw$doublesort( crit1, crit2, NUMPERGROUP=4 )
    stopifnot( identical(result , c(2, 1, 3, 2, 3, NA, 1, 3, 4, 2, 4, 4, 1) ) )
}
