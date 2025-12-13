
#' OOSLM
#'
#' @name ooslm
#'
#'  out-of-sample linear model
#'
#' @usage ooslm (formula, data, ...)
#'
#' @param formula on the data
#' @param the first observation
#' 
#' @return recursive residuals net of unconditional residuals
#'
#' @seealso 

iaw$ooslm <- function( formula, data, ... ) {
    library( strucchange )

    cond.esterr.standardized <- recresid( formula=formula, data=data, ... )
    K <- nrow(data) - length(cond.esterr.standardized)  ## for constant model, K=1
    cond.esterr <- cond.esterr.standardized * sqrt(1.0+1.0/(1:length(cond.esterr.standardized)))  ## not sure.
    cond.esterr <- c( rep(NA, K), cond.esterr)

    ## note: reports (yreal - yhat) / sqrt(1+1/(N-K)),
    ##  so multiply by sqrt(1+1/1:N) to recover unstandardized values, where N=1:(nrow(x)-K)

    get.lhs <- function(formula, data) {
        mf <- match.call(expand.dots = FALSE)
        m <- match(c("formula", "data"), names(mf), 0L)
        mf <- mf[c(1L, m)]
        mf$drop.unused.levels <- TRUE
        mf[[1L]] <- as.name("model.frame")
        mf <- eval(mf, parent.frame())
        y <- model.response(mf, "numeric")
        y
    }

    ## to.predict <- get.lhs( formula, data )
    to.predict <- data[[ all.vars(formula)[1] ]]  # , because all.vars( y ~ x1 + x3 ) gives "y", "x1", "x3"

    uncond.esterr <- sapply( 1:nrow(data), function(i) to.predict[i+1] - mean( to.predict[1:i] ) )
    ## uncond.esterr.standardized <- cond.esterr / sqrt(1.0+1.0/(1:length(cond.esterr)))  ## not sure.
    uncond.esterr <- head(c(NA,uncond.esterr), -1)

    ## message( length(uncond.esterr), " / ", length(cond.esterr), " / ", length(to.predict) )

    dr <- data.frame( delta.abserr=(abs(uncond.esterr) - abs(cond.esterr)), uncond.esterr, cond.esterr, to.predict )
    rownames(dr) <- rownames( data )
    dr
}

iaw$ooslm.test <- function() {
    N <- 10
    set.seed( 0 )
    df <- data.frame( x= rnorm(N), y= (1:N)^2, z1= rnorm(N)*100, z2= rnorm(N) )
    formula <- y ~ z1 + z2
    iaw$ooslm( formula, df )
}


