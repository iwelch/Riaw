
#' OLM
#'
#' @name olm
#'
#' run an ordinary-least-squares model.  Most of the time, you will want to call printolm instead.
#'
#' @seealso printolm
#'

iaw$olm <- function (..., newey.west=(0), stdcoefs=TRUE, include.anova=T, keep.pval=F, truncate.T=T, digits=4) {

    use("lmtest", "coeftest")

    ## R is painfully error-tolerant. I prefer reasonable and immediate error warnings.
    stopifnot( (is.vector(newey.west))&(length(newey.west)==1)|(is.numeric(newey.west)) )
    stopifnot( (is.vector(stdcoefs))&(length(stdcoefs)==1)|(is.logical(stdcoefs)) )
    ## stopifnot( (is.vector(keepx))&(length(keepx)==1)|(is.logical(keepx)) )
    ## I wish I could check lm()'s argument, but I cannot.

    lmo <- stats:::lm(..., x=TRUE, na.action=na.exclude)  ## keep NA values
    ## note that both the lmo$x matrix and residuals(lmo) omit all NA observations

    ## following two statements need to come early to apply to lmo (before we add stuff to it)

    if (newey.west >= 0) {
        library(sandwich)
        nw.se <- coeftest( lmo, vcov= NeweyWest( lmo, lag= newey.west, prewhite=F ) )[,2]
    }
    if (stdcoefs) {
        xsd <- apply( lmo$x, 2, sd)
        ysd <- sd( lmo$model[,1] )
        stdcoefs.v <- lmo$coefficients*xsd/ysd
    }

    basesigma <- sigma(lmo)

    full.x.matrix <- lmo$x
    lmo <- stats:::summary.lm( lmo )  ## add the summary.lm object; changes contents of lmo
    lmo$x <- full.x.matrix

    ## 3 is the T stat
    lmo$coefficients[,3] <-round(lmo$coefficients[,3],2)
    if (truncate.T) lmo$coefficients[,3] <- iaw$winsorize.level( lmo$coefficients[,3], -99.99, 99.99 )

    if (!keep.pval) lmo$coefficients <- lmo$coefficients[,1:3]

    if (newey.west>=0) {
        lmo$coefficients <- cbind(lmo$coefficients, nw.se)
        colnames(lmo$coefficients)[ncol(lmo$coefficients)] <- paste0(" se.nw(",newey.west,")")
        lmo$coefficients <- cbind(lmo$coefficients, round(lmo$coefficients[,1]/nw.se,2))
        K <- ncol(lmo$coefficients)
        if (truncate.T) lmo$coefficients[,K] <- iaw$winsorize.level( lmo$coefficients[,K], -99.99, 99.99 )
        colnames(lmo$coefficients)[K] <- paste0("T.nw(",newey.west,")")
    }

    if (stdcoefs) {
        lmo$coefficients <- cbind(lmo$coefficients, stdcoefs.v )
        colnames(lmo$coefficients)[ncol(lmo$coefficients)] <- " stdcoefs"
    }

    if (include.anova) {
        aa <- (data.frame( summary(aov(...))[[1]])[,2, drop=FALSE])[,1]
        sumaa <- sum(aa)  ## includes residuals!!
        aa <- aa/sumaa
        lmo$residssq <- round(aa[length(aa)], digits)
        aa <- c(0.0, aa[1:(length(aa)-1)])  ## chops them off
        lmo$coefficients <- cbind(lmo$coefficients, aa )
        colnames(lmo$coefficients)[ncol(lmo$coefficients)] <- "pctsumsq"
    } else {
        lmo$residssq <- NA
    }

    lmo$coefficients <- round(lmo$coefficients,digits)
    colnames(lmo$coefficients)[1:3] <- c("coefest", "  se.ols", "T.ols")
    lmo$sigma <- round(basesigma,8)
    
    lmo
}
