
#' summary
#'
#' @name summary
#'
#'   a nice matrix like printout
#'
#'  @details verbose can be a choicelist.  functions can be lists
#'
#'  @usage iaw.summary (d, verbose = 0, name = NA, as.latex = FALSE, digits = 4, extend.f =function)
#' 	 iaw.summary (d, verbose = 0, name = NA, as.latex = FALSE, digits = 4, extend.f =list("function"=...,"name"=...))
#'
#'  @param usually a data frame
#'
#'  @return a data frame containing the summary information
#'
#'  @examples
#' 	dd <- data.frame( x=rnorm(100), y=rnorm(100), z=rnorm(100) ); dd$y[20] <- NA
#' 	iaw.summary(dd, verbose=c("mean", "sd"), as.latex=TRUE, extend.f = function(x) c(sqm=mean(x^2), cbm=mean(x^3)) )
#'
#! use kable for latex output


iaw$summary <- function (df, verbose = "X", digits = 4) {
    if (is.vector(df)|is.matrix(df)) df <- data.frame(df)
    (is.data.frame(df)) %or% "iaw.summary requires data frame, not {{class(d)}}"
    if (nrow(df)==0) return("no observations\n")

    wantedstats <- list()
    ## these designate choices defined below
    wantedstats[['p']] <- c("nok", "pctna", "mean", "sd", "tstat") 
    wantedstats[['sr']] <-    c("nok", "pctna", "mean", "var", "sd", "tstat", "sharpe", "min", "max")
    wantedstats[['sr252']] <- c("nok", "pctna", "mean252", "var252", "sd252", "tstat", "sharpe252")
    wantedstats[['x']] <- c(wantedstats[['p']], c("min", "median", "max"))
    wantedstats[['X']] <- c(wantedstats[['p']], c("pmost", "auto"))
    wantedstats[['a']] <- c(wantedstats[['p']], c("pall", "frcpos", "trimmn", "sd2", "auto"))
    if (verbose %in% names(wantedstats)) w <- wantedstats[[verbose]] else stop("Sorry, but I do not know wantedstats of {{verbose}}")


    nok <- function(x, na.rm=F) sum(!is.na(x))
    pctna <- function(x, na.rm=F) if (sum(is.na(x))==0) -1 else as.integer(100*sum(is.na(x))/length(x))
    pall <- function(x, na.rm=T) quantile(x, c(0.0, 0.01, 0.05, 0.25, 0.50, 0.75, 0.95, 0.99, 1.00), na.rm=na.rm )
    pmost <- function(x, na.rm=T) quantile(x, c(0.0, 0.10, 0.50, 0.90, 1.00), na.rm=na.rm )
    trimmn <- function(x, na.rm=F) mean(x, trim=0.05, na.rm=na.rm)
    frcpos <- function(x, na.rm=F) round(mean(x>0, na.rm=na.rm),2)
    tstat <- function(x, na.rm=F) round(mean(x, na.rm=na.rm)/sd(x, na.rm=na.rm)*sqrt( nok(x) ),2)
    sharpe <- function(x, na.rm=F) round(mean(x, na.rm=na.rm)/sd(x, na.rm=na.rm),4)
    mean252 <- function(x, na.rm=F) round(252*mean(x, na.rm=na.rm),4)
    var252 <- function(x, na.rm=F) round(252*var(x, na.rm=na.rm),4)
    sd252 <- function(x, na.rm=F) round(sqrt(252)*sd(x, na.rm=na.rm),4)
    sharpe252 <- function(x, na.rm=F) round(sqrt(252)*mean(x, na.rm=na.rm)/sd(x, na.rm=na.rm),4)
    # sd2 <- function(x, na.rm=F) { quantile(x, 0.6745, na.rm=na.rm) - quantile(x, -0.6745, na.rm=na.rm) }
    sd2 <- function(x, na.rm=F) { pm <- 0.1915; quantile(x, 0.5+pm, na.rm=na.rm) - quantile(x, 0.5-pm, na.rm=na.rm) }
    auto <- function(x, na.rm=F) { suppressWarnings(cor( x[2:length(x)], x[1:(length(x)-1)], use="pair" )) }


    df <- as.data.frame( lapply(df, function(x) if(is.logical(x)) as.integer(x) else x) ) ## convert all T/F to 1/0

    nums <- unlist(lapply(df, is.numeric))
    if (all(!nums)) stop("Sorry, but not a single column seems to be numeric!")
    names.of.numerics <- names(nums[nums])

    ## bug in pmost---instead of 100%, it says pmost (in the colnames)
    describeonevar <- function(x, statswanted) {
        if (is.logical(x)) x <- as.numeric(x)
        if (!is.numeric(x)) return(NULL)
        s <- c()
        for (sw1 in statswanted) {
            s <- c(s, get(sw1)( x, na.rm=T ) )
            if (sw1 != "pall") names(s)[length(s)] <- sw1
        }
        s
    }

#    o <- do.call("rbind", mclapply( names.of.numerics, FUN=function(v) describeonevar(df[,v], w) ) )
    o <- t(simplify2array( lapply( names.of.numerics, FUN=function(v) describeonevar(df[,v], w) ) ))
    rownames(o)  <- names.of.numerics

    round( o, digits )
}

## for testing: df <- data.frame( a=1:20, bbbbb=rnorm(20), c=as.factor( 1:20 ), d=as.character(1:20), e= rep(NA, 20), f=c(1.5,c(1:19)) )
