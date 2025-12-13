
iaw$dict.lookup <- function( dwords, dictionary, by=NULL, by.w=NULL, by.d=NULL ) {
                                        # bad style, but just (mostly symmetric) error checking
    if (is.null(by.d)) by.d <- by; if (is.null(by.w)) by.w <- by
    stopifnot( (!is.null(by.d)) & (!is.null(by.w)))
                                        # valid input checking
    stopifnot( is.data.frame( dwords ) ); stopifnot( is.data.frame( dictionary ) )
    stopifnot( nrow( dwords ) > 0 ); stopifnot( nrow( dictionary ) > 0 )
    stopifnot( is.character(by.w) ); stopifnot( is.character(by.d) )
    stopifnot( length(by.w)==1 ); stopifnot( length(by.d)==1)
    stopifnot( by.w %in% names(dwords) ); stopifnot( by.d %in% names(dictionary) )
                                        # you cannot give the words directly.  hash them first
    stopifnot( is.numeric( dwords[[by.w]] ) )
    stopifnot( is.numeric( dictionary[[by.d]] ) )
                                        # a dictionary should have only unique entries
    stopifnot( anyDuplicated( dictionary[[by.d]] ) == 0 )

                                        # the actual work
    toright <- dictionary[ match(dwords[[by.w]], dictionary[[by.d]]), ]
    cbind(dwords, toright[ , names(toright) != by.d ])
}


# Usage Example
#
# set.seed(0); K <- 1000; M <- K*K
# rint <- function( n, minv=0, maxv=NA ) sample( minv:(if (is.na(maxv)) n else maxv), n, repl=T )
# 
# L <- 1*M  ## only 100M entries for 1GB*4 of int data
# dictionary  <- data.frame( idictwords=sample(1:L), cost2print=rint(L, 1,100),  tiresomeness=rint(L, 100,200) )
# message("created dictionary")
# 
# ## look up 10+1 words
# dwords <- data.frame( imywords= c(dictionary[ sample(1:L, 10) , "idictwords"], -99),  frombook=rint(11, 200,300) )
# message("created my words")
# 
# print( system.time( o <- iaw$dict.lookup( dwords, dictionary, by.w= "imywords", by.d= "idictwords" ) ) )
# message("looked up my words in dictionary done")
# 
# print(o)
