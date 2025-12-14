
iaw$boxtextmonoline <- function( x, y, text, bg="white", ... ) {
    text( x, y, sapply( nchar(text), function(n) paste(rep("\U2588", n), collapse="") ), col=bg )
    text( x, y, text, ... )
}
